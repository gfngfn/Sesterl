
open MyUtil
open Syntax
open Errors


exception PackageError of package_error


let load_config absdir_in =
  let abspath_in = Core.Filename.concat absdir_in Constants.config_file_name in
  let config =
    match ConfigLoader.load abspath_in with
    | Ok(config) -> config
    | Error(e)   -> raise (ConfigError(e))
  in
  match config.ConfigLoader.language_version with
  | None ->
      config

  | Some(language_version) ->
      if LanguageVersion.is_supported language_version then
        config
      else
        raise (ConfigError(UnsupportedLanguageVersion(language_version)))



module PackageDirMap = Map.Make(String)
module PackageNameMap = Map.Make(String)

type reading_state = {
  loaded_dirs : ConfigLoader.config PackageDirMap.t;
  loaded_names : absolute_dir PackageNameMap.t;
  graph        : FileDependencyGraph.t;
}


let main (external_map : external_map) (absdir : absolute_dir) : ((absolute_dir * ConfigLoader.config) list * ConfigLoader.config) =
  let rec aux ~(requires_test_deps : bool) (state : reading_state) (vertex : FileDependencyGraph.vertex) (absdir : absolute_dir) : ConfigLoader.config * reading_state =
    let config = load_config absdir in
    let pkgname = config.ConfigLoader.package_name in
    match state.loaded_names |> PackageNameMap.find_opt pkgname with
    | Some(absdir0) ->
        raise (PackageError(DuplicatedPackageName(pkgname, absdir0, absdir)))

    | None ->
        let loaded_dirs = state.loaded_dirs |> PackageDirMap.add absdir config in
        let loaded_names = state.loaded_names |> PackageNameMap.add pkgname absdir in
        let state = { state with loaded_dirs = loaded_dirs; loaded_names = loaded_names } in
        let state =
          let deps =
            if requires_test_deps then
              List.append
                config.ConfigLoader.dependencies
                config.ConfigLoader.test_dependencies
            else
              config.ConfigLoader.dependencies
          in
          deps |> List.fold_left (fun state dependency ->
            let graph = state.graph in
            let pkgname_sub = dependency.ConfigLoader.dependency_name in
            let absdir_sub =
              match dependency.ConfigLoader.dependency_source with
              | ConfigLoader.Local(absdir_sub) ->
                  absdir_sub

              | ConfigLoader.Git(_git_spec) ->
                  begin
                    match external_map |> ExternalMap.find_opt pkgname_sub with
                    | None             -> raise (PackageError(NotFoundInExternalMap(pkgname_sub, external_map)))
                    | Some(absdir_sub) -> absdir_sub
                  end
            in
            let absdir_sub =
              match canonicalize_path absdir_sub with
              | None         -> raise (PackageError(PackageDirNotFound(absdir_sub)))
              | Some(absdir) -> absdir
            in
            match graph |> FileDependencyGraph.find_vertex absdir_sub with
            | Some(vertex_sub) ->
              (* If the depended source file has already been parsed *)
                let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
                { state with graph = graph }

            | None ->
              (* If the depended source file has not been parsed yet *)
                let (graph, vertex_sub) = graph |> FileDependencyGraph.add_vertex absdir_sub in
                let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
                let (_, state) = aux ~requires_test_deps:false { state with graph = graph } vertex_sub absdir_sub in
                state
          ) state
        in
        (config, state)
  in
  let (config, state) =
    let (graph, vertex) = FileDependencyGraph.empty |> FileDependencyGraph.add_vertex absdir in
    let state =
      {
        graph        = graph;
        loaded_dirs  = PackageDirMap.empty;
        loaded_names = PackageNameMap.empty;
      }
    in
    aux ~requires_test_deps:true state vertex absdir
  in
  match FileDependencyGraph.topological_sort state.graph with
  | Error(cycle) ->
      raise (ConfigError(CyclicFileDependencyFound(cycle)))

  | Ok(absdirs) ->
      let pkgconfigs =
        absdirs |> List.map (fun absdir ->
          match state.loaded_dirs |> PackageDirMap.find_opt absdir with
          | None         -> assert false
          | Some(config) -> (absdir, config)
        )
      in
      (pkgconfigs, config)
