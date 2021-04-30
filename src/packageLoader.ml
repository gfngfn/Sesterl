
open MyUtil
open Syntax
open Errors


exception PackageError of package_error


let load_config absdir_in =
  let abspath_in = Core.Filename.concat absdir_in Constants.config_file_name in
  match ConfigLoader.load abspath_in with
  | Error(e)   -> raise (ConfigError(e))
  | Ok(config) -> config


let load_repository (repo_uri : string) (git_spec : git_spec) =
  failwith "TODO: load_repository" (*
  match git_spec with
  | ConfigLoader.Ref(commit_hash) ->
      let absdir_sub_cache_opt =
        match GitCache.find pkgname with
        | Some(commit_hash_cache, absdir_sub) ->
            if String.equal commit_hash commit_hash_cache then
              Some(absdir_sub)
            else
              None

        | None ->
            None
      in
      begin
        match absdir_sub_cache_opt with
        | None             -> GitClient.sync_fetch_and_checkout uri commit_hash
        | Some(absdir_sub) -> absdir_sub
      end

  | ConfigLoader.Branch(_) ->
      failwith "TODO: Branch; unsupported"

  | ConfigLoader.Tag(_) ->
      failwith "TODO: Tag; unsupported"
*)


module PackageDirMap = Map.Make(String)
module PackageNameMap = Map.Make(String)

type reading_state = {
  loaded_dirs : ConfigLoader.config PackageDirMap.t;
  loaded_names : dependency_source PackageNameMap.t;
  graph        : PackageDependencyGraph.t;
}


let main (absdir : absolute_dir) : (absolute_dir * ConfigLoader.config) list =
  let rec aux (state : reading_state) (vertex : PackageDependencyGraph.vertex) (pkg : dependency_source) : reading_state =
    let config = load_config absdir in
    let pkgname = config.ConfigLoader.package_name in
    match state.loaded_names |> PackageNameMap.find_opt pkgname with
    | Some(pkg0) ->
        raise (PackageError(DuplicatedPackageName(pkgname, pkg0, pkg)))

    | None ->
        let loaded_dirs = state.loaded_dirs |> PackageDirMap.add absdir config in
        let loaded_names = state.loaded_names |> PackageNameMap.add pkgname pkg in
        let state = { state with loaded_dirs = loaded_dirs; loaded_names = loaded_names } in
        config.ConfigLoader.dependencies |> List.fold_left (fun state dependency ->
          let graph = state.graph in
          match dependency.ConfigLoader.dependency_source with
          | Local(absdir_sub) ->
              let absdir_sub =
                match canonicalize_path absdir_sub with
                | None         -> raise (PackageError(PackageDirNotFound(absdir_sub)))
                | Some(absdir) -> absdir
              in
              begin
                match graph |> PackageDependencyGraph.find_local_vertex absdir_sub with
                | Some(vertex_sub) ->
                  (* If the depended source file has already been parsed *)
                    let graph = graph |> PackageDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
                    { state with graph = graph }

                | None ->
                  (* If the depended source file has not been parsed yet *)
                    let (graph, vertex_sub) = graph |> PackageDependencyGraph.add_local_vertex absdir_sub in
                    let graph = graph |> PackageDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
                    aux { state with graph = graph } vertex_sub absdir_sub
              end

          | Git{ repository = uri; git_spec = git_spec } ->
              let _absdir_sub = load_repository uri git_spec in
              failwith "TODO: Git"
        ) state
  in
  let state =
    let (graph, vertex) = PackageDependencyGraph.empty |> PackageDependencyGraph.add_local_vertex absdir in
    let state =
      {
        graph        = graph;
        loaded_dirs  = PackageDirMap.empty;
        loaded_names = PackageNameMap.empty;
      }
    in
    aux state vertex (Local(absdir))
  in
  match PackageDependencyGraph.topological_sort state.graph with
  | Error(cycle) ->
      raise (ConfigError(CyclicPackageDependencyFound(cycle)))

  | Ok(absdirs) ->
      absdirs |> List.map (fun absdir ->
        match state.loaded_dirs |> PackageDirMap.find_opt absdir with
        | None         -> assert false
        | Some(config) -> (absdir, config)
      )
