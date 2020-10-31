
open MyUtil
open Syntax
open Errors


exception PackageError of package_error


let load_config absdir_in =
  let abspath_in = Filename.concat absdir_in "package.yaml" in
  match ConfigLoader.load abspath_in with
  | Error(e)   -> raise (ConfigError(e))
  | Ok(config) -> config


module PackageDirMap = Map.Make(String)
module PackageNameMap = Map.Make(String)

type reading_state = {
  loaded_dirs : ConfigLoader.config PackageDirMap.t;
  loaded_names : absolute_dir PackageNameMap.t;
  graph        : FileDependencyGraph.t;
}


(* `main abspath` lists up all the packages
   on which the package at `abspath` depends either directly or indirectly,
   and sorts them in a topological order according to the dependency among them. *)
let main (absdir : absolute_dir) : (absolute_path * ConfigLoader.config) list =
  let rec aux (state : reading_state) (vertex : FileDependencyGraph.vertex) (absdir : absolute_dir) : reading_state =
    let config = load_config absdir in
    let pkgname = config.ConfigLoader.package_name in
    match state.loaded_names |> PackageNameMap.find_opt pkgname with
    | Some(absdir0) ->
        raise (PackageError(DuplicatedPackageName(pkgname, absdir0, absdir)))

    | None ->
        let loaded_dirs = state.loaded_dirs |> PackageDirMap.add absdir config in
        let loaded_names = state.loaded_names |> PackageNameMap.add pkgname absdir in
        let state = { state with loaded_dirs = loaded_dirs; loaded_names = loaded_names } in
        config.ConfigLoader.dependencies |> List.fold_left (fun state dependency ->
          let graph = state.graph in
          let ConfigLoader.Local(absdir_sub) = dependency.ConfigLoader.dependency_source in
          let absdir_sub = canonicalize_path absdir_sub in
          match graph |> FileDependencyGraph.find_vertex absdir_sub with
          | Some(vertex_sub) ->
            (* If the depended source file has already been parsed *)
              let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
              { state with graph = graph }

          | None ->
            (* If the depended source file has not been parsed yet *)
              let (graph, vertex_sub) = graph |> FileDependencyGraph.add_vertex absdir_sub in
              let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
              aux { state with graph = graph } vertex_sub absdir_sub
        ) state
  in
  let state =
    let (graph, vertex) = FileDependencyGraph.empty |> FileDependencyGraph.add_vertex absdir in
    let state =
      {
        graph        = graph;
        loaded_dirs  = PackageDirMap.empty;
        loaded_names = PackageNameMap.empty;
      }
    in
    aux state vertex absdir
  in
  match FileDependencyGraph.topological_sort state.graph with
  | Error(cycle) ->
      raise (ConfigError(CyclicFileDependencyFound(cycle)))

  | Ok(absdirs) ->
      absdirs |> List.map (fun absdir ->
        match state.loaded_dirs |> PackageDirMap.find_opt absdir with
        | None         -> assert false
        | Some(config) -> (absdir, config)
      )
