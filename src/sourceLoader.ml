
open MyUtil
open Syntax
open Errors

exception SyntaxError of syntax_error


type loaded_module = {
  source_path       : absolute_path;
  module_identifier : module_name ranged;
  module_content    : untyped_module;
}

type loaded_package = {
  space_name   : space_name;
  modules      : loaded_module list;
  dependencies : ConfigLoader.dependency list;
}


let listup_sources_in_directory (dir : absolute_dir) : absolute_path list =
  let filenames = Core.Sys.ls_dir dir in
  filenames |> List.filter_map (fun filename ->
    if Core.String.is_suffix filename ~suffix:".sest" then
      Some(Filename.concat dir filename)
    else
      None
  )


let read_source (fpath_in : absolute_path) : ((module_name ranged) list * (module_name ranged * untyped_module), syntax_error) result =
  let inc = open_in fpath_in in
  let lexbuf = Lexing.from_channel inc in
  let fname = Filename.basename fpath_in in
  let res =
    let open ResultMonad in
    ParserInterface.process ~fname:fname lexbuf >>= fun (deps, modident, utmod) ->
    return (deps, (modident, utmod))
  in
  close_in inc;
  res

(*
module ContentMap = Map.Make(String)

type reading_state = {
  loaded : (module_name ranged * untyped_module) ContentMap.t;
  graph  : FileDependencyGraph.t;
}


(* `read_source_recursively abspath` lists up all the parsed source files
   on which `abspath` depends either directly or indirectly,
   and sorts them in a topological order according to the dependency among them. *)
let read_source_recursively (abspath : absolute_path) : (absolute_path * (module_name ranged * untyped_module)) list =
  let rec aux (state : reading_state) (vertex : FileDependencyGraph.vertex) (abspath : absolute_path) : reading_state =
    Logging.begin_to_parse abspath;
    let (deps, content) =
      match read_source abspath with
      | Ok(source) -> source
      | Error(e)   -> raise (SyntaxError(e))
    in
    let loaded = state.loaded |> ContentMap.add abspath content in
    deps |> List.fold_left (fun state (_, abspath_sub) (* TEMPORARY *) ->
      let graph = state.graph in
      match graph |> FileDependencyGraph.find_vertex abspath_sub with
      | Some(vertex_sub) ->
        (* If the depended source file has already been parsed *)
          let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
          { state with graph = graph }

      | None ->
        (* If the depended source file has not been parsed yet *)
          let (graph, vertex_sub) = graph |> FileDependencyGraph.add_vertex abspath_sub in
          let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
          aux { state with graph = graph } vertex_sub abspath_sub
    ) { state with loaded = loaded }
  in
  let state =
    let (graph, vertex) = FileDependencyGraph.empty |> FileDependencyGraph.add_vertex abspath in
    let state = { graph = graph; loaded = ContentMap.empty } in
    aux state vertex abspath
  in
  match FileDependencyGraph.topological_sort state.graph with
  | Error(cycle) ->
      raise (ConfigError(CyclicFileDependencyFound(cycle)))

  | Ok(sources) ->
      sources |> List.map (fun abspath ->
        match state.loaded |> ContentMap.find_opt abspath with
        | None          -> assert false
        | Some(content) -> (abspath, content)
      )
*)

let read_sources (abspaths : absolute_path list) : loaded_module list =

  (* First, add vertices to the graph for solving dependency. *)
  let (graph, nmmap) =
    abspaths |> List.fold_left (fun (graph, nmmap) abspath ->
      match read_source abspath with
      | Ok(source) ->
          let (_, ((_, modnm), _)) = source in
          begin
            match nmmap |> ModuleNameMap.find_opt modnm with
            | Some((abspath0, _, _)) ->
                raise (ConfigError(MultipleModuleOfTheSameName(modnm, abspath0, abspath)))

            | None ->
                let (graph, vertex) = graph |> FileDependencyGraph.add_vertex modnm in
                let nmmap = nmmap |> ModuleNameMap.add modnm (abspath, vertex, source) in
                (graph, nmmap)
          end

      | Error(e) ->
          raise (SyntaxError(e))
            (* TODO: change error into warning *)

    ) (FileDependencyGraph.empty, ModuleNameMap.empty)
  in

  (* Second, add dependency edges to the graph. *)
  let graph =
    graph |> ModuleNameMap.fold (fun modnm (_, vertex, source) graph ->
      let (deps, _) = source in
      deps |> List.fold_left (fun graph (rng, modnm_dep) ->
        match nmmap |> ModuleNameMap.find_opt modnm_dep with
        | None ->
            raise (ConfigError(ModuleNotFound(rng, modnm_dep)))

        | Some((_, vertex_dep, _)) ->
            graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_dep

      ) graph
    ) nmmap
  in

  match FileDependencyGraph.topological_sort graph with
  | Error(cycle) ->
      raise (ConfigError(CyclicFileDependencyFound(cycle)))

  | Ok(sorted_paths) ->
      sorted_paths |> List.map (fun modnm ->
        match nmmap |> ModuleNameMap.find_opt modnm with
        | None ->
            assert false

        | Some((abspath, _, source)) ->
            let (_, (modident, utmod)) = source in
            {
              source_path       = abspath;
              module_identifier = modident;
              module_content    = utmod;
            }
      )



let single (abspath_in : absolute_path) : loaded_module =
  match read_source abspath_in with
  | Error(e) ->
      raise (SyntaxError(e))

  | Ok((deps, content)) ->
      if List.length deps > 0 then
        raise (ConfigError(CannotSpecifyDependency))
      else
        let (modident, utmod) = content in
        {
          source_path       = abspath_in;
          module_identifier = modident;
          module_content    = utmod;
        }


let main (config : ConfigLoader.config) : loaded_package =
  let srcdirs = config.ConfigLoader.source_directories in
  let abspaths = srcdirs |> List.map listup_sources_in_directory |> List.concat in
  let sources = read_sources abspaths in
  let spkgname =
    let pkgname = config.package_name in
    match OutputIdentifier.space_of_package_name pkgname with
    | Some(spkgname) -> spkgname
    | None           -> raise (ConfigError(InvalidPackageName(pkgname)))
  in
  {
    space_name   = spkgname;
    modules      = sources;
    dependencies = config.ConfigLoader.dependencies;
  }
