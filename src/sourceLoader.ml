
open MyUtil
open Syntax
open Errors

exception SyntaxError of syntax_error


type loaded_module = {
  source_path       : absolute_path;
  module_identifier : module_name ranged;
  signature         : untyped_signature option;
  module_content    : untyped_module;
  dependencies      : (module_name ranged) list;
}

type loaded_package = {
  space_name   : space_name;
  submodules   : loaded_module list;
  main_module  : loaded_module;
}


let listup_sources_in_directory (dir : absolute_dir) : absolute_path list =
  let filenames = Core.Sys.ls_dir dir in
  filenames |> List.filter_map (fun filename ->
    if Core.String.is_suffix filename ~suffix:".sest" then
      Some(Filename.concat dir filename)
    else
      None
  )


let read_source (abspath_in : absolute_path) : (loaded_module, syntax_error) result =
  Logging.begin_to_parse abspath_in;
  let inc = open_in abspath_in in
  let lexbuf = Lexing.from_channel inc in
  let fname = Filename.basename abspath_in in
  let res =
    let open ResultMonad in
    ParserInterface.process ~fname:fname lexbuf >>= fun (deps, modident, utsigopt, utmod) ->
    return {
      source_path       = abspath_in;
      module_identifier = modident;
      signature         = utsigopt;
      module_content    = utmod;
      dependencies      = deps;
    }
  in
  close_in inc;
  res


let resolve_dependency (baremods : loaded_module list) : loaded_module list =

  (* First, add vertices to the graph for solving dependency. *)
  let (graph, nmmap) =
    baremods |> List.fold_left (fun (graph, nmmap) baremod ->
      let (_, modnm) = baremod.module_identifier in
      let abspath = baremod.source_path in
      begin
        match nmmap |> ModuleNameMap.find_opt modnm with
        | Some((_, baremod0)) ->
            let abspath0 = baremod0.source_path in
            raise (ConfigError(MultipleModuleOfTheSameName(modnm, abspath0, abspath)))

        | None ->
            let (graph, vertex) = graph |> FileDependencyGraph.add_vertex modnm in
            let nmmap = nmmap |> ModuleNameMap.add modnm (vertex, baremod) in
            (graph, nmmap)
      end
    ) (FileDependencyGraph.empty, ModuleNameMap.empty)
  in

  (* Second, add dependency edges to the graph. *)
  let graph =
    graph |> ModuleNameMap.fold (fun modnm (vertex, baremod) graph ->
      let deps = baremod.dependencies in
      deps |> List.fold_left (fun graph (rng, modnm_dep) ->
        match nmmap |> ModuleNameMap.find_opt modnm_dep with
        | None ->
            raise (ConfigError(ModuleNotFound(rng, modnm_dep)))

        | Some((vertex_dep, _)) ->
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
        | None               -> assert false
        | Some((_, baremod)) -> baremod
      )



let single (abspath_in : absolute_path) : loaded_module =
  match read_source abspath_in with
  | Error(e) ->
      raise (SyntaxError(e))

  | Ok(baremod) ->
      let deps = baremod.dependencies in
      if List.length deps > 0 then
        raise (ConfigError(CannotSpecifyDependency))
      else
        baremod


let main (config : ConfigLoader.config) : loaded_package =
  let srcdirs = config.ConfigLoader.source_directories in
  let main_module_name = config.ConfigLoader.main_module_name in
  let abspaths = srcdirs |> List.map listup_sources_in_directory |> List.concat in
  let baremods =
    abspaths |> List.map (fun abspath ->
      match read_source abspath with
      | Ok(baremod) -> baremod
      | Error(e)    -> raise (SyntaxError(e))
    )
  in
  let (baremains, baresubs) =
    baremods |> List.partition (fun baremod ->
      let (_, modnm) = baremod.module_identifier in
      String.equal modnm main_module_name
    )
  in
  let main =
    match baremains with
    | [] ->
        let pkgname = config.ConfigLoader.package_name in
        raise (ConfigError(MainModuleNotFound(pkgname, main_module_name)))

    | _ :: _ :: _ ->
        assert false

    | [ baremain ] ->
        baremain
  in
  let subs = resolve_dependency baresubs in
  let spkgname =
    let pkgname = config.package_name in
    match OutputIdentifier.space_of_package_name pkgname with
    | Some(spkgname) -> spkgname
    | None           -> raise (ConfigError(InvalidPackageName(pkgname)))
  in
  {
    space_name  = spkgname;
    submodules  = subs;
    main_module = main;
  }
