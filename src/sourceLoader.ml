open MyUtil
open Syntax
open Errors

exception SyntaxError of syntax_error

type loaded_module = {
  source_path : absolute_path;
  module_identifier : module_name ranged;
  signature : untyped_signature option;
  module_content : untyped_module;
  dependencies : module_name ranged list;
}

type loaded_package = {
  space_name : space_name;
  aux_modules : loaded_module list;
  main_module : loaded_module;
  test_modules : loaded_module list;
}

let listup_sources_in_directory (dir : absolute_dir) : absolute_path list =
  let filenames = Core.Sys.ls_dir dir in
  filenames
  |> List.filter_map (fun filename ->
         if Core.String.is_suffix filename ~suffix:".sest" then
           Some (Core.Filename.concat dir filename)
         else None)

let read_source (abspath_in : absolute_path) : loaded_module =
  Logging.begin_to_parse abspath_in;
  let inc = open_in abspath_in in
  let lexbuf = Lexing.from_channel inc in
  let fname = Filename.basename abspath_in in
  let res =
    let open ResultMonad in
    ParserInterface.process ~fname lexbuf
    >>= fun (deps, modident, utsigopt, utmod) ->
    return
      {
        source_path = abspath_in;
        module_identifier = modident;
        signature = utsigopt;
        module_content = utmod;
        dependencies = deps;
      }
  in
  close_in inc;
  match res with Ok baremod -> baremod | Error err -> raise (SyntaxError err)

let resolve_dependency_scheme (nmmap_known : absolute_path ModuleNameMap.t)
    (baremods : loaded_module list) :
    loaded_module list * absolute_path ModuleNameMap.t =
  (* First, adds the vertices to the graph for solving dependency. *)
  let graph, nmmap =
    baremods
    |> List.fold_left
         (fun (graph, nmmap) baremod ->
           let _, modnm = baremod.module_identifier in
           let abspath = baremod.source_path in
           match nmmap |> ModuleNameMap.find_opt modnm with
           | Some (_, baremod0) ->
               let abspath0 = baremod0.source_path in
               raise
                 (ConfigError
                    (MultipleModuleOfTheSameName (modnm, abspath0, abspath)))
           | None -> (
               match nmmap_known |> ModuleNameMap.find_opt modnm with
               | Some abspath0 ->
                   raise
                     (ConfigError
                        (MultipleModuleOfTheSameName (modnm, abspath0, abspath)))
               | None ->
                   let graph, vertex =
                     graph |> FileDependencyGraph.add_vertex modnm
                   in
                   let nmmap =
                     nmmap |> ModuleNameMap.add modnm (vertex, baremod)
                   in
                   (graph, nmmap)))
         (FileDependencyGraph.empty, ModuleNameMap.empty)
  in

  (* Second, adds the dependency edges to the graph. *)
  let graph =
    ModuleNameMap.fold
      (fun modnm (vertex, baremod) graph ->
        let deps = baremod.dependencies in
        deps
        |> List.fold_left
             (fun graph (rng, modnm_dep) ->
               match nmmap |> ModuleNameMap.find_opt modnm_dep with
               | None ->
                   if nmmap_known |> ModuleNameMap.mem modnm_dep then
                     (* If the depended one has already been resolved
                        (i.e. if the dependency is on a source file from a test file) *)
                     graph
                   else raise (ConfigError (ModuleNotFound (rng, modnm_dep)))
               | Some (vertex_dep, baremod_dep) ->
                   graph
                   |> FileDependencyGraph.add_edge ~depending:vertex
                        ~depended:vertex_dep)
             graph)
      nmmap graph
  in

  (* Finally, resolves dependency among Auxs. *)
  let resolved_auxs =
    match FileDependencyGraph.topological_sort graph with
    | Error cycle -> raise (ConfigError (CyclicFileDependencyFound cycle))
    | Ok sorted_paths ->
        sorted_paths
        |> List.map (fun modnm ->
               match nmmap |> ModuleNameMap.find_opt modnm with
               | None -> assert false
               | Some (_, baremod) -> baremod)
  in
  let nmmap_added =
    ModuleNameMap.fold
      (fun modnm (_, baremod) nmmap_added ->
        nmmap_added |> ModuleNameMap.add modnm baremod.source_path)
      nmmap ModuleNameMap.empty
  in
  (resolved_auxs, nmmap_added)

let resolve_dependency_among_auxiliary ~aux:(bareauxs : loaded_module list) :
    loaded_module list * absolute_path ModuleNameMap.t =
  resolve_dependency_scheme ModuleNameMap.empty bareauxs

let check_dependency_of_main_on_auxiliary
    (nmmap_aux : absolute_path ModuleNameMap.t) ~main:(baremain : loaded_module)
    : unit =
  baremain.dependencies
  |> List.iter (fun (rng, modnm_dep) ->
         if nmmap_aux |> ModuleNameMap.mem modnm_dep then ()
         else raise (ConfigError (ModuleNotFound (rng, modnm_dep))))

let resolve_dependency_among_test (nmmap_src : absolute_path ModuleNameMap.t)
    ~test:(baretests : loaded_module list) : loaded_module list =
  let resolved_tests, _ = resolve_dependency_scheme nmmap_src baretests in
  resolved_tests

let resolve_dependency ~aux:(bareauxs : loaded_module list)
    ~main:(baremain : loaded_module) ~test:(baretests : loaded_module list) :
    loaded_module list * loaded_module list =
  let resolved_auxs, nmmap_aux =
    resolve_dependency_among_auxiliary ~aux:bareauxs
  in
  check_dependency_of_main_on_auxiliary nmmap_aux ~main:baremain;
  let nmmap_src =
    let _, modnm_main = baremain.module_identifier in
    let abspath_main = baremain.source_path in
    nmmap_aux |> ModuleNameMap.add modnm_main abspath_main
  in
  let resolved_tests =
    resolve_dependency_among_test nmmap_src ~test:baretests
  in
  (resolved_auxs, resolved_tests)

let single (abspath_in : absolute_path) : loaded_module =
  let baremod = read_source abspath_in in
  let deps = baremod.dependencies in
  if List.length deps > 0 then raise (ConfigError CannotSpecifyDependency)
  else baremod

let separate_main_module (config : ConfigLoader.config)
    (baresrcs : loaded_module list) : loaded_module * loaded_module list =
  let main_module_name = config.ConfigLoader.main_module_name in
  let baremains, baresubs =
    baresrcs
    |> List.partition (fun baremod ->
           let _, modnm = baremod.module_identifier in
           String.equal modnm main_module_name)
  in
  match baremains with
  | [] ->
      let pkgname = config.ConfigLoader.package_name in
      raise (ConfigError (MainModuleNotFound (pkgname, main_module_name)))
  | baremain1 :: baremain2 :: _ ->
      let abspath1 = baremain1.source_path in
      let abspath2 = baremain2.source_path in
      raise
        (ConfigError
           (MultipleModuleOfTheSameName (main_module_name, abspath1, abspath2)))
  | [ baremain ] -> (baremain, baresubs)

let main ~(requires_tests : bool) (config : ConfigLoader.config) :
    loaded_package =
  let srcdirs =
    let srcreldirs = config.ConfigLoader.source_directories in
    let confdir = config.ConfigLoader.config_directory in
    srcreldirs
    |> List.map (function RelativeDir reldir ->
           Core.Filename.concat confdir reldir)
  in
  let testdirs =
    let testreldirs = config.ConfigLoader.test_directories in
    let confdir = config.ConfigLoader.config_directory in
    testreldirs
    |> List.map (function RelativeDir reldir ->
           Core.Filename.concat confdir reldir)
  in
  let abspaths_src =
    srcdirs |> List.map listup_sources_in_directory |> List.concat
  in
  let abspaths_test =
    testdirs |> List.map listup_sources_in_directory |> List.concat
  in
  let baresrcs = abspaths_src |> List.map read_source in
  let baretests =
    if requires_tests then abspaths_test |> List.map read_source else []
  in
  let baremain, bareauxs = separate_main_module config baresrcs in
  let resolved_auxs, resolved_tests =
    resolve_dependency ~aux:bareauxs ~main:baremain ~test:baretests
  in
  let spkgname =
    let pkgname = config.package_name in
    match OutputIdentifier.space_of_package_name pkgname with
    | Some spkgname -> spkgname
    | None -> raise (ConfigError (InvalidPackageName pkgname))
  in
  {
    space_name = spkgname;
    aux_modules = resolved_auxs;
    main_module = baremain;
    test_modules = resolved_tests;
  }
