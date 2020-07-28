
open MyUtil
open Syntax
open Env

exception CyclicFileDependencyFound


let make_absolute_path (dir : absolute_dir) (fpath : string) : absolute_path =
  if Filename.is_relative fpath then
    Core.Filename.realpath (Filename.concat dir fpath)
  else
    fpath


let read_source (fpath_in : absolute_path) : absolute_path list * (module_name ranged * untyped_module) =
  let inc = open_in fpath_in in
  let lexbuf = Lexing.from_channel inc in
  let (deps_raw, modident, utmod) = ParserInterface.process lexbuf in
  let deps =
    let dir = Filename.dirname fpath_in in
    deps_raw |> List.map (make_absolute_path dir)
  in
  close_in inc;
  (deps, (modident, utmod))


module ContentMap = Map.Make(String)

type reading_state = {
  loaded : (module_name ranged * untyped_module) ContentMap.t;
  graph  : FileDependencyGraph.t;
}


let read_source_recursively (abspath : absolute_path) : (absolute_path * (module_name ranged * untyped_module)) list =
  let rec aux (state : reading_state) (vertex : FileDependencyGraph.vertex) (abspath : absolute_path) : reading_state =
    Logging.begin_to_parse abspath;
    let (deps, content) = read_source abspath in
    let loaded = state.loaded |> ContentMap.add abspath content in
    deps |> List.fold_left (fun state abspath_sub ->
      let graph = state.graph in
      if graph |> FileDependencyGraph.mem abspath_sub then
        state
      else
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
  | None ->
      raise CyclicFileDependencyFound

  | Some(sources) ->
      sources |> List.map (fun abspath ->
        match state.loaded |> ContentMap.find_opt abspath with
        | None          -> assert false
        | Some(content) -> (abspath, content)
      )


let main (fpath_in : string) (dir_out : string) (is_verbose : bool) =
  try
    let abspath_in =
      let dir = Sys.getcwd () in
      make_absolute_path dir fpath_in
    in
    let sources = read_source_recursively abspath_in in
    let (_, outacc) =
      let (tyenv, _) = Primitives.initial_environment in
      sources |> List.fold_left (fun (tyenv, outacc) (abspath, (modident, utmod)) ->
        Logging.begin_to_typecheck abspath;
        let (tyenv, (oidset, sigr), sname, binds) = Typechecker.main tyenv modident utmod in
        if is_verbose then display_structure 0 sigr;
        let outacc = Alist.extend outacc (sname, binds) in
        (tyenv, outacc)
      ) (tyenv, Alist.empty)
    in
    let (_, gmap) = Primitives.initial_environment in
    outacc |> Alist.to_list |> List.fold_left (fun gmap (sname, binds) ->
      OutputErlangCode.main dir_out gmap sname binds
    ) gmap |> ignore
  with
  | Sys_error(msg) ->
      Logging.report_system_error msg;
      exit 1

  | Failure(msg) ->
      Logging.report_unsupported_feature msg;
      exit 1

  | LexerError(e) ->
      Logging.report_lexer_error e;
      exit 1

  | ParserInterface.Error(rng) ->
      Logging.report_parser_error rng;
      exit 1

  | CyclicFileDependencyFound ->
      Format.printf "! [Build error] cyclic file dependency found (TODO: detailed explanation)\n";
      exit 1

  | ConflictInSignature(rng, x) ->
      Format.printf "! [Type error] %a: '%s' is already defined in the signature\n"
        Range.pp rng
        x;
      exit 1

  | Typechecker.Error(e) ->
      Logging.report_type_error e;
      exit 1


let flag_output : string Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(required (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_verbose : bool Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Makes reports more detailed." in
  Arg.(value (flag (info [ "verbose" ] ~doc)))


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let command_term : unit Cmdliner.Term.t =
  let open Cmdliner in
  Term.(const main $ arg_in $ flag_output $ flag_verbose)


let command_info : Cmdliner.Term.info =
  let open Cmdliner in
  Term.info
    ~version: "0.0.1"
    "sesterl"


let () =
  let open Cmdliner in
  Term.(exit (eval (command_term, command_info)))
