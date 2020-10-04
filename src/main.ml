
open MyUtil
open Syntax
open Env


let main (fpath_in : string) (dir_out : string) (is_verbose : bool) =
  try
    let sources = SourceLoader.main fpath_in in
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

  | SourceLoader.SyntaxError(LexerError(e)) ->
      Logging.report_lexer_error e;
      exit 1

  | SourceLoader.SyntaxError(ParseError(rng)) ->
      Logging.report_parser_error rng;
      exit 1

  | SourceLoader.ConfigError(e) ->
      Logging.report_config_error e;
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
