
open MyUtil
open Syntax
open Errors
open Env


let main (fpath_in : string) (dir_out : string) (is_verbose : bool) =
  try
    let abspath_in =
      let dir = Sys.getcwd () in
      make_absolute_path dir fpath_in
    in
    let (pkgnameopt, sources) =
        let (_, extopt) = Core.Filename.split_extension abspath_in in
        match extopt with
        | Some("sest") ->
            let source = SourceLoader.single abspath_in in
            (None, [source])

        | Some(ext) ->
            failwith (Printf.sprintf "TODO: unrecognizable extensions '%s'" ext)

        | _ ->
            if is_existing_directory abspath_in then
              let absdir_in = abspath_in in
              let pkgs = PackageLoader.main absdir_in in
              match pkgs with
              | [ (_, config) ] ->
                  let pkg = SourceLoader.main config in
                  (Some(pkg.SourceLoader.space_name), pkg.SourceLoader.modules)

              | _ ->
                  failwith "TODO: package having dependency"
            else
              failwith "TODO: non-existent directory"
    in
    let (_, outacc) =
      let (tyenv, _) = Primitives.initial_environment in
      sources |> List.fold_left (fun (tyenv, outacc) source ->
        let abspath = source.SourceLoader.source_path in
        let modident = source.SourceLoader.module_identifier in
        let utmod = source.SourceLoader.module_content in
        Logging.begin_to_typecheck abspath;
        let (tyenv, (oidset, sigr), sname, binds) = Typechecker.main tyenv modident utmod in
        if is_verbose then display_structure 0 sigr;
        let outacc = Alist.extend outacc (sname, binds) in
        (tyenv, outacc)
      ) (tyenv, Alist.empty)
    in
    let (_, gmap) = Primitives.initial_environment in
    outacc |> Alist.to_list |> List.fold_left (fun gmap (sname, binds) ->
      OutputErlangCode.main dir_out gmap ~package_name:pkgnameopt ~module_name:sname binds
    ) gmap |> ignore;
    OutputErlangCode.write_primitive_module dir_out
  with
  | Sys_error(msg) ->
      Logging.report_system_error msg;
      exit 1

  | Failure(msg) ->
      Logging.report_unsupported_feature msg;
      exit 1

  | PackageLoader.PackageError(e) ->
      Logging.report_package_error e;
      exit 1

  | SourceLoader.SyntaxError(LexerError(e)) ->
      Logging.report_lexer_error e;
      exit 1

  | SourceLoader.SyntaxError(ParseError(rng)) ->
      Logging.report_parser_error rng;
      exit 1

  | ConfigError(e) ->
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
