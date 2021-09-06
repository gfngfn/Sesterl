
open MyUtil
open Syntax
open Errors
open Env


exception InvalidExternalSpec of string


let catch_error (k : unit -> unit) =
  try
    k ()
  with
  | Sys_error(msg) ->
      Logging.report_system_error msg;
      exit 1

  | Failure(msg) ->
      Logging.report_unsupported_feature msg;
      exit 1

  | InvalidExternalSpec(s) ->
      Logging.report_invalid_external_spec s;
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

  | Typechecker.TypeError(e) ->
      Logging.report_type_error e;
      exit 1


let build (fpath_in : string) (dir_out_spec : string option) (is_verbose : bool) (externals : string list) =
  catch_error (fun () ->
    let current_directory = Sys.getcwd () in
    let abspath_in =
      make_absolute_path current_directory fpath_in
    in
    let external_map =
      externals |> List.fold_left (fun map s ->
        match String.split_on_char ':' s with
        | [pkgname; path_in] ->
            let absdir = make_absolute_path current_directory path_in in
            map |> ExternalMap.add pkgname absdir

        | _ ->
            raise (InvalidExternalSpec(s))

      ) ExternalMap.empty
    in
    let (pkgs, absdir_out, absdir_test_out) =
        let (_, extopt) = Core.Filename.split_extension abspath_in in
        match extopt with
        | Some("sest") ->
            let source = SourceLoader.single abspath_in in
            let pkgs = [ (None, [], source) ] in
            let absdir_out =
              match dir_out_spec with
              | None          -> raise (ConfigError(NoOutputSpecForSingleSource))
              | Some(dir_out) -> append_dir current_directory (RelativeDir(dir_out))
            in
            (pkgs, absdir_out, absdir_out)

        | Some(ext) ->
            raise (ConfigError(UnrecognizableExtension(ext)))

        | _ ->
            assert (is_existing_directory abspath_in);
              (* The existence of given directories has been checked by 'cmdliner'. *)
            let absdir_in = abspath_in in
            let (pkgconfigs, main_config) = PackageLoader.main external_map absdir_in in
            let pkgs =
              pkgconfigs |> List.map (fun (_, config) ->
                let requires_tests =
                  String.equal config.ConfigLoader.package_name main_config.ConfigLoader.package_name
                in
                let pkg = SourceLoader.main ~requires_tests config in
                (Some(pkg.SourceLoader.space_name), pkg.SourceLoader.submodules, pkg.SourceLoader.main_module)
              )
            in
            (pkgs,
             append_dir absdir_in main_config.erlang_config.output_directory,
             append_dir absdir_in main_config.erlang_config.test_output_directory)
    in

    (* Typecheck each package. *)
    let (tyenv, _) = Primitives.initial_environment in
    let (_, pkgoutsacc) =
      pkgs |> List.fold_left (fun (tyenv, outsacc) pkg ->
        let (pkgnameopt, submods, mainmod) = pkg in
        let (tyenv, outs) = PackageChecker.main is_verbose tyenv submods mainmod in
        (tyenv, Alist.extend outsacc (pkgnameopt, outs))
      ) (tyenv, Alist.empty)
    in

    let spec =
      {
        module_name_output_spec = DottedCamels;
      }
    in

    (* Generate and output code corresponding to each package. *)
    Core.Unix.mkdir_p absdir_out;
    Core.Unix.mkdir_p absdir_test_out;
    let (_, gmap) = Primitives.initial_environment in
    pkgoutsacc |> Alist.to_list |> List.fold_left (fun gmap (pkgnameopt, outs) ->
      outs |> List.fold_left (fun gmap out ->
        let sname = out.PackageChecker.space_name in
        let imod = (out.PackageChecker.attribute, out.PackageChecker.bindings) in
        let absdir = if out.PackageChecker.is_for_test then absdir_test_out else absdir_out in
        OutputErlangCode.main spec absdir gmap ~package_name:pkgnameopt ~module_name:sname imod
      ) gmap
    ) gmap |> ignore;
    OutputErlangCode.write_primitive_module absdir_out
  )


let config (fpath_in : string) =
  catch_error (fun () ->
    let absdir_in =
      let dir = Sys.getcwd () in
      make_absolute_path dir fpath_in
    in
    let absdir_out = absdir_in in
    let config = PackageLoader.load_config absdir_in in
    OutputRebarConfig.main absdir_out config
  )


let flag_output : (string option) Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(value (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_verbose : bool Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Makes reports more detailed." in
  Arg.(value (flag (info [ "verbose" ] ~doc)))


let flag_packages : (string list) Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify paths of external packages." in
  Arg.(value (opt_all string [] (info [ "p"; "package" ] ~docv:"PACKAGE" ~doc)))


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let command_build =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(const build $ arg_in $ flag_output $ flag_verbose $ flag_packages)
  in
  let info : Term.info =
    Term.info "build"
  in
  (term, info)


let command_config =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(const config $ arg_in)
  in
  let info : Term.info =
    Term.info "config"
  in
  (term, info)


let command_main =
  let open Cmdliner in
  let term : unit Term.t =
    Term.(ret (const (`Error(true, "No subcommand specified."))))
  in
  let info : Term.info =
    Term.info ~version:Constants.semantic_version "sesterl"
  in
  (term, info)


let () =
  let open Cmdliner in
  let subcommands =
    [
      command_build;
      command_config;
    ]
  in
  Term.(exit (eval_choice command_main subcommands))
