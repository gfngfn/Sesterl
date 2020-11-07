
open MyUtil
open Errors
open Syntax


type dependency_source =
  | Local of absolute_path

type dependency = {
  dependency_name   : package_name;
  dependency_source : dependency_source;
}

type git_spec =
  | Tag    of string
  | Ref    of string
  | Branch of string

type erlang_library_source =
  | ErlangLibFromHex of { version : string }
  | ErlangLibFromGit of { repository : string; git_spec : git_spec }

type erlang_library = {
  erlang_library_name   : string;
  erlang_library_source : erlang_library_source;
}

type erlang_config = {
  output_directory    : absolute_dir;
  erlang_dependencies : erlang_library list;
}


let default_erlang_config (confdir : absolute_dir) : erlang_config =
  {
    output_directory    = Core.Filename.concat confdir "_generated";
    erlang_dependencies = [];
  }


type config = {
  package_name       : package_name;
  main_module_name   : module_name;
  source_directories : absolute_dir list;
  dependencies       : dependency list;
  erlang_config      : erlang_config;
}


let source_decoder (confdir : absolute_dir) : dependency_source YamlDecoder.decoder =
  let open YamlDecoder in
  get "type" string >>= function
  | "local" ->
      get "directory" string >>= fun dirstr ->
      succeed (Local(make_absolute_path confdir dirstr))

  | other ->
      failure (Printf.sprintf "unsupported type '%s' for specifying dependency sources" other)


let dependency_decoder (confdir : absolute_dir) : dependency YamlDecoder.decoder =
  let open YamlDecoder in
  get "name" string >>= fun name ->
  get "source" (source_decoder confdir) >>= fun source ->
  succeed {
    dependency_name   = name;
    dependency_source = source;
  }


let git_spec_decoder : git_spec YamlDecoder.decoder =
  let open YamlDecoder in
  get "type" string >>= function
  | "tag" ->
      get "value" string >>= fun tag ->
      succeed (Tag(tag))

  | "ref" ->
      get "value" string >>= fun hash ->
      succeed (Ref(hash))

  | "branch" ->
      get "value" string >>= fun branch ->
      succeed (Branch(branch))

  | other ->
      failure (Printf.sprintf "unsupported type '%s' for specifying sources from Git" other)


let erlang_library_decoder : erlang_library_source YamlDecoder.decoder =
  let open YamlDecoder in
  get "type" string >>= function
  | "hex" ->
      get "version" string >>= fun version ->
      succeed (ErlangLibFromHex{ version = version })

  | "git" ->
      get "repository" string >>= fun repository ->
      get "spec" git_spec_decoder >>= fun git_spec ->
    succeed (ErlangLibFromGit{ repository = repository; git_spec = git_spec })

  | other ->
      failure (Printf.sprintf "unsupported type '%s' for specifying dependency sources" other)


let erlang_dependency_decoder : erlang_library YamlDecoder.decoder =
  let open YamlDecoder in
  get "name" string >>= fun name ->
  get "source" erlang_library_decoder >>= fun erlsrc ->
  succeed {
    erlang_library_name   = name;
    erlang_library_source = erlsrc;
  }


let erlang_config_decoder (confdir : absolute_dir) : erlang_config YamlDecoder.decoder =
  let open YamlDecoder in
  get_or_else "output_directory" string "_generated" >>= fun dir_out ->
  get_or_else "erlang_dependencies" (list erlang_dependency_decoder) [] >>= fun erldeps ->
  succeed {
    output_directory    = dir_out;
    erlang_dependencies = erldeps;
  }


let config_decoder (confdir : absolute_dir) : config YamlDecoder.decoder =
  let open YamlDecoder in
  get "package" string >>= fun package_name ->
  get "source_directories" (list string) >>= fun srcdirs ->
  get "main_module" string >>= fun main_module_name ->
  get_or_else "dependencies" (list (dependency_decoder confdir)) [] >>= fun dependencies ->
  get_or_else "erlang" (erlang_config_decoder confdir) (default_erlang_config confdir) >>= fun erlang_config ->
  let config =
    {
      package_name       = package_name;
      main_module_name   = main_module_name;
      source_directories = List.map (make_absolute_path confdir) srcdirs;
      dependencies       = dependencies;
      erlang_config      = erlang_config;
    }
  in
(*
  Format.printf "name: %s, srcdirs: %a, main: %s\n"
    config.package_name
    (Format.pp_print_list Format.pp_print_string) config.source_directories
    config.main_module_path;  (* for debug *)
*)
  succeed config


let load (confpath : absolute_path) : (config, config_error) result =
  let open ResultMonad in
  begin
    try return (open_in confpath) with
    | Sys_error(_) -> err (ConfigFileNotFound(confpath))
  end >>= fun fin ->
  let confdir = Filename.dirname confpath in
  let s = Core.In_channel.input_all fin in
  close_in fin;
  YamlDecoder.run (config_decoder confdir) s |> map_err (fun e -> ConfigFileError(e))
