
open MyUtil
open Syntax


type dependency_source =
  | Local of absolute_path

type dependency = {
  dependency_name   : package_name;
  dependency_source : dependency_source;
}

type config = {
  package_name       : package_name;
  main_module_name   : module_name;
  source_directories : absolute_dir list;
  dependencies       : dependency list;
}


let source_decoder (confdir : absolute_dir) : dependency_source YamlDecoder.decoder =
  let open YamlDecoder in
  let ( >>= ) = bind in
  get "type" string >>= function
  | "local" ->
      get "directory" string >>= fun dirstr ->
      succeed (Local(make_absolute_path confdir dirstr))

  | other ->
      failure (Printf.sprintf "unsupported type '%s' for specifying dependency sources" other)


let dependency_decoder (confdir : absolute_dir) : dependency YamlDecoder.decoder =
  let open YamlDecoder in
  let ( >>= ) = bind in
  get "name" string >>= fun name ->
  get "source" (source_decoder confdir) >>= fun source ->
  succeed {
    dependency_name   = name;
    dependency_source = source;
  }


let config_decoder (confdir : absolute_dir) : config YamlDecoder.decoder =
  let open YamlDecoder in
  let ( >>= ) = bind in
  get "package" string >>= fun package_name ->
  get "source_directories" (list string) >>= fun srcdirs ->
  get "main_module" string >>= fun main_module_name ->
  get_or_else "dependencies" (list (dependency_decoder confdir)) [] >>= fun dependencies ->
  let config =
    {
      package_name       = package_name;
      main_module_name   = main_module_name;
      source_directories = List.map (make_absolute_path confdir) srcdirs;
      dependencies       = dependencies;
    }
  in
(*
  Format.printf "name: %s, srcdirs: %a, main: %s\n"
    config.package_name
    (Format.pp_print_list Format.pp_print_string) config.source_directories
    config.main_module_path;  (* for debug *)
*)
  succeed config


let load (confpath : absolute_path) : (config, YamlDecoder.error) result =
  let open ResultMonad in
  let fin = open_in confpath in
  let confdir = Filename.dirname confpath in
  let s = Core.In_channel.input_all fin in
  YamlDecoder.run (config_decoder confdir) s
