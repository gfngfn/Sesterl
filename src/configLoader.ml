
open MyUtil


type config = {
  package_name       : string;
  main_module_path   : absolute_path;
  source_directories : absolute_dir list;
}


let config_decoder (confdir : absolute_dir) : config YamlDecoder.decoder =
  let open YamlDecoder in
  let ( >>= ) = bind in
  get "package" string >>= fun package_name ->
  get "source_directories" (list string) >>= fun srcdirs ->
  get "main_module" string >>= fun mainpath ->
  let config =
    {
      package_name       = package_name;
      main_module_path   = make_absolute_path confdir mainpath;
      source_directories = List.map (make_absolute_path confdir) srcdirs;
    }
  in

  Format.printf "name: %s, srcdirs: %a, main: %s\n"
    config.package_name
    (Format.pp_print_list Format.pp_print_string) config.source_directories
    config.main_module_path;

  succeed config


let load (confpath : absolute_path) : (config, YamlDecoder.error) result =
  let open ResultMonad in
  let fin = open_in confpath in
  let confdir = Filename.dirname confpath in
  let s = Core.In_channel.input_all fin in
  YamlDecoder.run (config_decoder confdir) s
