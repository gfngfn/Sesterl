
open MyUtil


type config = {
  package_name       : string;
  main_module        : string;
  source_directories : string list;
}


let config_decoder : config YamlDecoder.decoder =
  let open YamlDecoder in
  let ( >>= ) = bind in
  get "package" string >>= fun package_name ->
  get "source_directories" (list string) >>= fun source_directories ->
  get "main_module" string >>= fun main_module ->
  succeed { package_name; main_module; source_directories }


let load (srcpath : string) : (config, YamlDecoder.error) result =
  let open ResultMonad in
  let fin = open_in srcpath in
  let s = Core.In_channel.input_all fin in
  YamlDecoder.run config_decoder s
