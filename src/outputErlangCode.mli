
open MyUtil
open Syntax

val main : string -> global_name_map -> package_name:(space_name option) -> module_name:space_name -> binding list -> global_name_map

val write_primitive_module : absolute_dir -> unit
