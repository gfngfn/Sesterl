
open MyUtil
open Syntax

val main : string -> global_name_map -> package_name:space_name -> module_name:space_name -> binding list -> global_name_map

val write_primitive_module : package_name:space_name -> absolute_dir -> unit
