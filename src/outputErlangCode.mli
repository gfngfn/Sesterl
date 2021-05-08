
open MyUtil
open Syntax
open IntermediateSyntax


val main : output_spec -> string -> name_map -> package_name:(space_name option) -> module_name:space_name -> binding list -> name_map
(** [main spec dir_out nmap ~package_name:pkgopt ~module_name:sname binds]
    produces Erlang source files corresponding to [binds] in the directory [dir_out].
    The name of the resulting module is determined by [pkgopt] and [sname].
    The path [dir_out] can be either relative or absolute,
    and the directory specified by the path must be guaranteed existent beforehand. *)

val write_primitive_module : absolute_dir -> unit
