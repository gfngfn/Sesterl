
open Syntax
open Env

val primitive_module_name : string

type primitive_definition = {
  identifier  : string;
  typ         : poly_type;
  target_name : string;
  parameters  : string list;
  code        : string;
}

val primitive_definitions : primitive_definition list

val initial_environment : Typeenv.t * global_name_map
