
open Syntax
open Env

val primitive_module_name : string

val decode_option_function : string

type source_definition = {
  identifier  : string;
  typ         : poly_type;
}

type target_definition = {
  target_name : string;
  parameters  : string list;
  code        : string;
}

type primitive_definition = {
  source : source_definition option;
  target : target_definition;
}

val primitive_definitions : primitive_definition list

val option_type : mono_type -> mono_type

val initial_environment : Typeenv.t * global_name_map
