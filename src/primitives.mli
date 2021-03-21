
open Syntax
open Env

val primitive_module_name : string

val decode_option_function : string

val decode_option_function_with_default : string

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

val option_type : Range.t -> ('a, 'b) typ -> ('a, 'b) typ

val list_type : Range.t -> ('a, 'b) typ -> ('a, 'b) typ

val format_type : Range.t -> ('a, 'b) typ -> ('a, 'b) typ

val frozen_type : Range.t -> rest:('a, 'b) typ -> receive:('a, 'b) session_map -> return:('a, 'b) typ -> ('a, 'b) typ

val initial_environment : Typeenv.t * global_name_map
