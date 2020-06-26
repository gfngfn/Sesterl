
type space
(** The type for abstracting module names in outputs. *)

type local

type global

type operator

type t =
  | Local    of local
  | Global   of global
  | Operator of operator

type global_answer = {
  module_names  : string list;
  function_name : string;
  arity         : int;
}

val space : string -> space option

val fresh : unit -> local

val local : string -> local option

val global : string -> int -> global option

val push_space : space -> global -> global

val operator : string -> operator

val unused : local

val output_local : local -> string

val output_global : global -> global_answer

val output_operator : operator -> string

val pp_space : Format.formatter -> space -> unit

val pp_local : Format.formatter -> local -> unit

val pp_global : Format.formatter -> global -> unit

val pp_operator : Format.formatter -> operator -> unit

val pp : Format.formatter -> t -> unit
