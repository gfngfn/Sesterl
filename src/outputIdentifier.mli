
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
  function_name : string;
  arity         : int;
  has_option    : bool;
}

val space_of_module_name : string -> space option

val space_of_package_name : string -> space option

val fresh : unit -> local

val fresh_global_dummy : unit -> global

val generate_local : string -> local option

val generate_global : string -> suffix:string -> arity:int -> has_option:bool -> global option

val operator : string -> operator

val unused : local

module Space : sig

  type t = space

  val compare : t -> t -> int

end

module Local : sig

  type t = local

  val compare : t -> t -> int

end

module Global : sig

  type t = global

  val compare : t -> t -> int

end

val output_space_to_snake : space -> string

val output_space_to_camel : space -> string

val output_local : local -> string

val output_global : global -> global_answer

val output_operator : operator -> string

val pp_space : Format.formatter -> space -> unit

val pp_local : Format.formatter -> local -> unit

val pp_global : Format.formatter -> global -> unit

val pp_operator : Format.formatter -> operator -> unit

val pp : Format.formatter -> t -> unit
