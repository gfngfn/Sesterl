
type t

type answer =
  | Local    of string
  | Global   of {
      module_names  : string list;
      function_name : string;
      arity         : int;
    }
  | Operator of string

val local : string -> t option

val global : string -> int -> t option

val global_operator : string -> t

val output : t -> answer

val pp : Format.formatter -> t -> unit

val fresh : unit -> t

val unused : t
