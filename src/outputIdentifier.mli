
type answer =
  | Local    of string
  | Global   of string * int
  | Operator of string

type t

val local : string -> t

val global : string -> int -> t

val global_operator : string -> t

val output : t -> answer

val pp : Format.formatter -> t -> unit

val fresh : unit -> t

val unused : t
