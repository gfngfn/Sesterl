
type answer =
  | Normal   of string
  | Operator of string

type t

val local : string -> t

val global : string -> t

val global_operator : string -> t

val output : t -> answer

val pp : Format.formatter -> t -> unit

val fresh : unit -> t

val unused : t
