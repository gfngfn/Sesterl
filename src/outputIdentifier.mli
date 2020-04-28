
type t

val local : string -> t option

val global : string -> t option

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val fresh : unit -> t

val unused : t
