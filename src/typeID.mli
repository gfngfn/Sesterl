
type t

val fresh : string -> t

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
