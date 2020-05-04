
type t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val fresh : string -> t

val pp : Format.formatter -> t -> unit
