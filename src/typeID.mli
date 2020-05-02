
type t

val fresh : string -> t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
