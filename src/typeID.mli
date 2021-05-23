
type t

val fresh : string list -> string -> t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val name : t -> string

val pp : Format.formatter -> t -> unit

val pp_raw : Format.formatter -> t -> unit
