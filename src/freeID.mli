
type t

val equal : t -> t -> bool

val initialize : unit -> unit

val fresh : unit -> t

val pp : Format.formatter -> t -> unit
