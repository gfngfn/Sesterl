
type level = int

type t

val equal : t -> t -> bool

val hash : t -> int

val initialize : unit -> unit

val fresh : level -> t

val get_level : t -> level

val update_level : t -> level -> unit

val pp : Format.formatter -> t -> unit
