type t

val fresh : Address.t -> string -> t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val name : t -> string

val address : t -> Address.t

val pp : Format.formatter -> ?seen_from:Address.t -> t -> unit

val pp_raw : Format.formatter -> t -> unit
