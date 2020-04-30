
type t

val make : string -> t

val pp : Format.formatter -> t -> unit

val output : t -> string
