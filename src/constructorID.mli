
type t

val make : string -> t option

val pp : Format.formatter -> t -> unit

val output : t -> string
