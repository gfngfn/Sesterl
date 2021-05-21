
type t

val from_upper_camel_case : string -> t option

val from_snake_case : string -> t option

val pp : Format.formatter -> t -> unit

val output : t -> string
