
type t

val pp : Format.formatter -> t -> unit

val from_lexbuf : Lexing.lexbuf -> t

val from_positions : Lexing.position * Lexing.position -> t

val dummy : string -> t

val unite : t -> t -> t
