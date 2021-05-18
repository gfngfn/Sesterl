
type t

val pp : Format.formatter -> t -> unit

val from_lexbuf : Lexing.lexbuf -> t

val from_positions : Lexing.position * Lexing.position -> t

val dummy : string -> t

val unite : t -> t -> t

val get_file_name : t -> string

val get_start_line : t -> int
