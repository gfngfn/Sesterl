open Syntax

type t

val empty : t

val add : identifier -> poly_type -> t -> t

val find_opt : identifier -> t -> poly_type option

val fold : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a
