
open Syntax

type t

val empty : t

val add : identifier -> poly_type -> name -> t -> t

val find_opt : identifier -> t -> (poly_type * name) option

val fold : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a
