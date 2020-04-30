
open Syntax

type t

val empty : t

val add_val : identifier -> poly_type -> name -> t -> t

val find_val_opt : identifier -> t -> (poly_type * name) option

val fold_val : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a
