
open Syntax

type t

val empty : t

val add : identifier -> poly_type -> t -> t

val find_opt : identifier -> t -> poly_type option
