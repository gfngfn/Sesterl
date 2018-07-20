
open Syntax

type t

val empty : t

val add : identifier -> mono_type -> t -> t

val find_opt : identifier -> t -> mono_type option
