
open Syntax

type t

val empty : t

val add_vertex : TypeID.t -> type_name ranged -> t -> t

val add_edge : TypeID.t -> TypeID.t -> t -> t

val find_cycle : t -> ((TypeID.t * type_name ranged) cycle) option
