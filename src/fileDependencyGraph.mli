
open MyUtil
open Syntax

type vertex

type t

val empty : t

val find_vertex : absolute_path -> t -> vertex option

val add_vertex : absolute_path -> t -> t * vertex

val add_edge : depending:vertex -> depended:vertex -> t -> t

val topological_sort : t -> (absolute_path list, absolute_path list) result
(** [topological_sort g] returns either:
 {ul
   {- [Ok(paths)] where [paths] is the sorted list of absolute paths of source files, or}
   {- [Error(paths)] where [paths] is a list of mutually dependent source files.}
 } *)
