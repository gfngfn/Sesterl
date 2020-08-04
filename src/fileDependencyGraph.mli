
open MyUtil
open Syntax

type vertex

type t

val empty : t

val find_vertex : absolute_path -> t -> vertex option

val add_vertex : absolute_path -> t -> t * vertex

val add_edge : depending:vertex -> depended:vertex -> t -> t

val topological_sort : t -> (absolute_path list) option
