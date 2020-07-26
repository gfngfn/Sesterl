
open Syntax

type vertex

type t

type file_info = string * (module_name ranged * untyped_module)

val empty : t

val add_vertex : file_info -> t -> (t * vertex) option

val add_edge : depending:vertex -> depended:vertex -> t -> t * vertex

val topological_sort : t -> (file_info list) option
