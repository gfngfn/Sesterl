
open MyUtil
open Syntax


type vertex = unit (* TODO *)

type t = unit (* TODO *)

let empty = ()

let find_local_vertex (_abspath : absolute_path) (_g : t) : vertex option =
  failwith "TODO: find_local_vertex"


let add_local_vertex (_abspath : absolute_path) (_g : t) : t * vertex =
  failwith "TODO: add_local_vertex"


let add_edge ~(depending : vertex) ~(depended : vertex) (_g : t) : t =
  failwith "TODO: add_edge"


let topological_sort (_g : t) : (absolute_path list, absolute_path cycle) result =
  failwith "TODO: topological_sort"
