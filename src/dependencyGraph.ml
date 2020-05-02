
open Syntax

module TypeIDMap = Map.Make(TypeID)

module GraphImpl = Graph.Persistent.Digraph.Abstract(TypeID)

module TraverseImpl = Graph.Traverse.Dfs(GraphImpl)

type t = {
  labels : (type_name ranged * GraphImpl.V.t) TypeIDMap.t;
  main   : GraphImpl.t;
}


let empty : t =
  {
    labels = TypeIDMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (tyid : TypeID.t) (tyident : type_name ranged) (graph : t) : t =
  let vertex = GraphImpl.V.create tyid in
  {
    labels = graph.labels |> TypeIDMap.add tyid (tyident, vertex);
    main   = GraphImpl.add_vertex graph.main vertex;
  }


let get_vertex_token map (tyid : TypeID.t) : GraphImpl.V.t =
  match map |> TypeIDMap.find_opt tyid with
  | None            -> assert false
  | Some(_, vertex) -> vertex


let add_edge (tyid1 : TypeID.t) (tyid2 : TypeID.t) (graph : t) : t =
  let map = graph.labels in
  let vertex1 = get_vertex_token map tyid1 in
  let vertex2 = get_vertex_token map tyid2 in
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let has_cycle (graph : t) : bool =
(* --
   TODO: make this function return one cycle for proof
   Since ocamlgraph does not provide APIs for such a use case,
   it requires inspecting how `Graph.Traverse.Dfs(_).has_cycle` is implemented.
   -- *)
  TraverseImpl.has_cycle graph.main
