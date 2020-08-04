
open MyUtil
open Syntax

module GraphImpl = Graph.Persistent.Digraph.Abstract(String)

module TraverseImpl = Graph.Traverse.Dfs(GraphImpl)

module TopologicalImpl = Graph.Topological.Make(GraphImpl)

module PathMap = Map.Make(String)

type vertex = GraphImpl.V.t

type entry = {
  vertex  : vertex;
}

type t = {
  paths   : entry PathMap.t;
  main    : GraphImpl.t;
}


let empty : t = {
  paths = PathMap.empty;
  main  = GraphImpl.empty;
}


let find_vertex (fpath : absolute_path) (graph : t) : vertex option =
  graph.paths |> PathMap.find_opt fpath |> Option.map (fun entry -> entry.vertex)


let add_vertex (abspath : absolute_path) (graph : t) : t * vertex =
  let vertex = GraphImpl.V.create abspath in
  let entry = { vertex = vertex; } in
  let graph =
    {
      paths = graph.paths |> PathMap.add abspath entry;
      main  = GraphImpl.add_vertex graph.main vertex;
    }
  in
  (graph, vertex)


let add_edge ~depending:(vertex2 : vertex) ~depended:(vertex1 : vertex) (graph : t) : t =
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let topological_sort (graph : t) : (absolute_path list) option =
  if TraverseImpl.has_cycle graph.main then
    None
  else
    let acc =
      TopologicalImpl.fold (fun vertex acc ->
        let abspath = GraphImpl.V.label vertex in
        Alist.extend acc abspath
      ) graph.main Alist.empty
    in
    Some(Alist.to_list acc)
