
open MyUtil
open Syntax

module GraphImpl = Graph.Persistent.Digraph.Abstract(String)

module TraverseImpl = Graph.Traverse.Dfs(GraphImpl)

module TopologicalImpl = Graph.Topological.Make(GraphImpl)

module PathMap = Map.Make(String)

type vertex = GraphImpl.V.t

type file_info = string * (module_name ranged * untyped_module)

type entry = {
  vertex  : vertex;
  content : module_name ranged * untyped_module;
}

type t = {
  paths   : entry PathMap.t;
  main    : GraphImpl.t;
}


let empty : t = {
  paths = PathMap.empty;
  main  = GraphImpl.empty;
}


let add_vertex ((fpath, content) : file_info) (graph : t) : (t * vertex) option =
  if graph.paths |> PathMap.mem fpath then
    None
  else
    let vertex = GraphImpl.V.create fpath in
    let entry = { vertex = vertex; content = content; } in
    let graph =
      {
        paths = graph.paths |> PathMap.add fpath entry;
        main  = GraphImpl.add_vertex graph.main vertex;
      }
    in
    Some(graph, vertex)


let get_entry paths fpath =
  match paths |> PathMap.find_opt fpath with
  | None    -> assert false
  | Some(r) -> r


let add_edge ~depending:(vertex2 : vertex) ~depended:(vertex1 : vertex) (graph : t) : t * vertex =
  let graph = { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 } in
  (graph, vertex1)


let topological_sort (graph : t) : (file_info list) option =
  if TraverseImpl.has_cycle graph.main then
    None
  else
    let paths = graph.paths in
    let acc =
      TopologicalImpl.fold (fun vertex acc ->
        let fpath = GraphImpl.V.label vertex in
        let content = (get_entry paths fpath).content in
        Alist.extend acc (fpath, content)
      ) graph.main Alist.empty
    in
    Some(Alist.to_list acc)
