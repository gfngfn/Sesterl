open MyUtil
open Syntax
module GraphImpl = Graph.Persistent.Digraph.Abstract (String)
module ComponentImpl = Graph.Components.Make (GraphImpl)
module TopologicalImpl = Graph.Topological.Make (GraphImpl)
module PathMap = Map.Make (String)

type vertex = GraphImpl.V.t

type entry = { vertex : vertex }

type t = { paths : entry PathMap.t; main : GraphImpl.t }

let empty : t = { paths = PathMap.empty; main = GraphImpl.empty }

let find_vertex (fpath : absolute_path) (graph : t) : vertex option =
  graph.paths |> PathMap.find_opt fpath
  |> Option.map (fun entry -> entry.vertex)

let add_vertex (abspath : absolute_path) (graph : t) : t * vertex =
  let vertex = GraphImpl.V.create abspath in
  let entry = { vertex } in
  let graph =
    {
      paths = graph.paths |> PathMap.add abspath entry;
      main = GraphImpl.add_vertex graph.main vertex;
    }
  in
  (graph, vertex)

let add_edge ~depending:(vertex2 : vertex) ~depended:(vertex1 : vertex)
    (graph : t) : t =
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }

let find_loop g =
  GraphImpl.fold_vertex
    (fun v acc ->
      match acc with
      | Some _ -> acc
      | None -> if GraphImpl.mem_edge g v v then Some v else None)
    g None

let topological_sort (graph : t) :
    (absolute_path list, absolute_path cycle) result =
  match find_loop graph.main with
  | Some v -> Error (Loop (GraphImpl.V.label v))
  | None -> (
      let sccs = ComponentImpl.scc_list graph.main in
      match
        sccs
        |> List.find_map (fun vertices ->
               match vertices with
               | [] -> assert false
               | [ _ ] -> None
               | v1 :: v2 :: vrest ->
                   Some
                     (Cycle
                        (List2.make v1 v2 vrest |> List2.map GraphImpl.V.label)))
      with
      | Some cycle -> Error cycle
      | None ->
          let acc =
            TopologicalImpl.fold
              (fun vertex acc ->
                let abspath = GraphImpl.V.label vertex in
                Alist.extend acc abspath)
              graph.main Alist.empty
          in
          Ok (Alist.to_list acc))
