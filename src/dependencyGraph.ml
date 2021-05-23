
open Syntax

module IDMap = Map.Make(TypeID)

module GraphImpl = Graph.Persistent.Digraph.Abstract(TypeID)

module ComponentImpl = Graph.Components.Make(GraphImpl)

type t = {
  labels : (type_name ranged * GraphImpl.V.t) IDMap.t;
  main   : GraphImpl.t;
}


let empty : t =
  {
    labels = IDMap.empty;
    main   = GraphImpl.empty;
  }


let add_vertex (sid : TypeID.t) (tyident : type_name ranged) (graph : t) : t =
  let vertex = GraphImpl.V.create sid in
  {
    labels = graph.labels |> IDMap.add sid (tyident, vertex);
    main   = GraphImpl.add_vertex graph.main vertex;
  }


let get_vertex_token map (sid : TypeID.t) : GraphImpl.V.t =
  match map |> IDMap.find_opt sid with
  | None            -> assert false
  | Some(_, vertex) -> vertex


let add_edge (sid1 : TypeID.t) (sid2 : TypeID.t) (graph : t) : t =
  let map = graph.labels in
  let vertex1 = get_vertex_token map sid1 in
  let vertex2 = get_vertex_token map sid2 in
  { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


let extract_vertex_info graph v =
  let sid = GraphImpl.V.label v in
  match graph.labels |> IDMap.find_opt sid with
  | None               -> assert false
  | Some((tyident, _)) -> (sid, tyident)


let find_loop g =
  GraphImpl.fold_vertex (fun v acc ->
    match acc with
    | Some(_) -> acc
    | None    -> if GraphImpl.mem_edge g v v then Some(v) else None
  ) g None


let find_cycle (graph : t) : ((TypeID.t * type_name ranged) cycle) option =
  match find_loop graph.main with
  | Some(v) ->
      Some(Loop(extract_vertex_info graph v))

  | None ->
      let sccs = ComponentImpl.scc_list graph.main in
      sccs |> List.find_map (fun scc ->
        match scc with
        | [] ->
            assert false

        | [_] ->
            None

        | v1 :: v2 :: vrest ->
            let vs = List2.make v1 v2 vrest in
            Some(Cycle(vs |> List2.map (extract_vertex_info graph)))
      )
