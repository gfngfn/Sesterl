
open Syntax


module VarMap = Map.Make(String)


type entry = {
  typ  : poly_type;
  name : name;
}

type t = entry VarMap.t


let empty = VarMap.empty


let add x pty name tyenv =
  tyenv |> VarMap.add x { typ = pty; name = name }


let find_opt x tyenv =
  tyenv |> VarMap.find_opt x |> Option.map (fun entry ->
    (entry.typ, entry.name)
  )


let fold f =
  VarMap.fold (fun x entry acc -> f x entry.typ acc)
