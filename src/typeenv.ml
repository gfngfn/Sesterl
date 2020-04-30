
open Syntax


module VarMap = Map.Make(String)


type val_entry = {
  typ  : poly_type;
  name : name;
}

type t = {
  vals  : val_entry VarMap.t;
  types : unit;  (* temporary*)
}


let empty = {
  vals  = VarMap.empty;
  types = ();  (* temporary *)
}


let add_val x pty name tyenv =
  let vals = tyenv.vals |> VarMap.add x { typ = pty; name = name } in
  { tyenv with vals = vals; }


let find_val_opt x tyenv =
  tyenv.vals |> VarMap.find_opt x |> Option.map (fun entry ->
    (entry.typ, entry.name)
  )


let fold_val f tyenv acc =
  VarMap.fold (fun x entry acc -> f x entry.typ acc) tyenv.vals acc
