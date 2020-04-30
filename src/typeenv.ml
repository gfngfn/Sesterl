
open Syntax


module VarMap = Map.Make(String)


type val_entry = {
  typ  : poly_type;
  name : name;
  mutable is_used : bool;
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
  let entry =
    {
      typ  = pty;
      name = name;

      is_used = false;
    }
  in
  let vals = tyenv.vals |> VarMap.add x entry in
  { tyenv with vals = vals; }


let find_val_opt x tyenv =
  tyenv.vals |> VarMap.find_opt x |> Option.map (fun entry ->
    entry.is_used <- true;
    (entry.typ, entry.name)
  )


let is_val_properly_used x tyenv =
  tyenv.vals |> VarMap.find_opt x |> Option.map (fun entry ->
    entry.is_used
  )


let fold_val f tyenv acc =
  VarMap.fold (fun x entry acc -> f x entry.typ acc) tyenv.vals acc
