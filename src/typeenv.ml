
open Syntax


module VarMap = Map.Make(String)

type val_entry = {
  typ  : poly_type;
  name : name;
  mutable is_used : bool;
}

module TypeMap = Map.Make(String)

type type_entry = {
  type_id         : TypeID.t;
  type_parameters : type_parameter_assoc;
  constructors    : constructor_branch_map;
}

type t = {
  vals  : val_entry VarMap.t;
  types : type_entry TypeMap.t;
}


let empty = {
  vals  = VarMap.empty;
  types = TypeMap.empty;
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


let add_type (tynm : type_name) (tyid : TypeID.t) (typaramassoc : type_parameter_assoc) (ctormap : constructor_branch_map) (tyenv : t) : t =
  let entry =
    {
      type_id         = tyid;
      type_parameters = typaramassoc;
      constructors    = ctormap;
    }
  in
  { tyenv with types = tyenv.types |> TypeMap.add tynm entry }
