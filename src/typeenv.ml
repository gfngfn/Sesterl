
open Syntax


module VarMap = Map.Make(String)

type val_entry = {
  typ  : poly_type;
  name : name;
  mutable is_used : bool;
}

module TypeMap = Map.Make(String)

type type_entry =
  | Defining of {
      type_id              : TypeID.t;
      number_of_parameters : int;
    }
  | DefinedVariant of {
      id              : TypeID.Variant.t;
      type_parameters : BoundID.t list;
      branches        : constructor_branch_map;
    }
  | DefinedSynonym of {
      id              : TypeID.Synonym.t;
      type_parameters : BoundID.t list;
      real_type       : poly_type;
    }

module ConstructorMap = Map.Make(String)

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type t = {
  vals  : val_entry VarMap.t;
  types : type_entry TypeMap.t;
  constructors : constructor_entry ConstructorMap.t;
}


let empty = {
  vals  = VarMap.empty;
  types = TypeMap.empty;
  constructors = ConstructorMap.empty;
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


let add_variant_type (tynm : type_name) (vid : TypeID.Variant.t) (typarams : BoundID.t list) (brmap : constructor_branch_map) (tyenv : t) : t =
  let typesnew =
    let entry =
      DefinedVariant{
        id              = vid;
        type_parameters = typarams;
        branches        = brmap;
      }
    in
    tyenv.types |> TypeMap.add tynm entry
  in
  let ctorsnew =
    ConstructorBranchMap.fold (fun ctornm (ctorid, ptys) ctors ->
      let entry =
        {
          belongs         = vid;
          constructor_id  = ctorid;
          type_variables  = typarams;
          parameter_types = ptys;
        }
      in
      ctors |> ConstructorMap.add ctornm entry
    ) brmap tyenv.constructors
  in
  { tyenv with types = typesnew; constructors = ctorsnew }


let add_synonym_type (tynm : type_name) (sid : TypeID.Synonym.t) (typarams : BoundID.t list) (ptyreal : poly_type) (tyenv : t) : t =
  let entry =
    DefinedSynonym{
      id              = sid;
      type_parameters = typarams;
      real_type       = ptyreal
    }
  in
  { tyenv with types = tyenv.types |> TypeMap.add tynm entry }


let add_type_for_recursion (tynm : type_name) (tyid : TypeID.t) (paramlen : int) (tyenv : t) : t =
  let entry =
    Defining{
      type_id              = tyid;
      number_of_parameters = paramlen;
    }
  in
  { tyenv with types = tyenv.types |> TypeMap.add tynm entry }


let find_constructor (ctornm : constructor_name) (tyenv : t) =
  tyenv.constructors |> ConstructorMap.find_opt ctornm |> Option.map (fun entry ->
    (entry.belongs, entry.constructor_id, entry.type_variables, entry.parameter_types)
  )


let find_type (tynm : type_name) (tyenv : t) : (TypeID.t * int) option =
  tyenv.types |> TypeMap.find_opt tynm |> Option.map (function
  | Defining(record)       -> (record.type_id, record.number_of_parameters)
  | DefinedVariant(record) -> (TypeID.Variant(record.id), List.length record.type_parameters)
  | DefinedSynonym(record) -> (TypeID.Synonym(record.id), List.length record.type_parameters)
  )


let find_synonym_type (sid : TypeID.Synonym.t) (tyenv : t) : (BoundID.t list * poly_type) option =
  failwith "TODO: find_synonym_type"


let find_module_opt (m : module_name) (tyenv : t) : (concrete_signature * name) option =
  failwith "TODO: find_module_opt"
