
open Syntax
open Env


let synonym_table : (BoundID.t list * poly_type) SynonymIDHashTable.t =
  SynonymIDHashTable.create 1024


let find_synonym_type (sid : TypeID.Synonym.t) : BoundID.t list * poly_type =
  match SynonymIDHashTable.find_opt synonym_table sid with
  | None    -> assert false
  | Some(v) -> v


let add_synonym_type (sid : TypeID.Synonym.t) (typarams : BoundID.t list) (ptyreal : poly_type) : unit =
  SynonymIDHashTable.add synonym_table sid (typarams, ptyreal)


let variant_table : (BoundID.t list * constructor_branch_map) VariantIDHashTable.t =
  VariantIDHashTable.create 1024


let add_variant_type (vid : TypeID.Variant.t) (typarams : BoundID.t list) (ctorbrs : constructor_branch_map) : unit =
  VariantIDHashTable.add variant_table vid (typarams, ctorbrs)


let find_variant_type (vid : TypeID.Variant.t) : BoundID.t list * constructor_branch_map =
  match VariantIDHashTable.find_opt variant_table vid with
  | None    ->
      Format.printf "find_variant_type> NOT FOUND: %a\n" TypeID.Variant.pp vid;
      assert false
  | Some(v) -> v
