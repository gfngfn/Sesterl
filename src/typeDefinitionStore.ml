
open Syntax


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
  | None    -> assert false
  | Some(v) -> v


let free_id_table : mono_base_kind FreeIDHashTable.t =
  FreeIDHashTable.create 1024


let add_free_id (fid : FreeID.t) (bkd : mono_base_kind) : unit =
  FreeIDHashTable.add free_id_table fid bkd


let find_free_id (fid : FreeID.t) : mono_base_kind =
  match FreeIDHashTable.find_opt free_id_table fid with
  | None      -> assert false
  | Some(bkd) -> bkd
