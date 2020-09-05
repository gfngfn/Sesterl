
open Syntax


module FreeRowHashTable = Hashtbl.Make(FreeRowID)

module BoundRowHashTable = Hashtbl.Make(BoundRowID)


let free_row_hash_table =
  FreeRowHashTable.create 1024


let bound_row_hash_table =
  BoundRowHashTable.create 1024


let free_id_table : mono_base_kind FreeIDHashTable.t =
  FreeIDHashTable.create 1024


let register_free_row (frid : FreeRowID.t) (labmap : mono_type LabelAssoc.t) : unit =
  FreeRowHashTable.add free_row_hash_table frid labmap


let get_free_row (frid : FreeRowID.t) : mono_type LabelAssoc.t =
  match FreeRowHashTable.find_opt free_row_hash_table frid with
  | None         -> assert false
  | Some(labmap) -> labmap


let register_bound_row (brid : BoundRowID.t) (plabmap : poly_type LabelAssoc.t) : unit =
  BoundRowHashTable.add bound_row_hash_table brid plabmap


let get_bound_row (brid : BoundRowID.t) : poly_type LabelAssoc.t =
  match BoundRowHashTable.find_opt bound_row_hash_table brid with
  | None          -> assert false
  | Some(plabmap) -> plabmap


let register_free_id (fid : FreeID.t) (bkd : mono_base_kind) : unit =
  FreeIDHashTable.add free_id_table fid bkd


let get_free_id (fid : FreeID.t) : mono_base_kind =
  match FreeIDHashTable.find_opt free_id_table fid with
  | None      -> assert false
  | Some(bkd) -> bkd
