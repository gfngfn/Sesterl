
open Syntax
open Env


module FreeRowHashTable = Hashtbl.Make(FreeRowID)

module BoundRowHashTable = Hashtbl.Make(BoundRowID)


let free_row_hash_table =
  FreeRowHashTable.create 1024


let bound_row_hash_table =
  BoundRowHashTable.create 1024


let register_free_row (frid : FreeRowID.t) (labset : LabelSet.t) : unit =
  FreeRowHashTable.add free_row_hash_table frid labset


let get_free_row (frid : FreeRowID.t) : LabelSet.t =
  match FreeRowHashTable.find_opt free_row_hash_table frid with
  | None         -> assert false
  | Some(labset) -> labset


let register_bound_row (brid : BoundRowID.t) (labset : LabelSet.t) : unit =
  BoundRowHashTable.add bound_row_hash_table brid labset


let get_bound_row (brid : BoundRowID.t) : LabelSet.t =
  match BoundRowHashTable.find_opt bound_row_hash_table brid with
  | None         -> assert false
  | Some(labset) -> labset
