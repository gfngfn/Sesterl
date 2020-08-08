
open Syntax


module FreeRowHashTable = Hashtbl.Make(FreeRowID)


let free_row_hash_table =
  FreeRowHashTable.create 1024


let register_free_row (frid : FreeRowID.t) (labmap : mono_type LabelAssoc.t) : unit =
  FreeRowHashTable.add free_row_hash_table frid labmap


let get_free_row (frid : FreeRowID.t) : mono_type LabelAssoc.t =
  match FreeRowHashTable.find_opt free_row_hash_table frid with
  | None         -> assert false
  | Some(labmap) -> labmap
