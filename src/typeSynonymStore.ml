
open Syntax

module SynonymIDHashTable = Hashtbl.Make(TypeID.Synonym)


let table : (BoundID.t list * poly_type) SynonymIDHashTable.t =
  SynonymIDHashTable.create 1024


let find_synonym_type (sid : TypeID.Synonym.t) : BoundID.t list * poly_type =
  match SynonymIDHashTable.find_opt table sid with
  | None    -> assert false
  | Some(v) -> v


let add_synonym_type (sid : TypeID.Synonym.t) (typarams : BoundID.t list) (ptyreal : poly_type) : unit =
  SynonymIDHashTable.add table sid (typarams, ptyreal)
