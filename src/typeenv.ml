
open Syntax


module VarMap = Map.Make(String)


type t = mono_type VarMap.t


let empty = VarMap.empty

let add = VarMap.add

let find_opt = VarMap.find_opt
