type variable = string

type type_variable = string

type term = string

type typ = string

type entry =
  | EnterScope of variable * type_variable * entry list
  | Destruct of term * entry list list * typ
  | Equation of typ * typ
  | Solution of type_variable * typ

type t = entry list

let initial () = []
