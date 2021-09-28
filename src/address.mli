
open Syntax

type element =
  | Member      of module_name
  | FunctorBody of { arg : module_name }

type t

val root : t

val append_member : module_name -> t -> t

val append_functor_body : arg:module_name -> t -> t

val to_list : t -> element list
