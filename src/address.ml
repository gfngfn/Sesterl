
open MyUtil
open Syntax


type element =
  | Member      of module_name
  | FunctorBody of { arg : module_name }

type t = element Alist.t


let root =
  Alist.empty


let append_member (modnm : module_name) (address : t) =
  Alist.extend address (Member(modnm))


let append_functor_body ~arg:(modnm : module_name) (address : t) =
  Alist.extend address (FunctorBody{ arg = modnm })


let to_list (address : t) =
  Alist.to_list address
