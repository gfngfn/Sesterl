
open Syntax
open Env
open TypeError

exception Error of type_error

val main : Typeenv.t -> module_name ranged -> untyped_module -> Typeenv.t * SigRecord.t abstracted * space_name * binding list
