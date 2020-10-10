
open Syntax
open Env
open Errors

exception Error of type_error

val main : Typeenv.t -> module_name ranged -> untyped_signature option -> untyped_module -> Typeenv.t * SigRecord.t abstracted * space_name * binding list
