
open Syntax
open IntermediateSyntax
open Env
open Errors

exception Error of type_error

val typecheck_signature : address:address -> Typeenv.t -> untyped_signature -> module_signature abstracted

val main : Typeenv.t -> module_name ranged -> (module_signature abstracted) option -> untyped_module -> Typeenv.t * SigRecord.t abstracted * space_name * (ModuleAttribute.t * binding list)
