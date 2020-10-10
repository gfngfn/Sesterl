
open Syntax
open Errors

val process : fname:string -> Lexing.lexbuf -> ((module_name ranged) list * module_name ranged * untyped_signature option * untyped_module, syntax_error) result
