
open Syntax
open Errors

val process : fname:string -> Lexing.lexbuf -> (string list * module_name ranged * untyped_module, syntax_error) result
