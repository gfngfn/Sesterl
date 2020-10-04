
open Syntax

val process : fname:string -> Lexing.lexbuf -> (string list * module_name ranged * untyped_module, Range.t) result
