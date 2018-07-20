
exception UnidentifiedToken of Range.t * string
exception SeeEndOfFileInComment of Range.t


type identifier = Range.t * string


let pp_identifier ppf (_, s) =
  Format.fprintf ppf "\"%s\"" s


type untyped_ast = Range.t * untyped_ast_main
  [@printer (fun ppf (_, utastmain) -> pp_untyped_ast_main ppf utastmain)]

and untyped_ast_main =
  | Bool   of bool
  | Int    of int
  | Var    of identifier
  | Lambda of identifier * untyped_ast
  | Apply  of untyped_ast * untyped_ast
  | If     of untyped_ast * untyped_ast * untyped_ast
  | LetIn  of identifier * untyped_ast * untyped_ast
[@@deriving show]
