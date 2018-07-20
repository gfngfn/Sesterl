
exception UnidentifiedToken of Range.t * string
exception SeeEndOfFileInComment of Range.t


type identifier = string


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


type untyped_ast = Range.t * untyped_ast_main
  [@printer (fun ppf (_, utastmain) -> pp_untyped_ast_main ppf utastmain)]

and untyped_ast_main =
  | Bool   of bool
  | Int    of int
  | Var    of identifier
  | Lambda of (Range.t * identifier) * untyped_ast
  | Apply  of untyped_ast * untyped_ast
  | If     of untyped_ast * untyped_ast * untyped_ast
  | LetIn  of identifier * untyped_ast * untyped_ast
[@@deriving show { with_path = false; } ]


type base_type =
  | IntType
  | BoolType
[@@deriving show { with_path = false; } ]

type 'a typ = Range.t * 'a typ_main

and 'a typ_main =
  | BaseType of base_type
  | FuncType of 'a typ * 'a typ
  | TypeVar  of 'a
[@@deriving show { with_path = false; } ]

type mono_type_var =
  | Free of FreeID.t
  | Link of mono_type

and mono_type = (mono_type_var ref) typ
[@@deriving show { with_path = false; } ]
