
open MyUtil

module TupleList = List1

type module_name_output_spec =
  | SingleSnake
  | DottedCamels

type output_spec = {
  module_name_output_spec : module_name_output_spec;
}

type package_name = string

module ExternalMap = Map.Make(String)

type external_map = absolute_dir ExternalMap.t

type ('a, 'b) pure_or_effectful =
  | Pure      of 'a
  | Effectful of 'b
[@@deriving show { with_path = false; } ]

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a List2.t

type 'a ranged = Range.t * 'a

let pp_ranged ppsub ppf (_, x) =
  Format.fprintf ppf "%a" ppsub x

type identifier = string

type type_name = string
[@@deriving show { with_path = false; } ]

type kind_name = string
[@@deriving show { with_path = false; } ]

type constructor_name = string
[@@deriving show { with_path = false; } ]

type type_variable_name = string
[@@deriving show { with_path = false; } ]

type row_variable_name = string
[@@deriving show { with_path = false; } ]

type module_name = string
[@@deriving show { with_path = false; } ]

type signature_name = string
[@@deriving show { with_path = false; } ]

type label = string
[@@deriving show { with_path = false; } ]

module LabelAssoc : (sig
  include Map.S
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end with type key = string) = struct
  module Impl = Map.Make(String)
  include Impl

  let pp ppsub ppf labmap =
    labmap |> Impl.iter (fun label v ->
      Format.fprintf ppf "%s -> %a; " label ppsub v
    )
end

module LabelSet : (sig
  include Set.S
  val pp : Format.formatter -> t -> unit
end with type elt = label) = struct
  module Impl = Set.Make(String)
  include Impl

  let pp ppf labset =
    labset |> Impl.iter (fun label ->
      Format.fprintf ppf "%s,@ " label
    )
end


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


let pp_uchar ppf uchar =
  Format.fprintf ppf "U+%X" (Uchar.to_int uchar)


type module_name_chain =
  module_name ranged * (module_name ranged) list
[@@deriving show { with_path = false; } ]

type base_type =
  | IntType
  | FloatType
  | BoolType
  | UnitType
  | BinaryType
  | CharType
[@@deriving show { with_path = false; } ]

(* `format_*` are the types for representing format string literals.
   For the detail of format strings, see:
   http://erlang.org/doc/man/io.html *)
type format_hole =
  | HoleC  (* Characters. *)
  | HoleF  (* `[-]ddd.ddd` for floating-point numbers. *)
  | HoleE  (* `[-]d.ddde+-ddd` for floating-point numbers. *)
  | HoleG  (* Same as `HoleF` for `[0.1, 10000)` and same as `HoleE` otherwise. *)
  | HoleS  (* Strings. *)
  | HoleP
  | HoleW
[@@deriving show {with_path = false; } ]

type format_control = {
  field_width : int option;
  precision   : int option;
  padding     : char option;
}
[@@deriving show {with_path = false; } ]

type format_element =
  | FormatTilde
  | FormatBreak
  | FormatDQuote
  | FormatConst of string
  | FormatHole  of format_hole * format_control
[@@deriving show {with_path = false; } ]

type base_constant =
  | Unit
  | Bool           of bool
  | Int            of int
  | Float          of float
  | BinaryByString of string
  | BinaryByInts   of int list
  | String         of string
  | Char           of Uchar.t
      [@printer (fun ppf uchar -> Format.fprintf ppf "Char(%a)" pp_uchar uchar)]
  | FormatString   of format_element list
[@@deriving show { with_path = false; } ]

type manual_kind =
  manual_kind_main ranged

and manual_kind_main =
  | MKind of manual_base_kind list * manual_base_kind

and manual_base_kind =
  manual_base_kind_main ranged

and manual_base_kind_main =
  | MKindName   of kind_name

and manual_type = manual_type_main ranged

and manual_type_main =
  | MTypeName    of type_name * manual_type list
  | MFuncType    of manual_domain_type * manual_type
  | MProductType of manual_type TupleList.t
  | MRecordType  of manual_row
  | MEffType     of manual_domain_type * manual_type * manual_type
  | MTypeVar     of type_variable_name
  | MModProjType of untyped_module * type_name ranged * manual_type list
  | MPackType    of untyped_signature

and manual_domain_type =
  manual_type list * labeled_manual_type list * manual_row

and manual_row =
  | MRow of (label ranged * manual_type) list * (Range.t * row_variable_name) option

and binder = untyped_pattern * manual_type option

and constructor_branch =
  | ConstructorBranch of attribute list * constructor_name ranged * manual_type list

and synonym_or_variant =
  | BindSynonym of manual_type
  | BindVariant of constructor_branch list

and untyped_ast =
  untyped_ast_main ranged

and untyped_ast_main =
  | BaseConst    of base_constant
  | Var          of (module_name ranged) list * identifier ranged
  | Lambda       of untyped_parameters * untyped_ast
  | LambdaEff    of untyped_parameters * untyped_computation_ast
  | Apply        of untyped_ast * untyped_arguments
  | If           of untyped_ast * untyped_ast * untyped_ast
  | LetIn        of rec_or_nonrec * untyped_ast
  | LetPatIn     of untyped_pattern * untyped_ast * untyped_ast
  | Tuple        of untyped_ast TupleList.t
  | ListNil
  | ListCons     of untyped_ast * untyped_ast
  | Case         of untyped_ast * untyped_case_branch list
  | Constructor  of (module_name ranged) list * constructor_name * untyped_ast list
  | BinaryByList of (int ranged) list
  | Record       of labeled_untyped_ast list
  | RecordAccess of untyped_ast * label ranged
  | RecordUpdate of untyped_ast * label ranged * untyped_ast
  | Freeze       of Range.t * frozen_fun * untyped_ast list * Range.t list
  | FreezeUpdate of untyped_ast * untyped_ast list * Range.t list
  | Pack         of module_name_chain * untyped_signature
  | Assert       of untyped_ast

and untyped_parameters =
  binder list * labeled_binder list * labeled_optional_binder list

and untyped_computation_ast =
  untyped_computation_ast_main ranged

and untyped_computation_ast_main =
  | CompDo       of binder option * untyped_computation_ast * untyped_computation_ast
  | CompReceive  of untyped_receive_branch list
  | CompLetIn    of rec_or_nonrec * untyped_computation_ast
  | CompLetPatIn of untyped_pattern * untyped_ast * untyped_computation_ast
  | CompIf       of untyped_ast * untyped_computation_ast * untyped_computation_ast
  | CompCase     of untyped_ast * untyped_computation_case_branch list
  | CompApply    of untyped_ast * untyped_arguments

and untyped_arguments =
  untyped_ast list * labeled_untyped_ast list * labeled_untyped_ast list

and frozen_fun =
  | FrozenModFun of module_name_chain * identifier ranged
  | FrozenFun    of identifier ranged

and internal_or_external =
  | Internal of rec_or_nonrec
  | External of external_binding

and rec_or_nonrec =
  | NonRec of untyped_let_binding
  | Rec    of untyped_let_binding list

and type_variable_binder =
  type_variable_name ranged * manual_base_kind option

and external_binding = {
  ext_identifier  : identifier ranged;
  ext_type_params : type_variable_binder list;
  ext_row_params  : ((row_variable_name ranged) * (label ranged) list) list;
  ext_type_annot  : manual_type;
  ext_arity       : int;
  ext_has_option  : bool;
  ext_code        : string;
}

and untyped_let_binding = {
  vb_identifier  : identifier ranged;
  vb_forall      : type_variable_binder list;
  vb_forall_row  : (row_variable_name ranged * (label ranged) list) list;
  vb_parameters  : binder list;
  vb_mandatories : labeled_binder list;
  vb_optionals   : labeled_optional_binder list;
  vb_return      : (pure_return, effectful_return) pure_or_effectful;
}

and pure_return =
  manual_type option * untyped_ast

and effectful_return =
  (manual_type * manual_type) option * untyped_computation_ast

and untyped_receive_branch =
  | ReceiveBranch of untyped_pattern * untyped_computation_ast

and untyped_case_branch =
  | CaseBranch of untyped_pattern * untyped_ast

and untyped_computation_case_branch =
  | CompCaseBranch of untyped_pattern * untyped_computation_ast

and untyped_pattern =
  untyped_pattern_main ranged
[@printer (fun ppf (_, utpatmain) -> pp_untyped_pattern_main ppf utpatmain)]

and untyped_pattern_main =
  | PUnit
  | PBool        of bool
  | PInt         of int
  | PBinary      of string
  | PChar        of Uchar.t
      [@printer (fun ppf uchar -> Format.fprintf ppf "PChar(%a)" pp_uchar uchar) ]
  | PVar         of identifier
  | PWildCard
  | PListNil
  | PListCons    of untyped_pattern * untyped_pattern
  | PTuple       of untyped_pattern TupleList.t
  | PConstructor of (module_name ranged) list * constructor_name * untyped_pattern list
[@@deriving show { with_path = false; } ]

and untyped_module =
  untyped_module_main ranged

and untyped_module_main =
  | ModVar     of module_name
  | ModBinds   of attribute list * module_name_chain list * untyped_binding list
  | ModProjMod of untyped_module * module_name ranged
  | ModFunctor of module_name ranged * untyped_signature * untyped_module
  | ModApply   of module_name_chain * module_name_chain
  | ModCoerce  of module_name ranged * untyped_signature

and untyped_binding =
  untyped_binding_main ranged

and untyped_binding_main =
  | BindVal     of attribute list * internal_or_external
  | BindType    of type_binding list
  | BindModule  of module_name ranged * untyped_signature option * untyped_module
  | BindSig     of signature_name ranged * untyped_signature
  | BindInclude of untyped_module

and type_binding =
  type_name ranged * type_variable_binder list * synonym_or_variant

and untyped_signature =
  untyped_signature_main ranged

and untyped_signature_main =
  | SigVar     of signature_name
  | SigPath    of untyped_module * signature_name ranged
  | SigDecls   of module_name_chain list * untyped_declaration list
  | SigFunctor of module_name ranged * untyped_signature * untyped_signature
  | SigWith    of untyped_signature * (module_name ranged) list * type_binding list

and untyped_declaration =
  untyped_declaration_main ranged

and untyped_declaration_main =
  | DeclVal        of identifier ranged * type_variable_binder list * (row_variable_name ranged * (label ranged) list) list * manual_type * attribute list
  | DeclTypeOpaque of type_name ranged * manual_kind option * attribute list
  | DeclModule     of module_name ranged * untyped_signature * attribute list
  | DeclSig        of signature_name ranged * untyped_signature * attribute list
  | DeclInclude    of untyped_signature

and labeled_binder =
  label ranged * binder

and labeled_optional_binder =
  labeled_binder * untyped_ast option

and labeled_untyped_ast =
  label ranged * untyped_ast

and labeled_manual_type =
  label ranged * manual_type
[@@deriving show { with_path = false; } ]

and attribute =
  Attribute of (string * untyped_ast option) ranged

type attribute_warning = {
  position : Range.t;
  tag      : string;
  message  : string;
}

module FreeRowID = struct
  include FreeID
end

module BoundRowID = struct
  include BoundID
end

module MustBeBoundRowID = struct
  include MustBeBoundID
end


module BoundBothID = struct

  type t =
    | Type of BoundID.t
    | Row  of BoundRowID.t

  let hash = function
    | Type(bid) -> BoundID.hash bid
    | Row(brid) -> BoundRowID.hash brid

  let compare x1 x2 =
    match (x1, x2) with
    | (Type(bid1), Type(bid2)) -> BoundID.compare bid1 bid2
    | (Row(brid1), Row(brid2)) -> BoundRowID.compare brid1 brid2
    | (Type(_), Row(_))        -> 1
    | (Row(_), Type(_))        -> -1

  let equal x1 x2 =
    compare x1 x2 = 0
(*
  let pp ppf = function
    | Type(bid) -> BoundID.pp_raw ppf bid
    | Row(brid) -> BoundRowID.pp_raw ppf brid
*)
end


module FreeIDHashTable = Hashtbl.Make(FreeID)

module FreeRowIDHashTable = Hashtbl.Make(FreeRowID)

module BoundIDHashTable = Hashtbl.Make(BoundID)

module BoundRowIDHashTable = Hashtbl.Make(BoundRowID)

module BoundIDMap = Map.Make(BoundID)

type space_name = OutputIdentifier.space
[@@deriving show { with_path = false; } ]

type local_name = OutputIdentifier.local
[@@deriving show { with_path = false; } ]

type global_name = OutputIdentifier.global
[@@deriving show { with_path = false; } ]

type operator_name = OutputIdentifier.operator
[@@deriving show { with_path = false; } ]

type name = OutputIdentifier.t
[@@deriving show { with_path = false; } ]

module ConstructorMap = Map.Make(String)

module TypeParameterAssoc = AssocList.Make(String)

type type_parameter_assoc = MustBeBoundID.t TypeParameterAssoc.t

module TypeParameterMap = Map.Make(String)

type local_type_parameter_map = MustBeBoundID.t TypeParameterMap.t

module RowParameterMap = Map.Make(String)


module OpaqueIDMap = Map.Make(TypeID)


let stringify_opaque_id_quantifier qt =
  OpaqueIDMap.fold (fun oid pkd acc ->
    Alist.extend acc (Format.asprintf "%a" (TypeID.pp ~seen_from:Address.root) oid)
  ) qt Alist.empty |> Alist.to_list |> List.map (fun s -> " " ^ s) |> String.concat ","


let pp_opaque_id_quantifier ppf qt =
  Format.fprintf ppf "%s" (stringify_opaque_id_quantifier qt)


module OpaqueIDHashTable = Hashtbl.Make(TypeID)

module ValNameMap = Map.Make(String)

module TypeNameMap = Map.Make(String)

module ModuleNameMap = Map.Make(String)

module SignatureNameMap = Map.Make(String)
