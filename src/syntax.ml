
open MyUtil


type package_name = string

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t

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
      Format.fprintf ppf "%s ->@ %a;@ " label ppsub v
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
  | MRecordKind of labeled_manual_type list

and manual_type = manual_type_main ranged

and manual_type_main =
  | MTypeName    of type_name * manual_type list
  | MFuncType    of manual_type list * labeled_manual_type list * manual_row * manual_type
  | MProductType of manual_type TupleList.t
  | MRecordType  of labeled_manual_type list
  | MEffType     of manual_type * manual_type
  | MTypeVar     of type_variable_name
  | MModProjType of untyped_module * type_name ranged * manual_type list

and manual_row =
  | MFixedRow of (label ranged * manual_type) list
  | MRowVar   of Range.t * row_variable_name

and binder = identifier ranged * manual_type option

and constructor_branch =
  | ConstructorBranch of constructor_name ranged * manual_type list

and synonym_or_variant =
  | BindSynonym of manual_type
  | BindVariant of constructor_branch list

and untyped_ast =
  untyped_ast_main ranged

and untyped_ast_main =
  | BaseConst    of base_constant
  | Var          of identifier
  | Lambda       of binder list * labeled_binder list * labeled_optional_binder list * untyped_ast
  | Apply        of untyped_ast * untyped_ast list * labeled_untyped_ast list * labeled_untyped_ast list
  | If           of untyped_ast * untyped_ast * untyped_ast
  | LetIn        of rec_or_nonrec * untyped_ast
  | LetPatIn     of untyped_pattern * untyped_ast * untyped_ast
  | Do           of binder option * untyped_ast * untyped_ast
  | Receive      of untyped_branch list
  | Tuple        of untyped_ast TupleList.t
  | ListNil
  | ListCons     of untyped_ast * untyped_ast
  | Case         of untyped_ast * untyped_branch list
  | Constructor  of constructor_name * untyped_ast list
  | BinaryByList of (int ranged) list
  | Record       of labeled_untyped_ast list
  | RecordAccess of untyped_ast * label ranged
  | RecordUpdate of untyped_ast * label ranged * untyped_ast
  | Freeze       of Range.t * frozen_fun * untyped_ast list * labeled_untyped_ast list * labeled_untyped_ast list
  | ModProjVal   of module_name ranged * identifier ranged

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
  ext_row_params  : ((row_variable_name ranged) * labeled_manual_type list) list;
  ext_type_annot  : manual_type;
  ext_arity       : int;
  ext_has_option  : bool;
  ext_code        : string;
}

and untyped_let_binding = {
  vb_identifier  : identifier ranged;
  vb_forall      : type_variable_binder list;
  vb_forall_row  : (row_variable_name ranged * labeled_manual_type list) list;
  vb_parameters  : binder list;
  vb_mandatories : labeled_binder list;
  vb_optionals   : labeled_optional_binder list;
  vb_return_type : manual_type option;
  vb_body        : untyped_ast;
}

and untyped_branch =
  | Branch of untyped_pattern * untyped_ast option * untyped_ast

and untyped_pattern =
  untyped_pattern_main ranged
[@printer (fun ppf (_, utpatmain) -> pp_untyped_pattern_main ppf utpatmain)]

and untyped_pattern_main =
  | PUnit
  | PBool        of bool
  | PInt         of int
  | PChar        of Uchar.t
      [@printer (fun ppf uchar -> Format.fprintf ppf "PChar(%a)" pp_uchar uchar) ]
  | PVar         of identifier
  | PWildCard
  | PListNil
  | PListCons    of untyped_pattern * untyped_pattern
  | PTuple       of untyped_pattern TupleList.t
  | PConstructor of constructor_name * untyped_pattern list
[@@deriving show { with_path = false; } ]

and untyped_module =
  untyped_module_main ranged

and untyped_module_main =
  | ModVar     of module_name
  | ModBinds   of untyped_binding list
  | ModProjMod of untyped_module * module_name ranged
  | ModFunctor of module_name ranged * untyped_signature * untyped_module
  | ModApply   of module_name_chain * module_name_chain
  | ModCoerce  of module_name ranged * untyped_signature

and untyped_binding =
  untyped_binding_main ranged

and untyped_binding_main =
  | BindVal     of internal_or_external
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
  | SigDecls   of untyped_declaration list
  | SigFunctor of module_name ranged * untyped_signature * untyped_signature
  | SigWith    of untyped_signature * (module_name ranged) list * type_binding list

and untyped_declaration =
  untyped_declaration_main ranged

and untyped_declaration_main =
  | DeclVal        of identifier ranged * type_variable_binder list * (row_variable_name ranged * (label ranged * manual_type) list) list * manual_type
  | DeclTypeTrans  of type_name ranged * manual_type
  | DeclTypeOpaque of type_name ranged * manual_kind option
  | DeclModule     of module_name ranged * untyped_signature
  | DeclSig        of signature_name ranged * untyped_signature
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

module FreeRowID = FreeID  (* temporary *)

module BoundRowID = BoundID  (* temporary *)

module MustBeBoundRowID = MustBeBoundID  (* temporary *)


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

  let pp ppf = function
    | Type(bid) -> BoundID.pp ppf bid
    | Row(brid) -> BoundRowID.pp ppf brid

end


type ('a, 'b) typ =
  (('a, 'b) typ_main) ranged

and ('a, 'b) typ_main =
  | BaseType    of base_type
  | FuncType    of (('a, 'b) typ) list * (('a, 'b) typ) LabelAssoc.t * ('a, 'b) row * ('a, 'b) typ
  | PidType     of ('a, 'b) pid_type
  | EffType     of ('a, 'b) effect * ('a, 'b) typ
  | TypeVar     of 'a
  | ProductType of (('a, 'b) typ) TupleList.t
  | DataType    of TypeID.t * (('a, 'b) typ) list
  | RecordType  of (('a, 'b) typ) LabelAssoc.t

and ('a, 'b) effect =
  | Effect of ('a, 'b) typ

and ('a, 'b) pid_type =
  | Pid of ('a, 'b) typ

and ('a, 'b) row =
  | FixedRow of (('a, 'b) typ) LabelAssoc.t
  | RowVar   of 'b

and ('a, 'b) base_kind =
  | UniversalKind
  | RecordKind    of (('a, 'b) typ) LabelAssoc.t

type ('a, 'b) kind =
  | Kind of (('a, 'b) base_kind) list * ('a, 'b) base_kind
      (* Handles order-0 or order-1 kind only, *)

type mono_type_var_updatable =
  | Free of FreeID.t
  | Link of mono_type

and mono_type_var =
  | Updatable   of mono_type_var_updatable ref
  | MustBeBound of MustBeBoundID.t

and mono_row_var_updatable =
  | FreeRow of FreeRowID.t
  | LinkRow of mono_type LabelAssoc.t

and mono_row_var =
  | UpdatableRow   of mono_row_var_updatable ref
  | MustBeBoundRow of MustBeBoundRowID.t

and mono_type = (mono_type_var, mono_row_var) typ

type mono_row = (mono_type_var, mono_row_var) row

type mono_kind = (mono_type_var, mono_row_var) kind

type mono_base_kind = (mono_type_var, mono_row_var) base_kind

type poly_type_var =
  | Mono  of mono_type_var
  | Bound of BoundID.t

type poly_row_var =
  | MonoRow  of mono_row_var
  | BoundRow of BoundRowID.t

and poly_type = (poly_type_var, poly_row_var) typ

type poly_row = (poly_type_var, poly_row_var) row

type poly_kind = (poly_type_var, poly_row_var) kind

type poly_base_kind = (poly_type_var, poly_row_var) base_kind

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

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorMap.t

module TypeParameterAssoc = AssocList.Make(String)

type type_parameter_assoc = MustBeBoundID.t TypeParameterAssoc.t

module TypeParameterMap = Map.Make(String)

type local_type_parameter_map = MustBeBoundID.t TypeParameterMap.t

module RowParameterMap = Map.Make(String)

type local_row_parameter_map = (MustBeBoundRowID.t * poly_type LabelAssoc.t) RowParameterMap.t

module SynonymIDSet = Set.Make(TypeID.Synonym)

module SynonymIDMap = Map.Make(TypeID.Synonym)

module SynonymIDHashTable = Hashtbl.Make(TypeID.Synonym)

module VariantIDMap = Map.Make(TypeID.Variant)

module VariantIDHashTable = Hashtbl.Make(TypeID.Variant)

module OpaqueIDSet = Set.Make(TypeID.Opaque)


let stringify_opaque_id_set oidset =
  OpaqueIDSet.fold (fun oid acc ->
    Alist.extend acc (Format.asprintf "%a" TypeID.Opaque.pp oid)
  ) oidset Alist.empty |> Alist.to_list |> List.map (fun s -> " " ^ s) |> String.concat ","


let pp_opaque_id_set ppf oidset =
  Format.fprintf ppf "%s" (stringify_opaque_id_set oidset)


module OpaqueIDMap = Map.Make(TypeID.Opaque)

module OpaqueIDHashTable = Hashtbl.Make(TypeID.Opaque)

module ValNameMap = Map.Make(String)

module TypeNameMap = Map.Make(String)

module ModuleNameMap = Map.Make(String)

module SignatureNameMap = Map.Make(String)

type pattern =
  | IPUnit
  | IPBool        of bool
  | IPInt         of int
  | IPChar        of Uchar.t
      [@printer (fun ppf uchar -> Format.fprintf ppf "IPChar(%a)" pp_uchar uchar)]
  | IPVar         of local_name
  | IPWildCard
  | IPListNil
  | IPListCons    of pattern * pattern
  | IPTuple       of pattern TupleList.t
  | IPConstructor of ConstructorID.t * pattern list
[@@deriving show { with_path = false; } ]

type type_opacity = TypeID.t * poly_kind

type 'a abstracted = OpaqueIDSet.t * 'a

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type val_binding =
  | INonRec   of (identifier * global_name * poly_type * ast)
  | IRec      of (identifier * global_name * poly_type * ast) list
  | IExternal of global_name * string

and binding =
  | IBindVal     of val_binding
  | IBindModule  of space_name * binding list

and ast =
  | IBaseConst   of base_constant
  | IVar         of name
  | ILambda      of local_name option * local_name list * local_name LabelAssoc.t * (local_name * ast option) LabelAssoc.t * ast
  | IApply       of name * mono_row * ast list * ast LabelAssoc.t * ast LabelAssoc.t
  | ILetIn       of local_name * ast * ast
  | ICase        of ast * branch list
  | IReceive     of branch list
  | ITuple       of ast TupleList.t
  | IListNil
  | IListCons    of ast * ast
  | IConstructor of ConstructorID.t * ast list
  | IRecord      of ast LabelAssoc.t
  | IRecordAccess of ast * label
  | IRecordUpdate of ast * label * ast
  | IThunk       of ast
  | IForce       of ast
  | IFreeze       of global_name * mono_row * ast list * ast LabelAssoc.t * ast LabelAssoc.t

and branch =
  | IBranch of pattern * ast option * ast


let pp_sep_comma ppf () =
  Format.fprintf ppf ",@ "


let rec pp_val_binding_sub ppf (gname, e) =
  Format.fprintf ppf "%a =@[<hov>@ %a@]@,"
    OutputIdentifier.pp_global gname
    pp_ast e


and pp_val_binding ppf = function
  | INonRec(_, gname, _, e) ->
      Format.fprintf ppf "val %a"
        pp_val_binding_sub (gname, e)

  | IRec(recbinds) ->
      let pairs = recbinds |> List.map (fun (_, gname, _, e) -> (gname, e)) in
      Format.fprintf ppf "val %a"
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_val_binding_sub) pairs

  | IExternal(gname, code) ->
      Format.fprintf ppf "val %a = external@ \"%s\"@,"
        OutputIdentifier.pp_global gname
        code


and pp_binding ppf = function
  | IBindVal(valbind) ->
      pp_val_binding ppf valbind

  | IBindModule(sname, ibinds) ->
      Format.fprintf ppf "module %a = @[<v2>{%a}@]@,"
        OutputIdentifier.pp_space sname
        (Format.pp_print_list pp_binding) ibinds


and pp_ast ppf = function
  | IBaseConst(bc) ->
      pp_base_constant ppf bc

  | IVar(name) ->
      OutputIdentifier.pp ppf name

  | ILambda(lnamerecopt, lnameparams, mndnamemap, optnamemap, e) ->
      let snamerec =
        match lnamerecopt with
        | Some(lnamerec) -> Format.asprintf "%a" OutputIdentifier.pp_local lnamerec
        | None           -> ""
      in
      Format.fprintf ppf "\\%s(%a -{%a} ?{%a}) ->@[<hov2>@ %a@]"
        snamerec
        (Format.pp_print_list ~pp_sep:pp_sep_comma OutputIdentifier.pp_local) lnameparams
        (LabelAssoc.pp OutputIdentifier.pp_local) mndnamemap
        (LabelAssoc.pp (fun ppf (lname, astopt) ->
          match astopt with
          | None ->
              Format.fprintf ppf "%a"
                OutputIdentifier.pp_local lname

          | Some(ast) ->
              Format.fprintf ppf "%a = %a"
                OutputIdentifier.pp_local lname
                pp_ast ast
        )) optnamemap
        pp_ast e

  | IApply(name, _, eargs, mndargmap, optargmap) ->
      Format.fprintf ppf "%a@[<hov2>(%a -{%a} ?{%a})@]"
        OutputIdentifier.pp name
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_ast) eargs
        (LabelAssoc.pp pp_ast) mndargmap
        (LabelAssoc.pp pp_ast) optargmap

  | ILetIn(lname, e1, e2) ->
      Format.fprintf ppf "(let %a =@[<hov2>@ %a@]@ in@ %a)"
        OutputIdentifier.pp_local lname
        pp_ast e1
        pp_ast e2

  | ICase(e0, ibrs) ->
      Format.fprintf ppf "(case@[<hov2>@ %a@]@ of@[<hov2>@ %a@]@ end)"
        pp_ast e0
        (Format.pp_print_list pp_branch) ibrs

  | ITuple(es) ->
      Format.fprintf ppf "{%a}"
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_ast) (es |> TupleList.to_list)

  | _ ->
      Format.fprintf ppf "..."


and pp_branch ppf = function
  | IBranch(ipat, _, e) ->
      Format.fprintf ppf "%a (when ...) ->@[<hov2>@ %a@];@ "
        pp_pattern ipat
        pp_ast e


module GlobalNameMap = Map.Make(OutputIdentifier.Global)

type global_name_map = string GlobalNameMap.t
(* The type for maps tracking which module every global name belongs to.
   This is used by 'Primitives' and 'OutputErlangCode'. *)
