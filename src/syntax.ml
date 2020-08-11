
open MyUtil


type 'a ranged = Range.t * 'a

let pp_ranged ppsub ppf (_, x) =
  Format.fprintf ppf "%a" ppsub x

type identifier = string

type type_name = string
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


type module_name_chain =
  module_name ranged * (module_name ranged) list
[@@deriving show { with_path = false; } ]

type base_type =
  | IntType
  | BoolType
  | UnitType
  | BinaryType
[@@deriving show { with_path = false; } ]

type base_constant =
  | Unit
  | Bool           of bool
  | Int            of int
  | BinaryByString of string
  | BinaryByInts   of int list
[@@deriving show { with_path = false; } ]

type manual_kind = int
  (* -- order-0 or order-1 kind only; just tracks arity -- *)
[@@deriving show { with_path = false; } ]

type kind = int
  (* -- as `manual_kind`, this type handles order-0 or order-1 kind only -- *)

type manual_type = manual_type_main ranged

and manual_type_main =
  | MTypeName    of type_name * manual_type list
  | MFuncType    of manual_type list * manual_row * manual_type
  | MProductType of manual_type TupleList.t
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
  | Lambda       of binder list * (label ranged * binder) list * untyped_ast
  | Apply        of untyped_ast * untyped_ast list * (label ranged * untyped_ast) list
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
  | ModProjVal   of module_name ranged * identifier ranged

and internal_or_external =
  | Internal of rec_or_nonrec
  | External of external_binding

and rec_or_nonrec =
  | NonRec of untyped_let_binding
  | Rec    of untyped_let_binding list

and external_binding = {
  ext_identifier  : identifier ranged;
  ext_type_params : (type_variable_name ranged) list;
  ext_row_params  : ((row_variable_name ranged) * (label ranged * manual_type) list) list;
  ext_type_annot  : manual_type;
  ext_arity       : int;
  ext_has_option  : bool;
  ext_code        : string;
}

and untyped_let_binding = {
  vb_identifier  : identifier ranged;
  vb_forall      : (type_variable_name ranged) list;
  vb_forall_row  : (row_variable_name ranged * (label ranged * manual_type) list) list;
  vb_parameters  : binder list;
  vb_optionals   : (label ranged * binder) list;
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
  | BindType    of (type_name ranged * (type_variable_name ranged) list * synonym_or_variant) list
  | BindModule  of module_name ranged * untyped_module
  | BindSig     of signature_name ranged * untyped_signature
  | BindInclude of untyped_module

and untyped_signature =
  untyped_signature_main ranged

and untyped_signature_main =
  | SigVar     of signature_name
  | SigPath    of untyped_module * signature_name ranged
  | SigDecls   of untyped_declaration list
  | SigFunctor of module_name ranged * untyped_signature * untyped_signature
  | SigWith    of untyped_signature * (module_name ranged) list * type_name ranged * (type_variable_name ranged) list * manual_type

and untyped_declaration =
  untyped_declaration_main ranged

and untyped_declaration_main =
  | DeclVal        of identifier ranged * (type_variable_name ranged) list * (row_variable_name ranged * (label ranged * manual_type) list) list * manual_type
  | DeclTypeTrans  of type_name ranged * manual_type
  | DeclTypeOpaque of type_name ranged * manual_kind
  | DeclModule     of module_name ranged * untyped_signature
  | DeclSig        of signature_name ranged * untyped_signature
  | DeclInclude    of untyped_signature
[@@deriving show { with_path = false; } ]

module FreeRowID = FreeID  (* temporary *)

module BoundRowID = BoundID  (* temporary *)

module MustBeBoundRowID = MustBeBoundID  (* temporary *)

type ('a, 'b) typ =
  (('a, 'b) typ_main) ranged

and ('a, 'b) typ_main =
  | BaseType    of base_type
  | FuncType    of (('a, 'b) typ) list * ('a, 'b) row * ('a, 'b) typ
  | PidType     of ('a, 'b) pid_type
  | EffType     of ('a, 'b) effect * ('a, 'b) typ
  | TypeVar     of 'a
  | ProductType of (('a, 'b) typ) TupleList.t
  | ListType    of ('a, 'b) typ
  | DataType    of TypeID.t * (('a, 'b) typ) list

and ('a, 'b) effect =
  | Effect of ('a, 'b) typ

and ('a, 'b) pid_type =
  | Pid of ('a, 'b) typ

and ('a, 'b) row =
  | FixedRow of (('a, 'b) typ) LabelAssoc.t
  | RowVar   of 'b

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

type poly_type_var =
  | Mono  of mono_type_var
  | Bound of BoundID.t

type poly_row_var =
  | MonoRow  of mono_row_var
  | BoundRow of BoundRowID.t

and poly_type = (poly_type_var, poly_row_var) typ

type poly_row = (poly_type_var, poly_row_var) row

module FreeIDHashTable = Hashtbl.Make(FreeID)

module FreeRowIDHashTable = Hashtbl.Make(FreeRowID)

module BoundIDHashTable = Hashtbl.Make(BoundID)

module BoundRowIDHashTable = Hashtbl.Make(BoundRowID)

module BoundIDMap = Map.Make(BoundID)


let show_base_type = function
  | UnitType   -> "unit"
  | BoolType   -> "bool"
  | IntType    -> "int"
  | BinaryType -> "binary"


let rec show_label_assoc : 'a 'b. ('a -> string) -> ('b -> string) -> (('a, 'b) typ) LabelAssoc.t -> string =
fun showtv showrv labmap ->
  LabelAssoc.fold (fun label ty acc ->
    let sty = show_type showtv showrv ty in
    Alist.extend acc ("?" ^ label ^ " " ^ sty)
  ) labmap Alist.empty |> Alist.to_list |> String.concat ", "


and show_type : 'a 'b. ('a -> string) -> ('b -> string) -> ('a, 'b) typ -> string =
fun showtv showrv ty ->
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(bty) ->
        show_base_type bty

    | FuncType(tydoms, optrow, tycod) ->
        let sdoms = tydoms |> List.map aux in
        let sdomscat = String.concat ", " sdoms in
        let sopts = show_row showtv showrv optrow in
        let is_opts_empty =
          match optrow with
          | FixedRow(labmap) -> LabelAssoc.cardinal labmap = 0
          | RowVar(rv)       -> false
        in
        let smid =
          if List.length sdoms = 0 || is_opts_empty then
            ""
          else
            ", "
        in
        let scod = aux tycod in
        "fun(" ^ sdomscat ^ smid ^ sopts ^ ") -> " ^ scod

    | EffType(eff, ty0) ->
        let seff = aux_effect eff in
        let s0 = aux ty0 in
        seff ^ s0

    | PidType(pidty) ->
        let spid = aux_pid_type pidty in
        "pid<" ^ spid ^ ">"

    | TypeVar(tv) ->
        showtv tv

    | ProductType(tys) ->
        let ss = tys |> TupleList.to_list |> List.map aux in
        Printf.sprintf "(%s)" (String.concat ", " ss)

    | ListType(ty0) ->
        let s0 = aux ty0 in
        Printf.sprintf "list<%s>" s0

    | DataType(tyid, tyargs) ->
        begin
          match tyargs with
          | [] ->
              Format.asprintf "%a" TypeID.pp tyid

          | _ :: _ ->
              let ss = tyargs |> List.map aux in
              Format.asprintf "%a<%s>" TypeID.pp tyid (String.concat ", " ss)
        end

  and aux_effect (Effect(ty)) =
    let s = aux ty in
    "[" ^ s ^ "]"

  and aux_pid_type (Pid(ty)) =
    aux ty
  in
  aux ty


and show_row : 'a 'b. ('a -> string) -> ('b -> string) -> ('a, 'b) row -> string =
fun showtv showrv optrow ->
  match optrow with
  | FixedRow(labmap) -> labmap |> show_label_assoc showtv showrv
  | RowVar(rv)       -> "?" ^ showrv rv


and show_mono_type_var (mtv : mono_type_var) =
  match mtv with
  | MustBeBound(mbbid) -> Format.asprintf "%a" MustBeBoundID.pp mbbid
  | Updatable(mtvu)    -> show_mono_type_var_updatable !mtvu


and show_mono_type_var_updatable (mtvu : mono_type_var_updatable) =
  match mtvu with
  | Link(ty)  -> show_type show_mono_type_var show_mono_row_var ty
  | Free(fid) -> Format.asprintf "%a" FreeID.pp fid


and show_mono_row_var (mrv : mono_row_var) =
  match mrv with
  | UpdatableRow(mrvu)     -> show_mono_row_var_updatable !mrvu
  | MustBeBoundRow(mbbrid) -> Format.asprintf "%a" MustBeBoundRowID.pp mbbrid


and show_mono_row_var_updatable (mrvu : mono_row_var_updatable) =
  match mrvu with
  | LinkRow(labmap) -> show_label_assoc show_mono_type_var show_mono_row_var labmap
  | FreeRow(frid)   -> Format.asprintf "%a" FreeRowID.pp frid


let show_mono_type : mono_type -> string =
  show_type show_mono_type_var show_mono_row_var


let pp_mono_type ppf ty =
  Format.fprintf ppf "%s" (show_mono_type ty)


let show_poly_type_var = function
  | Bound(bid) -> Format.asprintf "%a" BoundID.pp bid
  | Mono(mtv)  -> show_mono_type_var mtv


let rec show_poly_row_var = function
  | BoundRow(brid) -> Format.asprintf "%a" BoundRowID.pp brid
  | MonoRow(mrv)   -> show_mono_row_var mrv


let show_poly_type : poly_type -> string =
  show_type show_poly_type_var show_poly_row_var


let pp_poly_type ppf pty =
  Format.fprintf ppf "%s" (show_poly_type pty)


let show_poly_row : poly_row -> string =
  show_row show_poly_type_var show_poly_row_var


let pp_poly_row ppf prow =
  Format.fprintf ppf "%s" (show_poly_row prow)


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
  | IPVar         of local_name
  | IPWildCard
  | IPListNil
  | IPListCons    of pattern * pattern
  | IPTuple       of pattern TupleList.t
  | IPConstructor of ConstructorID.t * pattern list
[@@deriving show { with_path = false; } ]

type type_opacity = TypeID.t * int

type 'a abstracted = OpaqueIDSet.t * 'a

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}
[@@deriving show { with_path = false; } ]

type val_binding =
  | INonRec   of (identifier * global_name * poly_type * ast)
  | IRec      of (identifier * global_name * poly_type * ast) list
  | IExternal of global_name * bool * string
      (* the second Boolean parameter stands for whether the external function has
         a variant version that can receive a map for labeled optional arguments.
      *)

and binding =
  | IBindVal     of val_binding
  | IBindModule  of space_name * binding list

and ast =
  | IBaseConst   of base_constant
  | IVar         of name
  | ILambda      of local_name option * local_name list * local_name LabelAssoc.t * ast
  | IApply       of name * mono_row * ast list * ast LabelAssoc.t
  | ILetIn       of local_name * ast * ast
  | ICase        of ast * branch list
  | IReceive     of branch list
  | ITuple       of ast TupleList.t
  | IListNil
  | IListCons    of ast * ast
  | IConstructor of ConstructorID.t * ast list
  | IThunk       of ast
  | IForce       of ast

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

  | IExternal(gname, has_option, code) ->
      Format.fprintf ppf "val %a = external%s@ \"%s\"@,"
        OutputIdentifier.pp_global gname
        (if has_option then "+" else "")
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

  | ILambda(None, lnameparams, optnamemap, e) ->
      let midcomma = if List.length lnameparams = 0 || LabelAssoc.cardinal optnamemap = 0 then "" else ", " in
      Format.fprintf ppf "\\(%a%s?%a) ->@[<hov2>@ %a@]"
        (Format.pp_print_list ~pp_sep:pp_sep_comma OutputIdentifier.pp_local) lnameparams
        midcomma
        (LabelAssoc.pp OutputIdentifier.pp_local) optnamemap
        pp_ast e

  | ILambda(Some(lnamerec), lnameparams, optnamemap, e) ->
      let midcomma = if List.length lnameparams = 0 || LabelAssoc.cardinal optnamemap = 0 then "" else ", " in
      Format.fprintf ppf "\\%a(%a%s?%a) ->@[<hov2>@ %a@]"
        OutputIdentifier.pp_local lnamerec
        (Format.pp_print_list ~pp_sep:pp_sep_comma OutputIdentifier.pp_local) lnameparams
        midcomma
        (LabelAssoc.pp OutputIdentifier.pp_local) optnamemap
        pp_ast e

  | IApply(name, _, eargs, optargmap) ->
      let midcomma = if List.length eargs = 0 || LabelAssoc.cardinal optargmap = 0 then "" else ", " in
      Format.fprintf ppf "%a@[<hov2>(%a%s?%a)@]"
        OutputIdentifier.pp name
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_ast) eargs
        midcomma
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
