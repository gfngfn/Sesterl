
exception UnidentifiedToken           of Range.t * string
exception SeeEndOfFileInComment       of Range.t
exception SeeEndOfFileInStringLiteral of Range.t
exception ConflictInSignature         of Range.t * string


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

type module_name = string
[@@deriving show { with_path = false; } ]

type signature_name = string
[@@deriving show { with_path = false; } ]


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


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
  | MFuncType    of manual_type list * manual_type
  | MProductType of manual_type TupleList.t
  | MEffType     of manual_type * manual_type
  | MTypeVar     of type_variable_name
  | MModProjType of untyped_module * type_name ranged * manual_type list

and binder = identifier ranged * manual_type option

and constructor_branch =
  | ConstructorBranch of constructor_name * manual_type list

and synonym_or_variant =
  | BindSynonym of manual_type
  | BindVariant of constructor_branch list

and untyped_ast =
  untyped_ast_main ranged

and untyped_ast_main =
  | BaseConst    of base_constant
  | Var          of identifier
  | Lambda       of binder list * untyped_ast
  | Apply        of untyped_ast * untyped_ast list
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

and rec_or_nonrec =
  | NonRec of untyped_let_binding
  | Rec    of untyped_let_binding list

and untyped_let_binding = {
  vb_identifier : identifier ranged;
  vb_forall     : (type_variable_name ranged) list;
  vb_parameters : binder list;
  vb_return_type : manual_type option;
  vb_body       : untyped_ast;
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
  | ModApply   of module_name ranged * module_name ranged
  | ModCoerce  of module_name ranged * untyped_signature

and untyped_binding =
  | BindVal    of rec_or_nonrec
  | BindType   of (type_name ranged * (type_variable_name ranged) list * synonym_or_variant) list
  | BindModule of module_name ranged * untyped_module
  | BindSig    of signature_name ranged * untyped_signature
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
  | DeclVal        of identifier ranged * (type_variable_name ranged) list * manual_type
  | DeclTypeTrans  of type_name ranged * manual_type
  | DeclTypeOpaque of type_name ranged * manual_kind
  | DeclModule     of module_name ranged * untyped_signature
  | DeclSig        of signature_name ranged * untyped_signature
  | DeclInclude    of untyped_signature
[@@deriving show { with_path = false; } ]

type 'a typ =
  ('a typ_main) ranged

and 'a typ_main =
  | BaseType    of base_type
  | FuncType    of ('a typ) list * 'a typ
  | PidType     of 'a pid_type
  | EffType     of 'a effect * 'a typ
  | TypeVar     of 'a
  | ProductType of ('a typ) TupleList.t
  | ListType    of 'a typ
  | DataType    of TypeID.t * ('a typ) list

and 'a effect =
  | Effect of 'a typ

and 'a pid_type =
  | Pid of 'a typ

type mono_type_var_updatable =
  | Free of FreeID.t
  | Link of mono_type

and mono_type_var =
  | Updatable   of mono_type_var_updatable ref
  | MustBeBound of MustBeBoundID.t

and mono_type = mono_type_var typ

type poly_type_var =
  | Mono  of mono_type_var
  | Bound of BoundID.t

type poly_type = poly_type_var typ

module FreeIDHashTable = Hashtbl.Make(FreeID)


(* --
Arguments:
+ `levpred`: Given a level of free/must-be-bound ID,
  this predicate returns whether it should be bound or not.
-- *)
let lift_scheme (rngf : Range.t -> Range.t) (levpred : int -> bool) (ty : mono_type) : poly_type =

  let fidht = FreeIDHashTable.create 32 in

  let intern fid =
    match FreeIDHashTable.find_opt fidht fid with
    | Some(bid) ->
        bid

    | None ->
        let bid = BoundID.fresh () in
        FreeIDHashTable.add fidht fid bid;
        bid
  in

  let rec aux (rng, tymain) =
    match tymain with
    | BaseType(bty) ->
        (rngf rng, BaseType(bty))

    | TypeVar(Updatable{contents = Link(ty)}) ->
        aux ty

    | TypeVar(Updatable{contents = Free(fid)} as mtv) ->
        let ptv =
          if levpred (FreeID.get_level fid) then
            let bid = intern fid in
            Bound(bid)
          else
            Mono(mtv)
        in
        (rngf rng, TypeVar(ptv))

    | TypeVar(MustBeBound(mbbid) as mtv) ->
        let ptv =
          if levpred (MustBeBoundID.get_level mbbid) then
            let bid = MustBeBoundID.to_bound mbbid in
            Bound(bid)
          else
            Mono(mtv)
        in
        (rngf rng, TypeVar(ptv))

    | FuncType(tydoms, tycod) ->
        let ptydoms = tydoms |> List.map aux in
        let ptycod = aux tycod in
        (rngf rng, FuncType(ptydoms, ptycod))

    | EffType(eff, ty0) ->
        (rngf rng, EffType(aux_effect eff, aux ty0))

    | PidType(pidty) ->
        (rngf rng, PidType(aux_pid_type pidty))

    | ProductType(tys) ->
        let ptys = tys |> TupleList.map aux in
        (rngf rng, ProductType(ptys))

    | ListType(ty0) ->
        (rngf rng, ListType(aux ty0))

    | DataType(tyid, tyargs) ->
        (rngf rng, DataType(tyid, tyargs |> List.map aux))

  and aux_effect (Effect(ty)) =
    let pty = aux ty in
    Effect(pty)

  and aux_pid_type (Pid(ty)) =
    let pty = aux ty in
    Pid(pty)
  in
  aux ty


(* --
  `generalize lev ty` transforms a monotype `ty` into a polytype
  by binding type variables the level of which is higher than `lev`.
-- *)
let generalize (lev : int) (ty : mono_type) : poly_type =
  lift_scheme
    (fun _ -> Range.dummy "erased")
    (fun levx -> lev < levx)
    ty


(* --
  `lift` projects monotypes into polytypes without binding any type variables.
--*)
let lift (ty : mono_type) : poly_type =
  lift_scheme (fun rng -> rng) (fun _ -> false) ty


module BoundIDHashTable = Hashtbl.Make(BoundID)

module BoundIDMap = Map.Make(BoundID)


let instantiate_scheme (intern : Range.t -> BoundID.t -> mono_type) (pty : poly_type) : mono_type =

  let rec aux (rng, ptymain) =
    match ptymain with
    | BaseType(bty) ->
        (rng, BaseType(bty))

    | TypeVar(Mono(mtv)) ->
        (rng, TypeVar(mtv))

    | TypeVar(Bound(bid)) ->
        intern rng bid

    | FuncType(ptydoms, ptycod) ->
        let tydoms = ptydoms |> List.map aux in
        let tycod = aux ptycod in
        (rng, FuncType(tydoms, tycod))

    | EffType(peff, pty0) ->
        let eff = aux_effect peff in
        let ty0 = aux pty0 in
        (rng, EffType(eff, ty0))

    | PidType(ppidty) ->
        let pidty = aux_pid_type ppidty in
        (rng, PidType(pidty))

    | ProductType(ptys) ->
        let tys = ptys |> TupleList.map aux in
        (rng, ProductType(tys))

    | ListType(pty0) ->
        (rng, ListType(aux pty0))

    | DataType(tyid, ptyargs) ->
        (rng, DataType(tyid, ptyargs |> List.map aux))

  and aux_effect (Effect(pty)) =
    let ty = aux pty in
    Effect(ty)

  and aux_pid_type (Pid(pty)) =
    let ty = aux pty in
    Pid(ty)
  in
  aux pty


let instantiate (lev : int) (pty : poly_type) : mono_type =
  let bidht = BoundIDHashTable.create 32 in
    (* -- a hash table is created at every (non-partial) call of `instantiate` -- *)
  let intern rng bid =
    let mtv =
      match BoundIDHashTable.find_opt bidht bid with
      | Some(mtvu) ->
          Updatable(mtvu)

      | None ->
          let fid = FreeID.fresh lev in
          let mtvu = ref (Free(fid)) in
          BoundIDHashTable.add bidht bid mtvu;
          Updatable(mtvu)
    in
    (rng, TypeVar(mtv))
  in
  instantiate_scheme intern pty


let instantiate_by_map (bfmap : mono_type_var BoundIDMap.t) =
  let intern rng bid =
    match bfmap |> BoundIDMap.find_opt bid with
    | None      -> assert false
    | Some(mtv) -> (rng, TypeVar(mtv))
  in
  instantiate_scheme intern


let substitute (substmap : mono_type BoundIDMap.t) =
  let intern _ bid =
    match substmap |> BoundIDMap.find_opt bid with
    | None     -> assert false
    | Some(ty) -> ty
  in
  instantiate_scheme intern


let overwrite_range_of_type (type a) (rng : Range.t) ((_, tymain) : a typ) : a typ =
  (rng, tymain)


let show_base_type = function
  | UnitType   -> "unit"
  | BoolType   -> "bool"
  | IntType    -> "int"
  | BinaryType -> "binary"


let rec show_mono_type_scheme (type a) (showtv : a -> string) (ty : a typ) =
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(bty) ->
        show_base_type bty

    | FuncType(tydoms, tycod) ->
        let sdoms = tydoms |> List.map aux in
        let scod = aux tycod in
        "fun(" ^ (String.concat ", " sdoms) ^ ") -> " ^ scod

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


and show_mono_type_var_scheme showty (mtv : mono_type_var) =
  match mtv with
  | MustBeBound(mbbid) -> Format.asprintf "%a" MustBeBoundID.pp mbbid
  | Updatable(mtvu)    -> show_mono_type_var_updatable_ref_scheme showty mtvu


and show_mono_type_var_updatable_ref_scheme showty (mtvu : mono_type_var_updatable ref) =
  match !mtvu with
  | Link(ty)  -> showty (show_mono_type_var_scheme showty) ty
  | Free(fid) -> Format.asprintf "%a" FreeID.pp fid


let show_mono_type_var_updatable_ref =
  show_mono_type_var_updatable_ref_scheme show_mono_type_scheme


let show_mono_type_var =
  show_mono_type_var_scheme show_mono_type_scheme


let show_mono_type : mono_type -> string =
  show_mono_type_scheme show_mono_type_var


let pp_mono_type ppf ty =
  Format.fprintf ppf "%s" (show_mono_type ty)


let show_poly_type_var = function
  | Bound(bid) -> Format.asprintf "%a" BoundID.pp bid
  | Mono(mtv)  -> show_mono_type_var mtv


let show_poly_type : poly_type -> string =
  show_mono_type_scheme show_poly_type_var


let pp_poly_type ppf pty =
  Format.fprintf ppf "%s" (show_poly_type pty)


type name = OutputIdentifier.t
[@@deriving show { with_path = false; } ]

module ConstructorBranchMap = Map.Make(String)

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorBranchMap.t

let pp_constructor_branch_map ppf _ =
  Format.fprintf ppf "<constructor-branch-map>"


module TypeParameterAssoc = AssocList.Make(String)

type type_parameter_assoc = MustBeBoundID.t TypeParameterAssoc.t

module TypeParameterMap = Map.Make(String)

type local_type_parameter_map = MustBeBoundID.t TypeParameterMap.t

module SynonymIDSet = Set.Make(TypeID.Synonym)

module OpaqueID = TypeID.Opaque

module OpaqueIDSet = Set.Make(OpaqueID)

module OpaqueIDMap = Map.Make(OpaqueID)
(*
type 'r concrete_signature_ =
  | AtomicPoly   of poly_type
  | AtomicKinded of poly_type * kind
  | AtomicAbs    of 'r abstract_signature_
  | ConcModule   of 'r module_signature_
*)
type 'r module_signature_ =
  | ConcStructure of 'r
  | ConcFunctor   of OpaqueIDSet.t * 'r module_signature_ * (OpaqueIDSet.t * 'r module_signature_)

module IdentifierMap = Map.Make(String)

module TypeNameMap = Map.Make(String)

module ModuleNameMap = Map.Make(String)

module SignatureNameMap = Map.Make(String)

type pattern =
  | IPUnit
  | IPBool        of bool
  | IPInt         of int
  | IPVar         of name
  | IPWildCard
  | IPListNil
  | IPListCons    of pattern * pattern
  | IPTuple       of pattern TupleList.t
  | IPConstructor of ConstructorID.t * pattern list
[@@deriving show { with_path = false; } ]

type single_type_binding =
  | IVariant of TypeID.Variant.t * constructor_branch_map
  | ISynonym of TypeID.Synonym.t * poly_type

type type_opacity =
  | Transparent of BoundID.t list * single_type_binding
  | Opaque      of kind * OpaqueID.t

type 'a abstracted = OpaqueIDSet.t * 'a

type signature_record = {
  sr_vals    : (poly_type * name) IdentifierMap.t;
  sr_types   : type_opacity TypeNameMap.t;
  sr_modules : (signature_record module_signature_ * name) ModuleNameMap.t;
  sr_sigs    : ((signature_record module_signature_) abstracted) SignatureNameMap.t;
}

type module_signature = signature_record module_signature_

type val_binding =
  | INonRec of (identifier * name * poly_type * ast)
  | IRec    of (identifier * name * poly_type * ast) list

and binding =
  | IBindVal     of val_binding
  | IBindType
  | IBindModule  of name * ast
  | IBindSig
  | IBindInclude of ast

and ast =
  | IBaseConst   of base_constant
  | IVar         of name
  | ILambda      of name option * name list * ast
  | IApply       of name * ast list
  | ILetIn       of name * ast * ast
  | ICase        of ast * branch list
  | IReceive     of branch list
  | ITuple       of ast TupleList.t
  | IListNil
  | IListCons    of ast * ast
  | IConstructor of ConstructorID.t * ast list
  | IStructure   of binding list
  | IAccess      of ast * name

and branch =
  | IBranch of pattern * ast option * ast

type witness_map = (TypeID.Synonym.t * BoundID.t list * poly_type) OpaqueIDMap.t

module SigRecord = struct

  type t = signature_record


  let empty =
    {
      sr_vals    = IdentifierMap.empty;
      sr_types   = TypeNameMap.empty;
      sr_modules = ModuleNameMap.empty;
      sr_sigs    = SignatureNameMap.empty;
    }


  let add_val (x : identifier) (pty : poly_type) (name : name) (sigr : t) : t =
    { sigr with sr_vals = sigr.sr_vals |> IdentifierMap.add x (pty, name) }


  let add_synonym_type (tynm : type_name) (sid : TypeID.Synonym.t) (typarams : BoundID.t list) (ptyreal : poly_type) (sigr : t) : t =
    { sigr with sr_types = sigr.sr_types |> TypeNameMap.add tynm (Transparent(typarams, ISynonym(sid, ptyreal))) }


  let add_variant_type (tynm : type_name) (vid : TypeID.Variant.t) (typarams : BoundID.t list) (ctorbrs : constructor_branch_map) (sigr : t) : t =
    { sigr with sr_types = sigr.sr_types |> TypeNameMap.add tynm (Transparent(typarams, IVariant(vid, ctorbrs))) }


  let find_type (tynm : type_name) (sigr : t) : type_opacity option =
    sigr.sr_types |> TypeNameMap.find_opt tynm


  let add_opaque_type (tynm : type_name) (oid : OpaqueID.t) (kd : kind) (sigr : t) : t =
    { sigr with sr_types = sigr.sr_types |> TypeNameMap.add tynm (Opaque(kd, oid)) }


  let add_module (modnm : module_name) (modsig : module_signature) (name : name) (sigr : t) : t =
    { sigr with sr_modules = sigr.sr_modules |> ModuleNameMap.add modnm (modsig, name) }


  let find_module (modnm : module_name) (sigr : t) : (module_signature * name) option =
    sigr.sr_modules |> ModuleNameMap.find_opt modnm


  let add_signature (signm : signature_name) (absmodsig : module_signature abstracted) (sigr : t) : t =
    { sigr with sr_sigs = sigr.sr_sigs |> SignatureNameMap.add signm absmodsig }


  let find_signature (signm : signature_name) (sigr : t) : (module_signature abstracted) option =
    sigr.sr_sigs |> SignatureNameMap.find_opt signm


  let fold (type a)
      ~v:(fv : identifier -> poly_type * name -> a -> a)
      ~t:(ft : type_name -> type_opacity -> a -> a)
      ~m:(fm : module_name -> module_signature * name -> a -> a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> a)
      (init : a) (sigr : t) : a =
    init
      |> SignatureNameMap.fold fs sigr.sr_sigs
      |> ModuleNameMap.fold fm sigr.sr_modules
      |> TypeNameMap.fold ft sigr.sr_types
      |> IdentifierMap.fold fv sigr.sr_vals


  let map
      ~v:(fv : poly_type * name -> poly_type * name)
      ~t:(ft : type_opacity -> type_opacity)
      ~m:(fm : module_signature * name -> module_signature * name)
      ~s:(fs : module_signature abstracted -> module_signature abstracted)
      (sigr : t) : t =
    {
      sr_vals    = sigr.sr_vals |> IdentifierMap.map fv;
      sr_types   = sigr.sr_types |> TypeNameMap.map ft;
      sr_modules = sigr.sr_modules |> ModuleNameMap.map fm;
      sr_sigs    = sigr.sr_sigs |> SignatureNameMap.map fs;
    }


  let overwrite (superior : t) (inferior : t) : t =
    let left _ x y = Some(x) in
    let sr_vals    = IdentifierMap.union    left superior.sr_vals    inferior.sr_vals in
    let sr_types   = TypeNameMap.union      left superior.sr_types   inferior.sr_types in
    let sr_modules = ModuleNameMap.union    left superior.sr_modules inferior.sr_modules in
    let sr_sigs    = SignatureNameMap.union left superior.sr_sigs    inferior.sr_sigs in
    { sr_vals; sr_types; sr_modules; sr_sigs }


  let disjoint_union (rng : Range.t) (sigr1 : t) (sigr2 : t) : t =
    let conflict s _ _ = raise (ConflictInSignature(rng, s)) in
    let sr_vals    = IdentifierMap.union    conflict sigr1.sr_vals    sigr2.sr_vals in
    let sr_types   = TypeNameMap.union      conflict sigr1.sr_types   sigr2.sr_types in
    let sr_modules = ModuleNameMap.union    conflict sigr1.sr_modules sigr2.sr_modules in
    let sr_sigs    = SignatureNameMap.union conflict sigr1.sr_sigs    sigr2.sr_sigs in
    { sr_vals; sr_types; sr_modules; sr_sigs }

end


let pp_comma ppf () =
  Format.fprintf ppf ", "


let rec display_signature (depth : int) (modsig : module_signature) : unit =
  let indent = String.make (depth * 2) ' ' in
  match modsig with
  | ConcStructure(sigr) ->
      Format.printf "%ssig\n" indent;
      display_structure (depth + 1) sigr;
      Format.printf "%send\n" indent

  | ConcFunctor(_oidset, _modsigdom, _absmodsigcod) ->
      Format.printf "%s: fun ...\n" indent


and display_structure (depth : int) (sigr : SigRecord.t) : unit =
  let indent = String.make (depth * 2) ' ' in
  sigr |> SigRecord.fold
      ~v:(fun x (pty, _) () ->
        Format.printf "%sval %s: %a\n" indent x pp_poly_type pty
      )
      ~t:(fun tynm tyopacity () ->
        match tyopacity with
        | Transparent(typarams, ISynonym(_sid, ptyreal)) ->
            Format.printf "%stype %s<%a> = %a\n"
              indent
              tynm
              (Format.pp_print_list ~pp_sep:pp_comma BoundID.pp) typarams
              pp_poly_type ptyreal

        | Transparent(typarams, IVariant(_vid, _ctorbrs)) ->
            Format.printf "%stype %s<%a> = (variant)\n"
              indent
              tynm
              (Format.pp_print_list ~pp_sep:pp_comma BoundID.pp) typarams

        | Opaque(kind, _) ->
            Format.printf "%stype %s:: %d\n" indent tynm kind
      )
      ~m:(fun modnm (modsig, _) () ->
        Format.printf "%smodule %s:\n" indent modnm;
        display_signature (depth + 1) modsig;
      )
      ~s:(fun signm _ () ->
        Format.printf "signature %s\n" signm
      )
      ()
