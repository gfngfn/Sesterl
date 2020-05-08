
open MyUtil

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
  | ModProjVal   of untyped_module * identifier ranged

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
  untyped_binding_main ranged

and untyped_binding_main =
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


let instantiate_scheme (type a) (intern : Range.t -> poly_type_var -> a typ) (pty : poly_type) : a typ =

  let rec aux (rng, ptymain) =
    match ptymain with
    | BaseType(bty) ->
        (rng, BaseType(bty))

    | TypeVar(ptv) ->
        intern rng ptv

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
  let intern (rng : Range.t) (ptv : poly_type_var) : mono_type =
    match ptv with
    | Mono(mtv) ->
        (rng, TypeVar(mtv))

    | Bound(bid) ->
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
  let intern (rng : Range.t) (ptv : poly_type_var) : mono_type =
    match ptv with
    | Mono(mtv) ->
        (rng, TypeVar(mtv))

    | Bound(bid) ->
        begin
          match bfmap |> BoundIDMap.find_opt bid with
          | None      -> assert false
          | Some(mtv) -> (rng, TypeVar(mtv))
        end
  in
  instantiate_scheme intern


let substitute_mono_type (substmap : mono_type BoundIDMap.t) : poly_type -> mono_type =
  let intern (rng : Range.t) (ptv : poly_type_var) : mono_type =
    match ptv with
    | Mono(mtv) ->
        (rng, TypeVar(mtv))

    | Bound(bid) ->
        begin
          match substmap |> BoundIDMap.find_opt bid with
          | None     -> assert false
          | Some(ty) -> ty
        end
  in
  instantiate_scheme intern


let substitute_poly_type (substmap : poly_type BoundIDMap.t) : poly_type -> poly_type =
  let intern (rng : Range.t) (ptv : poly_type_var) : poly_type =
    match ptv with
    | Mono(_) ->
        (rng, TypeVar(ptv))

    | Bound(bid) ->
        begin
          match substmap |> BoundIDMap.find_opt bid with
          | None      -> assert false
          | Some(pty) -> pty
        end
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
  | Updatable(mtvu)    -> show_mono_type_var_updatable_scheme showty !mtvu


and show_mono_type_var_updatable_scheme showty (mtvu : mono_type_var_updatable) =
  match mtvu with
  | Link(ty)  -> showty (show_mono_type_var_scheme showty) ty
  | Free(fid) -> Format.asprintf "%a" FreeID.pp fid


let show_mono_type_var_updatable =
  show_mono_type_var_updatable_scheme show_mono_type_scheme


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

module ConstructorMap = Map.Make(String)

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorMap.t

module TypeParameterAssoc = AssocList.Make(String)

type type_parameter_assoc = MustBeBoundID.t TypeParameterAssoc.t

module TypeParameterMap = Map.Make(String)

type local_type_parameter_map = MustBeBoundID.t TypeParameterMap.t

module SynonymIDSet = Set.Make(TypeID.Synonym)

module SynonymIDMap = Map.Make(TypeID.Synonym)

module SynonymIDHashTable = Hashtbl.Make(TypeID.Synonym)

module VariantIDMap = Map.Make(TypeID.Variant)

module VariantIDHashTable = Hashtbl.Make(TypeID.Variant)

module OpaqueIDSet = Set.Make(TypeID.Opaque)

module OpaqueIDMap = Map.Make(TypeID.Opaque)

module OpaqueIDHashTable = Hashtbl.Make(TypeID.Opaque)

type 'r module_signature_ =
  | ConcStructure of 'r
  | ConcFunctor   of OpaqueIDSet.t * 'r module_signature_ * (OpaqueIDSet.t * 'r module_signature_)

module ValNameMap = Map.Make(String)

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

type type_opacity = TypeID.t * int

type 'a abstracted = OpaqueIDSet.t * 'a

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type signature_record =
  signature_record_entry Alist.t

and signature_record_entry =
  | SRVal      of identifier * (poly_type * name)
  | SRRecTypes of (type_name * type_opacity) list
  | SRModule   of module_name * (signature_record module_signature_ * name)
  | SRSig      of signature_name * (signature_record module_signature_) abstracted
  | SRCtor     of constructor_name * constructor_entry

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


module SigRecord = struct

  type t = signature_record


  let empty : t =
    Alist.empty


  let add_val (x : identifier) (pty : poly_type) (name : name) (sigr : t) : t =
    Alist.extend sigr (SRVal(x, (pty, name)))


  let find_val (x0 : identifier) (sigr : t) : (poly_type * name) option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRVal(x, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                -> None
    )


  let add_types (tydefs : (type_name * type_opacity) list) (sigr : t) : t =
    Alist.extend sigr (SRRecTypes(tydefs))


  let add_constructors (vid : TypeID.Variant.t) (typarams : BoundID.t list) (ctorbrs : constructor_branch_map) (sigr : t) : t =
    ConstructorMap.fold (fun ctornm (ctorid, ptys) ctors ->
      let entry =
        {
          belongs         = vid;
          constructor_id  = ctorid;
          type_variables  = typarams;
          parameter_types = ptys;
        }
      in
      Alist.extend sigr (SRCtor(ctornm, entry))
    ) ctorbrs sigr


  let find_constructor (ctornm0 : constructor_name) (sigr : t) : constructor_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRCtor(ctornm, entry) -> if String.equal ctornm ctornm0 then Some(entry) else None
    | _                     -> None
    )


  let find_type (tynm0 : type_name) (sigr : t) : type_opacity option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRRecTypes(tydefs) ->
        tydefs |> List.find_map (fun (tynm, tyopac) ->
          if String.equal tynm tynm0 then Some(tyopac) else None
        )

    | _ ->
        None
    )


  let add_opaque_type (tynm : type_name) (oid : TypeID.Opaque.t) (kd : kind) (sigr : t) : t =
    Alist.extend sigr (SRRecTypes[ (tynm, (TypeID.Opaque(oid), kd)) ])


  let add_module (modnm : module_name) (modsig : module_signature) (name : name) (sigr : t) : t =
    Alist.extend sigr (SRModule(modnm, (modsig, name)))


  let find_module (modnm0 : module_name) (sigr : t) : (module_signature * name) option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRModule(modnm, mentry) -> if String.equal modnm modnm0 then Some(mentry) else None
    | _                       -> None
    )


  let add_signature (signm : signature_name) (absmodsig : module_signature abstracted) (sigr : t) : t =
    Alist.extend sigr (SRSig(signm, absmodsig))


  let find_signature (signm0 : signature_name) (sigr : t) : (module_signature abstracted) option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRSig(signm, absmodsig) -> if String.equal signm signm0 then Some(absmodsig) else None
    | _                       -> None
    )


  let fold (type a)
      ~v:(fv : identifier -> poly_type * name -> a -> a)
      ~t:(ft : (type_name * type_opacity) list -> a -> a)
      ~m:(fm : module_name -> module_signature * name -> a -> a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> a)
      ~c:(fc : constructor_name -> constructor_entry -> a -> a)
      (init : a) (sigr : t) : a =
    sigr |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SRVal(x, ventry)        -> fv x ventry acc
      | SRRecTypes(tydefs)      -> ft tydefs acc
      | SRModule(modnm, mentry) -> fm modnm mentry acc
      | SRSig(signm, absmodsig) -> fs signm absmodsig acc
      | SRCtor(ctor, ctorentry) -> fc ctor ctorentry acc
    ) init


  let map_and_fold (type a)
      ~v:(fv : poly_type * name -> a -> (poly_type * name) * a)
      ~t:(ft : type_opacity list -> a -> type_opacity list * a)
      ~m:(fm : module_signature * name -> a -> (module_signature * name) * a)
      ~s:(fs : module_signature abstracted -> a -> module_signature abstracted * a)
      ~c:(fc : constructor_entry -> a -> constructor_entry * a)
      (init : a) (sigr : t) : t * a =
      sigr |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SRVal(x, ventry) ->
            let (ventry, acc) = fv ventry acc in
            (Alist.extend sigracc (SRVal(x, ventry)), acc)

        | SRRecTypes(tydefs) ->
            let tynms = tydefs |> List.map fst in
            let (tyopacs, acc) = ft (tydefs |> List.map snd) acc in
            (Alist.extend sigracc (SRRecTypes(List.combine tynms tyopacs)), acc)

        | SRModule(modnm, mentry) ->
            let (mentry, acc) = fm mentry acc in
            (Alist.extend sigracc (SRModule(modnm, mentry)), acc)

        | SRSig(signm, absmodsig) ->
            let (absmodsig, acc) = fs absmodsig acc in
            (Alist.extend sigracc (SRSig(signm, absmodsig)), acc)

        | SRCtor(ctor, ctorentry) ->
            let (ctorentry, acc) = fc ctorentry acc in
            (Alist.extend sigracc (SRCtor(ctor, ctorentry)), acc)
      ) (Alist.empty, init)

(*
  let overwrite (superior : t) (inferior : t) : t =
    let left _ x _ = Some(x) in
    let sr_vals    = ValNameMap.union       left superior.sr_vals    inferior.sr_vals in
    let sr_types   = TypeNameMap.union      left superior.sr_types   inferior.sr_types in
    let sr_modules = ModuleNameMap.union    left superior.sr_modules inferior.sr_modules in
    let sr_sigs    = SignatureNameMap.union left superior.sr_sigs    inferior.sr_sigs in
    let sr_ctors   = ConstructorMap.union   left superior.sr_ctors   inferior.sr_ctors in
    { sr_vals; sr_types; sr_modules; sr_sigs; sr_ctors }
*)

  let disjoint_union (rng : Range.t) (sigr1 : t) (sigr2 : t) : t =
    let check_none s opt =
      match opt with
      | None    -> ()
      | Some(_) -> raise (ConflictInSignature(rng, s))
    in
    sigr2 |> Alist.to_list |> List.fold_left (fun sigracc entry ->
      let () =
        match entry with
        | SRVal(x, _)        -> check_none x (find_val x sigr1)
        | SRRecTypes(tydefs) -> tydefs |> List.iter (fun (tynm, _) -> check_none tynm (find_type tynm sigr1))
        | SRModule(modnm, _) -> check_none modnm (find_module modnm sigr1)
        | SRSig(signm, _)    -> check_none signm (find_signature signm sigr1)
        | SRCtor(ctor, _)    -> check_none ctor (find_constructor ctor sigr1)
      in
      Alist.extend sigracc entry
    ) sigr1

end
