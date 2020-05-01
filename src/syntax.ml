
exception UnidentifiedToken     of Range.t * string
exception SeeEndOfFileInComment of Range.t


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


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


type base_type =
  | IntType
  | BoolType
  | UnitType
[@@deriving show { with_path = false; } ]

type manual_type = manual_type_main ranged

and manual_type_main =
  | MTypeName    of type_name * manual_type list
  | MFuncType    of manual_type list * manual_type
  | MProductType of manual_type TupleList.t
  | MTypeVar     of type_variable_name
[@@deriving show { with_path = false; } ]

type binder = identifier ranged * manual_type option

let pp_binder ppf ((_, s), _) =
  pp_identifier ppf s


type base_constant =
  | Unit
  | Bool of bool
  | Int  of int
[@@deriving show { with_path = false; } ]

type untyped_ast =
  untyped_ast_main ranged
[@printer (fun ppf (_, utastmain) -> pp_untyped_ast_main ppf utastmain)]

and untyped_ast_main =
  | BaseConst   of base_constant
  | Var         of identifier
  | Lambda      of binder list * untyped_ast
  | Apply       of untyped_ast * untyped_ast list
  | If          of untyped_ast * untyped_ast * untyped_ast
  | LetIn       of rec_or_nonrec * untyped_ast
  | LetPatIn    of untyped_pattern * untyped_ast * untyped_ast
  | Do          of binder option * untyped_ast * untyped_ast
  | Receive     of untyped_branch list
  | Tuple       of untyped_ast TupleList.t
  | ListNil
  | ListCons    of untyped_ast * untyped_ast
  | Case        of untyped_ast * untyped_branch list
  | Constructor of constructor_name * untyped_ast list

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

type constructor_branch =
  | ConstructorBranch of constructor_name * manual_type list
[@@deriving show { with_path = false; } ]

type untyped_binding =
  | BindVal    of rec_or_nonrec
  | BindType   of type_name ranged * (type_variable_name ranged) list * constructor_branch list
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
  | VariantType of TypeID.t * ('a typ) list

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

    | VariantType(tyid, tyargs) ->
        (rngf rng, VariantType(tyid, tyargs |> List.map aux))

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


let instantiate_scheme (intern : BoundID.t -> mono_type_var) (lev : int) (pty : poly_type) : mono_type =

  let rec aux (rng, ptymain) =
    match ptymain with
    | BaseType(bty) ->
        (rng, BaseType(bty))

    | TypeVar(Mono(mtv)) ->
        (rng, TypeVar(mtv))

    | TypeVar(Bound(bid)) ->
        let mtv = intern bid in
        (rng, TypeVar(mtv))

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

    | VariantType(tyid, ptyargs) ->
        (rng, VariantType(tyid, ptyargs |> List.map aux))

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
  let intern bid =
    match BoundIDHashTable.find_opt bidht bid with
    | Some(mtvu) ->
        Updatable(mtvu)

    | None ->
        let fid = FreeID.fresh lev in
        let mtvu = ref (Free(fid)) in
        BoundIDHashTable.add bidht bid mtvu;
        Updatable(mtvu)
  in
  instantiate_scheme intern lev pty


let instantiate_by_map (bfmap : mono_type_var BoundIDMap.t) =
  let intern bid =
    match bfmap |> BoundIDMap.find_opt bid with
    | None      -> assert false
    | Some(mtv) -> mtv
  in
  instantiate_scheme intern


let overwrite_range_of_type (type a) (rng : Range.t) ((_, tymain) : a typ) : a typ =
  (rng, tymain)


let show_base_type = function
  | UnitType -> "unit"
  | BoolType -> "bool"
  | IntType  -> "int"


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
        "pid(" ^ spid ^ ")"

    | TypeVar(tv) ->
        showtv tv

    | ProductType(tys) ->
        let ss = tys |> TupleList.to_list |> List.map aux in
        Printf.sprintf "(%s)" (String.concat ", " ss)

    | ListType(ty0) ->
        let s0 = aux ty0 in
        Printf.sprintf "list(%s)" s0

    | VariantType(tyid, tyargs) ->
        begin
          match tyargs with
          | [] ->
              Format.asprintf "%a" TypeID.pp tyid

          | _ :: _ ->
              let ss = tyargs |> List.map aux in
              Format.asprintf "%a(%s)" TypeID.pp tyid (String.concat ", " ss)
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

type pattern =
  | IPUnit
  | IPBool of bool
  | IPInt  of int
  | IPVar  of name
  | IPWildCard
  | IPListNil
  | IPListCons of pattern * pattern
  | IPTuple    of pattern TupleList.t
  | IPConstructor of ConstructorID.t * pattern list
[@@deriving show { with_path = false; } ]

type ast =
  | IBaseConst of base_constant
  | IVar       of name
  | ILambda    of name option * name list * ast
  | IApply     of name * ast list
  | ILetIn     of name * ast * ast
  | ICase      of ast * branch list
  | IReceive   of branch list
  | ITuple     of ast TupleList.t
  | IListNil
  | IListCons  of ast * ast
  | IConstructor of ConstructorID.t * ast list

and branch =
  | IBranch of pattern * ast option * ast
[@@deriving show { with_path = false; } ]

type binding =
  | IBindVal of name * ast
[@@deriving show { with_path = false; } ]

module ConstructorBranchMap = Map.Make(String)

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorBranchMap.t

module TypeParameterAssoc = AssocList.Make(String)

type type_parameter_assoc = MustBeBoundID.t TypeParameterAssoc.t

module TypeParameterMap = Map.Make(String)

type local_type_parameter_map = MustBeBoundID.t TypeParameterMap.t
