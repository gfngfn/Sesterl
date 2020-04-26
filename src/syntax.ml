
exception UnidentifiedToken of Range.t * string
exception SeeEndOfFileInComment of Range.t


type identifier = string


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


type binder = Range.t * identifier


let pp_binder ppf (_, s) =
  Format.fprintf ppf "%a" pp_identifier s


type untyped_ast = Range.t * untyped_ast_main
  [@printer (fun ppf (_, utastmain) -> pp_untyped_ast_main ppf utastmain)]

and untyped_ast_main =
  | Unit
  | Bool     of bool
  | Int      of int
  | Var      of identifier
  | Lambda   of binder list * untyped_ast
  | Apply    of untyped_ast * untyped_ast list
  | If       of untyped_ast * untyped_ast * untyped_ast
  | LetIn    of binder * untyped_ast * untyped_ast
  | LetRecIn of binder * untyped_ast * untyped_ast
  | Do       of binder option * untyped_ast * untyped_ast
  | Receive  of untyped_branch list

and untyped_branch =
  | Branch of untyped_pattern * untyped_ast option * untyped_ast

and untyped_pattern = Range.t * untyped_pattern_main

and untyped_pattern_main =
  | PUnit
  | PBool of bool
  | PInt  of int
  | PVar  of identifier
  | PWildCard
[@@deriving show { with_path = false; } ]

type declaration =
  | ValDecl of bool * binder * untyped_ast
[@@deriving show { with_path = false; } ]

type base_type =
  | IntType
  | BoolType
  | UnitType
[@@deriving show { with_path = false; } ]

type 'a typ = Range.t * 'a typ_main
(*
  [@printer (fun (pp_sub : Format.formatter -> 'a -> unit) (ppf : Format.formatter) ((_, tymain) : 'a typ) -> Format.fprintf ppf "%a" (pp_typ_main pp_sub) tymain)]
*)
and 'a typ_main =
  | BaseType of base_type
  | FuncType of ('a typ) list * 'a typ
  | PidType  of 'a pid_type
  | EffType  of 'a effect * 'a typ
  | TypeVar  of 'a
[@@deriving show { with_path = false; } ]

and 'a effect =
  | Effect of 'a typ

and 'a pid_type =
  | Pid of 'a typ

type mono_type_var =
  | Free of FreeID.t
  | Link of mono_type

and mono_type = (mono_type_var ref) typ

type poly_type_var =
  | Mono  of mono_type_var ref
  | Bound of BoundID.t

type poly_type = poly_type_var typ

module FreeIDHashTable = Hashtbl.Make(FreeID)


let lift_scheme rngf pred ty =

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

    | TypeVar({contents = Link(ty)}) ->
        aux ty

    | TypeVar({contents = Free(fid)} as mtv) ->
        let ptv =
          if pred fid then
            Bound(intern fid)
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

  and aux_effect (Effect(ty)) =
    let pty = aux ty in
    Effect(pty)

  and aux_pid_type (Pid(ty)) =
    let pty = aux ty in
    Pid(pty)
  in
  aux ty


let generalize lev ty =
  lift_scheme
    (fun _ -> Range.dummy "erased")
    (fun fid ->
      let levx = FreeID.get_level fid in
      lev <= levx
    ) ty


let lift ty =
  lift_scheme (fun rng -> rng) (fun _ -> false) ty


module BoundIDHashTable = Hashtbl.Make(BoundID)


let instantiate lev pty =

  let bidht = BoundIDHashTable.create 32 in

  let intern bid =
    match BoundIDHashTable.find_opt bidht bid with
    | Some(mtv) ->
        mtv

    | None ->
        let fid = FreeID.fresh lev in
        let mtv = ref (Free(fid)) in
        BoundIDHashTable.add bidht bid mtv;
        mtv
  in

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

  and aux_effect (Effect(pty)) =
    let ty = aux pty in
    Effect(ty)

  and aux_pid_type (Pid(pty)) =
    let ty = aux pty in
    Pid(ty)
  in
  aux pty


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

  and aux_effect (Effect(ty)) =
    let s = aux ty in
    "[" ^ s ^ "]"

  and aux_pid_type (Pid(ty)) =
    aux ty
  in
  aux ty


and show_mono_type_var_scheme showty tvref =
  match !tvref with
  | Link(ty)  -> showty (show_mono_type_var_scheme showty) ty
  | Free(fid) -> Format.asprintf "%a" FreeID.pp fid


let show_mono_type_var_ref =
  show_mono_type_var_scheme show_mono_type_scheme


let show_mono_type =
  show_mono_type_scheme show_mono_type_var_ref


let pp_mono_type ppf ty =
  Format.fprintf ppf "%s" (show_mono_type ty)


let show_poly_type_var = function
  | Bound(bid)  -> Format.asprintf "%a" BoundID.pp bid
  | Mono(tvref) -> show_mono_type_var_ref tvref


let show_poly_type =
  show_mono_type_scheme show_poly_type_var


let pp_poly_type ppf pty =
  Format.fprintf ppf "%s" (show_poly_type pty)
