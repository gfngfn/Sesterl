
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
  | Bool     of bool
  | Int      of int
  | Var      of identifier
  | Lambda   of binder list * untyped_ast
  | Apply    of untyped_ast * untyped_ast list
  | If       of untyped_ast * untyped_ast * untyped_ast
  | LetIn    of binder * untyped_ast * untyped_ast
  | LetRecIn of binder * untyped_ast * untyped_ast
[@@deriving show { with_path = false; } ]

type declaration =
  | ValDecl of bool * binder * untyped_ast
[@@deriving show { with_path = false; } ]

type base_type =
  | IntType
  | BoolType
[@@deriving show { with_path = false; } ]

type 'a typ = Range.t * 'a typ_main
(*
  [@printer (fun (pp_sub : Format.formatter -> 'a -> unit) (ppf : Format.formatter) ((_, tymain) : 'a typ) -> Format.fprintf ppf "%a" (pp_typ_main pp_sub) tymain)]
*)
and 'a typ_main =
  | BaseType of base_type
  | FuncType of ('a typ) list * 'a typ
  | TypeVar  of 'a
[@@deriving show { with_path = false; } ]

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

  in
  aux pty


let show_base_type = function
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
        "(" ^ (String.concat ", " sdoms) ^ ") -> " ^ scod

    | TypeVar(tv) ->
        showtv tv
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
