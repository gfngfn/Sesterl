
open Syntax


exception UnboundVariable of Range.t * string
exception ContradictionError of mono_type * mono_type
exception InclusionError of FreeID.t * mono_type * mono_type


type unification_result =
  | Consistent
  | Contradiction
  | Inclusion of FreeID.t


let (&&&) res1 res2 =
  match (res1, res2) with
  | (Consistent, _) -> res2
  | _               -> res1


let occurs fid ty =
  let lev = FreeID.get_level fid in
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(_) ->
        false

    | FuncType(ty1, ty2) ->
        let b1 = aux ty1 in
        let b2 = aux ty2 in
        b1 || b2

    | TypeVar({contents = Link(ty)}) ->
        aux ty

    | TypeVar({contents = Free(fidx)}) ->
        if FreeID.equal fid fidx then true else
          begin
            FreeID.update_level fidx lev;
            false
          end

  in
  aux ty


let unify tyact tyexp =
  let rec aux ty1 ty2 =
    let (_, ty1main) = ty1 in
    let (_, ty2main) = ty2 in
    match (ty1main, ty2main) with
    | (TypeVar({contents = Link(ty1l)}), _) ->
        aux ty1l ty2

    | (_, TypeVar({contents = Link(ty2l)})) ->
        aux ty1 ty2l

    | (BaseType(bt1), BaseType(bt2)) ->
        if bt1 = bt2 then Consistent else Contradiction

    | (FuncType(ty1d, ty1c), FuncType(ty2d, ty2c)) ->
        let res1 = aux ty1d ty2d in
        let res2 = aux ty1c ty2c in
        res1 &&& res2

    | (TypeVar({contents = Free(fid1)} as tvref1), TypeVar({contents = Free(fid2)})) ->
        let () =
          if FreeID.equal fid1 fid2 then () else
            begin
              tvref1 := Link(ty2);  (* -- not `Free(fid2)`! -- *)
            end
        in
        Consistent

    | (TypeVar({contents = Free(fid1)} as tvref1), _) ->
        let b = occurs fid1 ty2 in
        if b then
          Inclusion(fid1)
        else
          begin
            tvref1 := Link(ty2);
            Consistent
          end

    | (_, TypeVar({contents = Free(fid2)} as tvref2)) ->
        let b = occurs fid2 ty1 in
        if b then
          Inclusion(fid2)
        else
          begin
            tvref2 := Link(ty1);
            Consistent
          end

    | _ ->
        Contradiction
  in
  let res = aux tyact tyexp in
  match res with
  | Consistent     -> ()
  | Contradiction  -> raise (ContradictionError(tyact, tyexp))
  | Inclusion(fid) -> raise (InclusionError(fid, tyact, tyexp))


let fresh_type lev rng =
  let fid = FreeID.fresh lev in
  let tvref = ref (Free(fid)) in
    (rng, TypeVar(tvref))


let rec aux lev tyenv (rng, utastmain) =
  match utastmain with
  | Int(_) -> (rng, BaseType(IntType))
  | Bool(_) -> (rng, BaseType(BoolType))

  | Var(x) ->
      begin
        match tyenv |> Typeenv.find_opt x with
        | None               -> raise (UnboundVariable(rng, x))
        | Some((_, ptymain)) -> instantiate lev (rng, ptymain)
      end

  | Lambda((rngv, x), utast0) ->
      let tydom = fresh_type lev rngv in
      let ptydom = lift tydom in
      let tycod = aux lev (tyenv |> Typeenv.add x ptydom) utast0 in
      (rng, FuncType(tydom, tycod))

  | Apply(utast1, utast2) ->
      let ty1 = aux lev tyenv utast1 in
      let ty2 = aux lev tyenv utast2 in
      let tyret = fresh_type lev rng in
      unify ty1 (Range.dummy "Apply", FuncType(ty2, tyret));
      tyret

  | If(utast0, utast1, utast2) ->
      let ty0 = aux lev tyenv utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let ty1 = aux lev tyenv utast1 in
      let ty2 = aux lev tyenv utast2 in
      unify ty1 ty2;
      ty1

  | LetIn(ident, utast1, utast2) ->
      let tyenv = typecheck_let lev tyenv ident utast1 in
      let ty2 = aux lev tyenv utast2 in
      ty2

  | LetRecIn(ident, utast1, utast2) ->
      let tyenv = typecheck_letrec lev tyenv ident utast1 in
      let ty2 = aux lev tyenv utast2 in
      ty2


and typecheck_let (lev : int) (tyenv : Typeenv.t) ((rngv, x) : Range.t * identifier) (utast1 : untyped_ast) : Typeenv.t =
  let ty1 = aux (lev + 1) tyenv utast1 in
  let pty1 = generalize lev ty1 in
  tyenv |> Typeenv.add x pty1


and typecheck_letrec (lev : int) (tyenv : Typeenv.t) ((rngv, x) : Range.t * identifier) (utast1 : untyped_ast) : Typeenv.t =
  let tyf = fresh_type (lev + 1) rngv in
  let ptyf = lift tyf in
  let tyenv = tyenv |> Typeenv.add x ptyf in
  let ty1 = aux (lev + 1) tyenv utast1 in
  unify ty1 tyf;
  tyenv


let main (decls : declaration list) : Typeenv.t =
  let tyenv = Primitives.initial_type_environment in
  decls |> List.fold_left (fun tyenv decl ->
    match decl with
    | ValDecl(isrec, binder, utast) ->
        if isrec then
          typecheck_letrec 0 tyenv binder utast
        else
          typecheck_let 0 tyenv binder utast
  ) tyenv
