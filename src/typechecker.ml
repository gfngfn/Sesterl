
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


let rec occurs fid (_, tymain) =
  match tymain with
  | BaseType(_)                      -> false
  | FuncType(ty1, ty2)               -> occurs fid ty1 || occurs fid ty2
  | TypeVar({contents = Link(ty)})   -> occurs fid ty
  | TypeVar({contents = Free(fidx)}) -> FreeID.equal fid fidx


let unify tyact tyexp =
  let rec aux ty1 ty2 =
    let (_, ty1main) = ty1 in
    let (_, ty2main) = ty2 in
    Format.printf "unify %a ==== %a\n" pp_mono_type ty1 pp_mono_type ty2;
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

    | (TypeVar({contents = Free(fid1)} as tvref1), TypeVar({contents = (Free(fid2) as tv2)})) ->
        let () =
          if FreeID.equal fid1 fid2 then
            Format.printf "equal\n"
          else
            let () = Format.printf "%a <--- %a\n" FreeID.pp fid1 FreeID.pp fid2 in
            tvref1 := tv2
        in
        Consistent

    | (TypeVar({contents = Free(fid1)} as tvref1), _) ->
        if occurs fid1 ty2 then
          Inclusion(fid1)
        else
          begin
            tvref1 := Link(ty2);
            Consistent
          end

    | (_, TypeVar({contents = Free(fid2)} as tvref2)) ->
        if occurs fid2 ty1 then
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


let fresh_type rng =
  let fid = FreeID.fresh () in
  let tvref = ref (Free(fid)) in
    (rng, TypeVar(tvref))


let rec aux tyenv (rng, utastmain) =
  match utastmain with
  | Int(_) -> (rng, BaseType(IntType))
  | Bool(_) -> (rng, BaseType(BoolType))

  | Var(x) ->
      begin
        match tyenv |> Typeenv.find_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some((_, tymain) as ty) ->
            Format.printf "$Var %s : %a\n" x pp_mono_type ty;
            (rng, tymain)
      end

  | Lambda((rngv, x), utast0) ->
      let tydom = fresh_type rngv in
      Format.printf "$Lambda %s : %a\n" x pp_mono_type tydom;
      let tycod = aux (tyenv |> Typeenv.add x tydom) utast0 in
      (rng, FuncType(tydom, tycod))

  | Apply(utast1, utast2) ->
      let ty1 = aux tyenv utast1 in
      let ty2 = aux tyenv utast2 in
      let tyret = fresh_type rng in
      let () = unify ty1 (Range.dummy "Apply", FuncType(ty2, tyret)) in
      tyret

  | If(utast0, utast1, utast2) ->
      let ty0 = aux tyenv utast0 in
      let () = unify ty0 (Range.dummy "If", BaseType(BoolType)) in
      let ty1 = aux tyenv utast1 in
      let ty2 = aux tyenv utast2 in
      let () = unify ty1 ty2 in
      ty1

  | LetIn((_, x), utast1, utast2) ->
      let ty1 = aux tyenv utast1 in  (* -- monomorphic -- *)
      let ty2 = aux (tyenv |> Typeenv.add x ty1) utast2 in
      ty2

  | LetRecIn((rngv, x), utast1, utast2) ->
      let tyf = fresh_type rngv in
      let tyenv = tyenv |> Typeenv.add x tyf in
      let ty1 = aux tyenv utast1 in  (* -- monomorphic -- *)
      let () = unify ty1 tyf in
      let ty2 = aux tyenv utast2 in
      ty2


let main utast =
  let tyenv = Primitives.initial_type_environment in
  aux tyenv utast
