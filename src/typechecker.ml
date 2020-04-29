
open MyUtil
open Syntax


exception UnboundVariable    of Range.t * identifier
exception ContradictionError of mono_type * mono_type
exception InclusionError     of FreeID.t * mono_type * mono_type
exception BoundMoreThanOnceInPattern of Range.t * identifier


module BindingMap = Map.Make(String)


type binding_map = (mono_type * name) BindingMap.t


type unification_result =
  | Consistent
  | Contradiction
  | Inclusion of FreeID.t


type scope =
  | Local
  | Global


let (&&&) res1 res2 =
  match (res1, res2) with
  | (Consistent, _) -> res2
  | _               -> res1


let iapply efun eargs =
  match efun with
  | IVar(name) ->
      IApply(name, eargs)

  | _ ->
      let name = OutputIdentifier.fresh () in
      ILetIn(name, efun, IApply(name, eargs))


let ilambda names e0 =
  ILambda(None, names, e0)


let ithunk e =
  ilambda [] e


let iforce e =
  iapply e []


let iletrecin name_outer name_inner e1 e2 =
  match e1 with
  | ILambda(None, names, e0) ->
      ILetIn(name_outer, ILambda(Some(name_inner), names, e1), e2)

  | _ ->
      assert false


let occurs fid ty =
  let lev = FreeID.get_level fid in
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(_) ->
        false

    | FuncType(tydoms, tycod) ->
        let b1 = aux_list tydoms in
        let b2 = aux tycod in
        b1 || b2
          (* -- must not be short-circuit due to the level inference -- *)

    | ProductType(tys) ->
        tys |> TupleList.to_list |> aux_list
          (* -- must not be short-circuit due to the level inference -- *)

    | ListType(ty) ->
        aux ty

    | EffType(eff, ty0) ->
        let beff = aux_effect eff in
        let b0 = aux ty0 in
        beff || b0
          (* -- must not be short-circuit due to the level inference -- *)

    | PidType(pidty) ->
        aux_pid_type pidty

    | TypeVar({contents = Link(ty)}) ->
        aux ty

    | TypeVar({contents = Free(fidx)}) ->
        if FreeID.equal fid fidx then true else
          begin
            FreeID.update_level fidx lev;
(*
            Format.printf "LEVEL %a L%d --> L%d\n" FreeID.pp fidx (FreeID.get_level fidx) lev;  (* for debug *)
*)
            false
          end

  and aux_effect (Effect(ty)) =
    aux ty

  and aux_pid_type (Pid(ty)) =
    aux ty

  and aux_list tys =
    tys |> List.map aux |> List.fold_left ( || ) false
  in
  aux ty


let unify tyact tyexp =
(*
  Format.printf "UNIFY %a =?= %a\n" pp_mono_type tyact pp_mono_type tyexp; (* for debug *)
*)
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

    | (FuncType(ty1doms, ty1cod), FuncType(ty2doms, ty2cod)) ->
        let res1 = aux_list ty1doms ty2doms in
        let res2 = aux ty1cod ty2cod in
        res1 &&& res2

    | (EffType(eff1, tysub1), EffType(eff2, tysub2)) ->
        let reseff = aux_effect eff1 eff2 in
        let ressub = aux tysub1 tysub2 in
        reseff &&& ressub

    | (PidType(pidty1), PidType(pidty2)) ->
        aux_pid_type pidty1 pidty2

    | (ProductType(tys1), ProductType(tys2)) ->
        aux_list (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

    | (ListType(ty1), ListType(ty2)) ->
        aux ty1 ty2

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

  and aux_list tys1 tys2 =
    try
      List.fold_left2 (fun res ty1 ty2 ->
        match res with
        | Consistent -> aux ty1 ty2
        | _          -> res
      ) Consistent tys1 tys2
    with
    | Invalid_argument(_) -> Contradiction

  and aux_effect (Effect(ty1)) (Effect(ty2)) =
    aux ty1 ty2

  and aux_pid_type (Pid(ty1)) (Pid(ty2)) =
    aux ty1 ty2
  in
  let res = aux tyact tyexp in
  match res with
  | Consistent     -> ()
  | Contradiction  -> raise (ContradictionError(tyact, tyexp))
  | Inclusion(fid) -> raise (InclusionError(fid, tyact, tyexp))


type pre = {
  level : int;
  tyenv : Typeenv.t;
}


let fresh_type lev rng =
  let fid = FreeID.fresh lev in
  let tvref = ref (Free(fid)) in
  let ty = (rng, TypeVar(tvref)) in
(*
  Format.printf "GEN L%d %a\n" lev pp_mono_type ty;  (* for debug *)
*)
  ty


let generate_output_identifier scope rng x =
  match scope with
  | Local  -> OutputIdentifier.local x
  | Global -> OutputIdentifier.global x


let type_of_base_constant (rng : Range.t) (bc : base_constant) =
  match bc with
  | Unit    -> ((rng, BaseType(UnitType)))
  | Int(n)  -> (rng, BaseType(IntType))
  | Bool(b) -> (rng, BaseType(BoolType))


let rec typecheck (pre : pre) ((rng, utastmain) : untyped_ast) : mono_type * ast =
  match utastmain with
  | BaseConst(bc) ->
      let ty = type_of_base_constant rng bc in
      (ty, IBaseConst(bc))

  | Var(x) ->
      begin
        match pre.tyenv |> Typeenv.find_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some((_, ptymain), name) ->
            let ty = instantiate pre.level (rng, ptymain) in
            (ty, IVar(name))
      end

  | Lambda(binders, utast0) ->
      let (tyenv, nameacc, tydomacc) =
        List.fold_left (fun (tyenv, nameacc, tydomacc) (rngv, x) ->
          let tydom = fresh_type pre.level rngv in
          let ptydom = lift tydom in
          let name = generate_output_identifier Local rngv x in
          (tyenv |> Typeenv.add x ptydom name, Alist.extend nameacc name, Alist.extend tydomacc tydom)
        ) (pre.tyenv, Alist.empty, Alist.empty) binders
      in
      let names = nameacc |> Alist.to_list in
      let tydoms = tydomacc |> Alist.to_list in
      let (tycod, e0) = typecheck { pre with tyenv } utast0 in
      let ty = (rng, FuncType(tydoms, tycod)) in
      (ty, ilambda names e0)

  | Apply(utastfun, utastargs) ->
      let (tyfun, efun) = typecheck pre utastfun in
      let tyeargs = List.map (typecheck pre) utastargs in
      let tyargs = List.map fst tyeargs in
      let eargs = List.map snd tyeargs in
      let tyret = fresh_type pre.level rng in
      unify tyfun (Range.dummy "Apply", FuncType(tyargs, tyret));
      (tyret, iapply efun eargs)

  | If(utast0, utast1, utast2) ->
      let (ty0, e0) = typecheck pre utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      unify ty1 ty2;
      let ibranches = [ IBranch(IPBool(true), None, e1); IBranch(IPBool(false), None, e2) ] in
      (ty1, ICase(e0, ibranches))

  | LetIn(ident, utast1, utast2) ->
      let (tyenv, name, e1) = typecheck_let Local pre ident utast1 in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      (ty2, ILetIn(name, e1, e2))

  | LetRecIn(((_, x) as ident), utast1, utast2) ->
      let (tyenv, name_inner, e1, pty) = typecheck_letrec Local pre ident utast1 in
      let name_outer = OutputIdentifier.fresh () in
      let tyenv = tyenv |> Typeenv.add x pty name_outer in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      (ty2, iletrecin name_inner name_outer e1 e2)

  | Do(identopt, utast1, utast2) ->
      let lev = pre.level in
      let (ty1, e1) = typecheck pre utast1 in
      let (tyx, tyenv, name) =
        match identopt with
        | None ->
            ((Range.dummy "do-unit", BaseType(UnitType)), pre.tyenv, OutputIdentifier.unused)

        | Some(rngv, x) ->
            let tyx = fresh_type lev rngv in
            let name = generate_output_identifier Local rngv x in
            (tyx, pre.tyenv |> Typeenv.add x (lift tyx) name, name)
      in
      let tyrecv = fresh_type lev (Range.dummy "do-recv") in
      unify ty1 (Range.dummy "do-eff2", EffType(Effect(tyrecv), tyx));
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      let tysome = fresh_type lev (Range.dummy "do-some") in
      unify ty2 (Range.dummy "do-eff2", EffType(Effect(tyrecv), tysome));
      let e2 =
        ithunk (ILetIn(name, iforce e1, iforce e2))
      in
      (ty2, e2)

  | Receive(branches) ->
      let lev = pre.level in
      let tyrecv = fresh_type lev (Range.dummy "receive-recv") in
      let tyret = fresh_type lev (Range.dummy "receive-ret") in
      let ibracc =
        branches |> List.fold_left (fun ibracc branch ->
          let ibranch = typecheck_branch pre tyrecv tyret branch in
          Alist.extend ibracc ibranch
        ) Alist.empty
      in
      let ty = (rng, EffType(Effect(tyrecv), tyret)) in
      (ty, IReceive(ibracc |> Alist.to_list))

  | Tuple(utasts) ->
      let tyes = utasts |> TupleList.map (typecheck pre) in
      let tys = tyes |> TupleList.map fst in
      let es = tyes |> TupleList.map snd in
      let ty = (rng, ProductType(tys)) in
      (ty, ITuple(es))

  | ListNil ->
      let tysub = fresh_type pre.level (Range.dummy "list-nil") in
      let ty = (rng, ListType(tysub)) in
      (ty, IListNil)

  | ListCons(utast1, utast2) ->
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      unify ty2 (Range.dummy "list-cons", ListType(ty1));
      (ty2, IListCons(e1, e2))

  | Case(utast0, branches) ->
      let (ty0, e0) = typecheck pre utast0 in
      let lev = pre.level in
      let tyret = fresh_type lev (Range.dummy "case-ret") in
      let ibracc =
        branches |> List.fold_left (fun ibracc branch ->
          let ibranch = typecheck_branch pre ty0 tyret branch in
          Alist.extend ibracc ibranch
        ) Alist.empty
      in
      (tyret, ICase(e0, ibracc |> Alist.to_list))


and typecheck_branch (pre : pre) typatexp tyret (Branch(pat, utast0opt, utast1)) : branch =
  let (typat, ipat, bindmap) = typecheck_pattern pre pat in
  let tyenv =
    BindingMap.fold (fun x (ty, name) tyenv ->
      tyenv |> Typeenv.add x (lift ty) name
    ) bindmap pre.tyenv
  in
  let pre = { pre with tyenv } in
  unify typat typatexp;
  let e0opt =
    utast0opt |> Option.map (fun utast0 ->
      let (ty0, e0) = typecheck pre utast0 in
      unify ty0 (Range.dummy "when", BaseType(BoolType));
      e0
    )
  in
  let (ty1, e1) = typecheck pre utast1 in
  unify ty1 (Range.dummy "branch", EffType(Effect(typatexp), tyret));
  IBranch(ipat, e0opt, e1)


and typecheck_pattern (pre : pre) ((rng, patmain) : untyped_pattern) : mono_type * pattern * binding_map =
  let immediate tymain ipat = ((rng, tymain), ipat, BindingMap.empty) in
  match patmain with
  | PUnit    -> immediate (BaseType(UnitType)) IPUnit
  | PBool(b) -> immediate (BaseType(BoolType)) (IPBool(b))
  | PInt(n)  -> immediate (BaseType(IntType)) (IPInt(n))

  | PVar(x) ->
      let ty = fresh_type pre.level rng in
      let name = generate_output_identifier Local rng x in
      (ty, IPVar(name), BindingMap.singleton x (ty, name))

  | PWildCard ->
      let ty = fresh_type pre.level rng in
      (ty, IPWildCard, BindingMap.empty)

  | PListNil ->
      let ty =
        let tysub = fresh_type pre.level rng in
        (rng, ListType(tysub))
      in
      (ty, IPListNil, BindingMap.empty)

  | PListCons(pat1, pat2) ->
      let (ty1, ipat1, bindmap1) = typecheck_pattern pre pat1 in
      let (ty2, ipat2, bindmap2) = typecheck_pattern pre pat2 in
      let bindmap =
        BindingMap.union (fun x _ _ ->
          raise (BoundMoreThanOnceInPattern(rng, x))
        ) bindmap1 bindmap2
      in
      unify ty2 (Range.dummy "pattern-cons", ListType(ty1));
      (ty2, IPListCons(ipat1, ipat2), bindmap)


and typecheck_let (scope : scope) (pre : pre) ((rngv, x) : Range.t * identifier) (utast1 : untyped_ast) : Typeenv.t * name * ast =
  let (ty1, e1) = typecheck { pre with level = pre.level + 1 } utast1 in
  let pty1 = generalize pre.level ty1 in
  let name = generate_output_identifier scope rngv x in
  (pre.tyenv |> Typeenv.add x pty1 name, name, e1)


and typecheck_letrec (scope : scope) (pre : pre) ((rngv, x) : Range.t * identifier) (utast1 : untyped_ast) : Typeenv.t * name * ast * poly_type =
  let lev = pre.level in
  let tyf = fresh_type (lev + 1) rngv in
  let name = generate_output_identifier scope rngv x in
  let tyenvsub = pre.tyenv |> Typeenv.add x (lift tyf) name in
  let (ty1, e1) = typecheck { level = lev + 1; tyenv = tyenvsub } utast1 in
  unify ty1 tyf;
  let ptyf = generalize lev tyf in
  (pre.tyenv |> Typeenv.add x ptyf name, name, e1, ptyf)


let main (utdecls : untyped_declaration list) : Typeenv.t * declaration list =
  let tyenv = Primitives.initial_type_environment in
  let (tyenv, declacc) =
    utdecls |> List.fold_left (fun (tyenv, declacc) utdecl ->
      match utdecl with
      | ValDecl(isrec, binder, utast) ->
          let (tyenv, name, e) =
            if isrec then
              let (tyenv, name, e, _) =
                typecheck_letrec Global { level = 0; tyenv = tyenv } binder utast
              in
              (tyenv, name, e)
            else
              typecheck_let Global { level = 0; tyenv = tyenv } binder utast
          in
          (tyenv, Alist.extend declacc (IValDecl(name, e)))
    ) (tyenv, Alist.empty)
  in
  (tyenv, declacc |> Alist.to_list)
