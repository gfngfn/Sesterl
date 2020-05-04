
open MyUtil
open Syntax


exception UnboundVariable                     of Range.t * identifier
exception ContradictionError                  of mono_type * mono_type
exception InclusionError                      of FreeID.t * mono_type * mono_type
exception BoundMoreThanOnceInPattern          of Range.t * identifier
exception UnboundTypeParameter                of Range.t * type_variable_name
exception UndefinedConstructor                of Range.t * constructor_name
exception InvalidNumberOfConstructorArguments of Range.t * constructor_name * int * int
exception UndefinedTypeName                   of Range.t * type_name
exception InvalidNumberOfTypeArguments        of Range.t * type_name * int * int
exception TypeParameterBoundMoreThanOnce      of Range.t * type_variable_name
exception InvalidByte                         of Range.t
exception CyclicSynonymTypeDefinition         of (type_name ranged) list
exception UnboundModuleName                   of Range.t * module_name
exception NotOfStructureType                  of Range.t * module_signature
exception NotOfFunctorType                    of Range.t * module_signature
exception NotAFunctorSignature                of Range.t * module_signature
exception NotAStructureSignature              of Range.t * module_signature
exception UnboundSignatureName                of Range.t * signature_name
exception CannotRestrictTransparentType       of Range.t * BoundID.t list * single_type_binding


module BindingMap = Map.Make(String)


type binding_map = (mono_type * name * Range.t) BindingMap.t


let binding_map_union rng =
  BindingMap.union (fun x _ _ ->
    raise (BoundMoreThanOnceInPattern(rng, x))
  )


type unification_result =
  | Consistent
  | Contradiction
  | Inclusion of FreeID.t


type scope =
  | Local
  | Global of int


type pre = {
  level : int;
  tyenv : Typeenv.t;
  local_type_parameters : local_type_parameter_map;
}


let make_bound_to_free_map (lev : int) (typarams : BoundID.t list) : mono_type list * mono_type_var BoundIDMap.t =
  let (tyargacc, bfmap) =
    typarams |> List.fold_left (fun (tyargacc, bfmap) bid ->
      let fid = FreeID.fresh lev in
      let mtvu = ref (Free(fid)) in
      let mtv = Updatable(mtvu) in
      let ty = (Range.dummy "constructor-arg", TypeVar(mtv)) in
(*
      Format.printf "BTOF L%d %a\n" lev pp_mono_type ty;  (* for debug *)
*)
      (Alist.extend tyargacc ty, bfmap |> BoundIDMap.add bid mtv)
    ) (Alist.empty, BoundIDMap.empty)
  in
  (Alist.to_list tyargacc, bfmap)


let add_local_type_parameter (typaramassoc : type_parameter_assoc) (localtyparams : local_type_parameter_map) : local_type_parameter_map =
  typaramassoc |> TypeParameterAssoc.fold_left (fun map tyvar mbbid ->
    map |> TypeParameterMap.add tyvar mbbid
  ) localtyparams


let make_type_parameter_assoc (lev : int) (tyvarnms : (type_variable_name ranged) list) : type_parameter_assoc =
  tyvarnms |> List.fold_left (fun assoc (rng, tyvarnm) ->
    let mbbid = MustBeBoundID.fresh lev in
(*
    Format.printf "MUST-BE-BOUND %s : L%d %a\n" tyvarnm lev MustBeBoundID.pp mbbid;  (* for debug *)
*)
    match assoc |> TypeParameterAssoc.add_last tyvarnm mbbid with
    | None        -> raise (TypeParameterBoundMoreThanOnce(rng, tyvarnm))
    | Some(assoc) -> assoc
  ) TypeParameterAssoc.empty


module TypeIDHashSet = Hashtbl.Make(TypeID)

module TypeIDSet = Set.Make(TypeID)


let decode_manual_type_scheme (k : TypeID.t -> unit) (tyenv : Typeenv.t) (typarams : local_type_parameter_map) (mty : manual_type) : mono_type =
  let invalid rng tynm ~expect:len_expected ~actual:len_actual =
    raise (InvalidNumberOfTypeArguments(rng, tynm, len_expected, len_actual))
  in
  let rec aux (rng, mtymain) =
    let tymain =
      match mtymain with
      | MTypeName(tynm, mtyargs) ->
          let ptyargs = mtyargs |> List.map aux in
          let len_actual = List.length ptyargs in
          begin
            match tyenv |> Typeenv.find_type tynm with
            | None ->
                begin
                  match (tynm, ptyargs) with
                  | ("unit", [])    -> BaseType(UnitType)
                  | ("unit", _)     -> invalid rng "unit" ~expect:0 ~actual:len_actual
                  | ("bool", [])    -> BaseType(BoolType)
                  | ("bool", _)     -> invalid rng "bool" ~expect:0 ~actual:len_actual
                  | ("int", [])     -> BaseType(IntType)
                  | ("int", _)      -> invalid rng "int" ~expect:0 ~actual:len_actual
                  | ("binary", [])  -> BaseType(BinaryType)
                  | ("binary", _)   -> invalid rng "binary" ~expect:0 ~actual:len_actual
                  | ("list", [ty])  -> ListType(ty)
                  | ("list", _)     -> invalid rng "list" ~expect:1 ~actual:len_actual
                  | ("pid", [ty])   -> PidType(Pid(ty))
                  | ("pid", _)      -> invalid rng "pid" ~expect:1 ~actual:len_actual
                  | _               -> raise (UndefinedTypeName(rng, tynm))
                end

            | Some(tyid, len_expected) ->
                if len_actual = len_expected then
                  begin
                    k tyid;
                    DataType(tyid, ptyargs)
                  end
                else
                  invalid rng tynm ~expect:len_expected ~actual:len_actual
          end

      | MFuncType(mtydoms, mtycod) ->
          FuncType(List.map aux mtydoms, aux mtycod)

      | MProductType(mtys) ->
          ProductType(TupleList.map aux mtys)

      | MEffType(mty1, mty2) ->
          EffType(Effect(aux mty1), aux mty2)

      | MTypeVar(typaram) ->
          begin
            match typarams |> TypeParameterMap.find_opt typaram with
            | None ->
                raise (UnboundTypeParameter(rng, typaram))

            | Some(mbbid) ->
                TypeVar(MustBeBound(mbbid))
          end
    in
    (rng, tymain)
  in
  aux mty


let decode_manual_type : Typeenv.t -> local_type_parameter_map -> manual_type -> mono_type =
  decode_manual_type_scheme (fun _ -> ())


let decode_manual_type_and_get_dependency (pre : pre) (mty : manual_type) : mono_type * TypeIDSet.t =
  let hashset = TypeIDHashSet.create 32 in
    (* -- a hash set is created on every (non-partial) call -- *)
  let k tyid =
    TypeIDHashSet.add hashset tyid ()
  in
  let ty = decode_manual_type_scheme k pre.tyenv pre.local_type_parameters mty in
  let dependencies =
    TypeIDHashSet.fold (fun tyid () set ->
      set |> TypeIDSet.add tyid
    ) hashset TypeIDSet.empty
  in
  (ty, dependencies)


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


let iletpatin (ipat : pattern) (e1 : ast) (e2 : ast) : ast =
  ICase(e1, [ IBranch(ipat, None, e2) ])


let iletrecin_single (_, _, name_outer, name_inner, e1) (e2 : ast) : ast =
  match e1 with
  | ILambda(None, names, e0) ->
      ILetIn(name_outer, ILambda(Some(name_inner), names, e0), e2)

  | _ ->
      assert false


let iletrecin_multiple (binds : (identifier * poly_type * name * name * ast) TupleList.t) (e2 : ast) : ast =
  let ipat_inner_tuple =
    IPTuple(binds |> TupleList.map (fun (_, _, _, name_inner, _) -> IPVar(name_inner)))
  in
  let name_for_whole_rec = OutputIdentifier.fresh () in
  let tuple_entries =
    binds |> TupleList.map (fun (_, _, name_outer, name_inner, e1) ->
      match e1 with
      | ILambda(None, names, e0) ->
          ILambda(None, names, iletpatin ipat_inner_tuple (IApply(name_for_whole_rec, [])) e0)

      | _ ->
          assert false
    )
  in
  let ipat_outer_tuple =
    IPTuple(binds |> TupleList.map (fun (_, _, name_outer, _, _) -> IPVar(name_outer)))
  in
  iletpatin
    ipat_outer_tuple
    (iapply (ILambda(Some(name_for_whole_rec), [], ITuple(tuple_entries))) [])
    e2


let iletrecin (binds : (identifier * poly_type * name * name * ast) list) (e2 : ast) : ast =
  match binds with
  | []                     -> assert false
  | [bind]                 -> iletrecin_single bind e2
  | bind1 :: bind2 :: rest -> iletrecin_multiple (TupleList.make bind1 bind2 rest) e2


let occurs (fid : FreeID.t) (ty : mono_type) : bool =
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

    | DataType(tyid, tyargs) ->
        aux_list tyargs

    | EffType(eff, ty0) ->
        let beff = aux_effect eff in
        let b0 = aux ty0 in
        beff || b0
          (* -- must not be short-circuit due to the level inference -- *)

    | PidType(pidty) ->
        aux_pid_type pidty

    | TypeVar(Updatable{contents = Link(ty)}) ->
        aux ty

    | TypeVar(Updatable{contents = Free(fidx)}) ->
        if FreeID.equal fid fidx then true else
          begin
            FreeID.update_level fidx lev;
(*
            Format.printf "LEVEL %a L%d --> L%d\n" FreeID.pp fidx (FreeID.get_level fidx) lev;  (* for debug *)
*)
            false
          end

    | TypeVar(MustBeBound(_)) ->
        false

  and aux_effect (Effect(ty)) =
    aux ty

  and aux_pid_type (Pid(ty)) =
    aux ty

  and aux_list tys =
    tys |> List.map aux |> List.fold_left ( || ) false
  in
  aux ty


let get_real_type (tyenv : Typeenv.t) (sid : TypeID.Synonym.t) (tyargs : mono_type list) : mono_type =
  match tyenv |> Typeenv.find_synonym_type sid with
  | None ->
      assert false

  | Some(typarams, ptyreal) ->
      begin
        try
          let substmap =
            List.fold_left2 (fun substmap typaram tyarg ->
              substmap |> BoundIDMap.add typaram tyarg
            ) BoundIDMap.empty typarams tyargs
          in
          substitute substmap ptyreal
        with
        | Invalid_argument(_) -> assert false
      end


let unify (tyenv : Typeenv.t) (tyact : mono_type) (tyexp : mono_type) : unit =
(*
  Format.printf "UNIFY %a =?= %a\n" pp_mono_type tyact pp_mono_type tyexp; (* for debug *)
*)
  let rec aux (ty1 : mono_type) (ty2 : mono_type) : unification_result =
    let (_, ty1main) = ty1 in
    let (_, ty2main) = ty2 in
    match (ty1main, ty2main) with
    | (TypeVar(Updatable{contents = Link(ty1l)}), _) ->
        aux ty1l ty2

    | (_, TypeVar(Updatable{contents = Link(ty2l)})) ->
        aux ty1 ty2l

    | (TypeVar(MustBeBound(mbbid1)), TypeVar(MustBeBound(mbbid2))) ->
        if MustBeBoundID.equal mbbid1 mbbid2 then Consistent else Contradiction

    | (DataType(TypeID.Synonym(sid1), tyargs1), _) ->
        let ty1real = get_real_type tyenv sid1 tyargs1 in
        aux ty1real ty2

    | (_, DataType(TypeID.Synonym(sid2), tyargs2)) ->
        let ty2real = get_real_type tyenv sid2 tyargs2 in
        aux ty1 ty2real

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

    | (DataType(TypeID.Variant(vid1), tyargs1), DataType(TypeID.Variant(vid2), tyargs2)) ->
        if TypeID.Variant.equal vid1 vid2 then
          aux_list tyargs1 tyargs2
        else
          Contradiction

    | (TypeVar(Updatable({contents = Free(fid1)} as mtvu1)), TypeVar(Updatable{contents = Free(fid2)})) ->
        let () =
          if FreeID.equal fid1 fid2 then () else
            begin
              mtvu1 := Link(ty2);  (* -- not `Free(fid2)`! -- *)
            end
        in
        Consistent

    | (TypeVar(Updatable({contents = Free(fid1)} as mtvu1)), _) ->
        let b = occurs fid1 ty2 in
        if b then
          Inclusion(fid1)
        else
          begin
            mtvu1 := Link(ty2);
            Consistent
          end

    | (_, TypeVar(Updatable({contents = Free(fid2)} as mtvu2))) ->
        let b = occurs fid2 ty1 in
        if b then
          Inclusion(fid2)
        else
          begin
            mtvu2 := Link(ty1);
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


let fresh_type ?name:nameopt (lev : int) (rng : Range.t) : mono_type =
  let fid = FreeID.fresh lev in
  let mtvu = ref (Free(fid)) in
  let ty = (rng, TypeVar(Updatable(mtvu))) in
(*
  let name = nameopt |> Option.map (fun x -> x ^ " : ") |> Option.value ~default:"" in
  Format.printf "GEN %sL%d %a\n" name lev pp_mono_type ty;  (* for debug *)
*)
  ty


let check_properly_used (tyenv : Typeenv.t) ((rng, x) : identifier ranged) =
  match tyenv |> Typeenv.is_val_properly_used x with
  | None        -> assert false
  | Some(true)  -> ()
  | Some(false) -> Logging.warn_val_not_used rng x



let generate_output_identifier scope rng x =
  match scope with
  | Local         -> OutputIdentifier.local x
  | Global(arity) -> OutputIdentifier.global x arity


let type_of_base_constant (rng : Range.t) (bc : base_constant) =
  match bc with
  | Unit    -> ((rng, BaseType(UnitType)))
  | Int(n)  -> (rng, BaseType(IntType))
  | Bool(b) -> (rng, BaseType(BoolType))
  | BinaryByString(_)
  | BinaryByInts(_)   -> (rng, BaseType(BinaryType))


let decode_type_annotation_or_fresh (pre : pre) (((rng, x), tyannot) : binder) : mono_type =
  match tyannot with
  | None ->
      fresh_type ~name:x pre.level rng

  | Some(mty) ->
      decode_manual_type pre.tyenv pre.local_type_parameters mty


let add_parameters_to_type_environment (pre : pre) (binders : binder list) : Typeenv.t * mono_type list * name list =
  let (tyenv, nameacc, tydomacc) =
    List.fold_left (fun (tyenv, nameacc, ptydomacc) (((rngv, x), _) as binder) ->
      let tydom = decode_type_annotation_or_fresh pre binder in
      let ptydom = lift tydom in
      let name = generate_output_identifier Local rngv x in
      (tyenv |> Typeenv.add_val x ptydom name, Alist.extend nameacc name, Alist.extend ptydomacc tydom)
    ) (pre.tyenv, Alist.empty, Alist.empty) binders
  in
  let names = nameacc |> Alist.to_list in
  let tydoms = tydomacc |> Alist.to_list in
  (tyenv, tydoms, names)


let rec typecheck (pre : pre) ((rng, utastmain) : untyped_ast) : mono_type * ast =
  match utastmain with
  | BaseConst(bc) ->
      let ty = type_of_base_constant rng bc in
      (ty, IBaseConst(bc))

  | Var(x) ->
      begin
        match pre.tyenv |> Typeenv.find_val_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some((_, ptymain), name) ->
            let ty = instantiate pre.level (rng, ptymain) in
(*
            Format.printf "INST %s : L%d %a\n" x pre.level pp_mono_type ty;  (* for debug *)
*)
            (ty, IVar(name))
      end

  | Lambda(binders, utast0) ->
      let (tyenv, tydoms, names) =
        add_parameters_to_type_environment pre binders
      in
      let (tycod, e0) = typecheck { pre with tyenv } utast0 in
      let ty = (rng, FuncType(tydoms, tycod)) in
      (ty, ilambda names e0)

  | Apply(utastfun, utastargs) ->
      let (tyfun, efun) = typecheck pre utastfun in
      let tyeargs = List.map (typecheck pre) utastargs in
      let tyargs = List.map fst tyeargs in
      let eargs = List.map snd tyeargs in
      let tyret = fresh_type pre.level rng in
      unify pre.tyenv tyfun (Range.dummy "Apply", FuncType(tyargs, tyret));
      (tyret, iapply efun eargs)

  | If(utast0, utast1, utast2) ->
      let (ty0, e0) = typecheck pre utast0 in
      unify pre.tyenv ty0 (Range.dummy "If", BaseType(BoolType));
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      unify pre.tyenv ty1 ty2;
      let ibranches = [ IBranch(IPBool(true), None, e1); IBranch(IPBool(false), None, e2) ] in
      (ty1, ICase(e0, ibranches))

  | LetIn(NonRec(letbind), utast2) ->
      let (pty, name, e1) = typecheck_let Local pre letbind in
      let tyenv =
        let (_, x) = letbind.vb_identifier in
        pre.tyenv |> Typeenv.add_val x pty name in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      check_properly_used tyenv letbind.vb_identifier;
      (ty2, ILetIn(name, e1, e2))

  | LetIn(Rec(letbinds), utast2) ->
      let name_inner_f letbind =
        let (rngv, x) = letbind.vb_identifier in
        generate_output_identifier Local rngv x
      in
      let name_outer_f _ =
        OutputIdentifier.fresh ()
      in
      let binds = typecheck_letrec_mutual name_inner_f name_outer_f pre letbinds in
      let (ty2, e2) =
        let tyenv =
          binds |> List.fold_left (fun tyenv (x, pty, name_outer, _, _) ->
            tyenv |> Typeenv.add_val x pty name_outer
          ) pre.tyenv
        in
        typecheck { pre with tyenv } utast2
      in
      (ty2, iletrecin binds e2)

  | Do(identopt, utast1, utast2) ->
      let lev = pre.level in
      let (ty1, e1) = typecheck pre utast1 in
      let (tyx, tyenv, name) =
        match identopt with
        | None ->
            ((Range.dummy "do-unit", BaseType(UnitType)), pre.tyenv, OutputIdentifier.unused)

        | Some(((rngv, x), _) as binder) ->
            let tyx = decode_type_annotation_or_fresh pre binder in
            let name = generate_output_identifier Local rngv x in
            (tyx, pre.tyenv |> Typeenv.add_val x (lift tyx) name, name)
      in
      let tyrecv = fresh_type lev (Range.dummy "do-recv") in
      unify tyenv ty1 (Range.dummy "do-eff2", EffType(Effect(tyrecv), tyx));
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      let tysome = fresh_type lev (Range.dummy "do-some") in
      unify tyenv ty2 (Range.dummy "do-eff2", EffType(Effect(tyrecv), tysome));
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
          let ibranch = typecheck_receive_branch pre tyrecv tyret branch in
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
      unify pre.tyenv ty2 (Range.dummy "list-cons", ListType(ty1));
      (ty2, IListCons(e1, e2))

  | Case(utast0, branches) ->
      let (ty0, e0) = typecheck pre utast0 in
      let lev = pre.level in
      let tyret = fresh_type lev (Range.dummy "case-ret") in
      let ibracc =
        branches |> List.fold_left (fun ibracc branch ->
          let ibranch = typecheck_case_branch pre ty0 tyret branch in
          Alist.extend ibracc ibranch
        ) Alist.empty
      in
      (tyret, ICase(e0, ibracc |> Alist.to_list))

  | LetPatIn(utpat, utast1, utast2) ->
      let (ty1, e1) = typecheck { pre with level = pre.level + 1 } utast1 in
      let (typat, ipat, bindmap) = typecheck_pattern pre utpat in
      unify pre.tyenv ty1 typat;
      let tyenv =
        BindingMap.fold (fun x (ty, name, _) tyenv ->
          let pty = generalize pre.level ty in
          tyenv |> Typeenv.add_val x pty name
        ) bindmap pre.tyenv
      in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      BindingMap.iter (fun x (_, _, rng) ->
        check_properly_used tyenv (rng, x)
      ) bindmap;
      (ty2, iletpatin ipat e1 e2)

  | Constructor(ctornm, utastargs) ->
      let (vid, ctorid, tyargs, tys_expected) = typecheck_constructor pre rng ctornm in
      begin
        try
          let es =
            List.fold_left2 (fun acc ty_expected utast ->
              let (ty, e) = typecheck pre utast in
              unify pre.tyenv ty ty_expected;
              Alist.extend acc e
            ) Alist.empty tys_expected utastargs |> Alist.to_list
          in
          let ty = (rng, DataType(TypeID.Variant(vid), tyargs)) in
          let e = IConstructor(ctorid, es) in
          (ty, e)
        with
        | Invalid_argument(_) ->
            let len_expected = List.length tys_expected in
            let len_actual = List.length utastargs in
            raise (InvalidNumberOfConstructorArguments(rng, ctornm, len_expected, len_actual))
      end

  | BinaryByList(nrs) ->
      let ns =
        nrs |> List.map (fun (rngn, n) ->
          if 0 <= n && n <= 255 then n else
            raise (InvalidByte(rngn))
        )
      in
      ((rng, BaseType(BinaryType)), IBaseConst(BinaryByInts(ns)))


and typecheck_constructor (pre : pre) (rng : Range.t) (ctornm : constructor_name) =
  match pre.tyenv |> Typeenv.find_constructor ctornm with
  | None ->
      raise (UndefinedConstructor(rng, ctornm))

  | Some(tyid, ctorid, typarams, ptys) ->
      let lev = pre.level in
      let (tyargs, bfmap) = make_bound_to_free_map lev typarams in
      let tys_expected = ptys |> List.map (instantiate_by_map bfmap) in
      (tyid, ctorid, tyargs, tys_expected)


and typecheck_case_branch (pre : pre) =
  typecheck_branch_scheme (fun ty1 _ tyret ->
    unify pre.tyenv ty1 tyret
  ) pre


and typecheck_receive_branch (pre : pre) =
  typecheck_branch_scheme (fun ty1 typatexp tyret ->
    unify pre.tyenv ty1 (Range.dummy "branch", EffType(Effect(typatexp), tyret))
  ) pre


and typecheck_branch_scheme (unifyk : mono_type -> mono_type -> mono_type -> unit) (pre : pre) typatexp tyret (Branch(pat, utast0opt, utast1)) : branch =
  let (typat, ipat, bindmap) = typecheck_pattern pre pat in
  let tyenv =
    BindingMap.fold (fun x (ty, name, _) tyenv ->
      tyenv |> Typeenv.add_val x (lift ty) name
    ) bindmap pre.tyenv
  in
  let pre = { pre with tyenv } in
  unify tyenv typat typatexp;
  let e0opt =
    utast0opt |> Option.map (fun utast0 ->
      let (ty0, e0) = typecheck pre utast0 in
      unify tyenv ty0 (Range.dummy "when", BaseType(BoolType));
      e0
    )
  in
  let (ty1, e1) = typecheck pre utast1 in
  BindingMap.iter (fun x (_, _, rng) ->
    check_properly_used tyenv (rng, x)
  ) bindmap;
  unifyk ty1 typatexp tyret;
  IBranch(ipat, e0opt, e1)


and typecheck_pattern (pre : pre) ((rng, patmain) : untyped_pattern) : mono_type * pattern * binding_map =
  let immediate tymain ipat = ((rng, tymain), ipat, BindingMap.empty) in
  match patmain with
  | PUnit    -> immediate (BaseType(UnitType)) IPUnit
  | PBool(b) -> immediate (BaseType(BoolType)) (IPBool(b))
  | PInt(n)  -> immediate (BaseType(IntType)) (IPInt(n))

  | PVar(x) ->
      let ty = fresh_type ~name:x pre.level rng in
      let name = generate_output_identifier Local rng x in
      (ty, IPVar(name), BindingMap.singleton x (ty, name, rng))

  | PWildCard ->
      let ty = fresh_type ~name:"_" pre.level rng in
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
      let bindmap = binding_map_union rng bindmap1 bindmap2 in
      unify pre.tyenv ty2 (Range.dummy "pattern-cons", ListType(ty1));
      (ty2, IPListCons(ipat1, ipat2), bindmap)

  | PTuple(pats) ->
      let triples = pats |> TupleList.map (typecheck_pattern pre) in
      let tys = triples |> TupleList.map (fun (ty, _, _) -> ty) in
      let ipats = triples |> TupleList.map (fun (_, ipat, _) -> ipat) in
      let bindmaps = triples |> TupleList.map (fun (_, _, bindmap) -> bindmap) in
      let bindmap =
        bindmaps |> TupleList.to_list
          |> List.fold_left (binding_map_union rng) BindingMap.empty
      in
      let ty = (rng, ProductType(tys)) in
      (ty, IPTuple(ipats), bindmap)

  | PConstructor(ctornm, pats) ->
      let (vid, ctorid, tyargs, tys_expected) = typecheck_constructor pre rng ctornm in
      begin
        try
          let (ipatacc, bindmap) =
            List.fold_left2 (fun (ipatacc, bindmapacc) ty_expected pat ->
              let (ty, ipat, bindmap) = typecheck_pattern pre pat in
              unify pre.tyenv ty ty_expected;
              (Alist.extend ipatacc ipat, binding_map_union rng bindmapacc bindmap)
            ) (Alist.empty, BindingMap.empty) tys_expected pats
          in
          let ty = (rng, DataType(TypeID.Variant(vid), tyargs)) in
          (ty, IPConstructor(ctorid, Alist.to_list ipatacc), bindmap)
        with
        | Invalid_argument(_) ->
            let len_expected = List.length tys_expected in
            let len_actual = List.length pats in
            raise (InvalidNumberOfConstructorArguments(rng, ctornm, len_expected, len_actual))
      end


and typecheck_let (scope : scope) (pre : pre) (letbind : untyped_let_binding) : poly_type * name * ast =
  let (rngv, x) = letbind.vb_identifier in
  let params = letbind.vb_parameters in
  let utast0 = letbind.vb_body in

  let (ty0, e0, tys, names) =
    let levS = pre.level + 1 in
    let assoc = make_type_parameter_assoc levS letbind.vb_forall in
    let localtyparams = pre.local_type_parameters |> add_local_type_parameter assoc in
    let pre = { pre with level = levS; local_type_parameters = localtyparams } in
        (* -- add local type parameters at level `levS` -- *)
    let (tyenv, tys, names) =
      add_parameters_to_type_environment pre params
    in
    let (ty0, e0) = typecheck { pre with tyenv } utast0 in

    letbind.vb_return_type |> Option.map (fun mty0 ->
      let ty0_expected = decode_manual_type tyenv localtyparams mty0 in
      unify tyenv ty0 ty0_expected
    ) |> Option.value ~default:();
    (ty0, e0, tys, names)
  in
  let ty1 = (rngv, FuncType(tys, ty0)) in
  let e1 = ILambda(None, names, e0) in

  let pty1 = generalize pre.level ty1 in
  let name = generate_output_identifier scope rngv x in
  (pty1, name, e1)


and typecheck_letrec_mutual (name_inner_f : untyped_let_binding -> name) (name_outer_f : untyped_let_binding -> name) (pre : pre) (letbinds : untyped_let_binding list) : (identifier * poly_type * name * name * ast) list =

  (* -- register type variables and names for output corresponding to bound names
        before traversing definitions -- *)
  let (tupleacc, tyenv) =
    letbinds |> List.fold_left (fun (tupleacc, tyenv) letbind ->
      let (rngv, x) = letbind.vb_identifier in
      let name_inner = name_inner_f letbind in
      let levS = pre.level + 1 in
      let tyf = fresh_type ~name:x levS rngv in
      let tyenv = tyenv |> Typeenv.add_val x (lift tyf) name_inner in
      (Alist.extend tupleacc (letbind, name_inner, tyf), tyenv)
    ) (Alist.empty, pre.tyenv)
  in

  let (bindacc, tyenv) =
    tupleacc |> Alist.to_list |> List.fold_left (fun (bindacc, tyenv) (letbind, name_inner, tyf) ->
      let (pty, e1) = typecheck_letrec_single { pre with tyenv } letbind name_inner tyf in
      let tyenv =
        let (_, x) = letbind.vb_identifier in
        tyenv |> Typeenv.add_val x pty name_inner
      in
      let name_outer = name_outer_f letbind in
      let (_, x) as ident = letbind.vb_identifier in
      (Alist.extend bindacc (x, pty, name_outer, name_inner, e1), tyenv)
    ) (Alist.empty, tyenv)
  in
  bindacc |> Alist.to_list


and typecheck_letrec_single (pre : pre) (letbind : untyped_let_binding) (name_inner : name) (tyf : mono_type) : poly_type * ast =
  let (rngv, x) = letbind.vb_identifier in
  let params = letbind.vb_parameters in
  let utast0 = letbind.vb_body in

  let levS = pre.level + 1 in
  let (ty0, e0, tys, names) =
    let assoc = make_type_parameter_assoc levS letbind.vb_forall in
    let localtyparams = pre.local_type_parameters |> add_local_type_parameter assoc in
    let pre = { pre with level = levS; local_type_parameters = localtyparams } in
    let (tyenv, tys, names) =
      add_parameters_to_type_environment pre params
    in
    let (ty0, e0) = typecheck { pre with tyenv } utast0 in
    letbind.vb_return_type |> Option.map (fun mty0 ->
      let ty0_expected = decode_manual_type tyenv localtyparams mty0 in
      unify tyenv ty0 ty0_expected;
    ) |> Option.value ~default:();
    (ty0, e0, tys, names)
  in
  let ty1 = (rngv, FuncType(tys, ty0)) in
  let e1 = ILambda(None, names, e0) in
  unify pre.tyenv ty1 tyf;
  let ptyf = generalize pre.level ty1 in
  (ptyf, e1)


let make_constructor_branch_map (pre : pre) (ctorbrs : constructor_branch list) =
  ctorbrs |> List.fold_left (fun ctormap ctorbr ->
    match ctorbr with
    | ConstructorBranch(ctornm, mtyargs) ->
        let tyargs = mtyargs |> List.map (decode_manual_type pre.tyenv pre.local_type_parameters) in
        let ptyargs = tyargs |> List.map (generalize pre.level) in
        let ctorid = ConstructorID.make ctornm in
        ctormap |> ConstructorBranchMap.add ctornm (ctorid, ptyargs)
  ) ConstructorBranchMap.empty


let find_module (tyenv : Typeenv.t) ((rng, m) : module_name ranged) =
  match tyenv |> Typeenv.find_module_opt m with
  | None    -> raise (UnboundModuleName(rng, m))
  | Some(v) -> v


let subtype_concrete_with_abstract (tyenv : Typeenv.t) (modsig1 : module_signature) (absmodsig2 : module_signature abstracted) : witness_map =
  failwith "TODO: subtype_concrete_with_abstract"


let rec substitute_concrete (wtmap : witness_map) (modsig : module_signature) : module_signature =
    match modsig with
    | ConcFunctor(oidset, modsigdom, absmodsigcod) ->
        let modsigdom = modsigdom |> substitute_concrete wtmap in
        let absmodsigcod = absmodsigcod |> substitute_abstract wtmap in
        ConcFunctor(oidset, modsigdom, absmodsigcod)
          (* -- Strictly speaking, we should assert that `oidset` and the domain of `wtmap` be disjoint. -- *)

    | ConcStructure(sigr) ->
        let sigr =
          sigr |> SigRecord.map
              ~v:(fun (pty, name) -> (substitute_poly_type wtmap pty, name))
              ~t:(fun tyopac ->
                match tyopac with
                | Transparent(_) ->
                    tyopac

                | Opaque(kd, oid) ->
                    begin
                      match wtmap |> OpaqueIDMap.find_opt oid with
                      | None                    -> tyopac
                      | Some(sid, typarams, pty) -> Transparent(typarams, ISynonym(sid, pty))
                    end
              )
              ~m:(fun (modsig, name) -> (substitute_concrete wtmap modsig, name))
              ~s:(substitute_abstract wtmap)
        in
        ConcStructure(sigr)


and substitute_abstract (wtmap : witness_map) (absmodsig : module_signature abstracted) : module_signature abstracted =
  let (oidset, modsig) = absmodsig in
  (oidset, substitute_concrete wtmap modsig)
    (* -- Strictly speaking, we should assert that `oidset` and the domain of `wtmap` be disjoint. -- *)


and substitute_poly_type (wtmap : witness_map) (pty : poly_type) : poly_type =
  failwith "TODO: substitute_poly_type"


let update_type_environment_by_signature_record (sigr : SigRecord.t) (tyenv : Typeenv.t) : Typeenv.t =
  sigr |> SigRecord.fold
    ~v:(fun x (pty, name) ->
      Typeenv.add_val x pty name
    )
    ~t:(fun tynm tyopacity ->
      match tyopacity with
      | Transparent(typarams, ISynonym(sid, ptyreal)) -> Typeenv.add_synonym_type tynm sid typarams ptyreal
      | Transparent(typarams, IVariant(vid, ctorbrs)) -> Typeenv.add_variant_type tynm vid typarams ctorbrs
      | Opaque(kind, oid)                             -> Typeenv.add_opaque_type tynm oid kind
    )
    ~m:(fun modnm (modsig, name) ->
      Typeenv.add_module modnm modsig name
    )
    ~s:(fun signm absmodsig ->
      Typeenv.add_signature signm absmodsig
    )
    tyenv


let rec typecheck_declaration (tyenv : Typeenv.t) (utdecl : untyped_declaration) : SigRecord.t abstracted =
  let (rng, utdeclmain) = utdecl in
  match utdeclmain with
  | DeclVal(ident, tyvaridents, mty) ->
      let (_, x) = ident in
      let typaramassoc = make_type_parameter_assoc 1 tyvaridents in
      let localtyparams = TypeParameterMap.empty |> add_local_type_parameter typaramassoc in
      let ty = decode_manual_type tyenv localtyparams mty in
      let pty = generalize 0 ty in
      let name = OutputIdentifier.fresh () in
      let sigr = SigRecord.empty |> SigRecord.add_val x pty name in
      (OpaqueIDSet.empty, sigr)

  | DeclTypeTrans(tyident, mty) ->
      failwith "TODO: DeclTypeTrans"
        (* -- maybe should handle mutually recursive types -- *)

  | DeclTypeOpaque(tyident, mkd) ->
      let (_, tynm) = tyident in
      let kd = mkd in
      let oid = OpaqueID.fresh tynm in
      let sigr = SigRecord.empty |> SigRecord.add_opaque_type tynm oid kd in
      (OpaqueIDSet.singleton oid, sigr)

  | DeclModule(modident, utsig) ->
      let (_, m) = modident in
      let absmodsig = typecheck_signature tyenv utsig in
      let (oidset, modsig) = absmodsig in
      let name = OutputIdentifier.fresh () in
      let sigr = SigRecord.empty |> SigRecord.add_module m modsig name in
      (oidset, sigr)

  | DeclSig(sigident, utsig) ->
      let (_, signm) = sigident in
      let absmodsig = typecheck_signature tyenv utsig in
      let sigr = SigRecord.empty |> SigRecord.add_signature signm absmodsig in
      (OpaqueIDSet.empty, sigr)

  | DeclInclude(utsig) ->
      let absmodsig = typecheck_signature tyenv utsig in
      let (oidset, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utsig in
            raise (NotAFunctorSignature(rng, modsig))

        | ConcStructure(sigr) ->
            (oidset, sigr)
      end


and typecheck_declaration_list (tyenv : Typeenv.t) (utdecls : untyped_declaration list) : SigRecord.t abstracted =
  let (oidsetacc, sigracc, _) =
    utdecls |> List.fold_left (fun (oidsetacc, sigracc, tyenv) ((rng, _) as utdecl) ->
      let (oidset, sigr) = typecheck_declaration tyenv utdecl in
      let oidsetacc = OpaqueIDSet.union oidsetacc oidset in
      let sigracc = SigRecord.disjoint_union rng sigracc sigracc in
      let tyenv = tyenv |> update_type_environment_by_signature_record sigr in
      (oidsetacc, sigracc, tyenv)
    ) (OpaqueIDSet.empty, SigRecord.empty, tyenv)
  in
  (oidsetacc, sigracc)


and typecheck_signature (tyenv : Typeenv.t) (utsig : untyped_signature) : module_signature abstracted =
  let (rng, utsigmain) = utsig in
  match utsigmain with
  | SigVar(signm) ->
      begin
        match tyenv |> Typeenv.find_signature_opt signm with
        | None ->
            raise (UnboundSignatureName(rng, signm))

        | Some(absmodsig) ->
            absmodsig
      end

  | SigPath(utmod, sigident) ->
      let (absmodsig1, e) = typecheck_module tyenv utmod in
      let (oidset1, modsig1) = absmodsig1 in
      begin
        match modsig1 with
        | ConcFunctor(_) ->
            let (rng1, _) = utmod in
            raise (NotOfStructureType(rng1, modsig1))

        | ConcStructure(sigr1) ->
            let (rng2, signm2) = sigident in
            begin
              match sigr1 |> SigRecord.find_signature signm2 with
              | None ->
                  raise (UnboundSignatureName(rng2, signm2))

              | Some((_, modsig2) as absmodsig2) ->
                  begin
                    match modsig2 with
                    | ConcFunctor(_)   -> raise (NotAStructureSignature(rng2, modsig2))
                    | ConcStructure(_) -> absmodsig2
                  end
                    (* Combining typing rules (P-Mod) and (S-Path)
                       in the original paper "F-ing modules" [Rossberg, Russo & Dreyer 2014],
                       we can ignore `oldset1` here.
                       (we have noticed this thanks to `@elpinal`.)
                     *)
            end
      end

  | SigDecls(utdecls) ->
      let (oidset, sigr) = typecheck_declaration_list tyenv utdecls in
      (oidset, ConcStructure(sigr))

  | SigFunctor(modident, utsigdom, utsigcod) ->
      let (oidset, sigdom) = typecheck_signature tyenv utsigdom in
      let abssigcod =
        let (_, m) = modident in
        let name = OutputIdentifier.fresh () in
        let tyenv = tyenv |> Typeenv.add_module m sigdom name in
        typecheck_signature tyenv utsigcod
      in
      (OpaqueIDSet.empty, ConcFunctor(oidset, sigdom, abssigcod))

  | SigWith(utsig0, modidents, tyident1, tyvaridents, mty) ->
      let (rng0, _) = utsig0 in
      let absmodsig0 = typecheck_signature tyenv utsig0 in
      let (oidset0, modsig0) = absmodsig0 in
      let (rnglast, modsiglast) =
        modidents |> List.fold_left (fun (rngpre, modsig) (rng, modnm) ->
          match modsig with
          | ConcFunctor(_) ->
              raise (NotAStructureSignature(rngpre, modsig))

          | ConcStructure(sigr) ->
              begin
                match sigr |> SigRecord.find_module modnm with
                | None            -> raise (UnboundModuleName(rng, modnm))
                | Some(modsig, _) -> (rng, modsig)
              end
        ) (rng0, modsig0)
      in
      let (rng1, tynm1) = tyident1 in
      begin
        match modsiglast with
        | ConcFunctor(_) ->
            raise (NotAStructureSignature(rnglast, modsiglast))

        | ConcStructure(sigrlast) ->
            begin
              match sigrlast |> SigRecord.find_type tynm1 with
              | None ->
                  raise (UndefinedTypeName(rng1, tynm1))

              | Some(Transparent(typarams, tytrans)) ->
                  raise (CannotRestrictTransparentType(rng1, typarams, tytrans))

              | Some(Opaque(kd, oid)) ->
                  assert (oidset0 |> OpaqueIDSet.mem oid);
                  let typaramassoc = make_type_parameter_assoc 1 tyvaridents in
                  let pty =
                    let localtyparams = TypeParameterMap.empty |> add_local_type_parameter typaramassoc in
                    let ty = decode_manual_type tyenv localtyparams mty in
                    generalize 0 ty
                  in
                  let typarams = typaramassoc |> TypeParameterAssoc.values |> List.map MustBeBoundID.to_bound in
                  let modsigret =
                    let sid = TypeID.Synonym.fresh tynm1 in
                    let wtmap = OpaqueIDMap.singleton oid (sid, typarams, pty) in
                    substitute_concrete wtmap modsig0
                  in
                  (oidset0 |> OpaqueIDSet.remove oid, modsigret)

            end
      end


and typecheck_binding (tyenv : Typeenv.t) (utbind : untyped_binding) : SigRecord.t abstracted * binding =
  match utbind with
  | BindVal(rec_or_nonrec) ->
      let pre =
        {
          level                 = 0;
          tyenv                 = tyenv;
          local_type_parameters = TypeParameterMap.empty;
        }
      in
      let (sigr, i_rec_or_nonrec) =
        match rec_or_nonrec with
        | Rec([]) ->
            assert false

        | Rec(valbinds) ->
            let namef valbind =
              let params = valbind.vb_parameters in
              let arity = List.length params in
              let (rngv, x) = valbind.vb_identifier in
              generate_output_identifier (Global(arity)) rngv x
            in
            let recbinds = typecheck_letrec_mutual namef namef pre valbinds in
            let (sigr, irecbindacc) =
              recbinds |> List.fold_left (fun (sigr, irecbindacc) (x, pty, name_outer, _, e) ->
                let sigr = sigr |> SigRecord.add_val x pty name_outer in
                let irecbindacc = Alist.extend irecbindacc (x, name_outer, pty, e) in
                (sigr, irecbindacc)
              ) (SigRecord.empty, Alist.empty)
            in
            (sigr, IRec(Alist.to_list irecbindacc))

        | NonRec(valbind) ->
            let (pty, name, e) =
              let params = valbind.vb_parameters in
              let arity = List.length params in
              typecheck_let (Global(arity)) pre valbind
            in
            let (_, x) = valbind.vb_identifier in
            let sigr = SigRecord.empty |> SigRecord.add_val x pty name in
            (sigr, INonRec(x, name, pty, e))
      in
      ((OpaqueIDSet.empty, sigr), IBindVal(i_rec_or_nonrec))

  | BindType([]) ->
      assert false

  | BindType((_ :: _) as tybinds) ->
      let (synacc, vntacc, graph, tyenv) =
        tybinds |> List.fold_left (fun (synacc, vntacc, graph, tyenv) (tyident, typarams, syn_or_vnt) ->
          let (_, tynm) = tyident in
          let arity = List.length typarams in
          match syn_or_vnt with
          | BindSynonym(synbind) ->
              let sid = TypeID.Synonym.fresh tynm in
              let graph = graph |> DependencyGraph.add_vertex sid tyident in
              let tyenv = tyenv |> Typeenv.add_type_for_recursion tynm (TypeID.Synonym(sid)) arity in
              let synacc = Alist.extend synacc (tyident, typarams, synbind, sid) in
              (synacc, vntacc, graph, tyenv)

          | BindVariant(vntbind) ->
              let vid = TypeID.Variant.fresh tynm in
              let tyenv = tyenv |> Typeenv.add_type_for_recursion tynm (TypeID.Variant(vid)) arity in
              let vntacc = Alist.extend vntacc (tyident, typarams, vntbind, vid) in
              (synacc, vntacc, graph, tyenv)
        ) (Alist.empty, Alist.empty, DependencyGraph.empty, tyenv)
      in
      let pre =
        {
          level                 = 0;
          tyenv                 = tyenv;
          local_type_parameters = TypeParameterMap.empty;
        }
      in
      let syns = synacc |> Alist.to_list in
      let (graph, sigr, tybindacc) =
        syns |> List.fold_left (fun (graph, sigr, tybindacc) syn ->
          let ((_, tynm), typarams, mtyreal, sid) = syn in
          let typaramassoc = make_type_parameter_assoc 1 typarams in
          let typarams = typaramassoc |> TypeParameterAssoc.values |> List.map MustBeBoundID.to_bound in
          let pre =
            let localtyparams = pre.local_type_parameters |> add_local_type_parameter typaramassoc in
            { pre with local_type_parameters = localtyparams }
          in
          let (tyreal, dependencies) = decode_manual_type_and_get_dependency pre mtyreal in
          let ptyreal = generalize pre.level tyreal in
          let graph =
            graph |> TypeIDSet.fold (fun tyiddep graph ->
              match tyiddep with
              | TypeID.Synonym(siddep) -> graph |> DependencyGraph.add_edge sid siddep
              | TypeID.Variant(_)      -> graph
              | TypeID.Opaque(_)       -> graph
            ) dependencies
          in
          let tybindacc = Alist.extend tybindacc (tynm, typarams, ISynonym(sid, ptyreal)) in
          let sigr = sigr |> SigRecord.add_synonym_type tynm sid typarams ptyreal in
          (graph, sigr, tybindacc)
        ) (graph, SigRecord.empty, Alist.empty)
      in
      let (sigr, tybindacc) =
        vntacc |> Alist.to_list |> List.fold_left (fun (sigr, tybindacc) vnt ->
          let ((_, tynm), typarams, ctorbrs, vid) = vnt in
          let typaramassoc = make_type_parameter_assoc 1 typarams in
          let typarams = typaramassoc |> TypeParameterAssoc.values |> List.map MustBeBoundID.to_bound in
          let pre =
            let localtyparams = pre.local_type_parameters |> add_local_type_parameter typaramassoc in
            { pre with local_type_parameters = localtyparams }
          in
          let ctorbrmap =
            make_constructor_branch_map { pre with tyenv } ctorbrs
          in
          let tybindacc = Alist.extend tybindacc (tynm, typarams, IVariant(vid, ctorbrmap)) in
          let sigr = sigr |> SigRecord.add_variant_type tynm vid typarams ctorbrmap in
          (sigr, tybindacc)
        ) (sigr, tybindacc)
      in
      if DependencyGraph.has_cycle graph then
        let tyidents = syns |> List.map (fun (tyident, _, _, _) -> tyident) in
        raise (CyclicSynonymTypeDefinition(tyidents))
      else
        ((OpaqueIDSet.empty, sigr), IBindType)

  | BindModule(modident, utmod) ->
      let (_, m) = modident in
      let (absmodsig, e) = typecheck_module tyenv utmod in
      let (oidset, modsig) = absmodsig in
      let name = OutputIdentifier.fresh () in
        (* temporary; it may be appropriate to generate name from `m` *)
      let sigr = SigRecord.empty |> SigRecord.add_module m modsig name in
      ((oidset, sigr), IBindModule(name, e))

  | BindInclude(utmod) ->
      let (absmodsig, e) = typecheck_module tyenv utmod in
      let (oidset, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utmod in
            raise (NotOfStructureType(rng, modsig))

        | ConcStructure(sigr) ->
            ((oidset, sigr), IBindInclude(e))
      end

  | BindSig(sigident, sigbind) ->
      let (_, signm) = sigident in
      let absmodsig = typecheck_signature tyenv sigbind in
      let sigr = SigRecord.empty |> SigRecord.add_signature signm absmodsig in
      ((OpaqueIDSet.empty, sigr), IBindSig)


and typecheck_module (tyenv : Typeenv.t) (utmod : untyped_module) : module_signature abstracted * ast =
  let (rng, utmodmain) = utmod in
  match utmodmain with
  | ModVar(m) ->
      let (modsig, name) = find_module tyenv (rng, m) in
      let absmodsig = (OpaqueIDSet.empty, modsig) in
      (absmodsig, IVar(name))

  | ModBinds(utbinds) ->
      let (abssigr, ibinds) = typecheck_binding_list tyenv utbinds in
      let (oidset, sigr) = abssigr in
      let absmodsig = (oidset, ConcStructure(sigr)) in
      (absmodsig, IStructure(ibinds))

  | ModProjMod(utmod, modident) ->
      let (absmodsig, e) = typecheck_module tyenv utmod in
      let (oidset, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utmod in
            raise (NotOfStructureType(rng, modsig))

        | ConcStructure(sigr) ->
            let (rng, m) = modident in
            begin
              match sigr |> SigRecord.find_module m with
              | None ->
                  raise (UnboundModuleName(rng, m))

              | Some(modsigp, name) ->
                  let absmodsigp = (oidset, modsigp) in
                  let ep = IAccess(e, name) in
                  (absmodsigp, ep)
            end
      end

  | ModFunctor(modident, utsigdom, utmod0) ->
      let absmodsigdom = typecheck_signature tyenv utsigdom in
      let (oidset, modsigdom) = absmodsigdom in
      let name = OutputIdentifier.fresh () in
        (* temporary; it may be appropriate to generate name from `m` *)
      let (absmodsigcod, e0) =
        let (_, m) = modident in
        let tyenv = tyenv |> Typeenv.add_module m modsigdom name in
        typecheck_module tyenv utmod0
      in
      let absmodsig = (OpaqueIDSet.empty, ConcFunctor(oidset, modsigdom, absmodsigcod)) in
      let e = ILambda(None, [name], e0) in
      (absmodsig, e)

  | ModApply(modident1, modident2) ->
      let (modsig1, name1) = find_module tyenv modident1 in
      let (modsig2, name2) = find_module tyenv modident2 in
      begin
        match modsig1 with
        | ConcStructure(_) ->
            let (rng1, _) = modident1 in
            raise (NotOfFunctorType(rng1, modsig1))

        | ConcFunctor(oidset, modsigdom1, absmodsigcod1) ->
            let witnessmap = subtype_concrete_with_abstract tyenv modsig2 (oidset, modsigdom1) in
            let absmodsig = substitute_abstract witnessmap absmodsigcod1 in
            (absmodsig, IApply(name1, [ IVar(name2) ]))
      end

  | ModCoerce(modident0, utsig) ->
      let (modsig0, name0) = find_module tyenv modident0 in
      let absmodsig = typecheck_signature tyenv utsig in
      let _ = subtype_concrete_with_abstract tyenv modsig0 absmodsig in
      (absmodsig, IVar(name0))


and typecheck_binding_list (tyenv : Typeenv.t) (utbinds : untyped_binding list) : SigRecord.t abstracted * binding list =
  let (tyenv, oidsetacc, sigracc, ibindacc) =
    utbinds |> List.fold_left (fun (tyenv, oidsetacc, sigracc, ibindacc) utbind ->
      let (abssigr, ibind) = typecheck_binding tyenv utbind in
      let (oidset, sigr) = abssigr in
      let tyenv = tyenv |> update_type_environment_by_signature_record sigr in
      let oidsetacc = OpaqueIDSet.union oidsetacc oidset in
      let sigracc = sigracc |> SigRecord.overwrite sigr in
      let ibindacc = Alist.extend ibindacc ibind in
      (tyenv, oidsetacc, sigracc, ibindacc)
    ) (tyenv, OpaqueIDSet.empty, SigRecord.empty, Alist.empty)
  in
  ((oidsetacc, sigracc), Alist.to_list ibindacc)


let main (utbinds : untyped_binding list) : SigRecord.t abstracted * binding list =
  let tyenv = Primitives.initial_type_environment in
  typecheck_binding_list tyenv utbinds
