
open Syntax


module rec MonoTypeVarUpdatable : sig
  type t = mono_type_var_updatable
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end = struct

  type t = mono_type_var_updatable

  let pp ppf mtv =
    Format.fprintf ppf "%s" (show_mono_type_var_updatable mtv)

  let equal (mtvu1 : t) (mtvu2 : t) : bool =
    match (mtvu1, mtvu2) with
    | (Link(ty1), Link(ty2))   -> MonoType.equal ty1 ty2
    | (Free(fid1), Free(fid2)) -> FreeID.equal fid1 fid2
    | _                        -> false
end

and MonoType : sig
  type t = mono_type
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end = struct

  type t = mono_type

  let pp = pp_mono_type

  let equal (ty1 : t) (ty2 : t) : bool =

    let rec aux ((_, tymain1) : t) ((_, tymain2) : t) : bool =
      match (tymain1, tymain2) with
      | (BaseType(bt1), BaseType(bt2)) ->
          bt1 = bt2

      | (FuncType(tydoms1, tycod1), FuncType(tydoms2, tycod2)) ->
          aux_list tydoms1 tydoms2 && aux tycod1 tycod2

      | (PidType(pidty1), PidType(pidty2)) ->
          aux_pid pidty1 pidty2

      | (EffType(effty1, ty1), EffType(effty2, ty2)) ->
          aux_effect effty1 effty2 && aux ty1 ty2

      | (TypeVar(MustBeBound(mbbid1)), TypeVar(MustBeBound(mbbid2))) ->
          MustBeBoundID.equal mbbid1 mbbid2

      | (TypeVar(Updatable(r1)), TypeVar(Updatable(r2))) ->
          MonoTypeVarUpdatable.equal !r1 !r2

      | (ProductType(tys1), ProductType(tys2)) ->
          aux_list (TupleList.to_list tys1) (TupleList.to_list tys2)

      | (ListType(ty1), ListType(ty2)) ->
          aux ty1 ty2

      | (DataType(tyid1, tys1), DataType(tyid2, tys2)) ->
          TypeID.equal tyid1 tyid2 && aux_list tys1 tys2

      | _ ->
          false

    and aux_list (tys1 : t list) (tys2 : t list) : bool =
      match List.combine tys1 tys2 with
      | exception Invalid_argument(_) -> false
      | typairs                       -> typairs |> List.for_all (fun (ty1, ty2) -> aux ty1 ty2)


    and aux_pid (Pid(ty1)) (Pid(ty2)) =
      aux ty1 ty2

    and aux_effect (Effect(ty1)) (Effect(ty2)) =
      aux ty1 ty2

    in
    aux ty1 ty2

end


let mono_type_var_updatable_witness : mono_type_var_updatable Alcotest.testable =
  (module MonoTypeVarUpdatable : Alcotest.TESTABLE with type t = mono_type_var_updatable)


let test_unify (ty1, ty2, assoc) () =
  Typechecker.unify Typeenv.empty ty1 ty2;
  assoc |> List.iter (fun (fid, mtvu_nonref) ->
    Alcotest.check mono_type_var_updatable_witness "free variable" !fid mtvu_nonref
  )


let dr = Range.dummy "test"


let () =
  let open Alcotest in
  run "Typecheck" [
    ("unify",
      List.map (fun tuple ->
        test_case "unify" `Quick (test_unify tuple)
      ) [
       begin
         let fid = FreeID.fresh 0 in
         let mtvu = ref (Free(fid)) in
         let ty1 = (dr, TypeVar(Updatable(mtvu))) in
         let ty2 = (dr, BaseType(IntType)) in
         (ty1, ty2, [ (mtvu, Link(ty2)) ])
       end
     ]);
  ]
