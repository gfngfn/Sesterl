
open Syntax


(* Arguments:
   - `levpred`:
     Given a level of free/must-be-bound ID,
     this predicate returns whether it should be bound or not. *)
let lift_scheme (rngf : Range.t -> Range.t) (levpred : int -> bool) (ty : mono_type) : poly_type =

  let fidht = FreeIDHashTable.create 32 in
  let fridht = FreeRowIDHashTable.create 32 in

  let rec intern fid =
    match FreeIDHashTable.find_opt fidht fid with
    | Some(bid) ->
        bid

    | None ->
        let bid = BoundID.fresh () in
        FreeIDHashTable.add fidht fid bid;
        bid

  and intern_row frid =
    match FreeRowIDHashTable.find_opt fridht frid with
    | Some(brid) ->
        brid

    | None ->
        let labmap = KindStore.get_free_row frid in
        let plabmap = labmap |> LabelAssoc.map aux in
        let brid = BoundRowID.fresh () in
        KindStore.register_bound_row brid plabmap;
        FreeRowIDHashTable.add fridht frid brid;
        brid

  and aux (rng, tymain) =
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

    | FuncType(tydoms, optrow, tycod) ->
        let ptydoms = tydoms |> List.map aux in
        let ptycod = aux tycod in
        (rngf rng, FuncType(ptydoms, aux_option_row optrow, ptycod))

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

  and aux_option_row = function
    | FixedRow(labmap) ->
        let plabmap = labmap |> LabelAssoc.map aux in
        FixedRow(plabmap)

    | RowVar(UpdatableRow{contents = LinkRow(labmap)}) ->
        let plabmap = labmap |> LabelAssoc.map aux in
        FixedRow(plabmap)

    | RowVar((UpdatableRow{contents = FreeRow(frid)}) as mrv) ->
        if levpred (FreeRowID.get_level frid) then
          let brid = intern_row frid in
          RowVar(BoundRow(brid))
        else
          RowVar(MonoRow(mrv))

  in
  aux ty


(* `generalize lev ty` transforms a monotype `ty` into a polytype
   by binding type variables the level of which is higher than `lev`. *)
let generalize (lev : int) (ty : mono_type) : poly_type =
  lift_scheme
    (fun _ -> Range.dummy "erased")
    (fun levx -> lev < levx)
    ty


(* `lift` projects monotypes into polytypes without binding any type variables. *)
let lift (ty : mono_type) : poly_type =
  lift_scheme (fun rng -> rng) (fun _ -> false) ty


let instantiate_scheme : 'a 'b. (Range.t -> poly_type_var -> ('a, 'b) typ) -> (poly_row_var -> 'b) -> poly_type -> ('a, 'b) typ =
fun intern intern_row pty ->
  let rec aux (rng, ptymain) =
    match ptymain with
    | BaseType(bty) ->
        (rng, BaseType(bty))

    | TypeVar(ptv) ->
        intern rng ptv

    | FuncType(ptydoms, optrow, ptycod) ->
        let tydoms = ptydoms |> List.map aux in
        let poptrow =
          match optrow with
          | FixedRow(labmap) ->
              let plabmap = labmap |> LabelAssoc.map aux in
              FixedRow(plabmap)

          | RowVar(prv) ->
              RowVar(intern_row prv)
        in
        let tycod = aux ptycod in
        (rng, FuncType(tydoms, poptrow, tycod))

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
  let bridht = BoundRowIDHashTable.create 32 in
    (* -- hash tables are created at every (non-partial) call of `instantiate` -- *)

  let rec intern (rng : Range.t) (ptv : poly_type_var) : mono_type =
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

  and intern_row (prv : poly_row_var) : mono_row_var =
    match prv with
    | MonoRow(mrv) ->
        mrv

    | BoundRow(brid) ->
        begin
          match BoundRowIDHashTable.find_opt bridht brid with
          | Some(mrvu) ->
              UpdatableRow(mrvu)

          | None ->
              let plabmap = KindStore.get_bound_row brid in
              let labmap = plabmap |> LabelAssoc.map aux in
              let frid = FreeRowID.fresh lev in
              KindStore.register_free_row frid labmap;
              let mrvu = ref (FreeRow(frid)) in
              UpdatableRow(mrvu)
        end

  and aux pty =
    instantiate_scheme intern intern_row pty
  in
  aux pty


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
  let intern_row (prv : poly_row_var) =
    failwith "TODO: instantiate_by_map, intern_row"
  in
  instantiate_scheme intern intern_row


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
  let intern_row (prv : poly_row_var) =
    failwith "TODO: substitute_mono_type, intern_row"
  in
  instantiate_scheme intern intern_row


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
  let intern_row (prv : poly_row_var) =
    failwith "TODO: substitute_poly_type, intern_row"
  in
  instantiate_scheme intern intern_row


let overwrite_range_of_type (rng : Range.t) (_, tymain) =
  (rng, tymain)
