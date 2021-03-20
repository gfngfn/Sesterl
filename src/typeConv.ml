
open MyUtil
open Syntax


module BoundBothIDSet = Set.Make(BoundBothID)

module BoundBothIDHashTable = Hashtbl.Make(BoundBothID)

module BoundBothIDDependencyGraph : sig
  type vertex = BoundBothID.t
  type t
  val empty : t
  val add_vertex : t -> vertex -> t
  val add_edge : t -> vertex -> vertex -> t
  val find_cycle : t -> (vertex cycle) option
end = struct

  module GraphImpl = Graph.Persistent.Digraph.Concrete(BoundBothID)

  module ComponentImpl = Graph.Components.Make(GraphImpl)

  type vertex = BoundBothID.t

  type t = GraphImpl.t

  let empty =
    GraphImpl.empty

  let add_vertex graph bbid =
    GraphImpl.add_vertex graph bbid

  let add_edge graph bbid1 bbid2 =
    GraphImpl.add_edge graph bbid1 bbid2

  let find_loop graph =
    GraphImpl.fold_vertex (fun v acc ->
      match acc with
      | Some(_) -> acc
      | None    -> if GraphImpl.mem_edge graph v v then Some(v) else None
    ) graph None

  let find_cycle graph =
    match find_loop graph with
    | Some(v) ->
        Some(Loop(v))

    | None ->
    let sccs = ComponentImpl.scc_list graph in
    sccs |> List.find_map (fun vertices ->
      match vertices with
      | []                -> assert false
      | [ _ ]             -> None
      | v1 :: v2 :: vrest -> Some(Cycle(List2.make v1 v2 vrest))
    )


end

(* Arguments:
   - `levpred`:
     Given a level of free/must-be-bound ID,
     this predicate returns whether it should be bound or not. *)
let lift_scheme (rngf : Range.t -> Range.t) (levpred : int -> bool) (ty : mono_type) : (poly_type, BoundBothID.t cycle * poly_type) result =

  let fidht = FreeIDHashTable.create 32 in
  let fridht = FreeRowIDHashTable.create 32 in

  let bdepsht = BoundBothIDHashTable.create 32 in

  let rec intern (fid : FreeID.t) : BoundID.t =
    match FreeIDHashTable.find_opt fidht fid with
    | Some(bid) ->
        bid

    | None ->
        let bid = BoundID.fresh () in
        FreeIDHashTable.add fidht fid bid;
        let mbkd = KindStore.get_free_id fid in
        let (bbidset, pbkd) = aux_base_kind mbkd in
        BoundBothIDHashTable.add bdepsht (BoundBothID.Type(bid)) bbidset;
        KindStore.register_bound_id bid pbkd;
        bid

  and intern_row (frid : FreeRowID.t) : BoundRowID.t =
    match FreeRowIDHashTable.find_opt fridht frid with
    | Some(brid) ->
        brid

    | None ->
        let brid = BoundRowID.fresh () in
        FreeRowIDHashTable.add fridht frid brid;
        let labmap = KindStore.get_free_row frid in
        let (bbidset, plabmap) = aux_label_assoc labmap in
        BoundBothIDHashTable.add bdepsht (BoundBothID.Row(brid)) bbidset;
        KindStore.register_bound_row brid plabmap;
        brid

  and aux_label_assoc (labmap : mono_type LabelAssoc.t) : BoundBothIDSet.t * poly_type LabelAssoc.t =
    LabelAssoc.fold (fun label ty (bbidsetacc, plabmap) ->
      let (bbidset, pty) = aux ty in
      (BoundBothIDSet.union bbidsetacc bbidset, plabmap |> LabelAssoc.add label pty)
    ) labmap (BoundBothIDSet.empty, LabelAssoc.empty)

  and aux_base_kind (mbkd : mono_base_kind) : BoundBothIDSet.t * poly_base_kind =
    match mbkd with
    | UniversalKind ->
        (BoundBothIDSet.empty, UniversalKind)

    | RecordKind(labmap) ->
        let (bbidset, plabmap) = aux_label_assoc labmap in
        (bbidset, RecordKind(plabmap))

  and aux_domain (domain : mono_domain_type) : BoundBothIDSet.t * poly_domain_type =
    let {ordered = tydoms; mandatory = mndlabmap; optional = optrow} = domain in
    let (bbidset1, ptydoms) = aux_list tydoms in
    let (bbidset2, pmndlabmap) = aux_label_assoc mndlabmap in
    let (bbidset3, poptrow) = aux_option_row optrow in
    let bbidset = List.fold_left BoundBothIDSet.union bbidset1 [bbidset2; bbidset3] in
    (bbidset, {ordered = ptydoms; mandatory = pmndlabmap; optional = poptrow})

  and aux ((rng, tymain) : mono_type) : BoundBothIDSet.t * poly_type =
    match tymain with
    | BaseType(bty) ->
        let pty = (rngf rng, BaseType(bty)) in
        (BoundBothIDSet.empty, pty)

    | TypeVar(Updatable{contents = Link(ty)}) ->
        aux ty

    | TypeVar(Updatable{contents = Free(fid)} as mtv) ->
        let (bbidset, ptv) =
          if levpred (FreeID.get_level fid) then
            let bid = intern fid in
            (BoundBothIDSet.singleton (BoundBothID.Type(bid)), Bound(bid))
          else
            (BoundBothIDSet.empty, Mono(mtv))
        in
        let pty = (rngf rng, TypeVar(ptv)) in
        (bbidset, pty)

    | TypeVar(MustBeBound(mbbid) as mtv) ->
        let (bbidset, ptv) =
          if levpred (MustBeBoundID.get_level mbbid) then
            let bid = MustBeBoundID.to_bound mbbid in
            (BoundBothIDSet.singleton (BoundBothID.Type(bid)), Bound(bid))
          else
            (BoundBothIDSet.empty, Mono(mtv))
        in
        let pty = (rngf rng, TypeVar(ptv)) in
        (bbidset, pty)

    | FuncType(domain, tycod) ->
        let (bbidsetdom, pdomain) = aux_domain domain in
        let (bbidsetcod, ptycod) = aux tycod in
        let bbidset = BoundBothIDSet.union bbidsetdom bbidsetcod in
        let pty = (rngf rng, FuncType(pdomain, ptycod)) in
        (bbidset, pty)

    | EffType(domain, eff, ty0) ->
        let (bbidset0, pdomain) = aux_domain domain in
        let (bbidset1, peff) = aux_effect eff in
        let (bbidset2, pty0) = aux ty0 in
        let pty = (rngf rng, EffType(pdomain, peff, pty0)) in
        (BoundBothIDSet.union (BoundBothIDSet.union bbidset0 bbidset1) bbidset2, pty)

    | PidType(pidty) ->
        let (bbidset, ppidty) = aux_pid_type pidty in
        let pty = (rngf rng, PidType(ppidty)) in
        (bbidset, pty)

    | ProductType(tys) ->
        let (bbidset, ptys) =
          tys |> TupleList.map_and_fold (fun bbidsetacc ty ->
            let (bbidset, pty) = aux ty in
            (BoundBothIDSet.union bbidsetacc bbidset, pty)
          ) BoundBothIDSet.empty
        in
        let pty = (rngf rng, ProductType(ptys)) in
        (bbidset, pty)

    | RecordType(labmap) ->
        let (bbidset, plabmap) = aux_label_assoc labmap in
        let pty = (rngf rng, RecordType(plabmap)) in
        (bbidset, pty)

    | DataType(tyid, tyargs) ->
        let (bbidset, ptyargs) = aux_list tyargs in
        let pty = (rngf rng, DataType(tyid, ptyargs)) in
        (bbidset, pty)

  and aux_list (tys : mono_type list) =
    let (bbidsetacc, ptyacc) =
      tys |> List.fold_left (fun (bbidsetacc, ptyacc) ty ->
        let (bbidset, pty) = aux ty in
        (BoundBothIDSet.union bbidsetacc bbidset, Alist.extend ptyacc pty)
      ) (BoundBothIDSet.empty, Alist.empty)
    in
    (bbidsetacc, Alist.to_list ptyacc)

  and aux_effect (Effect(ty)) =
    let (bbidset, pty) = aux ty in
    (bbidset, Effect(pty))

  and aux_pid_type (Pid(ty)) =
    let (bbidset, pty) = aux ty in
    (bbidset, Pid(pty))

  and aux_option_row : mono_row -> BoundBothIDSet.t * poly_row = function
    | FixedRow(labmap) ->
        let (bbidset, plabmap) = aux_label_assoc labmap in
        (bbidset, FixedRow(plabmap))

    | RowVar(UpdatableRow{contents = LinkRow(labmap)}) ->
        let (bbidset, plabmap) = aux_label_assoc labmap in
        (bbidset, FixedRow(plabmap))

    | RowVar((UpdatableRow{contents = FreeRow(frid)}) as mrv) ->
        if levpred (FreeRowID.get_level frid) then
          let brid = intern_row frid in
          (BoundBothIDSet.singleton (BoundBothID.Row(brid)), RowVar(BoundRow(brid)))
        else
          (BoundBothIDSet.empty, RowVar(MonoRow(mrv)))

    | RowVar(MustBeBoundRow(mbbrid)) ->
        if levpred (MustBeBoundRowID.get_level mbbrid) then
          let brid = MustBeBoundRowID.to_bound mbbrid in
          (BoundBothIDSet.singleton (BoundBothID.Row(brid)), RowVar(BoundRow(brid)))
            (* We do not need to register a kind to `KindStore`,
               since it has been done when `mbbrid` was created. *)
        else
          (BoundBothIDSet.empty, RowVar(MonoRow(MustBeBoundRow(mbbrid))))

  in
  let (bbidset, pty) = aux ty in
  let graph =
    BoundBothIDDependencyGraph.empty |> BoundBothIDSet.fold (fun bbid graph ->
      BoundBothIDDependencyGraph.add_vertex graph bbid
    ) bbidset
  in
  let graph =
    BoundBothIDHashTable.fold (fun bbid_from bbidset_to graph ->
      graph |> BoundBothIDSet.fold (fun bbid_to graph ->
        BoundBothIDDependencyGraph.add_edge graph bbid_from bbid_to
      ) bbidset_to
    ) bdepsht graph
  in
  match BoundBothIDDependencyGraph.find_cycle graph with
  | Some(cycle) -> Error((cycle, pty))
  | None        -> Ok(pty)


(* `generalize lev ty` transforms a monotype `ty` into a polytype
   by binding type variables the level of which is higher than `lev`. *)
let generalize (lev : int) (ty : mono_type) : (poly_type, BoundBothID.t cycle * poly_type) result =
  lift_scheme
    (fun _ -> Range.dummy "erased")
    (fun levx -> lev < levx)
    ty


(* `lift` projects monotypes into polytypes without binding any type variables. *)
let lift (ty : mono_type) : poly_type =
  let res = lift_scheme (fun rng -> rng) (fun _ -> false) ty in
  match res with
  | Ok(pty)  -> pty
  | Error(_) -> assert false


let instantiate_scheme : 'a 'b. (Range.t -> poly_type_var -> ('a, 'b) typ) -> (poly_row_var -> 'b) -> poly_type -> ('a, 'b) typ =
fun intern intern_row pty ->
  let rec aux (rng, ptymain) =
    match ptymain with
    | BaseType(bty) ->
        (rng, BaseType(bty))

    | TypeVar(ptv) ->
        intern rng ptv

    | FuncType(pdomain, ptycod) ->
        let domain = aux_domain pdomain in
        let tycod = aux ptycod in
        (rng, FuncType(domain, tycod))

    | EffType(pdomain, peff, pty0) ->
        let domain = aux_domain pdomain in
        let eff = aux_effect peff in
        let ty0 = aux pty0 in
        (rng, EffType(domain, eff, ty0))

    | PidType(ppidty) ->
        let pidty = aux_pid_type ppidty in
        (rng, PidType(pidty))

    | ProductType(ptys) ->
        let tys = ptys |> TupleList.map aux in
        (rng, ProductType(tys))

    | RecordType(plabmap) ->
        let labmap = plabmap |> LabelAssoc.map aux in
        (rng, RecordType(labmap))

    | DataType(tyid, ptyargs) ->
        (rng, DataType(tyid, ptyargs |> List.map aux))

  and aux_domain pdomain =
    let {ordered = ptydoms; mandatory = pmndlabmap; optional = poptrow} = pdomain in
    let tydoms = ptydoms |> List.map aux in
    let mndlabmap = pmndlabmap |> LabelAssoc.map aux in
    let optrow =
      match poptrow with
      | FixedRow(plabmap) ->
          let labmap = plabmap |> LabelAssoc.map aux in
          FixedRow(labmap)

      | RowVar(prv) ->
          RowVar(intern_row prv)
    in
    {ordered = tydoms; mandatory = mndlabmap; optional = optrow}

  and aux_effect (Effect(pty)) =
    let ty = aux pty in
    Effect(ty)

  and aux_pid_type (Pid(pty)) =
    let ty = aux pty in
    Pid(ty)
  in
  aux pty


let instantiate_by_hash_table bidht bridht (lev : int) (pty : poly_type) : mono_type =

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
              let fid = FreeID.fresh ~message:"instantiate, intern" lev in
              let pbkd = KindStore.get_bound_id bid in
              let mbkd = aux_base_kind pbkd in
              KindStore.register_free_id fid mbkd;
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
              let frid = FreeRowID.fresh ~message:"instantiate, intern_row" lev in
              KindStore.register_free_row frid labmap;
              let mrvu = ref (FreeRow(frid)) in
              UpdatableRow(mrvu)
        end

  and aux_base_kind (pbkd : poly_base_kind) : mono_base_kind =
    match pbkd with
    | UniversalKind ->
        UniversalKind

    | RecordKind(plabmap) ->
        RecordKind(plabmap |> LabelAssoc.map aux)

  and aux pty =
    instantiate_scheme intern intern_row pty
  in
  aux pty


let instantiate (lev : int) (pty : poly_type) =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
    (* Hash tables are created at every (non-partial) call of `instantiate`. *)
  instantiate_by_hash_table bidht bridht lev pty


let instantiate_base_kind_by_hash_table bidht bridht (lev : int) (pbkd : poly_base_kind) : mono_base_kind =
  match pbkd with
  | UniversalKind ->
      UniversalKind

  | RecordKind(plabmap) ->
      RecordKind(plabmap |> LabelAssoc.map (instantiate_by_hash_table bidht bridht lev))


let make_bound_to_free_hash_table bidht bridht (lev : int) (typarams : BoundID.t list) : mono_type list =
  let tyargacc =
    typarams |> List.fold_left (fun tyargacc bid ->
      let mtv =
        match BoundIDHashTable.find_opt bidht bid with
        | Some(mtvu) ->
            Updatable(mtvu)

        | None ->
            let fid = FreeID.fresh ~message:"make_bound_to_free_hash_table" lev in
            let pbkd = KindStore.get_bound_id bid in
            let mbkd = instantiate_base_kind_by_hash_table bidht bridht lev pbkd in
            KindStore.register_free_id fid mbkd;
            let mtvu = ref (Free(fid)) in
            BoundIDHashTable.add bidht bid mtvu;
            Updatable(mtvu)
      in
      let ty = (Range.dummy "constructor-arg", TypeVar(mtv)) in
(*
      Format.printf "BTOF L%d %a\n" lev pp_mono_type ty;  (* for debug *)
*)
      Alist.extend tyargacc ty
    ) Alist.empty
  in
  Alist.to_list tyargacc


let instantiate_type_arguments (lev : int) (typarams : BoundID.t list) (ptys : poly_type list) : mono_type list * mono_type list =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  let tyargs = make_bound_to_free_hash_table bidht bridht lev typarams in
  let tys_expected = ptys |> List.map (instantiate_by_hash_table bidht bridht lev) in
  (tyargs, tys_expected)


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


let can_row_take_optional : mono_row -> bool = function
  | FixedRow(labmap) ->
      LabelAssoc.cardinal labmap > 0

  | RowVar(UpdatableRow{contents = FreeRow(frid)}) ->
      let labmap = KindStore.get_free_row frid in
      LabelAssoc.cardinal labmap > 0

  | RowVar(UpdatableRow{contents = LinkRow(labmap)}) ->
      LabelAssoc.cardinal labmap > 0

  | RowVar(MustBeBoundRow(mbbrid)) ->
      let labmap = KindStore.get_bound_row (MustBeBoundID.to_bound mbbrid) in
      LabelAssoc.cardinal labmap > 0


let rec kind_of_arity n =
  let bkddoms = List.init n (fun _ -> UniversalKind) in
  Kind(bkddoms, UniversalKind)


let lift_base_kind_scheme (rngf : Range.t -> Range.t) (levf : int -> bool) (mbkd : mono_base_kind) : (poly_base_kind, BoundBothID.t cycle * poly_type) result =
  let open ResultMonad in
  match mbkd with
  | UniversalKind ->
      return UniversalKind

  | RecordKind(labmap) ->
      begin
        LabelAssoc.fold (fun label ty acc ->
          acc >>= fun plabmap ->
          lift_scheme rngf levf ty >>= fun pty ->
          return (plabmap |> LabelAssoc.add label pty)
        ) labmap (Ok(LabelAssoc.empty))
      end >>= fun plabmap ->
      return (RecordKind(plabmap))


let generalize_base_kind (lev : int) : mono_base_kind -> (poly_base_kind, BoundBothID.t cycle * poly_type) result =
  lift_base_kind_scheme
    (fun _ -> Range.dummy "erased")
    (fun levx -> lev < levx)


let lift_base_kind =
  lift_base_kind_scheme
    (fun rng -> rng)
    (fun _ -> false)


let lift_kind (kd : mono_kind) : poly_kind =
  let open ResultMonad in
  match kd with
  | Kind(bkddoms, bkdcod) ->
      let res =
        begin
          bkddoms |> List.fold_left (fun acc bkddom ->
            acc >>= fun pbkddomacc ->
            lift_base_kind bkddom >>= fun pbkddom ->
            return (Alist.extend pbkddomacc pbkddom)
          ) (return Alist.empty)
        end >>= fun pbkddomacc ->
        lift_base_kind bkdcod >>= fun pbkdcod ->
        return (Kind(Alist.to_list pbkddomacc, pbkdcod))
      in
      begin
        match res with
        | Ok(pkd)  -> pkd
        | Error(_) -> assert false
      end


let rec arity_of_kind = function
  Kind(bkddoms, _) -> List.length bkddoms


let get_real_type_scheme : 'a 'b. ((('a, 'b) typ) BoundIDMap.t -> poly_type -> ('a, 'b) typ) -> TypeID.Synonym.t -> (('a, 'b) typ) list -> ('a, 'b) typ =
fun substf sid tyargs ->
  let (typarams, ptyreal) = TypeDefinitionStore.find_synonym_type sid in
  try
    let substmap =
      List.fold_left2 (fun substmap typaram tyarg ->
        substmap |> BoundIDMap.add typaram tyarg
      ) BoundIDMap.empty typarams tyargs
    in
    substf substmap ptyreal
  with
  | Invalid_argument(_) -> assert false


let get_real_mono_type : TypeID.Synonym.t -> mono_type list -> mono_type =
  get_real_type_scheme substitute_mono_type

let get_real_poly_type : TypeID.Synonym.t -> poly_type list -> poly_type =
  get_real_type_scheme substitute_poly_type


(* Omit redundant structures of the given type. *)
let rec canonicalize_root = function
  | (_, TypeVar(Updatable({contents = Link(ty)}))) ->
      canonicalize_root ty

  | (_, DataType(TypeID.Synonym(sid), tyargs)) ->
      let ty = get_real_mono_type sid tyargs in
      canonicalize_root ty

  | ty ->
      ty


let show_base_type = function
  | UnitType   -> "unit"
  | BoolType   -> "bool"
  | IntType    -> "int"
  | FloatType  -> "float"
  | BinaryType -> "binary"
  | CharType   -> "char"


let rec show_label_assoc : 'a 'b. prefix:string -> suffix:string -> ('a -> string) -> ('b -> string option) -> (('a, 'b) typ) LabelAssoc.t -> string option =
fun ~prefix:prefix ~suffix:suffix showtv showrv labmap ->
  if LabelAssoc.cardinal labmap = 0 then
    None
  else
    let s =
      LabelAssoc.fold (fun label ty acc ->
        let sty = show_type showtv showrv ty in
        Alist.extend acc (prefix ^ label ^ suffix ^ " " ^ sty)
      ) labmap Alist.empty |> Alist.to_list |> String.concat ", "
    in
    Some(s)


and show_domain : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) domain_type -> string =
fun showtv showrv domain ->
  let sdoms = domain.ordered |> List.map (show_type showtv showrv) in
  let sdomscat = String.concat ", " sdoms in
  let is_ord_empty = (List.length sdoms = 0) in
  let (is_mnds_empty, smnds) =
    match show_label_assoc ~prefix:"-" ~suffix:"" showtv showrv domain.mandatory with
    | None    -> (true, "")
    | Some(s) -> (false, s)
  in
  let (is_opts_empty, sopts) =
    match show_row showtv showrv domain.optional with
    | None    -> (true, "")
    | Some(s) -> (false, s)
  in
  let smid1 =
    if is_ord_empty then
      ""
    else
      if is_mnds_empty && is_opts_empty then "" else ", "
  in
  let smid2 =
    if is_mnds_empty || is_opts_empty then
      ""
    else
      if is_ord_empty then "" else ", "
  in
  Printf.sprintf "%s%s%s%s%s"
    sdomscat smid1 smnds smid2 sopts


and show_type : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) typ -> string =
fun showtv showrv ty ->
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(bty) ->
        show_base_type bty

    | FuncType(domain, tycod) ->
        let sdom = show_domain showtv showrv domain in
        let scod = aux tycod in
        Printf.sprintf "fun(%s) -> %s"
           sdom scod

    | EffType(domain, eff, ty0) ->
        let sdom = show_domain showtv showrv domain in
        let seff = aux_effect eff in
        let s0 = aux ty0 in
        Printf.sprintf "fun(%s) -> %s%s"
          sdom seff s0

    | PidType(pidty) ->
        let spid = aux_pid_type pidty in
        "pid<" ^ spid ^ ">"

    | TypeVar(tv) ->
        showtv tv

    | ProductType(tys) ->
        let ss = tys |> TupleList.to_list |> List.map aux in
        Printf.sprintf "{%s}" (String.concat ", " ss)

    | RecordType(labmap) ->
        begin
          match show_label_assoc ~prefix:"" ~suffix:" :" showtv showrv labmap with
          | None    -> "{}"
          | Some(s) -> Printf.sprintf "{%s}" s
        end

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


and show_row : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) row -> string option =
fun showtv showrv optrow ->
  match optrow with
  | FixedRow(labmap) -> labmap |> show_label_assoc ~prefix:"?" ~suffix:"" showtv showrv
  | RowVar(rv)       -> showrv rv |> Option.map (fun s -> "?" ^ s)


and show_kind : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) kind -> string =
fun showtv showrv kd ->
  let showbkd = show_base_kind showtv showrv in
  match kd with
  | Kind([], bkd) ->
      showbkd bkd

  | Kind((_ :: _) as bkds, bkd) ->
      let sdom = bkds |> List.map showbkd |> String.concat ", " in
      let scod = showbkd bkd in
      Printf.sprintf "(%s) -> %s" sdom scod

and show_base_kind : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) base_kind -> string =
fun showtv showrv ->
  function
  | UniversalKind ->
      "o"

  | RecordKind(labmap) ->
      let s =
        labmap |> show_label_assoc ~prefix:"" ~suffix:" :" showtv showrv |> Option.value ~default:""
      in
      Printf.sprintf "{%s}" s


and show_mono_type_var (mtv : mono_type_var) : string =
  match mtv with
  | MustBeBound(mbbid) -> Format.asprintf "%a" MustBeBoundID.pp mbbid
  | Updatable(mtvu)    -> show_mono_type_var_updatable !mtvu


and show_mono_type_var_updatable (mtvu : mono_type_var_updatable) : string =
  match mtvu with
  | Link(ty)  -> show_type show_mono_type_var show_mono_row_var ty
  | Free(fid) -> Format.asprintf "%a" FreeID.pp fid


and show_mono_row_var (mrv : mono_row_var) : string option =
  match mrv with
  | UpdatableRow(mrvu)     -> show_mono_row_var_updatable !mrvu
  | MustBeBoundRow(mbbrid) -> Some(Format.asprintf "%a" MustBeBoundRowID.pp mbbrid)


and show_mono_row_var_updatable (mrvu : mono_row_var_updatable) : string option =
  match mrvu with
  | LinkRow(labmap) -> show_label_assoc ~prefix:"?" ~suffix:"" show_mono_type_var show_mono_row_var labmap
  | FreeRow(frid)   -> Some(Format.asprintf "%a" FreeRowID.pp frid)


let show_mono_type : mono_type -> string =
  show_type show_mono_type_var show_mono_row_var


let pp_mono_type ppf ty =
  Format.fprintf ppf "%s" (show_mono_type ty)


let show_mono_base_kind : mono_base_kind -> string =
  show_base_kind show_mono_type_var show_mono_row_var


let pp_mono_base_kind ppf mbkd =
  Format.fprintf ppf "%s" (show_mono_base_kind mbkd)


type 'a state =
  | Touching
  | Touched of 'a

type hash_tables = ((string option) state) BoundIDHashTable.t * ((string LabelAssoc.t) state) BoundRowIDHashTable.t


(* Does NOT fall into an infinite loop even when type variables are mutually dependent or cyclic. *)
let rec show_poly_type_var (hts : hash_tables) = function
  | Bound(bid) ->
      let (bidht, _) = hts in
      if BoundIDHashTable.mem bidht bid then () else begin
        BoundIDHashTable.add bidht bid Touching;
        let pbkd = KindStore.get_bound_id bid in
        let skdopt =
          match pbkd with
          | UniversalKind ->
              None

          | _ ->
              let skd = show_poly_base_kind_sub hts pbkd in
              Some(skd)
        in
        BoundIDHashTable.replace bidht bid (Touched(skdopt))
      end;
      Format.asprintf "%a" BoundID.pp bid

  | Mono(mtv) ->
      show_mono_type_var mtv


and show_poly_row_var (hts : hash_tables) = function
  | BoundRow(brid) ->
      let (_, bridht) = hts in
      if BoundRowIDHashTable.mem bridht brid then () else begin
        BoundRowIDHashTable.add bridht brid Touching;
        let plabmap = KindStore.get_bound_row brid in
        let smap = plabmap |> LabelAssoc.map (show_poly_type_sub hts) in
        BoundRowIDHashTable.replace bridht brid (Touched(smap))
      end;
      Some(Format.asprintf "%a" BoundRowID.pp brid)

  | MonoRow(mrv) ->
      show_mono_row_var mrv


and show_poly_type_sub (hts : hash_tables) : poly_type -> string =
  show_type (show_poly_type_var hts) (show_poly_row_var hts)


and show_poly_base_kind_sub (hts : hash_tables) : poly_base_kind -> string =
  show_base_kind (show_poly_type_var hts) (show_poly_row_var hts)


let show_bound_type_ids ((bidht, _) : hash_tables) =
  BoundIDHashTable.fold (fun bid skdopt acc ->
    let s =
      match skdopt with
      | Touching           -> assert false
      | Touched(Some(skd)) -> Format.asprintf "%a :: %s" BoundID.pp bid skd
      | Touched(None)      -> Format.asprintf "%a" BoundID.pp bid
    in
    Alist.extend acc s
  ) bidht Alist.empty |> Alist.to_list


let show_bound_row_ids ((_, bridht) : hash_tables) =
  BoundRowIDHashTable.fold (fun brid state acc ->
    match state with
    | Touching ->
        assert false

    | Touched(smap) ->
        let skd =
          LabelAssoc.fold (fun label sty acc ->
            Alist.extend acc (Format.asprintf "?%s %s" label sty)
          ) smap Alist.empty |> Alist.to_list |> String.concat ", "
        in
        Alist.extend acc (Format.asprintf "%a :: (%s)" BoundRowID.pp brid skd)

  ) bridht Alist.empty |> Alist.to_list


let create_initial_hash_tables () : hash_tables =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  (bidht, bridht)


let show_poly_type (pty : poly_type) : string list * string list * string =
  let hts = create_initial_hash_tables () in
  let smain = show_poly_type_sub hts pty in
  let sbids = show_bound_type_ids hts in
  let sbrids = show_bound_row_ids hts in
  (sbids, sbrids, smain)


let show_poly_base_kind (pbkd : poly_base_kind) : string list * string list * string =
  let hts = create_initial_hash_tables () in
  let smain = show_poly_base_kind_sub hts pbkd in
  let sbids = show_bound_type_ids hts in
  let sbrids = show_bound_row_ids hts in
  (sbids, sbrids, smain)


let show_poly_kind (pkd : poly_kind) : string list * string list * string =
  let hts = create_initial_hash_tables () in
  match pkd with
  | Kind(pbkddoms, pbkdcod) ->
      let smain =
        let sdoms = pbkddoms |> List.map (show_poly_base_kind_sub hts) in
        let scod = show_poly_base_kind_sub hts pbkdcod in
        if List.length sdoms > 0 then
          Printf.sprintf "(%s) -> %s" (String.concat ", " sdoms) scod
        else
          scod
      in
      let sbids = show_bound_type_ids hts in
      let sbrids = show_bound_row_ids hts in
      (sbids, sbrids, smain)


let pp_poly_type ppf pty =
  let (_, _, sty) = show_poly_type pty in
  Format.fprintf ppf "%s" sty

(*
let show_poly_row : poly_row -> string option =
  show_row show_poly_type_var show_poly_row_var


let pp_poly_row (ppf : Format.formatter) (prow : poly_row) : unit =
  match show_poly_row prow with
  | None    -> ()
  | Some(s) -> Format.fprintf ppf "%s" s


let pp_poly_kind (ppf : Format.formatter) (pkd : poly_kind) =
  Format.fprintf ppf "%s" (show_poly_kind pkd)
*)
