
open MyUtil
open Syntax
open Env


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


module DisplayMap : sig
  type t
  val empty : t
  val add_free_id : FreeID.t -> t -> t
  val add_free_row_id : FreeRowID.t -> t -> t
  val add_bound_id : BoundID.t -> t -> t
  val add_bound_row_id : BoundRowID.t -> t -> t
  val find_free_id : FreeID.t -> t -> string
  val find_free_row_id : FreeRowID.t -> t -> string
  val find_bound_id : BoundID.t -> t -> string
  val find_bound_row_id : BoundRowID.t -> t -> string
  val make_free_id_hash_set : t -> unit FreeIDHashTable.t
  val make_free_row_id_hash_set : t -> unit FreeRowIDHashTable.t
  val make_bound_id_hash_set : t -> unit BoundIDHashTable.t
  val make_bound_row_id_hash_set : t -> unit BoundRowIDHashTable.t
  val fold_free_id : (FreeID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a
  val fold_free_row_id : (FreeRowID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a
  val fold_bound_id : (BoundID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a
  val fold_bound_row_id : (BoundRowID.t -> string -> 'a -> 'a) -> 'a -> t -> 'a
end = struct

  module FreeIDMap = Map.Make(FreeID)
  module FreeRowIDMap = Map.Make(FreeRowID)
  module BoundIDMap = Map.Make(BoundID)
  module BoundRowIDMap = Map.Make(BoundRowID)

  type t = {
    current_max   : int;
    free_ids      : string FreeIDMap.t;
    free_row_ids  : string FreeRowIDMap.t;
    bound_ids     : string BoundIDMap.t;
    bound_row_ids : string BoundRowIDMap.t;
  }

  let empty =
    {
      current_max   = 0;
      free_ids      = FreeIDMap.empty;
      free_row_ids  = FreeRowIDMap.empty;
      bound_ids     = BoundIDMap.empty;
      bound_row_ids = BoundRowIDMap.empty;
    }

  let make_value prefix i =
    let rec aux chs i =
      let q = i / 26 in
      let r = i mod 26 in
      let ch = Char.chr (Char.code 'a' + r) in
      if q <= 0 then
        ch :: chs
      else
        aux (ch :: chs) r
    in
    let chs = aux [] i in
    Core_kernel.String.of_char_list (prefix :: chs)

  let add_free_id fid dispmap =
    let fids = dispmap.free_ids in
    if fids |> FreeIDMap.mem fid then
      dispmap
    else
      let i = dispmap.current_max in
      let s = make_value '\'' i in
      { dispmap with
        current_max = i + 1;
        free_ids    = fids |> FreeIDMap.add fid s;
      }

  let add_free_row_id frid dispmap =
    let frids = dispmap.free_row_ids in
    if frids |> FreeRowIDMap.mem frid then
      dispmap
    else
      let i = dispmap.current_max in
      let s = make_value '\'' i in
      { dispmap with
        current_max  = i + 1;
        free_row_ids = dispmap.free_row_ids |> FreeRowIDMap.add frid s;
      }

  let add_bound_id bid dispmap =
    let bids = dispmap.bound_ids in
    if bids |> BoundIDMap.mem bid then
      dispmap
    else
      let i = dispmap.current_max in
      let s = make_value '#' i in
      { dispmap with
        current_max = i + 1;
        bound_ids   = bids |> BoundIDMap.add bid s;
      }

  let add_bound_row_id brid dispmap =
    let brids = dispmap.bound_row_ids in
    if brids |> BoundRowIDMap.mem brid then
      dispmap
    else
      let i = dispmap.current_max in
      let s = make_value '#' i in
      { dispmap with
        current_max   = i + 1;
        bound_row_ids = brids |> BoundRowIDMap.add brid s;
      }

  let find_free_id fid dispmap =
    match dispmap.free_ids |> FreeIDMap.find_opt fid with
    | Some(s) -> s
    | None    -> Format.asprintf "!!%a!!" FreeID.pp fid

  let find_free_row_id frid dispmap =
    match dispmap.free_row_ids |> FreeRowIDMap.find_opt frid with
    | Some(s) -> s
    | None    -> Format.asprintf "!!%a!!" FreeRowID.pp frid

  let find_bound_id bid dispmap =
    match dispmap.bound_ids |> BoundIDMap.find_opt bid with
    | Some(s) -> s
    | None    -> Format.asprintf "!!%a!!" BoundID.pp bid

  let find_bound_row_id brid dispmap =
    match dispmap.bound_row_ids |> BoundRowIDMap.find_opt brid with
    | Some(s) -> s
    | None    -> Format.asprintf "!!%a!!" BoundRowID.pp brid

  let make_free_id_hash_set dispmap =
    let fidht = FreeIDHashTable.create 32 in
    dispmap.free_ids |> FreeIDMap.iter (fun fid _ ->
      FreeIDHashTable.add fidht fid ()
    );
    fidht

  let make_free_row_id_hash_set dispmap =
    let fridht = FreeRowIDHashTable.create 32 in
    dispmap.free_row_ids |> FreeRowIDMap.iter (fun frid _ ->
      FreeRowIDHashTable.add fridht frid ()
    );
    fridht

  let make_bound_id_hash_set dispmap =
    let bidht = BoundIDHashTable.create 32 in
    dispmap.bound_ids |> BoundIDMap.iter (fun bid _ ->
      BoundIDHashTable.add bidht bid ()
    );
    bidht

  let make_bound_row_id_hash_set dispmap =
    let bridht = BoundRowIDHashTable.create 32 in
    dispmap.bound_row_ids |> BoundRowIDMap.iter (fun brid _ ->
      BoundRowIDHashTable.add bridht brid ()
    );
    bridht

  let fold_free_id f acc dispmap =
    FreeIDMap.fold f dispmap.free_ids acc

  let fold_free_row_id f acc dispmap =
    FreeRowIDMap.fold f dispmap.free_row_ids acc

  let fold_bound_id f acc dispmap =
    BoundIDMap.fold f dispmap.bound_ids acc

  let fold_bound_row_id f acc dispmap =
    BoundRowIDMap.fold f dispmap.bound_row_ids acc

end


let collect_ids_scheme
    (fidht : unit FreeIDHashTable.t)
    (fridht : unit FreeRowIDHashTable.t)
    (bidht : unit BoundIDHashTable.t)
    (bridht : unit BoundRowIDHashTable.t) =
  let rec aux_free_id (fid : FreeID.t) =
    if FreeIDHashTable.mem fidht fid then
      ()
    else
      FreeIDHashTable.add fidht fid ()

  and aux_free_row_id (frid : FreeRowID.t) =
    if FreeRowIDHashTable.mem fridht frid then
      ()
    else
      FreeRowIDHashTable.add fridht frid ()

  and aux_mono ((_, tymain) : mono_type) : unit =
    match tymain with
    | BaseType(_) ->
        ()

    | TypeVar(Updatable{contents = Link(ty)}) ->
        aux_mono ty

    | TypeVar(Updatable{contents = Free(fid)}) ->
        aux_free_id fid

    | TypeVar(MustBeBound(mbbid)) ->
        ()

    | FuncType(domain, tycod) ->
        aux_mono_domain domain;
        aux_mono tycod

    | EffType(domain, eff, ty0) ->
        aux_mono_domain domain;
        aux_mono_effect eff;
        aux_mono ty0

    | PidType(pidty) ->
        aux_mono_pid_type pidty

    | ProductType(tys) ->
        tys |> TupleList.to_list |> List.iter aux_mono

    | RecordType(row) ->
        aux_mono_row row

    | TypeApp(tyid, tyargs) ->
        tyargs |> List.iter aux_mono

    | PackType(_absmodsig) ->
        () (* TODO: traverse signatures *)

  and aux_poly ((_, ptymain) : poly_type) : unit =
    match ptymain with
    | BaseType(_) ->
        ()

    | TypeVar(ptv) ->
        begin
          match ptv with
          | Mono(Updatable{contents = Link(ty)}) ->
              aux_mono ty

          | Mono(Updatable{contents = Free(fid)}) ->
              aux_free_id fid

          | Mono(MustBeBound(_)) ->
              ()

          | Bound(bid) ->
              if BoundIDHashTable.mem bidht bid then
                ()
              else
                BoundIDHashTable.add bidht bid ()
        end

    | FuncType(pdomain, ptycod) ->
        aux_poly_domain pdomain;
        aux_poly ptycod

    | EffType(pdomain, peff, pty0) ->
        aux_poly_domain pdomain;
        aux_poly_effect peff;
        aux_poly pty0

    | PidType(ppidty) ->
        aux_poly_pid_type ppidty

    | ProductType(ptys) ->
        ptys |> TupleList.to_list |> List.iter aux_poly

    | RecordType(prow) ->
        aux_poly_row prow

    | TypeApp(tyid, ptyargs) ->
        ptyargs |> List.iter aux_poly

    | PackType(_absmodsig) ->
        () (* TODO: traverse signatures *)

  and aux_mono_label_assoc (labmap : mono_type LabelAssoc.t) : unit =
    LabelAssoc.iter (fun _ ty -> aux_mono ty) labmap

  and aux_poly_label_assoc (plabmap : poly_type LabelAssoc.t) : unit =
    LabelAssoc.iter (fun _ pty -> aux_poly pty) plabmap

  and aux_mono_domain (domain : mono_domain_type) : unit =
    domain.ordered |> List.iter aux_mono;
    aux_mono_label_assoc domain.mandatory;
    aux_mono_row domain.optional

  and aux_poly_domain (pdomain : poly_domain_type) : unit =
    pdomain.ordered |> List.iter aux_poly;
    aux_poly_label_assoc pdomain.mandatory;
    aux_poly_row pdomain.optional

  and aux_mono_effect (Effect(ty)) =
    aux_mono ty

  and aux_poly_effect (Effect(pty)) =
    aux_poly pty

  and aux_mono_pid_type (Pid(ty)) =
    aux_mono ty

  and aux_poly_pid_type (Pid(pty)) =
    aux_poly pty

  and aux_mono_row : mono_row -> unit = function
    | RowCons(_rlabel, ty, row)                      -> aux_mono ty; aux_mono_row row
    | RowVar(UpdatableRow{contents = LinkRow(row)})  -> aux_mono_row row
    | RowVar(UpdatableRow{contents = FreeRow(frid)}) -> aux_free_row_id frid
    | RowVar(MustBeBoundRow(mbbrid))                 -> ()
    | RowEmpty                                       -> ()

  and aux_poly_row : poly_row -> unit = function
    | RowCons(_rlabel, pty, prow) ->
        aux_poly pty;
        aux_poly_row prow

    | RowVar(MonoRow(prv)) ->
        begin
          match prv with
          | UpdatableRow{contents = LinkRow(row)}  -> aux_mono_row row
          | UpdatableRow{contents = FreeRow(frid)} -> aux_free_row_id frid
          | MustBeBoundRow(_)                      -> ()
        end

    | RowVar(BoundRow(brid)) ->
        if BoundRowIDHashTable.mem bridht brid then
          ()
        else
          BoundRowIDHashTable.add bridht brid ()

    | RowEmpty ->
        ()
  in
  (aux_mono, aux_poly)


let collect_ids_mono (ty : mono_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fidht = DisplayMap.make_free_id_hash_set dispmap in
  let fridht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bidht = DisplayMap.make_bound_id_hash_set dispmap in
  let bridht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (aux_mono, _) = collect_ids_scheme fidht fridht bidht bridht in
  aux_mono ty;
  let dispmap =
    FreeIDHashTable.fold (fun fid () dispmap ->
      dispmap |> DisplayMap.add_free_id fid
    ) fidht dispmap
  in
  let dispmap =
    FreeRowIDHashTable.fold (fun frid () dispmap ->
      dispmap |> DisplayMap.add_free_row_id frid
    ) fridht dispmap
  in
  dispmap


let collect_ids_poly (pty : poly_type) (dispmap : DisplayMap.t) : DisplayMap.t =
  let fidht = DisplayMap.make_free_id_hash_set dispmap in
  let fridht = DisplayMap.make_free_row_id_hash_set dispmap in
  let bidht = DisplayMap.make_bound_id_hash_set dispmap in
  let bridht = DisplayMap.make_bound_row_id_hash_set dispmap in
  let (_, aux_poly) = collect_ids_scheme fidht fridht bidht bridht in
  aux_poly pty;
  let dispmap =
    FreeIDHashTable.fold (fun fid () dispmap ->
      dispmap |> DisplayMap.add_free_id fid
    ) fidht dispmap
  in
  let dispmap =
    FreeRowIDHashTable.fold (fun frid () dispmap ->
      dispmap |> DisplayMap.add_free_row_id frid
    ) fridht dispmap
  in
  let dispmap =
    BoundIDHashTable.fold (fun bid () dispmap ->
      dispmap |> DisplayMap.add_bound_id bid
    ) bidht dispmap
  in
  let dispmap =
    BoundRowIDHashTable.fold (fun brid () dispmap ->
      dispmap |> DisplayMap.add_bound_row_id brid
    ) bridht dispmap
  in
  dispmap


let normalize_row_general : ('a, 'b) row -> ('a, 'b) normalized_row =
fun prow ->
  let rec aux plabmap = function
    | RowCons((_, label), pty, prow) -> aux (plabmap |> LabelAssoc.add label pty) prow
    | RowVar(prv)                    -> NormalizedRow(plabmap, Some(prv))
    | RowEmpty                       -> NormalizedRow(plabmap, None)
  in
  aux LabelAssoc.empty prow


(* Normalizes the polymorphic row `prow`. Here, `MonoRow` is not supposed to occur in `prow`. *)
let normalize_poly_row (prow : poly_row) : normalized_poly_row =
  normalize_row_general prow


let normalize_mono_row (row : mono_row) : normalized_mono_row =
  let rec aux labmap = function
    | RowCons((_, label), ty, row)                   -> aux (labmap |> LabelAssoc.add label ty) row
    | RowVar(UpdatableRow{contents = LinkRow(row)})  -> aux labmap row
    | RowVar(rv)                                     -> NormalizedRow(labmap, Some(rv))
    | RowEmpty                                       -> NormalizedRow(labmap, None)
  in
  aux LabelAssoc.empty row


(* Arguments:
   - `levpred`:
     Given a level of free/must-be-bound ID,
     this predicate returns whether it should be bound or not. *)
let lift_scheme (rngf : Range.t -> Range.t) (levpred : int -> bool) (ty : mono_type) : poly_type =

  let fidht = FreeIDHashTable.create 32 in
  let fridht = FreeRowIDHashTable.create 32 in

  let rec intern (fid : FreeID.t) : BoundID.t =
    match FreeIDHashTable.find_opt fidht fid with
    | Some(bid) ->
        bid

    | None ->
        let bid = BoundID.fresh () in
        FreeIDHashTable.add fidht fid bid;
        bid

  and intern_row (frid : FreeRowID.t) : BoundRowID.t =
    match FreeRowIDHashTable.find_opt fridht frid with
    | Some(brid) ->
        brid

    | None ->
        let brid = BoundRowID.fresh () in
        FreeRowIDHashTable.add fridht frid brid;
        let labset = KindStore.get_free_row frid in
        KindStore.register_bound_row brid labset;
        brid

  and aux_label_assoc (labmap : mono_type LabelAssoc.t) : BoundBothIDSet.t * poly_type LabelAssoc.t =
    LabelAssoc.fold (fun label ty (bbidsetacc, plabmap) ->
      let (bbidset, pty) = aux ty in
      (BoundBothIDSet.union bbidsetacc bbidset, plabmap |> LabelAssoc.add label pty)
    ) labmap (BoundBothIDSet.empty, LabelAssoc.empty)

  and aux_domain (domain : mono_domain_type) : BoundBothIDSet.t * poly_domain_type =
    let {ordered = tydoms; mandatory = mndlabmap; optional = optrow} = domain in
    let (bbidset1, ptydoms) = aux_list tydoms in
    let (bbidset2, pmndlabmap) = aux_label_assoc mndlabmap in
    let (bbidset3, poptrow) = aux_row optrow in
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

    | RecordType(row) ->
        let (bbidset, prow) = aux_row row in
        let pty = (rngf rng, RecordType(prow)) in
        (bbidset, pty)

    | TypeApp(tyid, tyargs) ->
        let (bbidset, ptyargs) = aux_list tyargs in
        let pty = (rngf rng, TypeApp(tyid, ptyargs)) in
        (bbidset, pty)

    | PackType(absmodsig) ->
        let pty = (rngf rng, PackType(absmodsig)) in
        (BoundBothIDSet.empty, pty)

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

  and aux_row : mono_row -> BoundBothIDSet.t * poly_row = function
    | RowCons(rlabel, ty, row) ->
        let (bbidset1, pty) = aux ty in
        let (bbidset2, prow) = aux_row row in
        (BoundBothIDSet.union bbidset1 bbidset2, RowCons(rlabel, pty, prow))

    | RowVar(UpdatableRow{contents = LinkRow(row)}) ->
        let (bbidset, prow) = aux_row row in
        (bbidset, prow)

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

    | RowEmpty ->
        (BoundBothIDSet.empty, RowEmpty)

  in
  let (_bbidset, pty) = aux ty in
  pty


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

    | RecordType(prow) ->
        let row = aux_row prow in
        (rng, RecordType(row))

    | TypeApp(tyid, ptyargs) ->
        (rng, TypeApp(tyid, ptyargs |> List.map aux))

    | PackType(absmodsig) ->
        (rng, PackType(absmodsig))

  and aux_row = function
    | RowCons(rlabel, pty, prow) ->
        let ty = aux pty in
        let row = aux_row prow in
        RowCons(rlabel, ty, row)

    | RowVar(prv) ->
        RowVar(intern_row prv)

    | RowEmpty ->
        RowEmpty

  and aux_domain pdomain =
    let {ordered = ptydoms; mandatory = pmndlabmap; optional = poptrow} = pdomain in
    let tydoms = ptydoms |> List.map aux in
    let mndlabmap = pmndlabmap |> LabelAssoc.map aux in
    let optrow = aux_row poptrow in
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
              let labset = KindStore.get_bound_row brid in
              let frid = FreeRowID.fresh ~message:"instantiate, intern_row" lev in
              KindStore.register_free_row frid labset;
              let mrvu = ref (FreeRow(frid)) in
              UpdatableRow(mrvu)
        end

  and aux pty =
    instantiate_scheme intern intern_row pty
  in
  aux pty


let instantiate (lev : int) (pty : poly_type) =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
    (* Hash tables are created at every (non-partial) call of `instantiate`. *)
  instantiate_by_hash_table bidht bridht lev pty


let make_bound_to_free_hash_table bidht bridht (lev : int) (typarams : BoundID.t list) : mono_type list =
  let tyargacc =
    typarams |> List.fold_left (fun tyargacc bid ->
      let mtv =
        match BoundIDHashTable.find_opt bidht bid with
        | Some(mtvu) ->
            Updatable(mtvu)

        | None ->
            let fid = FreeID.fresh ~message:"make_bound_to_free_hash_table" lev in
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


let apply_type_scheme_mono ((bids, pty_body) : type_scheme) (tyargs : mono_type list) : mono_type option =
  try
    let substmap =
      List.fold_left2 (fun substmap bid tyarg ->
        substmap |> BoundIDMap.add bid tyarg
      ) BoundIDMap.empty bids tyargs
    in
    Some(substitute_mono_type substmap pty_body)
  with
  | _ -> None


let apply_type_scheme_poly ((bids, pty_body) : type_scheme) (ptyargs : poly_type list) : poly_type option =
  try
    let substmap =
      List.fold_left2 (fun substmap bid ptyarg ->
        substmap |> BoundIDMap.add bid ptyarg
      ) BoundIDMap.empty bids ptyargs
    in
    Some(substitute_poly_type substmap pty_body)
  with
  | _ -> None

let make_opaque_type_scheme (bids : BoundID.t list) (tyid : TypeID.t) : type_scheme =
  let dr = Range.dummy "make_opaque_type_scheme" in
  let ptyargs = bids |> List.map (fun bid -> (dr, TypeVar(Bound(bid)))) in
  (bids, (dr, TypeApp(tyid, ptyargs)))


let make_opaque_type_scheme_from_base_kinds (bkds : base_kind list) (tyid : TypeID.t) : type_scheme =
  let bids = bkds |> List.map (fun _bkd -> BoundID.fresh ()) in
  make_opaque_type_scheme bids tyid


let get_opaque_type ((bids, pty_body) : type_scheme) : TypeID.t option =
  match pty_body with
  | (_, TypeApp(tyid, ptyargs)) ->
      begin
        match List.combine bids ptyargs with
        | exception Invalid_argument(_) ->
            None

        | zipped ->
            if
              zipped |> List.for_all (fun (bid, ptyarg) ->
                match ptyarg with
                | (_, TypeVar(Bound(bid0))) -> BoundID.equal bid bid0
                | _                         -> false
              )
            then
              Some(tyid)
            else
              None
      end

  | _ ->
      None


let overwrite_range_of_type (rng : Range.t) (_, tymain) =
  (rng, tymain)


let rec can_row_take_optional : mono_row -> bool = function
  | RowCons(_, _, _)                               -> true
  | RowVar(UpdatableRow{contents = FreeRow(frid)}) -> false
  | RowVar(UpdatableRow{contents = LinkRow(row)})  -> can_row_take_optional row
  | RowVar(MustBeBoundRow(mbbrid))                 -> false
  | RowEmpty                                       -> false


let rec kind_of_arity n =
  let bkddoms = List.init n (fun _ -> TypeKind) in
  Kind(bkddoms, TypeKind)


let rec arity_of_kind = function
  Kind(bkddoms, _) -> List.length bkddoms


(* Omit redundant structures of the given type. *)
let rec canonicalize_root = function
  | (_, TypeVar(Updatable({contents = Link(ty)}))) ->
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
    match show_row ~prefix:"?" ~suffix:"" showtv showrv domain.optional with
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

    | RecordType(row) ->
        begin
          match show_row ~prefix:"" ~suffix:" :" showtv showrv row with
          | None    -> "{}"
          | Some(s) -> Printf.sprintf "{%s}" s
        end

    | TypeApp(tyid, tyargs) ->
        begin
          match tyargs with
          | [] ->
              Format.asprintf "%a" TypeID.pp tyid

          | _ :: _ ->
              let ss = tyargs |> List.map aux in
              Format.asprintf "%a<%s>" TypeID.pp tyid (String.concat ", " ss)
        end

    | PackType(_absmodsig) ->
        "(signature)" (* TODO: show signatures *)

  and aux_effect (Effect(ty)) =
    let s = aux ty in
    "[" ^ s ^ "]"

  and aux_pid_type (Pid(ty)) =
    aux ty
  in
  aux ty


and show_row : 'a 'b. prefix:string -> suffix:string -> ('a -> string) -> ('b -> string option) -> ('a, 'b) row -> string option =
fun ~prefix ~suffix showtv showrv row ->
  let NormalizedRow(labmap, rowvar_opt) = normalize_row_general row in
  let smain_opt = labmap |> show_label_assoc ~prefix ~suffix showtv showrv in
  let svar_opt =
    match rowvar_opt with
    | Some(rv) -> showrv rv |> Option.map (fun s -> prefix ^ s)
    | None     -> None
  in
  match (smain_opt, svar_opt) with
  | (Some(smain), Some(svar)) -> Some(Printf.sprintf "%s, %s" smain svar)
  | (Some(smain), None)       -> Some(smain)
  | (None, Some(svar))        -> Some(svar)
  | (None, None)              -> None


and show_mono_type_var (dispmap : DisplayMap.t) (mtv : mono_type_var) : string =
  match mtv with
  | MustBeBound(mbbid) -> Format.asprintf "%a" MustBeBoundID.pp mbbid
  | Updatable(mtvu)    -> show_mono_type_var_updatable dispmap !mtvu


and show_mono_type_var_updatable (dispmap : DisplayMap.t) (mtvu : mono_type_var_updatable) : string =
  match mtvu with
  | Link(ty)  -> show_type (show_mono_type_var dispmap) (show_mono_row_var dispmap) ty
  | Free(fid) -> dispmap |> DisplayMap.find_free_id fid


and show_mono_row_var (dispmap : DisplayMap.t) (mrv : mono_row_var) : string option =
  match mrv with
  | UpdatableRow(mrvu)     -> show_mono_row_var_updatable dispmap !mrvu
  | MustBeBoundRow(mbbrid) -> Some(Format.asprintf "%a" MustBeBoundRowID.pp mbbrid)


and show_mono_row_var_updatable (dispmap : DisplayMap.t) (mrvu : mono_row_var_updatable) : string option =
  match mrvu with
  | LinkRow(row) ->
      show_row ~prefix:"?" ~suffix:"" (show_mono_type_var dispmap) (show_mono_row_var dispmap) row

  | FreeRow(frid) ->
      let s = dispmap |> DisplayMap.find_free_row_id frid in
      Some(s)


let show_mono_type (dispmap : DisplayMap.t) : mono_type -> string =
  show_type (show_mono_type_var dispmap) (show_mono_row_var dispmap)


let show_mono_row ~(prefix : string) ~(suffix : string) (dispmap : DisplayMap.t) : mono_row -> string option =
  show_row ~prefix ~suffix (show_mono_type_var dispmap) (show_mono_row_var dispmap)


let pp_mono_type dispmap ppf ty =
  Format.fprintf ppf "%s" (show_mono_type dispmap ty)


let pp_mono_row dispmap ppf row =
  Format.fprintf ppf "%s" (Option.value ~default:"(empty)" (show_mono_row ~prefix:"" ~suffix:"" dispmap row))


type hash_tables = unit BoundIDHashTable.t * LabelSet.t BoundRowIDHashTable.t


let rec show_poly_type_var (dispmap : DisplayMap.t) (hts : hash_tables) = function
  | Bound(bid) ->
      let (bidht, _) = hts in
      if BoundIDHashTable.mem bidht bid then () else begin
        BoundIDHashTable.add bidht bid ()
      end;
      dispmap |> DisplayMap.find_bound_id bid

  | Mono(mtv) ->
      show_mono_type_var dispmap mtv


and show_poly_row_var (dispmap : DisplayMap.t) (hts : hash_tables) = function
  | BoundRow(brid) ->
      let (_, bridht) = hts in
      if BoundRowIDHashTable.mem bridht brid then () else begin
        let labset = KindStore.get_bound_row brid in
        BoundRowIDHashTable.add bridht brid labset;
      end;
      let s = dispmap |> DisplayMap.find_bound_row_id brid in
      Some(s)

  | MonoRow(mrv) ->
      show_mono_row_var dispmap mrv


and show_poly_type_sub (dispmap : DisplayMap.t) (hts : hash_tables) : poly_type -> string =
  show_type (show_poly_type_var dispmap hts) (show_poly_row_var dispmap hts)


let show_poly_row_sub (dispmap : DisplayMap.t) (hts : hash_tables) : poly_row -> string option =
  show_row ~prefix:"" ~suffix:"" (show_poly_type_var dispmap hts) (show_poly_row_var dispmap hts)


let show_bound_type_ids (dispmap : DisplayMap.t) ((bidht, _) : hash_tables) =
  BoundIDHashTable.fold (fun bid skdopt acc ->
    let sb = dispmap |> DisplayMap.find_bound_id bid in
    Alist.extend acc (Printf.sprintf "%s :: o" sb)
  ) bidht Alist.empty |> Alist.to_list


let show_bound_row_ids (dispmap : DisplayMap.t) ((_, bridht) : hash_tables) =
  BoundRowIDHashTable.fold (fun brid labset acc ->
    let sb = dispmap |> DisplayMap.find_bound_row_id brid in
    let skd =
      LabelSet.fold (fun label acc ->
        Alist.extend acc label
      ) labset Alist.empty |> Alist.to_list |> String.concat ", "
    in
    Alist.extend acc (Printf.sprintf "%s :: (%s)" sb skd)

  ) bridht Alist.empty |> Alist.to_list


let create_initial_hash_tables () : hash_tables =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  (bidht, bridht)


let show_poly_type (dispmap : DisplayMap.t) (pty : poly_type) : string list * string list * string =
  let hts = create_initial_hash_tables () in
  let smain = show_poly_type_sub dispmap hts pty in
  let sbids = show_bound_type_ids dispmap hts in
  let sbrids = show_bound_row_ids dispmap hts in
  (sbids, sbrids, smain)


let show_poly_row (dispmap : DisplayMap.t) (prow : poly_row) : string list * string list * string =
  let hts = create_initial_hash_tables () in
  let smain = Option.value ~default:"(empty)" (show_poly_row_sub dispmap hts prow) in
  let sbids = show_bound_type_ids dispmap hts in
  let sbrids = show_bound_row_ids dispmap hts in
  (sbids, sbrids, smain)


let pp_poly_type dispmap ppf pty =
  let (_, _, sty) = show_poly_type dispmap pty in
  Format.fprintf ppf "%s" sty


let debug_print_poly_type message pty =
  let dispmap = DisplayMap.empty |> collect_ids_poly pty in
  let (ss1, ss2, s3) = show_poly_type dispmap pty in
  Format.printf "!!! %s ((%s), (%s), %s)\n" message (String.concat ", " ss1) (String.concat ", " ss2) s3


let print_base_kinds (dispmap : DisplayMap.t) =
  let row_names =
    dispmap |> DisplayMap.fold_free_row_id (fun frid row_name acc ->
      let labset = KindStore.get_free_row frid in
      let s = labset |> LabelSet.elements |> String.concat ", " in
      Alist.extend acc (row_name, s)
    ) Alist.empty |> Alist.to_list
  in
  match row_names with
  | [] ->
      ()

  | _ :: _ ->
      Format.printf "  where\n";
      row_names |> List.iter (fun (row_name, skd) ->
        Format.printf "  - %s :: (%s)\n" row_name skd
      )


let debug_print_mono_type message ty =
  let dispmap = DisplayMap.empty |> collect_ids_mono ty in
  let s = show_mono_type dispmap ty in
  Format.printf "!!! %s (%s)\n" message s;
  print_base_kinds dispmap


let show_base_kind (bkd : base_kind) : string =
  match bkd with
  | TypeKind        -> "o"
  | RowKind(labset) -> Printf.sprintf "(%s)" (labset |> LabelSet.elements |> String.concat ", ")


let show_kind (kd : kind) : string =
  let Kind(bkddoms, bkdcod) = kd in
  let sdoms = bkddoms |> List.map show_base_kind in
  let scod = show_base_kind bkdcod in
  match sdoms with
  | []     -> scod
  | _ :: _ -> Printf.sprintf "(%s) -> %s" (String.concat ", " sdoms) scod
