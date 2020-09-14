
open MyUtil
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
        let mbkd = KindStore.get_free_id fid in
        let bid = BoundID.fresh () in
        let pbkd = aux_base_kind mbkd in
        KindStore.register_bound_id bid pbkd;
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

  and aux_base_kind (mbkd : mono_base_kind) : poly_base_kind =
    match mbkd with
    | UniversalKind ->
        UniversalKind

    | RecordKind(labmap) ->
        RecordKind(labmap |> LabelAssoc.map aux)

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

    | FuncType(tydoms, mndlabmap, optrow, tycod) ->
        let ptydoms = tydoms |> List.map aux in
        let pmndlabmap = mndlabmap |> LabelAssoc.map aux in
        let poptrow = aux_option_row optrow in
        let ptycod = aux tycod in
        (rngf rng, FuncType(ptydoms, pmndlabmap, poptrow, ptycod))

    | EffType(eff, ty0) ->
        (rngf rng, EffType(aux_effect eff, aux ty0))

    | PidType(pidty) ->
        (rngf rng, PidType(aux_pid_type pidty))

    | ProductType(tys) ->
        let ptys = tys |> TupleList.map aux in
        (rngf rng, ProductType(ptys))

    | ListType(ty0) ->
        (rngf rng, ListType(aux ty0))

    | RecordType(labmap) ->
        let plabmap = labmap |> LabelAssoc.map aux in
        (rngf rng, RecordType(plabmap))

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

    | RowVar(MustBeBoundRow(mbbrid)) ->
        if levpred (MustBeBoundRowID.get_level mbbrid) then
          let brid = MustBeBoundRowID.to_bound mbbrid in
          RowVar(BoundRow(brid))
            (* We do not need to register a kind to `KindStore`,
               since it has been done when `mbbrid` was created. *)
        else
          RowVar(MonoRow(MustBeBoundRow(mbbrid)))

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

    | FuncType(ptydoms, mndlabmap, optrow, ptycod) ->
        let tydoms = ptydoms |> List.map aux in
        let pmndlabmap = mndlabmap |> LabelAssoc.map aux in
        let poptrow =
          match optrow with
          | FixedRow(labmap) ->
              let plabmap = labmap |> LabelAssoc.map aux in
              FixedRow(plabmap)

          | RowVar(prv) ->
              RowVar(intern_row prv)
        in
        let tycod = aux ptycod in
        (rng, FuncType(tydoms, pmndlabmap, poptrow, tycod))

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

    | RecordType(plabmap) ->
        let labmap = plabmap |> LabelAssoc.map aux in
        (rng, RecordType(labmap))

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


let lift_base_kind_scheme (rngf : Range.t -> Range.t) (levf : int -> bool) (mbkd : mono_base_kind) : poly_base_kind =
  match mbkd with
  | UniversalKind ->
      UniversalKind

  | RecordKind(labmap) ->
      RecordKind(labmap |> LabelAssoc.map (lift_scheme rngf levf))


let generalize_base_kind (lev : int) : mono_base_kind -> poly_base_kind =
  lift_base_kind_scheme
    (fun _ -> Range.dummy "erased")
    (fun levx -> lev < levx)


let lift_base_kind =
  lift_base_kind_scheme
    (fun rng -> rng)
    (fun _ -> false)


let lift_kind (kd : mono_kind) : poly_kind =
  match kd with
  | Kind(bkddoms, bkdcod) ->
      let pbkddoms = bkddoms |> List.map lift_base_kind in
      let pbkdcod = lift_base_kind bkdcod in
      Kind(pbkddoms, pbkdcod)


let rec arity_of_kind = function
  Kind(bkddoms, _) -> List.length bkddoms


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


and show_type : 'a 'b. ('a -> string) -> ('b -> string option) -> ('a, 'b) typ -> string =
fun showtv showrv ty ->
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(bty) ->
        show_base_type bty

    | FuncType(tydoms, mndlabmap, optrow, tycod) ->
        let sdoms = tydoms |> List.map aux in
        let sdomscat = String.concat ", " sdoms in
        let is_ord_empty = (List.length sdoms = 0) in
        let (is_mnds_empty, smnds) =
          match show_label_assoc ~prefix:"-" ~suffix:"" showtv showrv mndlabmap with
          | None    -> (true, "")
          | Some(s) -> (false, s)
        in
        let (is_opts_empty, sopts) =
          match show_row showtv showrv optrow with
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
        let scod = aux tycod in
        Printf.sprintf "fun(%s%s%s%s%s) -> %s"
          sdomscat smid1 smnds smid2 sopts scod

    | EffType(eff, ty0) ->
        let seff = aux_effect eff in
        let s0 = aux ty0 in
        seff ^ s0

    | PidType(pidty) ->
        let spid = aux_pid_type pidty in
        "pid<" ^ spid ^ ">"

    | TypeVar(tv) ->
        showtv tv

    | ProductType(tys) ->
        let ss = tys |> TupleList.to_list |> List.map aux in
        Printf.sprintf "(%s)" (String.concat ", " ss)

    | ListType(ty0) ->
        let s0 = aux ty0 in
        Printf.sprintf "list<%s>" s0

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


let rec show_poly_type_var bidht bridht = function
  | Bound(bid) ->
      begin
        if BoundIDHashTable.mem bidht bid then () else
          let pbkd = KindStore.get_bound_id bid in
          let skdopt =
            match pbkd with
            | UniversalKind ->
                None

            | _ ->
                let skd = show_poly_base_kind_sub bidht bridht pbkd in
                Some(skd)
          in
          BoundIDHashTable.add bidht bid skdopt
      end;
      Format.asprintf "%a" BoundID.pp bid

  | Mono(mtv) ->
      show_mono_type_var mtv


and show_poly_row_var bidht bridht = function
  | BoundRow(brid) ->
      begin
        if BoundRowIDHashTable.mem bridht brid then () else
          let plabmap = KindStore.get_bound_row brid in
          let smap = plabmap |> LabelAssoc.map (show_poly_type_sub bidht bridht) in
          BoundIDHashTable.add bridht brid smap
      end;
      Some(Format.asprintf "%a" BoundRowID.pp brid)

  | MonoRow(mrv) ->
      show_mono_row_var mrv


and show_poly_type_sub bidht bridht : poly_type -> string =
  show_type (show_poly_type_var bidht bridht) (show_poly_row_var bidht bridht)


and show_poly_base_kind_sub bidht bridht : poly_base_kind -> string =
  show_base_kind (show_poly_type_var bidht bridht) (show_poly_row_var bidht bridht)


let show_bound_type_ids bidht =
  BoundIDHashTable.fold (fun bid skdopt acc ->
    let s =
      match skdopt with
      | Some(skd) -> Format.asprintf "%a :: %s" BoundID.pp bid skd
      | None      -> Format.asprintf "%a" BoundID.pp bid
    in
    Alist.extend acc s
  ) bidht Alist.empty |> Alist.to_list


let show_bound_row_ids bridht =
  BoundRowIDHashTable.fold (fun brid smap acc ->
    let skd =
      LabelAssoc.fold (fun label sty acc ->
        Alist.extend acc (Format.asprintf "?%s %s" label sty)
      ) smap Alist.empty |> Alist.to_list |> String.concat ", "
    in
    Alist.extend acc (Format.asprintf "%a :: (%s)" BoundRowID.pp brid skd)
  ) bridht Alist.empty |> Alist.to_list


let show_poly_type (pty : poly_type) : string list * string list * string =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  let smain = show_poly_type_sub bidht bridht pty in
  let sbids = show_bound_type_ids bidht in
  let sbrids = show_bound_row_ids bridht in
  (sbids, sbrids, smain)


let show_poly_base_kind (pbkd : poly_base_kind) : string list * string list * string =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  let smain = show_poly_base_kind_sub bidht bridht pbkd in
  let sbids = show_bound_type_ids bidht in
  let sbrids = show_bound_row_ids bridht in
  (sbids, sbrids, smain)


let show_poly_kind (pkd : poly_kind) : string list * string list * string =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  match pkd with
  | Kind(pbkddoms, pbkdcod) ->
      let smain =
        let sdoms = pbkddoms |> List.map (show_poly_base_kind_sub bidht bridht) in
        let scod = show_poly_base_kind_sub bidht bridht pbkdcod in
        Printf.sprintf "(%s) -> %s" (String.concat ", " sdoms) scod
      in
      let sbids = show_bound_type_ids bidht in
      let sbrids = show_bound_row_ids bridht in
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
