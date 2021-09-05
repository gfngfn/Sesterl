
open MyUtil
open Syntax
open IntermediateSyntax
open Env
open Errors


exception TypeError of type_error


module BindingMap = Map.Make(String)

module SubstMap = Map.Make(TypeID)

type substitution = type_scheme SubstMap.t

type type_intern = BoundID.t -> poly_type -> bool

type row_intern = BoundRowID.t -> normalized_poly_row -> bool

type subtyping_error = unit

type binding_map = (mono_type * local_name * Range.t) BindingMap.t

type variant_definition = type_name * TypeID.t * BoundID.t list * constructor_branch_map

type rec_morph =
  | MonoRec of mono_type
  | PolyRec of poly_type

type pre = {
  level : int;
  tyenv : Typeenv.t;
  local_type_parameters : local_type_parameter_map;
  local_row_parameters  : local_row_parameter_map;
}


module GlobalNameMap = Map.Make(OutputIdentifier.Global)

module SynonymNameSet = Set.Make(String)

module SynonymNameHashSet =
  Hashtbl.Make(
    struct
      type t = type_name
      let equal = String.equal
      let hash = Hashtbl.hash
    end)


let raise_error e =
  raise (TypeError(e))


let merge_quantifier (quant1 : quantifier) (quant2 : quantifier) : quantifier =
  OpaqueIDMap.union (fun _ pkd1 _pkd2 -> Some(pkd1)) quant1 quant2


let internbidf (bidmap : BoundID.t BoundIDMap.t) (bid1 : BoundID.t) (pty2 : poly_type) : bool =
  match pty2 with
  | (_, TypeVar(Bound(bid2))) ->
      begin
        match bidmap |> BoundIDMap.find_opt bid1 with
        | None      -> false
        | Some(bid) -> BoundID.equal bid bid2
      end

  | _ ->
      false


let internbridf (_bidmap : BoundID.t BoundIDMap.t) (_brid1 : BoundRowID.t) (_nomrow2 : normalized_poly_row) : bool =
  (* TODO: implement this when type definitions become able to take row parameters *)
  false


let add_dummy_fold (tynm : type_name) (tyid : TypeID.t) (bids : BoundID.t list) (ctorbrmap : constructor_branch_map) (sigr : SigRecord.t) : SigRecord.t =
  let bid = BoundID.fresh () in
  let dr = Range.dummy "add_dummy_fold" in
  let plabmap =
    ConstructorMap.fold (fun ctornm (_ctorid, ptyargs) plabmap ->
      let domty =
        {
          ordered   = ptyargs;
          mandatory = LabelAssoc.empty;
          optional  = RowEmpty;
        }
      in
      plabmap |> LabelAssoc.add ctornm (dr, FuncType(domty, (dr, TypeVar(Bound(bid)))))
    ) ctorbrmap LabelAssoc.empty
  in
  let domty =
    {
      ordered   = [(dr, TypeApp(tyid, bids |> List.map (fun bid -> (dr, TypeVar(Bound(bid))))))];
      mandatory = plabmap;
      optional  = RowEmpty;
    }
  in
  let pty = (dr, FuncType(domty, (dr, TypeVar(Bound(bid))))) in
  sigr |> SigRecord.add_dummy_fold tynm pty


let add_constructor_definitions (ctordefs : variant_definition list) (sigr : SigRecord.t) : SigRecord.t =
  ctordefs |> List.fold_left (fun sigr ctordef ->
    let (tynm, tyid, bids, ctorbrmap) = ctordef in
    let sigr =
      ConstructorMap.fold (fun ctornm (ctorid, ptyargs) sigr ->
        let centry =
          {
            belongs         = tyid;
            constructor_id  = ctorid;
            type_variables  = bids;
            parameter_types = ptyargs;
          }
        in
        sigr |> SigRecord.add_constructor ctornm centry
      ) ctorbrmap sigr
    in
    sigr |> add_dummy_fold tynm tyid bids ctorbrmap
  ) sigr


let make_type_scheme_from_constructor_entry (centry : constructor_entry) : type_scheme =
  let
    {
      belongs         = tyid;
      type_variables  = bids;
      parameter_types = ptys;
      _
    } = centry
  in
  let dr = Range.dummy "make_type_scheme_from_constructor_entry" in
  let domty =
    {
      ordered   = ptys;
      mandatory = LabelAssoc.empty;
      optional  = RowEmpty;
    }
  in
  let ty_cod = (dr, TypeApp(tyid, bids |> List.map (fun bid -> (dr, TypeVar(Bound(bid)))))) in
  (bids, (dr, FuncType(domty, ty_cod)))


let get_module_name_chain_position (modchain : module_name_chain) : Range.t =
  let ((rngL, _), projs) = modchain in
  match List.rev projs with
  | []             -> rngL
  | (rngR, _) :: _ -> Range.unite rngL rngR


let binding_map_union rng =
  BindingMap.union (fun x _ _ ->
    raise_error (BoundMoreThanOnceInPattern(rng, x))
  )


let get_dependency_on_synonym_types (vertices : SynonymNameSet.t) (pre : pre) (mty : manual_type) : SynonymNameSet.t =
  let hashset = SynonymNameHashSet.create 32 in
    (* A hash set is created on every (non-partial) call. *)
  let register_if_needed (tynm : type_name) : unit =
    if vertices |> SynonymNameSet.mem tynm then
      SynonymNameHashSet.add hashset tynm ()
    else
      ()
  in
  let rec aux ((_, mtymain) : manual_type) : unit =
    match mtymain with
    | MTypeName(tynm, mtyargs) ->
        List.iter aux mtyargs;
        register_if_needed tynm

    | MFuncType((mtydoms, mndlabmtys, mrow), mtycod) ->
        aux_labeled_list mndlabmtys;
        aux_row mrow;
        aux mtycod

    | MProductType(mtys) ->
        mtys |> TupleList.to_list |> List.iter aux

    | MRecordType(mrow) ->
        aux_row mrow

    | MEffType((mtydoms, mndlabmtys, mrow), mty1, mty2) ->
        aux_labeled_list mndlabmtys;
        aux_row mrow;
        List.iter aux mtydoms;
        aux mty1;
        aux mty2

    | MTypeVar(typaram) ->
        ()

    | MModProjType(utmod1, tyident2, mtyargs) ->
        ()

    | MPackType(utsig) ->
        aux_signature utsig

  and aux_labeled_list (labmtys : labeled_manual_type list) : unit =
    labmtys |> List.iter (fun (_, mty) -> aux mty)

  and aux_row (mrow : manual_row) : unit =
    match mrow with
    | MRow(optlabmtys, _) -> aux_labeled_list optlabmtys

  and aux_signature (utsig : untyped_signature) : unit =
    () (* TODO: implement this or restrict the syntax of `pack` *)
  in
  aux mty;
  SynonymNameHashSet.fold (fun sid () set ->
    set |> SynonymNameSet.add sid
  ) hashset SynonymNameSet.empty


let find_module (tyenv : Typeenv.t) ((rng, m) : module_name ranged) : module_entry =
  match tyenv |> Typeenv.find_module m with
  | None    -> raise_error (UnboundModuleName(rng, m))
  | Some(v) -> v


let find_module_from_chain (tyenv : Typeenv.t) ((modident, projs) : module_name_chain) : module_entry =
  let init = find_module tyenv modident in
  let (rng, _) = modident in
  let (ret, _) =
    projs |> List.fold_left (fun (mentry, rng) proj ->
      let modsig = mentry.mod_signature in
      match modsig with
      | ConcFunctor(_) ->
          raise_error (NotOfStructureType(rng, modsig))

      | ConcStructure(sigr) ->
          let (rngproj, modnm) = proj in
          begin
            match sigr |> SigRecord.find_module modnm with
            | None ->
                raise_error (UnboundModuleName(rngproj, modnm))

            | Some(mentry) ->
                let (rng, _) = proj in
                (mentry, rng)
          end
    ) (init, rng)
  in
  ret


let update_type_environment_by_signature_record (sigr : SigRecord.t) (tyenv : Typeenv.t) : Typeenv.t =
  sigr |> SigRecord.fold
    ~v:(fun x ventry tyenv ->
      let pty = ventry.val_type in
      let gname = ventry.val_global in
      tyenv |> Typeenv.add_value x pty (OutputIdentifier.Global(gname))
    )
    ~c:(fun ctornm centry tyenv ->
      tyenv |> Typeenv.add_constructor ctornm centry
    )
    ~f:(fun _tynm _pty tyenv ->
      tyenv
    )
    ~t:(fun tynm tentry tyenv ->
      tyenv |> Typeenv.add_type tynm tentry
    )
    ~m:(fun modnm mentry tyenv ->
      tyenv |> Typeenv.add_module modnm mentry
    )
    ~s:(fun signm absmodsig ->
      Typeenv.add_signature signm absmodsig
    )
    tyenv


let add_open_specs_to_type_environment (openspecs : module_name_chain list) (tyenv : Typeenv.t) : Typeenv.t =
  openspecs |> List.fold_left (fun tyenv openspec ->
    let mentry = find_module_from_chain tyenv openspec in
    let modsig = mentry.mod_signature in
    match modsig with
    | ConcFunctor(_) ->
        let rng0 = get_module_name_chain_position openspec in
        raise_error (NotOfStructureType(rng0, modsig))

    | ConcStructure(sigr) ->
        tyenv |> update_type_environment_by_signature_record sigr
  ) tyenv


let iapply (efun : ast) (mrow : mono_row) ((eargs, mndargmap, optargmap) : ast list * ast LabelAssoc.t * ast LabelAssoc.t) : ast =
  match efun with
  | IVar(name) ->
      IApply(name, mrow, eargs, mndargmap, optargmap)

  | _ ->
      let lname = OutputIdentifier.fresh () in
      ILetIn(lname, efun, IApply(OutputIdentifier.Local(lname), mrow, eargs, mndargmap, optargmap))


let ilambda ((ordnames, mndnamemap, optnamemap) : local_name list * local_name LabelAssoc.t * (local_name * ast option) LabelAssoc.t) (e0 : ast) : ast =
  ILambda(None, ordnames, mndnamemap, optnamemap, e0)


let iletpatin (ipat : pattern) (e1 : ast) (e2 : ast) : ast =
  ICase(e1, [ IBranch(ipat, e2) ])


let iletrecin_single (_, _, name_outer, name_inner, e1) (e2 : ast) : ast =
  match e1 with
  | ILambda(None, ordnames, mndnamemap, optnamemap, e0) ->
      ILetIn(name_outer, ILambda(Some(name_inner), ordnames, mndnamemap, optnamemap, e0), e2)

  | _ ->
      assert false


let iletrecin_multiple (binds : (identifier * poly_type * local_name * local_name * ast) List2.t) (e2 : ast) : ast =
  let (bind1, bind2, bindrest) = List2.decompose binds in
  let binds = TupleList.make bind1 (bind2 :: bindrest) in

  let ipat_inner_tuple =
    IPTuple(binds |> TupleList.map (fun (_, _, _, name_inner, _) -> IPVar(name_inner)))
  in
  let name_for_whole_rec = OutputIdentifier.fresh () in
  let tuple_entries =
    binds |> TupleList.map (fun (_, _, _name_outer, _name_inner, e1) ->
      match e1 with
      | ILambda(None, ordnames, mndnamemap, optnamemap, e0) ->
          ILambda(None, ordnames, mndnamemap, optnamemap,
            iletpatin ipat_inner_tuple
              (IApply(OutputIdentifier.Local(name_for_whole_rec),
                RowEmpty, [], LabelAssoc.empty, LabelAssoc.empty)) e0)

      | _ ->
          assert false
    )
  in
  let ipat_outer_tuple =
    IPTuple(binds |> TupleList.map (fun (_, _, name_outer, _, _) -> IPVar(name_outer)))
  in
  iletpatin ipat_outer_tuple
    (iapply
      (ILambda(Some(name_for_whole_rec), [], LabelAssoc.empty, LabelAssoc.empty, ITuple(tuple_entries)))
      RowEmpty
      ([], LabelAssoc.empty, LabelAssoc.empty))
    e2


let iletrecin (binds : (identifier * poly_type * local_name * local_name * ast) list) (e2 : ast) : ast =
  match binds with
  | []                     -> assert false
  | [bind]                 -> iletrecin_single bind e2
  | bind1 :: bind2 :: rest -> iletrecin_multiple (List2.make bind1 bind2 rest) e2


let occurs (fid : FreeID.t) (ty : mono_type) : bool =
  let lev = FreeID.get_level fid in
  let rec aux ((_, tymain) : mono_type) : bool =
    match tymain with
    | BaseType(_) ->
        false

    | FuncType(domain, tycod) ->
        let bdom = aux_domain domain in
        let bcod = aux tycod in
        bdom || bcod
          (* Must not be short-circuit due to the level inference. *)

    | ProductType(tys) ->
        tys |> TupleList.to_list |> aux_list

    | RecordType(row) ->
        aux_row row

    | TypeApp(_tyid, tyargs) ->
        aux_list tyargs

    | EffType(domain, eff, ty0) ->
        let bdom = aux_domain domain in
        let beff = aux_effect eff in
        let b0 = aux ty0 in
        bdom || beff || b0
          (* Must not be short-circuit due to the level inference. *)

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

    | PackType(_modsig) ->
        false
          (* Signatures do not contain free IDs. *)

  and aux_domain (domain : mono_domain_type) : bool =
    let {ordered = tydoms; mandatory = mndlabmap; optional = optrow} = domain in
    let b1 = aux_list tydoms in
    let bmnd = aux_label_assoc mndlabmap in
    let bopt = aux_row optrow in
    b1 || bmnd || bopt

  and aux_effect (Effect(ty)) =
    aux ty

  and aux_pid_type (Pid(ty)) =
    aux ty

  and aux_row = function
    | RowCons(_, ty, row) ->
        let b1 = aux ty in
        let b2 = aux_row row in
        b1 || b2

    | RowVar(UpdatableRow{contents = FreeRow(_)}) ->
        false

    | RowVar(UpdatableRow{contents = LinkRow(row)}) ->
        aux_row row

    | RowVar(MustBeBoundRow(_)) ->
        false

    | RowEmpty ->
        false

  and aux_label_assoc (labmap : mono_type LabelAssoc.t) =
    LabelAssoc.fold (fun _ ty bacc ->
      let b = aux ty in
      b || bacc
    ) labmap false

  and aux_list (tys : mono_type list) : bool =
    tys |> List.map aux |> List.fold_left ( || ) false
      (* Must not be short-circuit due to the level inference *)
  in
  aux ty


let occurs_row (frid : FreeRowID.t) (row : mono_row) : bool =
  let rec aux (_, tymain) =
    match tymain with
    | BaseType(_) ->
        false

    | FuncType(domain, tycod) ->
        let bdom = aux_domain domain in
        let bcod = aux tycod in
        bdom || bcod
          (* Must not be short-circuit due to the level inference. *)

    | PidType(pidty) ->
        aux_pid pidty

    | EffType(domain, effty, ty0) ->
        let bdom = aux_domain domain in
        let beff = aux_effect effty in
        let b0 = aux ty0 in
        bdom || beff || b0
          (* Must not be short-circuit due to the level inference. *)

    | TypeVar(_) ->
        false

    | ProductType(tys) ->
        tys |> TupleList.to_list |> aux_list

    | RecordType(row) ->
        aux_row row

    | TypeApp(_tyid, tyargs) ->
        aux_list tyargs

    | PackType(_modsig) ->
        false
          (* Signatures do not contain free row IDs. *)

  and aux_domain (domain : mono_domain_type) =
    let {ordered = tydoms; mandatory = mndlabmap; optional = optrow} = domain in
    let b1 = aux_list tydoms in
    let bmnd = aux_label_assoc mndlabmap in
    let bopt = aux_row optrow in
    b1 || bmnd || bopt

  and aux_pid (Pid(ty)) =
    aux ty

  and aux_effect (Effect(ty)) =
    aux ty

  and aux_row = function
    | RowCons(_, ty, row) ->
        let b1 = aux ty in
        let b2 = aux_row row in
        b1 || b2

    | RowVar(UpdatableRow{contents = LinkRow(row)}) ->
        aux_row row

    | RowVar(UpdatableRow{contents = FreeRow(fridx)}) ->
        FreeRowID.equal fridx frid

    | RowVar(MustBeBoundRow(_mbbrid)) ->
        false

    | RowEmpty ->
        false

  and aux_label_assoc (labmap : mono_type LabelAssoc.t) =
    LabelAssoc.fold (fun _ ty bacc ->
      let b = aux ty in
      bacc || b
    ) labmap false

  and aux_list tys =
    tys |> List.map aux |> List.fold_left ( || ) false
      (* Must not be short-circuit due to the level inference. *)
  in
  aux_row row


let rec opaque_occurs_in_type_scheme : 'a 'b. (quantifier -> TypeID.t -> bool) -> ('a -> bool) -> quantifier -> ('a, 'b) typ -> bool =
fun tyidp tvp quant ->
  let rec aux (_, ptymain) =
    match ptymain with
    | BaseType(_)             -> false
    | PidType(typid)          -> aux_pid typid
    | ProductType(tys)        -> tys |> TupleList.to_list |> List.exists aux

    | EffType(domain, tyeff, tysub) ->
        aux_domain domain || aux_effect tyeff || aux tysub

    | FuncType(domain, tycod) ->
        aux_domain domain || aux tycod

    | TypeApp(tyid, tyargs) ->
        tyidp quant tyid || List.exists aux tyargs

    | RecordType(row) ->
        aux_row row

    | TypeVar(tv) ->
        tvp tv

    | PackType(absmodsig) ->
        let (_quant, modsig) = absmodsig in
        opaque_occurs quant modsig
          (* Strictly speaking, we should ensure that `quant` and `_quant` are disjoint. *)

  and aux_domain domain =
    let {ordered = tydoms; mandatory = mndlabmap; optional = optrow} = domain in
    List.exists aux tydoms || aux_label_assoc mndlabmap || aux_row optrow

  and aux_pid = function
    | Pid(ty) -> aux ty

  and aux_effect = function
    | Effect(ty) -> aux ty

  and aux_row = function
    | RowCons(_, ty, row) ->
        let b1 = aux ty in
        let b2 = aux_row row in
        b1 || b2

    | RowVar(_) ->
        false

    | RowEmpty ->
        false

  and aux_label_assoc labmap =
    LabelAssoc.fold (fun _ ty bacc ->
      let b = aux ty in
      b || bacc
    ) labmap false
  in
  aux


and opaque_occurs_in_mono_type (quant : quantifier) : mono_type -> bool =
  let tvp : mono_type_var -> bool = function
    | Updatable({contents = Link(ty)}) -> opaque_occurs_in_mono_type quant ty
    | _                                -> false
  in
  opaque_occurs_in_type_scheme opaque_occurs_in_type_id tvp quant


and opaque_occurs_in_poly_type (quant : quantifier) : poly_type -> bool =
  let tvp : poly_type_var -> bool = function
    | Mono(Updatable({contents = Link(ty)})) -> opaque_occurs_in_mono_type quant ty
    | _                                      -> false
  in
  opaque_occurs_in_type_scheme opaque_occurs_in_type_id tvp quant


and opaque_occurs_in_type_id (quant : quantifier) (tyid : TypeID.t) : bool =
  quant |> OpaqueIDMap.mem tyid


and opaque_occurs (quant : quantifier) (modsig : module_signature) : bool =
  match modsig with
  | ConcStructure(sigr) ->
      opaque_occurs_in_structure quant sigr

  | ConcFunctor(sigftor) ->
      let Domain(sigr) = sigftor.domain in
      let (_quantcod, modsigcod) = sigftor.codomain in
      opaque_occurs_in_structure quant sigr || opaque_occurs quant modsigcod


and opaque_occurs_in_structure (quant : quantifier) (sigr : SigRecord.t) : bool =
  sigr |> SigRecord.fold
      ~v:(fun _x ventry b ->
        let pty = ventry.val_type in
        b || opaque_occurs_in_poly_type quant pty
      )
      ~c:(fun _ctornm centry b ->
        let ptys = centry.parameter_types in
        b || ptys |> List.exists (opaque_occurs_in_poly_type quant)
      )
      ~f:(fun _tynm _pty b ->
        b
      )
      ~t:(fun _tynm tentry b ->
        let (_bids, pty_body) = tentry.type_scheme in
        b || opaque_occurs_in_poly_type quant pty_body
      )
      ~m:(fun _modnm mentry b ->
        let modsig = mentry.mod_signature in
        b || opaque_occurs quant modsig
      )
      ~s:(fun _ (_quant, modsig) b ->
        b || opaque_occurs quant modsig
      )
      false


let label_assoc_union =
  LabelAssoc.union (fun _ _ ty2 -> Some(ty2))


let fresh_type_variable ?name:nameopt (lev : int) (rng : Range.t) : mono_type =
  let fid = FreeID.fresh ~message:"fresh_type_variable" lev in
  let mtvu = ref (Free(fid)) in
  let ty = (rng, TypeVar(Updatable(mtvu))) in
(*
  let name = nameopt |> Option.map (fun x -> x ^ " : ") |> Option.value ~default:"" in
  Format.printf "GEN %sL%d %a :: %a\n" name lev pp_mono_type ty pp_mono_base_kind mbkd;  (* for debug *)
*)
  ty


let fresh_row_variable (lev : int) (labset : LabelSet.t) : mono_row =
  let frid = FreeRowID.fresh ~message:"fresh_row_variable" lev in
  KindStore.register_free_row frid labset;
  let mrvu = ref (FreeRow(frid)) in
  RowVar(UpdatableRow(mrvu))


let check_properly_used (tyenv : Typeenv.t) ((rng, x) : identifier ranged) =
  match tyenv |> Typeenv.is_val_properly_used x with
  | None        -> assert false
  | Some(true)  -> ()
  | Some(false) -> Logging.warn_val_not_used rng x


let get_space_name (rng : Range.t) (m : module_name) : space_name =
  match OutputIdentifier.space_of_module_name m with
  | None        -> raise_error (InvalidIdentifier(rng, m))
  | Some(sname) -> sname


let generate_local_name (rng : Range.t) (x : identifier) : local_name =
  match OutputIdentifier.generate_local x with
  | None        -> raise_error (InvalidIdentifier(rng, x))
  | Some(lname) -> lname


let generate_global_name ~(is_test_suite : bool) ~(arity : int) ~(has_option : bool) (rng : Range.t) (x : identifier) : global_name =
  let suffix = if is_test_suite then "_test_" else "" in
  match OutputIdentifier.generate_global x ~suffix:suffix ~arity:arity ~has_option:has_option with
  | None        -> raise_error (InvalidIdentifier(rng, x))
  | Some(gname) -> gname


let local_name_scheme letbind =
  let (rngv, x) = letbind.vb_identifier in
  let lname_inner = generate_local_name rngv x in
  let lname_outer = OutputIdentifier.fresh () in
  (lname_inner, lname_outer)


let global_name_scheme (is_test_suite : bool) valbind =
  let arity = List.length valbind.vb_parameters + List.length valbind.vb_mandatories in
  let has_option = (List.length valbind.vb_optionals > 0) in
  let (rngv, x) = valbind.vb_identifier in
  let gname = generate_global_name ~is_test_suite ~arity:arity ~has_option:has_option rngv x in
  (gname, gname)


let types_of_format (lev : int) (fmtelems : format_element list) : mono_type list =
  fmtelems |> List.map (function
  | FormatHole(hole, _) ->
      let rng = Range.dummy "format" in
      let ty =
        match hole with
        | HoleC ->
            (rng, BaseType(CharType))

        | HoleF
        | HoleE
        | HoleG ->
            (rng, BaseType(FloatType))

        | HoleS ->
            Primitives.list_type rng (rng, BaseType(CharType))

        | HoleP
        | HoleW ->
            fresh_type_variable lev rng
      in
      [ ty ]

  | FormatConst(_)
  | FormatDQuote
  | FormatBreak
  | FormatTilde ->
      []

  ) |> List.concat


let type_of_base_constant (lev : int) (rng : Range.t) (bc : base_constant) =
  match bc with
  | Unit     -> (rng, BaseType(UnitType))
  | Bool(_)  -> (rng, BaseType(BoolType))
  | Int(_)   -> (rng, BaseType(IntType))
  | Float(_) -> (rng, BaseType(FloatType))
  | BinaryByString(_)
  | BinaryByInts(_)   -> (rng, BaseType(BinaryType))
  | String(_) -> Primitives.list_type rng (Range.dummy "string_literal", BaseType(CharType))
  | Char(_)   -> (rng, BaseType(CharType))

  | FormatString(fmtelems) ->
      let tyarg =
        match types_of_format lev fmtelems with
        | []         -> (Range.dummy "format", BaseType(UnitType))
        | ty1 :: tys -> (Range.dummy "format", ProductType(TupleList.make ty1 tys))
      in
      Primitives.format_type rng tyarg


let rec unify_aux (ty1 : mono_type) (ty2 : mono_type) : (unit, unification_error) result =
  let open ResultMonad in
  let (_, ty1main) = ty1 in
  let (_, ty2main) = ty2 in
  match (ty1main, ty2main) with
  | (TypeVar(Updatable{contents = Link(ty1l)}), _) ->
      unify_aux ty1l ty2

  | (_, TypeVar(Updatable{contents = Link(ty2l)})) ->
      unify_aux ty1 ty2l

  | (TypeVar(MustBeBound(mbbid1)), TypeVar(MustBeBound(mbbid2))) ->
      if MustBeBoundID.equal mbbid1 mbbid2 then
        return ()
      else
        err Contradiction

  | (TypeApp(tyid1, tyargs1), TypeApp(tyid2, tyargs2)) ->
      if TypeID.equal tyid1 tyid2 then
        unify_aux_list tyargs1 tyargs2
      else
        err Contradiction

  | (BaseType(bt1), BaseType(bt2)) ->
      if bt1 = bt2 then
        return ()
      else
        err Contradiction

  | (FuncType(domain1, ty1cod), FuncType(domain2, ty2cod)) ->
      unify_aux_domain domain1 domain2 >>= fun () ->
      unify_aux ty1cod ty2cod

  | (EffType(domain1, eff1, tysub1), EffType(domain2, eff2, tysub2)) ->
      unify_aux_domain domain1 domain2 >>= fun () ->
      unify_aux_effect eff1 eff2 >>= fun () ->
      unify_aux tysub1 tysub2

  | (PidType(pidty1), PidType(pidty2)) ->
      unify_aux_pid_type pidty1 pidty2

  | (ProductType(tys1), ProductType(tys2)) ->
      unify_aux_list (tys1 |> TupleList.to_list) (tys2 |> TupleList.to_list)

  | (RecordType(row1), RecordType(row2)) ->
      unify_aux_row row1 row2

  | (PackType(absmodsig1), PackType(absmodsig2)) ->
      begin
        try
          subtype_abstract_with_abstract
            ~cause:(Range.dummy "unify1")
            ~address:Alist.empty
            absmodsig1 absmodsig2;
          subtype_abstract_with_abstract
            ~cause:(Range.dummy "unify2")
            ~address:Alist.empty
            absmodsig2 absmodsig1;
          return ()
        with
        | _ ->
            err Contradiction
      end

  | (TypeVar(Updatable({contents = Free(fid1)} as mtvu1)), TypeVar(Updatable{contents = Free(fid2)})) ->
      if FreeID.equal fid1 fid2 then
        return ()
      else begin
        mtvu1 := Link(ty2);
        return ()
      end

  | (TypeVar(Updatable({contents = Free(fid1)} as mtvu1)), _) ->
      unify_aux_free_id_and_record fid1 mtvu1 ty2

  | (_, TypeVar(Updatable({contents = Free(fid2)} as mtvu2))) ->
      unify_aux_free_id_and_record fid2 mtvu2 ty1

  | _ ->
      err Contradiction


and unify_aux_free_id_and_record (fid1 : FreeID.t) (mtvu1 : mono_type_var_updatable ref) (ty2 : mono_type) =
  let open ResultMonad in
  let b = occurs fid1 ty2 in
  if b then
    err @@ Inclusion(fid1)
  else begin
    mtvu1 := Link(ty2);
    return ()
  end


and unify_aux_list tys1 tys2 =
  let open ResultMonad in
  try
    List.fold_left2 (fun res ty1 ty2 ->
      res >>= fun () ->
      unify_aux ty1 ty2
    ) (return ()) tys1 tys2
  with
  | Invalid_argument(_) ->
      err Contradiction


and unify_aux_domain domain1 domain2 =
  let open ResultMonad in
  let {ordered = ty1doms; mandatory = mndlabmap1; optional = optrow1} = domain1 in
  let {ordered = ty2doms; mandatory = mndlabmap2; optional = optrow2} = domain2 in
  unify_aux_list ty1doms ty2doms >>= fun () ->
  unify_aux_label_assoc_exact mndlabmap1 mndlabmap2 >>= fun () ->
  unify_aux_row optrow1 optrow2


and unify_aux_effect (Effect(ty1)) (Effect(ty2)) =
  unify_aux ty1 ty2


and unify_aux_pid_type (Pid(ty1)) (Pid(ty2)) =
  unify_aux ty1 ty2


and unify_aux_row (row1 : mono_row) (row2 : mono_row) =
  let open ResultMonad in
  match (row1, row2) with
  | (RowVar(UpdatableRow{contents = LinkRow(row1sub)}), _) ->
      unify_aux_row row1sub row2

  | (_, RowVar(UpdatableRow{contents = LinkRow(row2sub)})) ->
      unify_aux_row row1 row2sub

  | (RowVar(UpdatableRow({contents = FreeRow(frid1)} as mtvu1)), RowVar(UpdatableRow{contents = FreeRow(frid2)})) ->
      if FreeRowID.equal frid1 frid2 then
        return ()
      else begin
        let labset1 = KindStore.get_free_row frid1 in
        let labset2 = KindStore.get_free_row frid2 in
        let labset = LabelSet.union labset1 labset2 in
        mtvu1 := LinkRow(row2);
        KindStore.register_free_row frid2 labset;
        return ()
      end

  | (RowVar(UpdatableRow({contents = FreeRow(frid1)} as mrvu1)), _) ->
      if occurs_row frid1 row2 then
        err @@ InclusionRow(frid1)
      else begin
        let labset1 = KindStore.get_free_row frid1 in
        solve_disjointness_aux row2 labset1 >>= fun () ->
        mrvu1 := LinkRow(row2);
        return ()
      end

  | (_, RowVar(UpdatableRow({contents = FreeRow(frid2)} as mrvu2))) ->
      if occurs_row frid2 row1 then
        err @@ InclusionRow(frid2)
      else begin
        let labset2 = KindStore.get_free_row frid2 in
        solve_disjointness_aux row1 labset2 >>= fun () ->
        mrvu2 := LinkRow(row1);
        return ()
      end

  | (RowVar(MustBeBoundRow(mbbrid1)), RowVar(MustBeBoundRow(mbbrid2))) ->
      if MustBeBoundRowID.equal mbbrid1 mbbrid2 then
        return ()
      else
        err Contradiction

  | (RowVar(MustBeBoundRow(_)), _)
  | (_, RowVar(MustBeBoundRow(_))) ->
      err Contradiction

  | (RowCons((rng, label), ty, row1sub), _) ->
      solve_membership_aux rng label ty row2 >>= fun row2rest ->
      unify_aux_row row1sub row2rest

  | (RowEmpty, RowEmpty) ->
      return ()

  | (RowEmpty, RowCons(_, _, _)) ->
      err Contradiction


(* Check that `labmap2` is more specific than or equal to `labmap1`,
   i.e., the domain of `labmap1` is contained in that of `labmap2`. *)
and unify_aux_label_assoc_subtype ~specific:labmap2 ~general:labmap1 =
  let open ResultMonad in
  LabelAssoc.fold (fun label ty1 res ->
    res >>= fun () ->
    match labmap2 |> LabelAssoc.find_opt label with
    | None      -> err Contradiction
    | Some(ty2) -> unify_aux ty1 ty2
  ) labmap1 (return ())


and unify_aux_label_assoc_exact labmap1 labmap2 =
  let open ResultMonad in
  let merged =
    LabelAssoc.merge (fun _ tyopt1 tyopt2 ->
      match (tyopt1, tyopt2) with
      | (None, None)           -> None
      | (Some(ty1), Some(ty2)) -> Some(unify_aux ty1 ty2)
      | _                      -> Some(err Contradiction)
    ) labmap1 labmap2
  in
  LabelAssoc.fold (fun _ res resacc -> resacc >>= fun () -> res) merged (return ())


and unify_aux_label_assoc_intersection labmap1 labmap2 =
  let open ResultMonad in
  let intersection =
    LabelAssoc.merge (fun _ opt1 opt2 ->
      match (opt1, opt2) with
      | (Some(ty1), Some(ty2)) -> Some((ty1, ty2))
      | _                      -> None
    ) labmap1 labmap2
  in
  LabelAssoc.fold (fun label (ty1, ty2) res ->
    res >>= fun () ->
    unify_aux ty1 ty2
  ) intersection (return ())


(* Solves the constraint that `label : ty` is a field of `row`.
   Returns `Ok(row_rest)` if the constraint is solved where `row_rest` stands for the other fields. *)
and solve_membership_aux (rng : Range.t) (label : label) (ty : mono_type) (row : mono_row) : (mono_row, unification_error) result =
  let open ResultMonad in
  match row with
  | RowCons((rng0, label0), ty0, row0) ->
      if String.equal label0 label then
        unify_aux ty0 ty >>= fun () ->
        return row0
      else
        solve_membership_aux rng label ty row0 >>= fun row0rest ->
        return @@ RowCons((rng0, label0), ty0, row0rest)

  | RowVar(UpdatableRow{contents = LinkRow(row0)}) ->
      solve_membership_aux rng label ty row0

  | RowVar(UpdatableRow({contents = FreeRow(frid0)} as mrvu0)) ->
      let labset0 = KindStore.get_free_row frid0 in
      if labset0 |> LabelSet.mem label then
        err Contradiction (* TODO (error): reject for the disjointness *)
      else begin
        let lev = FreeRowID.get_level frid0 in
        let frid1 = FreeRowID.fresh ~message:"solve_membership_aux" lev in
        KindStore.register_free_row frid1 LabelSet.empty;
        let mrvu1 = ref (FreeRow(frid1)) in
        let row_rest = RowVar(UpdatableRow(mrvu1)) in
        let row_new = RowCons((rng, label), ty, row_rest) in
        mrvu0 := LinkRow(row_new);
        return row_rest
      end

  | RowVar(MustBeBoundRow(_)) ->
      err Contradiction (* TODO (error): solve_membership_aux, reject for must-be-bound row IDs *)

  | RowEmpty ->
      err Contradiction (* TODO (error): solve_membership_aux, RowEmpty *)


(* Solves the constraint that `row` does not have any label in `labset`. *)
and solve_disjointness_aux (row : mono_row) (labset : LabelSet.t) =
  let open ResultMonad in
  match row with
  | RowCons((rng, label), ty, rowsub) ->
      if labset |> LabelSet.mem label then
        err Contradiction
      else
        solve_disjointness_aux rowsub labset

  | RowVar(UpdatableRow{contents = LinkRow(rowsub)}) ->
      solve_disjointness_aux rowsub labset

  | RowVar(UpdatableRow{contents = FreeRow(frid0)}) ->
      let labset0 = KindStore.get_free_row frid0 in
      KindStore.register_free_row frid0 (LabelSet.union labset0 labset);
      return ()

  | RowVar(MustBeBoundRow(mbbrid0)) ->
      let labset0 = KindStore.get_bound_row (MustBeBoundRowID.to_bound mbbrid0) in
      if LabelSet.subset labset labset0 then
        return ()
      else
        err @@ InsufficientRowConstraint{ id = mbbrid0; given = labset0; required = labset; }

  | RowEmpty ->
      return ()


and unify (tyact : mono_type) (tyexp : mono_type) : unit =
  let res = unify_aux tyact tyexp in
  match res with
  | Ok(())   -> ()
  | Error(e) -> raise_error (UnificationError(tyact, tyexp, e))


and unify_effect (Effect(tyact) : mono_effect) (Effect(tyexp) : mono_effect) : unit =
  let res = unify_aux tyact tyexp in
  match res with
  | Ok(())   -> ()
  | Error(e) -> raise_error (UnificationError(tyact, tyexp, e))


and make_rec_initial_type_from_annotation (preL : pre) (letbind : untyped_let_binding) : pre * poly_type option =
  let (rngv, _) = letbind.vb_identifier in
  let ordparams = letbind.vb_parameters in
  let mndparams = letbind.vb_mandatories in
  let optparams = letbind.vb_optionals in

  (* First, add local type/row parameters at level `levS`. *)
  let preS =
    let (pre, _assoc) = make_type_parameter_assoc preL letbind.vb_forall in
    let levS = pre.level + 1 in
    let preS = { pre with level = levS } in
    preS |> add_local_row_parameter letbind.vb_forall_row
  in

  let ptyopt =
    let open OptionMonad in

    ordparams |> List.fold_left (fun opt ordparam ->
      opt >>= fun tyacc ->
      let (_, mtyopt) = ordparam in
      mtyopt |> Option.map (fun mty ->
        let ty = decode_manual_type preS mty in
        Alist.extend tyacc ty
      )
    ) (Some(Alist.empty)) >>= fun ordtyacc ->

    mndparams |> List.fold_left (fun opt mndparam ->
      opt >>= fun labmap ->
      let ((rnglabel, label), (_, mtyopt)) = mndparam in
      if labmap |> LabelAssoc.mem label then
        raise_error (DuplicatedLabel(rnglabel, label))
      else
        mtyopt |> Option.map (fun mty ->
          let ty = decode_manual_type preS mty in
          labmap |> LabelAssoc.add label ty
        )
    ) (Some(LabelAssoc.empty)) >>= fun mndlabmap ->

    optparams |> List.fold_left (fun opt optparam ->
      opt >>= fun (labset_defined, row) ->
      let (((rnglabel, label), (_, mtyopt)), _) = optparam in
      if labset_defined |> LabelSet.mem label then
        raise_error (DuplicatedLabel(rnglabel, label))
      else
        mtyopt |> Option.map (fun mty ->
          let ty = decode_manual_type preS mty in
          let row = RowCons((rnglabel, label), ty, row) in
          let labset_defined = labset_defined |> LabelSet.add label in
          (labset_defined, row)
        )
    ) (Some((LabelSet.empty, RowEmpty))) >>= fun (_, row) ->

    let domty =
      {
        ordered   = Alist.to_list ordtyacc;
        mandatory = mndlabmap;
        optional  = row;
      }
    in
    let tyopt =
      match letbind.vb_return with
      | Pure((mtyopt, _)) ->
          mtyopt |> Option.map (fun mtycod ->
            let tycod = decode_manual_type preS mtycod in
            (rngv, FuncType(domty, tycod))
          )

      | Effectful((mtypairopt, _)) ->
          mtypairopt |> Option.map (fun (mtyeff, mtycod) ->
            let tyeff = decode_manual_type preS mtyeff in
            let tycod = decode_manual_type preS mtycod in
            (rngv, EffType(domty, Effect(tyeff), tycod))
          )
    in
    tyopt |> Option.map (TypeConv.generalize preL.level)
  in
  (preS, ptyopt)


and make_type_parameter_assoc (pre : pre) (tyvarnms : type_variable_binder list) : pre * type_parameter_assoc =
  tyvarnms |> List.fold_left (fun (pre, assoc) ((rng, tyvarnm), kdannot) ->
    let mbbid = MustBeBoundID.fresh (pre.level + 1) in
(*
    Format.printf "MUST-BE-BOUND %s : L%d %a\n" tyvarnm lev MustBeBoundID.pp mbbid;  (* for debug *)
*)
    match assoc |> TypeParameterAssoc.add_last tyvarnm mbbid with
    | None ->
        raise_error (TypeParameterBoundMoreThanOnce(rng, tyvarnm))

    | Some(assoc) ->
        let localtyparams = pre.local_type_parameters |> TypeParameterMap.add tyvarnm mbbid in
        let pre = { pre with local_type_parameters = localtyparams } in
        (pre, assoc)
  ) (pre, TypeParameterAssoc.empty)


and decode_manual_base_kind (pre : pre) ((rng, mnbkdmain) : manual_base_kind) : base_kind =
  match mnbkdmain with
  | MKindName(kdnm) ->
      begin
        match kdnm with
        | "o" -> TypeKind
        | _   -> raise_error (UndefinedKindName(rng, kdnm))
      end


and decode_manual_kind (pre : pre) (mnkd : manual_kind) : kind =
  match mnkd with
  | (_, MKind(mnbkddoms, mnbkdcod)) ->
      let bkddoms = mnbkddoms |> List.map (decode_manual_base_kind pre) in
      let bkdcod = decode_manual_base_kind pre mnbkdcod in
      Kind(bkddoms, bkdcod)


and decode_manual_type (pre : pre) (mty : manual_type) : mono_type =

  let tyenv = pre.tyenv in
  let typarams = pre.local_type_parameters in
  let rowparams = pre.local_row_parameters in

  let invalid rng tynm ~expect:len_expected ~actual:len_actual =
    raise_error (InvalidNumberOfTypeArguments(rng, tynm, len_expected, len_actual))
  in

  let aux_labeled_list =
    decode_manual_record_type pre
  in

  let rec aux (rng, mtymain) =
    let tymain =
      match mtymain with
      | MTypeName(tynm, mtyargs) ->
          let tyargs = mtyargs |> List.map aux in
          let len_actual = List.length tyargs in
          begin
            match tyenv |> Typeenv.find_type tynm with
            | None ->
                begin
                  match (tynm, tyargs) with
                  | ("unit", [])    -> BaseType(UnitType)
                  | ("unit", _)     -> invalid rng "unit" ~expect:0 ~actual:len_actual
                  | ("bool", [])    -> BaseType(BoolType)
                  | ("bool", _)     -> invalid rng "bool" ~expect:0 ~actual:len_actual
                  | ("int", [])     -> BaseType(IntType)
                  | ("int", _)      -> invalid rng "int" ~expect:0 ~actual:len_actual
                  | ("float", [])   -> BaseType(FloatType)
                  | ("float", _)    -> invalid rng "float" ~expect:0 ~actual:len_actual
                  | ("binary", [])  -> BaseType(BinaryType)
                  | ("binary", _)   -> invalid rng "binary" ~expect:0 ~actual:len_actual
                  | ("char", [])    -> BaseType(CharType)
                  | ("char", _)     -> invalid rng "char" ~expect:0 ~actual:len_actual
                  | ("pid", [ty])   -> PidType(Pid(ty))
                  | ("pid", _)      -> invalid rng "pid" ~expect:1 ~actual:len_actual
                  | _               -> raise_error (UndefinedTypeName(rng, tynm))
                end

            | Some(tentry) ->
                let len_expected = TypeConv.arity_of_kind tentry.type_kind in
                let tyscheme = tentry.type_scheme in
                begin
                  match TypeConv.apply_type_scheme_mono tyscheme tyargs with
                  | Ok((_, tymain)) -> tymain
                  | Error(None)     -> invalid rng tynm ~expect:len_expected ~actual:len_actual
                  | Error(_)        -> failwith "TODO (error): report error (base kind mismatch, MTypeName)"
                end
          end

      | MFuncType((mtydoms, mndlabmtys, mrow), mtycod) ->
          let mndlabmap = aux_labeled_list mndlabmtys in
          let optrow = aux_row mrow in
          FuncType({ordered = List.map aux mtydoms; mandatory = mndlabmap; optional = optrow}, aux mtycod)

      | MProductType(mtys) ->
          ProductType(TupleList.map aux mtys)

      | MRecordType(mrow) ->
          let row = aux_row mrow in
          RecordType(row)

      | MEffType((mtydoms, mndlabmtys, mrow), mty1, mty2) ->
          let mndlabmap = aux_labeled_list mndlabmtys in
          let optrow = aux_row mrow in
          let domain = {ordered = List.map aux mtydoms; mandatory = mndlabmap; optional = optrow} in
          EffType(domain, Effect(aux mty1), aux mty2)

      | MTypeVar(typaram) ->
          begin
            match typarams |> TypeParameterMap.find_opt typaram with
            | None ->
                raise_error (UnboundTypeParameter(rng, typaram))

            | Some(mbbid) ->
                TypeVar(MustBeBound(mbbid))
          end

      | MModProjType(utmod1, tyident2, mtyargs) ->
          let (rng2, tynm2) = tyident2 in
          let (absmodsig1, _) = typecheck_module Alist.empty tyenv utmod1 in
          let (quant1, modsig1) = absmodsig1 in
          begin
            match modsig1 with
            | ConcFunctor(_) ->
                let (rng1, _) = utmod1 in
                raise_error (NotOfStructureType(rng1, modsig1))

            | ConcStructure(sigr) ->
                begin
                  match sigr |> SigRecord.find_type tynm2 with
                  | None ->
                      raise_error (UndefinedTypeName(rng2, tynm2))

                  | Some(tentry2) ->
                      let tyargs = mtyargs |> List.map aux in
                      let len_actual = List.length tyargs in
                      let len_expected = TypeConv.arity_of_kind tentry2.type_kind in
                      let tyscheme = tentry2.type_scheme in
                      begin
                        match TypeConv.apply_type_scheme_mono tyscheme tyargs with
                        | Ok((_, tymain) as ty) ->
                            if opaque_occurs_in_mono_type quant1 ty then
                              (* Combining (T-Path) and the second premise “Γ ⊢ Σ : Ω” of (P-Mod)
                                 in the original paper “F-ing modules” [Rossberg, Russo & Dreyer 2014],
                                 we must assert that opaque type variables do not extrude their scope. *)
                              raise_error (OpaqueIDExtrudesScopeViaType(rng, tentry2))
                            else
                              tymain

                        | Error(None) ->
                            invalid rng tynm2 ~expect:len_expected ~actual:len_actual

                        | Error(_) ->
                            failwith "TODO (error): error report (base kind mismatch, MModProjType)"
                      end
                end
          end

      | MPackType(utsig) ->
          let absmodsig = typecheck_signature ~address:Alist.empty tyenv utsig in
          PackType(absmodsig)
    in
    (rng, tymain)

  and aux_row (mrow : manual_row) : mono_row =
    match mrow with
    | MRow(optlabmtys, rowvar_opt) ->
        let row_last =
          match rowvar_opt with
          | None ->
              RowEmpty

          | Some((rng, rowparam)) ->
              begin
                match rowparams |> RowParameterMap.find_opt rowparam with
                | None ->
                    raise_error (UnboundRowParameter(rng, rowparam))

                | Some((mbbrid, _)) ->
                    RowVar(MustBeBoundRow(mbbrid))
              end
        in
        optlabmtys |> List.fold_left (fun row_acc (rlabel, mty) ->
          let ty = aux mty in
          RowCons(rlabel, ty, row_acc)
        ) row_last

  in
  aux mty


and decode_manual_record_type (pre : pre) (labmtys : labeled_manual_type list) : mono_type LabelAssoc.t =
  let aux = decode_manual_type pre in
  labmtys |> List.fold_left (fun labmap (rlabel, mty) ->
    let (rnglabel, label) = rlabel in
    if labmap |> LabelAssoc.mem label then
      raise_error (DuplicatedLabel(rnglabel, label))
    else
      let ty = aux mty in
      labmap |> LabelAssoc.add label ty
  ) LabelAssoc.empty


and add_local_row_parameter (rowvars : (row_variable_name ranged * (label ranged) list) list) (pre : pre) : pre =
  rowvars |> List.fold_left (fun pre ((rng, rowvarnm), mkind) ->
    let rowparams = pre.local_row_parameters in
    if rowparams |> RowParameterMap.mem rowvarnm then
      raise_error (RowParameterBoundMoreThanOnce(rng, rowvarnm))
    else
      let mbbrid = MustBeBoundRowID.fresh pre.level in
      let labset =
        mkind |> List.fold_left (fun labset rlabel ->
          let (rnglabel, label) = rlabel in
          if labset |> LabelSet.mem label then
            raise_error (DuplicatedLabel(rnglabel, label))
          else
            labset |> LabelSet.add label
        ) LabelSet.empty
      in
      KindStore.register_bound_row (MustBeBoundRowID.to_bound mbbrid) labset;
      let rowparams = rowparams |> RowParameterMap.add rowvarnm (mbbrid, labset) in
      { pre with local_row_parameters = rowparams }
  ) pre


and decode_type_annotation_or_fresh (pre : pre) (((rng, x), tyannot) : binder) : mono_type =
  match tyannot with
  | None ->
      fresh_type_variable ~name:x pre.level rng

  | Some(mty) ->
      decode_manual_type pre mty


and decode_parameter (pre : pre) (binder : binder) =
  let ((rngv, x), _) = binder in
  let tydom = decode_type_annotation_or_fresh pre binder in
  let lname : local_name = generate_local_name rngv x in
  (x, tydom, lname)


and add_ordered_parameters_to_type_environment (pre : pre) (binders : binder list) : Typeenv.t * mono_type list * local_name list =
  let (tyenv, lnameacc, tydomacc) =
    List.fold_left (fun (tyenv, lnameacc, ptydomacc) binder ->
      let (x, tydom, lname) = decode_parameter pre binder in
      let ptydom = TypeConv.lift tydom in
      let tyenv = tyenv |> Typeenv.add_value x ptydom (OutputIdentifier.Local(lname)) in
      (tyenv, Alist.extend lnameacc lname, Alist.extend ptydomacc tydom)
    ) (pre.tyenv, Alist.empty, Alist.empty) binders
  in
  let lnames = lnameacc |> Alist.to_list in
  let tydoms = tydomacc |> Alist.to_list in
  (tyenv, tydoms, lnames)


and add_labeled_optional_parameters_to_type_environment (pre : pre) (optbinders : (labeled_binder * untyped_ast option) list) : Typeenv.t * mono_row * (local_name * ast option) LabelAssoc.t =
  optbinders |> List.fold_left (fun (tyenv, optrow, optnamemap) ((rlabel, binder), utdefault) ->
    let (rnglabel, label) = rlabel in
    if optnamemap |> LabelAssoc.mem label then
      raise_error (DuplicatedLabel(rnglabel, label))
    else
      let (x, ty_inner, lname) = decode_parameter pre binder in
      let (ty_outer, default) =
        match utdefault with
        | None ->
            let ty_outer = fresh_type_variable pre.level (Range.dummy "optional") in
            unify ty_inner (Primitives.option_type (Range.dummy "option") ty_outer);
            (ty_outer, None)

        | Some(utast) ->
            let (ty, e) = typecheck pre utast in
            unify ty_inner ty;
            (ty_inner, Some(e))
      in
      let optrow = RowCons(rlabel, ty_outer, optrow) in
      let tyenv = tyenv |> Typeenv.add_value x (TypeConv.lift ty_inner) (OutputIdentifier.Local(lname)) in
      let optnamemap = optnamemap |> LabelAssoc.add label (lname, default) in
      (tyenv, optrow, optnamemap)
  ) (pre.tyenv, RowEmpty, LabelAssoc.empty)


and add_labeled_mandatory_parameters_to_type_environment (pre : pre) (mndbinders : labeled_binder list) : Typeenv.t * mono_type LabelAssoc.t * local_name LabelAssoc.t =
  mndbinders |> List.fold_left (fun (tyenv, labmap, optnamemap) (rlabel, binder) ->
    let (rnglabel, label) = rlabel in
    if labmap |> LabelAssoc.mem label then
      raise_error (DuplicatedLabel(rnglabel, label))
    else
      let (x, ty, lname) = decode_parameter pre binder in
      let labmap = labmap |> LabelAssoc.add label ty in
      let tyenv = tyenv |> Typeenv.add_value x (TypeConv.lift ty) (OutputIdentifier.Local(lname)) in
      let optnamemap = optnamemap |> LabelAssoc.add label lname in
      (tyenv, labmap, optnamemap)
  ) (pre.tyenv, LabelAssoc.empty, LabelAssoc.empty)


and add_parameters_to_type_environment (pre : pre) ((ordbinders, mndbinders, optbinders) : untyped_parameters) =
  let (tyenv, tydoms, ordnames) =
    add_ordered_parameters_to_type_environment pre ordbinders
  in
  let (tyenv, mndlabmap, mndnamemap) =
    add_labeled_mandatory_parameters_to_type_environment { pre with tyenv } mndbinders
  in
  let (tyenv, optrow, optnamemap) =
    add_labeled_optional_parameters_to_type_environment { pre with tyenv } optbinders
  in
  let domain = {ordered = tydoms; mandatory = mndlabmap; optional = optrow} in
  let ibinders = (ordnames, mndnamemap, optnamemap) in
  (tyenv, domain, ibinders)


and typecheck (pre : pre) ((rng, utastmain) : untyped_ast) : mono_type * ast =
  match utastmain with
  | BaseConst(bc) ->
      let ty = type_of_base_constant pre.level rng bc in
      (ty, IBaseConst(bc))

  | Var(modidents1, (rng2, x2)) ->
      begin
        match modidents1 with
        | [] ->
            begin
              match pre.tyenv |> Typeenv.find_value x2 with
              | None ->
                  raise_error (UnboundVariable(rng2, x2))

              | Some((_, ptymain), name) ->
                  let pty = (rng, ptymain) in
                  let ty = TypeConv.instantiate pre.level pty in
                  (ty, IVar(name))
            end

        | modident :: projs ->
            let sigr1 = get_structure_signature pre.tyenv modident projs in
(*
            let (quant1, modsig1) = absmodsig1 in
*)
            begin
              match sigr1 |> SigRecord.find_value x2 with
              | None ->
                  raise_error (UnboundVariable(rng2, x2))

              | Some(ventry) ->
                  let (_, ptymain2) = ventry.val_type in
                  let gname2 = ventry.val_global in
                  let pty2 = (rng, ptymain2) in
(*
                  if opaque_occurs_in_poly_type quant1 pty2 then
                  (* Combining (E-Path) and the second premise “Γ ⊢ Σ : Ω” of (P-Mod)
                     in the original paper “F-ing modules” [Rossberg, Russo & Dreyer 2014],
                     we must assert that opaque type variables do not extrude their scope.
                  *)
                    raise_error (OpaqueIDExtrudesScopeViaValue(rng, pty2))
                  else
*)
                    let ty = TypeConv.instantiate pre.level pty2 in
                    (ty, IVar(OutputIdentifier.Global(gname2)))
            end
      end

  | Lambda(binders, utast0) ->
      let (tyenv, domain, ibinders) = add_parameters_to_type_environment pre binders in
      let pre = { pre with tyenv } in
      let (tycod, e0) = typecheck pre utast0 in
      let ty = (rng, FuncType(domain, tycod)) in
      (ty, ilambda ibinders e0)

  | LambdaEff(binders, utcomp0) ->
      let (tyenv, domain, ibinders) = add_parameters_to_type_environment pre binders in
      let pre = { pre with tyenv } in
      let ((eff, ty0), e0) = typecheck_computation pre utcomp0 in
      let ty = (rng, EffType(domain, eff, ty0)) in
      (ty, ilambda ibinders e0)

  | Apply(utastfun, utargs) ->
      let (tyfun, efun) = typecheck pre utastfun in
      begin
        match TypeConv.canonicalize_root tyfun with
        | (_, FuncType(domain_expected, tyret)) ->
          (* A slight trick for making error messages easier to comprehend. *)
            let iargs = typecheck_arguments_against_domain pre rng utargs domain_expected in
            let tyret =
              let (_, tyretmain) = tyret in
              (rng, tyretmain)
            in
            (tyret, iapply efun domain_expected.optional iargs)

        | _ ->
            let (domain, optrow, iargs) = typecheck_arguments pre rng utargs in
            let tyret = fresh_type_variable ~name:"(Apply)" pre.level rng in
            unify tyfun (Range.dummy "Apply", FuncType(domain, tyret));
            (tyret, iapply efun optrow iargs)
      end

  | Freeze(rngapp, frozenfun, utastargs, restrngs) ->
      let (ptyfun, gname) =
        match frozenfun with
        | FrozenModFun(modidentchain1, ident2) ->
            let mentry = find_module_from_chain pre.tyenv modidentchain1 in
            let modsig1 = mentry.mod_signature in
            begin
              match modsig1 with
              | ConcFunctor(_) ->
                  let ((rng1, _), _) = modidentchain1 in
                  raise_error (NotOfStructureType(rng1, modsig1))

              | ConcStructure(sigr) ->
                  let (rng2, x) = ident2 in
                  begin
                    match sigr |> SigRecord.find_value x with
                    | None         -> raise_error (UnboundVariable(rng2, x))
                    | Some(ventry) -> (ventry.val_type, ventry.val_global)
                  end
            end

        | FrozenFun((rng0, x)) ->
            begin
              match pre.tyenv |> Typeenv.find_value x with
              | None ->
                  raise_error (UnboundVariable(rng0, x))

              | Some((_, ptymain), name) ->
                  begin
                    match name with
                    | OutputIdentifier.Global(gname) -> ((rng0, ptymain), gname)
                    | _                              -> raise_error (CannotFreezeNonGlobalName(rng0, x))
                  end
            end
      in
      let tyfun = TypeConv.instantiate pre.level ptyfun in
      let tyeargs = List.map (typecheck pre) utastargs in
      let tyargs = List.map fst tyeargs in
      let eargs = List.map snd tyeargs in
      let tyrests =
        restrngs |> List.map (fun restrng ->
          fresh_type_variable ~name:"Freeze, rest" pre.level restrng
        )
      in
      let tyargsall = List.append tyargs tyrests in

      let tyrecv = fresh_type_variable ~name:"Freeze, recv" pre.level rng in
      let eff = Effect(tyrecv) in
      let tyret = fresh_type_variable ~name:"Freeze, ret" pre.level rng in
      let domain = {ordered = tyargsall; mandatory = LabelAssoc.empty; optional = RowEmpty} in
      unify tyfun (Range.dummy "Freeze1", EffType(domain, eff, tyret));
      let tyrest =
        let dr = Range.dummy "Freeze2" in
        match tyrests with
        | []        -> (dr, BaseType(UnitType))
        | ty :: tys -> (dr, ProductType(TupleList.make ty tys))
      in
      (Primitives.frozen_type rng ~rest:tyrest ~receive:tyrecv ~return:tyret, IFreeze(gname, eargs))

  | FreezeUpdate(utast0, utastargs, restrngs) ->
      let (ty0, e0) = typecheck pre utast0 in
      let tyeargs = List.map (typecheck pre) utastargs in
      let tyargs = List.map fst tyeargs in
      let eargs = List.map snd tyeargs in
      let tyholes =
        restrngs |> List.map (fun restrng ->
          fresh_type_variable ~name:"FreezeUpdate, rest1" pre.level restrng
        )
      in
      let tyrecv =
        fresh_type_variable ~name:"FreezeUpdate, recv" pre.level (Range.dummy "FreezeUpdate, recv")
      in
      let tyret =
        fresh_type_variable ~name:"FreezeUpdate, ret" pre.level (Range.dummy "FreezeUpdate, ret")
      in
      let ty_expected =
        let tyrest_expected =
          let dr = Range.dummy "FreezeUpdate, rest2" in
          match List.append tyargs tyholes with
          | []        -> (dr, BaseType(UnitType))
          | ty :: tys -> (dr, ProductType(TupleList.make ty tys))
        in
        Primitives.frozen_type (Range.dummy "FreezeUpdate") ~rest:tyrest_expected ~receive:tyrecv ~return:tyret
      in
      unify ty0 ty_expected;
      let tyrest =
        let dr = Range.dummy "FreezeUpdate, rest3" in
        match tyholes with
        | []        -> (dr, BaseType(UnitType))
        | ty :: tys -> (dr, ProductType(TupleList.make ty tys))
      in
      (Primitives.frozen_type rng ~rest:tyrest ~receive:tyrecv ~return:tyret, IFreezeUpdate(e0, eargs))


  | If(utast0, utast1, utast2) ->
      let (ty0, e0) = typecheck pre utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      unify ty1 ty2;
      let ibranches = [ IBranch(IPBool(true), e1); IBranch(IPBool(false), e2) ] in
      (ty1, ICase(e0, ibranches))

  | LetIn(NonRec(letbind), utast2) ->
      let (pty, lname, e1) = typecheck_let generate_local_name pre letbind in
      let tyenv =
        let (_, x) = letbind.vb_identifier in
        pre.tyenv |> Typeenv.add_value x pty (OutputIdentifier.Local(lname)) in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      check_properly_used tyenv letbind.vb_identifier;
      (ty2, ILetIn(lname, e1, e2))

  | LetIn(Rec(letbinds), utast2) ->
      let proj lname = OutputIdentifier.Local(lname) in
      let binds = typecheck_letrec_mutual local_name_scheme proj pre letbinds in
      let (ty2, e2) =
        let tyenv =
          binds |> List.fold_left (fun tyenv (x, pty, lname_outer, _, _) ->
            tyenv |> Typeenv.add_value x pty (OutputIdentifier.Local(lname_outer))
          ) pre.tyenv
        in
        typecheck { pre with tyenv } utast2
      in
      (ty2, iletrecin binds e2)

  | Tuple(utasts) ->
      let tyes = utasts |> TupleList.map (typecheck pre) in
      let tys = tyes |> TupleList.map fst in
      let es = tyes |> TupleList.map snd in
      let ty = (rng, ProductType(tys)) in
      (ty, ITuple(es))

  | ListNil ->
      let tysub = fresh_type_variable pre.level (Range.dummy "list-nil") in
      let ty = Primitives.list_type rng tysub in
      (ty, IListNil)

  | ListCons(utast1, utast2) ->
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      unify ty2 (Primitives.list_type (Range.dummy "list-cons") ty1);
      (ty2, IListCons(e1, e2))

  | Case(utast0, branches) ->
      let (ty0, e0) = typecheck pre utast0 in
      let tyret = fresh_type_variable pre.level rng in
      let ibrs = branches |> List.map (typecheck_pure_case_branch pre ~pattern:ty0 ~return:tyret) in
      (tyret, ICase(e0, ibrs))

  | LetPatIn(utpat, utast1, utast2) ->
      let (tyenv, ipat, bindmap, e1) = typecheck_let_pattern pre rng utpat utast1 in
      let (ty2, e2) = typecheck { pre with tyenv } utast2 in
      BindingMap.iter (fun x (_, _, rng) ->
        check_properly_used tyenv (rng, x)
      ) bindmap;
      (ty2, iletpatin ipat e1 e2)

  | Constructor(modidents, ctornm, utastargs) ->
      let (tyid, ctorid, tyargs, tys_expected) = typecheck_constructor pre rng modidents ctornm in
      begin
        match List.combine utastargs tys_expected with
        | exception Invalid_argument(_) ->
            let len_expected = List.length tys_expected in
            let len_actual = List.length utastargs in
            raise_error (InvalidNumberOfConstructorArguments(rng, ctornm, len_expected, len_actual))

        | zipped ->
            let eacc =
              zipped |> List.fold_left (fun eacc (utast, ty_expected) ->
                let (ty, e) = typecheck pre utast in
                unify ty ty_expected;
                Alist.extend eacc e
              ) Alist.empty
            in
            let ty = (rng, TypeApp(tyid, tyargs)) in
            let e = IConstructor(ctorid, Alist.to_list eacc) in
            (ty, e)
      end

  | BinaryByList(nrs) ->
      let ns =
        nrs |> List.map (fun (rngn, n) ->
          if 0 <= n && n <= 255 then n else
            raise_error (InvalidByte(rngn))
        )
      in
      ((rng, BaseType(BinaryType)), IBaseConst(BinaryByInts(ns)))

  | Record(labasts) ->
      let (emap, row) =
        labasts |> List.fold_left (fun (emap, row) (rlabel, utast) ->
          let (rnglabel, label) = rlabel in
          if emap |> LabelAssoc.mem label then
            raise_error (DuplicatedLabel(rnglabel, label))
          else
            let (ty, e) = typecheck pre utast in
            let row = RowCons(rlabel, ty, row) in
            let emap = emap |> LabelAssoc.add label e in
            (emap, row)
        ) (LabelAssoc.empty, RowEmpty)
      in
      ((rng, RecordType(row)), IRecord(emap))

  | RecordAccess(utast1, ((_, label) as rlabel)) ->
      let (ty1, e1) = typecheck pre utast1 in
      let ty_ret = fresh_type_variable pre.level rng in
      let row_rest = fresh_row_variable pre.level (LabelSet.singleton label) in
      unify ty1 (Range.dummy "RecordAccess", RecordType(RowCons(rlabel, ty_ret, row_rest)));
      (ty_ret, IRecordAccess(e1, label))

  | RecordUpdate(utast1, ((_, label) as rlabel), utast2) ->
      let (ty1, e1) = typecheck pre utast1 in
      let (ty2, e2) = typecheck pre utast2 in
      let row_rest = fresh_row_variable pre.level (LabelSet.singleton label) in
      unify ty1 (Range.dummy "RecordUpdate", RecordType(RowCons(rlabel, ty2, row_rest)));
      (ty1, IRecordUpdate(e1, label, e2))

  | Pack(modidentchain1, utsig2) ->
      let mentry = find_module_from_chain pre.tyenv modidentchain1 in
      let modsig1 = mentry.mod_signature in
      let sname1 = mentry.mod_name in
      let absmodsig2 = typecheck_signature ~address:Alist.empty pre.tyenv utsig2 in
      let absmodsig = coerce_signature ~cause:rng ~address:Alist.empty modsig1 absmodsig2 in
      ((rng, PackType(absmodsig)), IPack(sname1))

  | Assert(utast0) ->
      let (ty0, e0) = typecheck pre utast0 in
      unify ty0 Primitives.assertion_function_type;
      ((rng, BaseType(UnitType)), IAssert(rng, e0))


and typecheck_let_pattern (pre : pre) (rng : Range.t) (utpat : untyped_pattern) (utast1 : untyped_ast) =
  let (ty1, e1) = typecheck { pre with level = pre.level + 1 } utast1 in
  let (typat, ipat, bindmap) = typecheck_pattern pre utpat in
  unify ty1 typat;
  let tyenv =
    BindingMap.fold (fun x (ty, lname, _) tyenv ->
      let pty = TypeConv.generalize pre.level ty in
      tyenv |> Typeenv.add_value x pty (OutputIdentifier.Local(lname))
    ) bindmap pre.tyenv
  in
  (tyenv, ipat, bindmap, e1)


and typecheck_computation (pre : pre) (utcomp : untyped_computation_ast) : (mono_effect * mono_type) * ast =
  let (rng, utcompmain) = utcomp in
  match utcompmain with
  | CompDo(identopt, utcomp1, utcomp2) ->
      let ((eff1, ty1), e1) = typecheck_computation pre utcomp1 in
      let (tyx, tyenv, lname) =
        match identopt with
        | None ->
            ((Range.dummy "do-unit", BaseType(UnitType)), pre.tyenv, OutputIdentifier.unused)

        | Some(((rngv, x), _) as binder) ->
            let tyx = decode_type_annotation_or_fresh pre binder in
            let lname = generate_local_name rngv x in
            (tyx, pre.tyenv |> Typeenv.add_value x (TypeConv.lift tyx) (OutputIdentifier.Local(lname)), lname)
      in
      unify ty1 tyx;
      let ((eff2, ty2), e2) = typecheck_computation { pre with tyenv } utcomp2 in
      unify_effect eff1 eff2;
      let e2 = ILetIn(lname, e1, e2) in
      ((eff2, ty2), e2)

  | CompReceive(branches) ->
      let lev = pre.level in
      let effexp =
        let ty = fresh_type_variable lev (Range.dummy "receive-recv") in
        Effect(ty)
      in
      let tyret = fresh_type_variable lev (Range.dummy "receive-ret") in
      let ibrs = branches |> List.map (typecheck_receive_branch pre effexp tyret) in
      ((effexp, tyret), IReceive(ibrs))

  | CompLetIn(NonRec(letbind), utcomp2) ->
      let (pty, lname, e1) = typecheck_let generate_local_name pre letbind in
      let tyenv =
        let (_, x) = letbind.vb_identifier in
        pre.tyenv |> Typeenv.add_value x pty (OutputIdentifier.Local(lname)) in
      let ((eff2, ty2), e2) = typecheck_computation { pre with tyenv } utcomp2 in
      check_properly_used tyenv letbind.vb_identifier;
      ((eff2, ty2), ILetIn(lname, e1, e2))

  | CompLetIn(Rec(letbinds), utcomp2) ->
      let proj lname = OutputIdentifier.Local(lname) in
      let binds = typecheck_letrec_mutual local_name_scheme proj pre letbinds in
      let ((eff2, ty2), e2) =
        let tyenv =
          binds |> List.fold_left (fun tyenv (x, pty, lname_outer, _, _) ->
            tyenv |> Typeenv.add_value x pty (OutputIdentifier.Local(lname_outer))
          ) pre.tyenv
        in
        typecheck_computation { pre with tyenv } utcomp2
      in
      ((eff2, ty2), iletrecin binds e2)

  | CompLetPatIn(utpat, utast1, utcomp2) ->
      let (tyenv, ipat, bindmap, e1) = typecheck_let_pattern pre rng utpat utast1 in
      let ((eff2, ty2), e2) = typecheck_computation { pre with tyenv } utcomp2 in
      ((eff2, ty2), iletpatin ipat e1 e2)

  | CompIf(utast0, utcomp1, utcomp2) ->
      let (ty0, e0) = typecheck pre utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let ((eff1, ty1), e1) = typecheck_computation pre utcomp1 in
      let ((eff2, ty2), e2) = typecheck_computation pre utcomp2 in
      unify_effect eff1 eff2;
      unify ty1 ty2;
      let ibranches = [ IBranch(IPBool(true), e1); IBranch(IPBool(false), e2) ] in
      ((eff1, ty1), ICase(e0, ibranches))

  | CompCase(utast0, branches) ->
      let (ty0, e0) = typecheck pre utast0 in
      let eff =
        let tyrecv = fresh_type_variable pre.level (Range.dummy "CompCase1") in
        Effect(tyrecv)
      in
      let tyret = fresh_type_variable pre.level (Range.dummy "CompCase2") in
      let ibrs = branches |> List.map (typecheck_effectful_case_branch pre ~pattern:ty0 ~return:(eff, tyret)) in
      ((eff, tyret), ICase(e0, ibrs))

  | CompApply(utastfun, utargs) ->
      let (tyfun, efun) = typecheck pre utastfun in
      let (domain, optrow, iargs) = typecheck_arguments pre rng utargs in
      let eff =
        let tyrecv = fresh_type_variable ~name:"(CompApply2)" pre.level rng in
        Effect(tyrecv)
      in
      let tyret = fresh_type_variable ~name:"(CompApply1)" pre.level rng in
      unify tyfun (Range.dummy "CompApply", EffType(domain, eff, tyret));
      ((eff, tyret), iapply efun optrow iargs)


and get_structure_signature (tyenv : Typeenv.t) (modident : module_name ranged) (projs : (module_name ranged) list) : SigRecord.t =
  let (rnginit, _) = modident in
  let mentry = find_module tyenv modident in
  let modsig = mentry.mod_signature in
  let (modsig, rnglast) =
    projs |> List.fold_left (fun (modsig, rnglast) proj ->
      match modsig with
      | ConcFunctor(_) ->
          raise_error (NotOfStructureType(rnglast, modsig))

      | ConcStructure(sigr) ->
          let (rng, modnm) = proj in
          begin
            match sigr |> SigRecord.find_module modnm with
            | None         -> raise_error (UnboundModuleName(rng, modnm))
            | Some(mentry) -> (mentry.mod_signature, rng)
          end
    ) (modsig, rnginit)
  in
  begin
    match modsig with
    | ConcFunctor(_)      -> raise_error (NotOfStructureType(rnglast, modsig))
    | ConcStructure(sigr) -> sigr
  end


and typecheck_arguments (pre : pre) (rng : Range.t) ((utastargs, mndutastargs, optutastargs) : untyped_arguments) =
  let tyeargs = List.map (typecheck pre) utastargs in
  let tyargs = List.map fst tyeargs in
  let eargs = List.map snd tyeargs in

  let (mndlabmap, mndargmap) =
    mndutastargs |> List.fold_left (fun (mndlabmap, mndargmap) (rlabel, utast) ->
      let (rnglabel, label) = rlabel in
      if mndlabmap |> LabelAssoc.mem label then
        raise_error (DuplicatedLabel(rnglabel, label))
      else
        let (ty, e) = typecheck pre utast in
        let mndlabmap = mndlabmap |> LabelAssoc.add label ty in
        let mndargmap = mndargmap |> LabelAssoc.add label e in
        (mndlabmap, mndargmap)
    ) (LabelAssoc.empty, LabelAssoc.empty)
  in

  let (optrow, optargmap) =
    let frid = FreeRowID.fresh ~message:"Apply, row" pre.level in
      (* Note: the initial kind for `frid` will be assigned after traversing the given optional arguments. *)
    let row_init =
      let mrvu = ref (FreeRow(frid)) in
      RowVar(UpdatableRow(mrvu))
    in
    let (optrow, optlabset, optargmap) =
      optutastargs |> List.fold_left (fun (optrow, optlabset, optargmap) (rlabel, utast) ->
        let (rnglabel, label) = rlabel in
        if optlabset |> LabelSet.mem label then
          raise_error (DuplicatedLabel(rnglabel, label))
        else
          let (ty, e) = typecheck pre utast in
          let optrow = RowCons(rlabel, ty, optrow) in
          let optlabset = optlabset |> LabelSet.add label in
          let optargmap = optargmap |> LabelAssoc.add label e in
          (optrow, optlabset, optargmap)
      ) (row_init, LabelSet.empty, LabelAssoc.empty)
    in
    KindStore.register_free_row frid optlabset;
(*
    Format.printf "!!! typecheck_arguments (range: %a, length: %d, optrow: %a)\n" Range.pp rng (List.length optutastargs) TypeConv.(pp_mono_row DisplayMap.empty) optrow;
*)
    (optrow, optargmap)
  in

  let domain = {ordered = tyargs; mandatory = mndlabmap; optional = optrow} in
  (domain, optrow, (eargs, mndargmap, optargmap))


and typecheck_arguments_against_domain (pre : pre) (rng : Range.t) ((utastargs, mndutastargs, optutastargs) : untyped_arguments) (domain_expected : mono_domain_type) =
  let {ordered = tys_expected; mandatory = mndlabmap_expected; optional = optrow_expected} = domain_expected in
  let eargs =
    let numord_got = List.length utastargs in
    let numord_expected = List.length tys_expected in
    if numord_got = numord_expected then
      List.fold_left2 (fun eargacc utastarg ty_expected ->
        let (ty_got, e) = typecheck pre utastarg in
        unify ty_got ty_expected;
        Alist.extend eargacc e
      ) Alist.empty utastargs tys_expected |> Alist.to_list
    else
      raise_error @@ BadArityOfOrderedArguments{range = rng; got = numord_got; expected = numord_expected}
  in
  let mndargmap =
    let (mndlabmap_rest, mndargmap) =
      mndutastargs |> List.fold_left (fun (mndlabmap_rest, mndargmap) (rlabel, utast) ->
        let (rnglabel, label) = rlabel in
        if mndargmap |> LabelAssoc.mem label then
          raise_error @@ DuplicatedLabel(rnglabel, label)
        else
          match mndlabmap_rest |> LabelAssoc.find_opt label with
          | None ->
              raise_error @@ UnexpectedMandatoryLabel{range = rnglabel; label = label}

          | Some(ty_expected) ->
              let (ty_got, e) = typecheck pre utast in
              unify ty_got ty_expected;
              let mndlabmap_rest = mndlabmap_rest |> LabelAssoc.remove label in
              let mndargmap = mndargmap |> LabelAssoc.add label e in
              (mndlabmap_rest, mndargmap)

      ) (mndlabmap_expected, LabelAssoc.empty)
    in
    match mndlabmap_rest |> LabelAssoc.bindings with
    | []               -> mndargmap
    | (label, ty) :: _ -> raise_error @@ MissingMandatoryLabel{range = rng; label = label; typ = ty}
  in
  let optargmap =
    let NormalizedRow(labmap_known, rowvar_opt) = TypeConv.normalize_mono_row optrow_expected in
    let (all_labset, unknown_labels, optargmap) =
      optutastargs |> List.fold_left (fun (all_labset, unknown_labels, optargmap) (rlabel, utast) ->
        let (rng_label, label) = rlabel in
        if optargmap |> LabelAssoc.mem label then
          raise_error (DuplicatedLabel(rng, label))
        else
          let (ty_got, e) = typecheck pre utast in
          let optargmap = optargmap |> LabelAssoc.add label e in
          let all_labset = all_labset |> LabelSet.add label in
          match labmap_known |> LabelAssoc.find_opt label with
          | None ->
              (all_labset, unknown_labels |> LabelAssoc.add label (rng_label, ty_got), optargmap)

          | Some(ty_expected) ->
              unify ty_got ty_expected;
              (all_labset, unknown_labels, optargmap)

      ) (LabelSet.empty, LabelAssoc.empty, LabelAssoc.empty)
    in

    begin
      match LabelAssoc.bindings unknown_labels with
      | (label, _) :: _ ->
          begin
            match rowvar_opt with
            | Some(UpdatableRow({contents = FreeRow(frid)} as mrvu)) ->
                let row_unknown =
                  let row_init =
                    let frid0 = FreeRowID.fresh ~message:"typecheck_arguments_against_domain" pre.level in
                    KindStore.register_free_row frid0 all_labset;
                    let mrvu0 = ref (FreeRow(frid0)) in
                    RowVar(UpdatableRow(mrvu0))
                  in
                  LabelAssoc.fold (fun label (rng, ty) row_acc ->
                    RowCons((rng, label), ty, row_acc)
                  ) unknown_labels row_init
                in
(*
                Format.printf "!!! against_domain (range: %a, unknown: %a, row: %a)\n" Range.pp rng (LabelAssoc.pp (fun ppf _ -> Format.fprintf ppf "_")) unknown_labels TypeConv.(pp_mono_row DisplayMap.empty) row_unknown;
*)
                mrvu := LinkRow(row_unknown)

            | _ ->
                raise_error @@ UnexpectedOptionalLabel{range = rng; label = label}
          end

      | _ ->
          ()
    end;
    optargmap
  in
  (eargs, mndargmap, optargmap)


and typecheck_constructor (pre : pre) (rng : Range.t) (modidents : (module_name ranged) list) (ctornm : constructor_name) =
  match modidents with
  | [] ->
      begin
        match pre.tyenv |> Typeenv.find_constructor ctornm with
        | None ->
            raise_error (UndefinedConstructor(rng, ctornm))

        | Some(centry) ->
            let tyid = centry.belongs in
            let ctorid = centry.constructor_id in
            let bids = centry.type_variables in
            let ptys = centry.parameter_types in
            let (tyargs, tys_expected) = TypeConv.instantiate_type_arguments pre.level bids ptys in
            (tyid, ctorid, tyargs, tys_expected)
      end

  | modident :: projs ->
      let sigr1 = get_structure_signature pre.tyenv modident projs in
      begin
        match sigr1 |> SigRecord.find_constructor ctornm with
        | None ->
            raise_error (UndefinedConstructor(rng, ctornm))

        | Some(centry) ->
            let vid = centry.belongs in
            let ctorid = centry.constructor_id in
            let bids = centry.type_variables in
            let ptys = centry.parameter_types in
            let (tyargs, tys_expected) = TypeConv.instantiate_type_arguments pre.level bids ptys in
            (vid, ctorid, tyargs, tys_expected)
      end


and typecheck_pure_case_branch (pre : pre) ~pattern:typatexp ~return:tyret (CaseBranch(pat, utast1)) =
  let (typat, ipat, bindmap) = typecheck_pattern pre pat in
  let tyenv =
    BindingMap.fold (fun x (ty, lname, _) tyenv ->
      tyenv |> Typeenv.add_value x (TypeConv.lift ty) (OutputIdentifier.Local(lname))
    ) bindmap pre.tyenv
  in
  let (ty1, e1) = typecheck { pre with tyenv } utast1 in
  BindingMap.iter (fun x (_, _, rng) ->
    check_properly_used tyenv (rng, x)
  ) bindmap;
  unify typat typatexp;
  unify ty1 tyret;
  IBranch(ipat, e1)


and typecheck_effectful_case_branch (pre : pre) ~pattern:typatexp ~return:(eff, tyret) (CompCaseBranch(pat, utcomp1)) =
  let (typat, ipat, bindmap) = typecheck_pattern pre pat in
  let tyenv =
    BindingMap.fold (fun x (ty, lname, _) tyenv ->
      tyenv |> Typeenv.add_value x (TypeConv.lift ty) (OutputIdentifier.Local(lname))
    ) bindmap pre.tyenv
  in
  let ((eff1, ty1), e1) = typecheck_computation { pre with tyenv } utcomp1 in
  BindingMap.iter (fun x (_, _, rng) ->
    check_properly_used tyenv (rng, x)
  ) bindmap;
  unify typat typatexp;
  unify_effect eff1 eff;
  unify ty1 tyret;
  IBranch(ipat, e1)


and typecheck_receive_branch (pre : pre) (effexp : mono_effect) (tyret : mono_type) (ReceiveBranch(pat, utcomp1)) =
  let (typat, ipat, bindmap) = typecheck_pattern pre pat in
  let tyenv =
    BindingMap.fold (fun x (ty, lname, _) tyenv ->
      tyenv |> Typeenv.add_value x (TypeConv.lift ty) (OutputIdentifier.Local(lname))
    ) bindmap pre.tyenv
  in
  let ((eff1, ty1), e1) = typecheck_computation { pre with tyenv } utcomp1 in
  BindingMap.iter (fun x (_, _, rng) ->
    check_properly_used tyenv (rng, x)
  ) bindmap;
  unify_effect (Effect(typat)) effexp;
  unify_effect eff1 effexp;
  unify ty1 tyret;
  IBranch(ipat, e1)


and typecheck_pattern (pre : pre) ((rng, patmain) : untyped_pattern) : mono_type * pattern * binding_map =
  let immediate tymain ipat = ((rng, tymain), ipat, BindingMap.empty) in
  match patmain with
  | PUnit    -> immediate (BaseType(UnitType)) IPUnit
  | PBool(b) -> immediate (BaseType(BoolType)) (IPBool(b))
  | PInt(n)  -> immediate (BaseType(IntType)) (IPInt(n))

  | PBinary(s) -> immediate (BaseType(BinaryType)) (IPBinary(s))

  | PChar(uchar) ->
      immediate (BaseType(CharType)) (IPChar(uchar))

  | PVar(x) ->
      let ty = fresh_type_variable ~name:x pre.level rng in
      let lname = generate_local_name rng x in
      (ty, IPVar(lname), BindingMap.singleton x (ty, lname, rng))

  | PWildCard ->
      let ty = fresh_type_variable ~name:"_" pre.level rng in
      (ty, IPWildCard, BindingMap.empty)

  | PListNil ->
      let ty =
        let tysub = fresh_type_variable pre.level rng in
        Primitives.list_type rng tysub
      in
      (ty, IPListNil, BindingMap.empty)

  | PListCons(pat1, pat2) ->
      let (ty1, ipat1, bindmap1) = typecheck_pattern pre pat1 in
      let (ty2, ipat2, bindmap2) = typecheck_pattern pre pat2 in
      let bindmap = binding_map_union rng bindmap1 bindmap2 in
      unify ty2 (Primitives.list_type (Range.dummy "pattern-cons") ty1);
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

  | PConstructor(modidents, ctornm, pats) ->
      let (tyid, ctorid, tyargs, tys_expected) = typecheck_constructor pre rng modidents ctornm in
      begin
        try
          let (ipatacc, bindmap) =
            List.fold_left2 (fun (ipatacc, bindmapacc) ty_expected pat ->
              let (ty, ipat, bindmap) = typecheck_pattern pre pat in
              unify ty ty_expected;
              (Alist.extend ipatacc ipat, binding_map_union rng bindmapacc bindmap)
            ) (Alist.empty, BindingMap.empty) tys_expected pats
          in
          let ty = (rng, TypeApp(tyid, tyargs)) in
          (ty, IPConstructor(ctorid, Alist.to_list ipatacc), bindmap)
        with
        | Invalid_argument(_) ->
            let len_expected = List.length tys_expected in
            let len_actual = List.length pats in
            raise_error (InvalidNumberOfConstructorArguments(rng, ctornm, len_expected, len_actual))
      end


and typecheck_let : 'n. (Range.t -> identifier -> 'n) -> pre -> untyped_let_binding -> poly_type * 'n * ast =
fun namef preL letbind ->
  let (rngv, x) = letbind.vb_identifier in
  let ordparams = letbind.vb_parameters in
  let mndparams = letbind.vb_mandatories in
  let optparams = letbind.vb_optionals in

  let (ty1, e0, ibinders) =

   (* First, add local type/row parameters at level `levS`. *)
    let preS =
      let (preL, _assoc) = make_type_parameter_assoc preL letbind.vb_forall in
      { preL with level = preL.level + 1 } |> add_local_row_parameter letbind.vb_forall_row
    in

   (* Second, add local value parameters at level `levS`. *)
    let (tyenv, domain, ibinders) = add_parameters_to_type_environment preS (ordparams, mndparams, optparams) in
    let preS = { preS with tyenv } in

    (* Finally, typecheck the body expression. *)
    match letbind.vb_return with
    | Pure((tyretopt, utast0)) ->
        let (ty0, e0) = typecheck preS utast0 in
        tyretopt |> Option.map (fun mty0 ->
          let ty0_expected = decode_manual_type preS mty0 in
          unify ty0 ty0_expected
        ) |> Option.value ~default:();
        let ty1 = (rngv, FuncType(domain, ty0)) in
        (ty1, e0, ibinders)

    | Effectful((tyretopt, utcomp0)) ->
        let ((eff0, ty0), e0) = typecheck_computation preS utcomp0 in
        tyretopt |> Option.map (fun (mty1, mty2) ->
          let ty1_expected = decode_manual_type preS mty1 in
          let ty2_expected = decode_manual_type preS mty2 in
          unify_effect eff0 (Effect(ty1_expected));
          unify ty0 ty2_expected
        ) |> Option.value ~default:();
        let ty1 = (rngv, EffType(domain, eff0, ty0)) in
        (ty1, e0, ibinders)
  in
  let e1 = ilambda ibinders e0 in
  let pty1 = TypeConv.generalize preL.level ty1 in
  let name = namef rngv x in
  (pty1, name, e1)


and typecheck_letrec_mutual : 'n. (untyped_let_binding -> 'n * 'n) -> ('n -> name) -> pre -> untyped_let_binding list -> (identifier * poly_type * 'n * 'n * ast) list =
fun namesf proj preL letbinds ->

  let levS = preL.level + 1 in

  (* Register type variables and names for output corresponding to bound names
     before traversing definitions *)
  let (tupleacc, tyenv) =
    letbinds |> List.fold_left (fun (tupleacc, tyenv) letbind ->
      let (rngv, x) = letbind.vb_identifier in
      let (name_inner, name_outer) = namesf letbind in
      let (preS, ptyopt) = make_rec_initial_type_from_annotation preL letbind in
      let (tyenv, morph) =
        match ptyopt with
        | Some(pty) ->
            let tyenv = tyenv |> Typeenv.add_value x pty (proj name_inner) in
            (tyenv, PolyRec(pty))

        | None ->
            let tyf = fresh_type_variable ~name:x levS rngv in
            let tyenv = tyenv |> Typeenv.add_value x (TypeConv.lift tyf) (proj name_inner) in
            (tyenv, MonoRec(tyf))
      in
      (Alist.extend tupleacc (letbind, name_inner, name_outer, morph, preS), tyenv)
    ) (Alist.empty, preL.tyenv)
  in

  let bindacc =
    tupleacc |> Alist.to_list |> List.fold_left (fun bindacc (letbind, name_inner, name_outer, morph, preS) ->
      let preS = { preS with tyenv } in
      let (pty, e1) = typecheck_letrec_single preS letbind morph in
      let (_, x) = letbind.vb_identifier in
      Alist.extend bindacc (x, pty, name_outer, name_inner, e1)
    ) Alist.empty
  in
  bindacc |> Alist.to_list


and typecheck_letrec_single (preS : pre) (letbind : untyped_let_binding) (morph : rec_morph) : poly_type * ast =
  let (rngv, x) = letbind.vb_identifier in
  let ordparams = letbind.vb_parameters in
  let mndparams = letbind.vb_mandatories in
  let optparams = letbind.vb_optionals in

  let (ty1, e0, ibinders) =
    (* Add local value parameters at level `pre.level`. *)
    let (tyenv, domain, ibinders) = add_parameters_to_type_environment preS (ordparams, mndparams, optparams) in
    let preS = { preS with tyenv } in

    (* Finally, typecheck the body expression. *)
    match letbind.vb_return with
    | Pure((tyretopt, utast0)) ->
        let (ty0, e0) = typecheck preS utast0 in
        begin
          match (morph, tyretopt) with
          | (MonoRec(_), Some(mty0)) ->
              let ty0_expected = decode_manual_type preS mty0 in
              unify ty0 ty0_expected

          | _ ->
              ()
        end;
        let ty1 = (rngv, FuncType(domain, ty0)) in
        (ty1, e0, ibinders)

    | Effectful((tyretopt, utcomp0)) ->
        let ((eff0, ty0), e0) = typecheck_computation preS utcomp0 in
        begin
          match (morph, tyretopt) with
          | (MonoRec(_), Some((mty1, mty2))) ->
              let ty1_expected = decode_manual_type preS mty1 in
              let ty2_expected = decode_manual_type preS mty2 in
              unify_effect eff0 (Effect(ty1_expected));
              unify ty0 ty2_expected

          | _ ->
              ()
        end;
        let ty1 = (rngv, EffType(domain, eff0, ty0)) in
        (ty1, e0, ibinders)
  in
  let e1 = ilambda ibinders e0 in
  let ptyf = TypeConv.generalize (preS.level - 1) ty1 in
  begin
    match morph with
    | MonoRec(tyf) ->
        unify ty1 tyf

    | PolyRec(ptyannot) ->
        if subtype_poly_type ptyf ptyannot then
          ()
        else
          raise_error (PolymorphicContradiction(rngv, x, ptyf, ptyannot))
  end;
  (ptyf, e1)


and make_constructor_branch_map (pre : pre) (ctorbrs : constructor_branch list) : constructor_branch_map =
  ctorbrs |> List.fold_left (fun ctormap ctorbr ->
    match ctorbr with
    | ConstructorBranch(attrs, (rng, ctornm), mtyargs) ->
        let (ctorattr, warnings) = ConstructorAttribute.decode attrs in
        warnings |> List.iter Logging.warn_invalid_attribute;
        let tyargs = mtyargs |> List.map (decode_manual_type pre) in
        let ptyargs = tyargs |> List.map (TypeConv.generalize pre.level) in
        let ctorid =
          match ctorattr.target_atom with
          | None ->
              begin
                match ConstructorID.from_upper_camel_case ctornm with
                | Some(ctorid) -> ctorid
                | None         -> raise_error (InvalidIdentifier(rng, ctornm))
              end

          | Some((rng_atom, target_atom)) ->
              begin
                match ConstructorID.from_snake_case target_atom with
                | Some(ctorid) -> ctorid
                | None         -> raise_error (InvalidIdentifier(rng_atom, target_atom))
              end
        in
        ctormap |> ConstructorMap.add ctornm (ctorid, ptyargs)
  ) ConstructorMap.empty


(* `subtype_poly_type_impl internbid internbrid pty1 pty2` checks that
   whether `pty1` is more general than (or equal to) `pty2`.
   Note that being more general means being smaller as polymorphic types;
   we have `pty1 <= pty2` in that if `x : pty1` holds and `pty1` is more general than `pty2`, then `x : pty2`.
   For example, we have `(∀α. α → α) <= (int → int)`.
   The parameter `internbid` is used for `internbid bid pty`, which returns
   whether the bound ID `bid` occurring in `pty1` is mapped to a type equivalent to `pty`.
*)
and subtype_poly_type_impl (internbid : type_intern) (internbrid : row_intern) (pty1 : poly_type) (pty2 : poly_type) : bool =
  let rec aux pty1 pty2 =
(*
  let (sbt1, sbr1, sty1) = TypeConv.show_poly_type TypeConv.DisplayMap.empty pty1 in
  let (sbt2, sbr2, sty2) = TypeConv.show_poly_type TypeConv.DisplayMap.empty pty2 in
  Format.printf "!!! {subtype_poly_type_impl> %s <?= %s\n" sty1 sty2;  (* for debug *)
  Format.printf "!!! - %a\n" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Format.pp_print_string) (List.concat [sbt1; sbr1; sbt2; sbr2]);
*)
    let (_, ptymain1) = pty1 in
    let (_, ptymain2) = pty2 in
    match (ptymain1, ptymain2) with
    | (TypeVar(Mono(_)), _)
    | (_, TypeVar(Mono(_))) ->
        assert false
          (* Monomorphic type variables cannot occur at level 0, according to type generalization. *)

    | (BaseType(bt1), BaseType(bt2)) ->
        bt1 = bt2

    | (FuncType(pdomain1, ptycod1), FuncType(pdomain2, ptycod2)) ->
        let bdom = aux_domain pdomain1 pdomain2 in
        let bcod = aux ptycod1 ptycod2 in
        bdom && bcod

    | (PidType(pidty1), PidType(pidty2)) ->
        aux_pid pidty1 pidty2

    | (EffType(domain1, effty1, pty1), EffType(domain2, effty2, pty2)) ->
        let b0 = aux_domain domain1 domain2 in
        let b1 = aux_effect effty1 effty2 in
        let b2 = aux pty1 pty2 in
        b0 && b1 && b2

    | (ProductType(ptys1), ProductType(ptys2)) ->
        aux_list (TupleList.to_list ptys1) (TupleList.to_list ptys2)

    | (RecordType(prow1), RecordType(prow2)) ->
        subtype_row_with_equal_domain internbid internbrid prow1 prow2

    | (PackType(absmodsig1), PackType(absmodsig2)) ->
        begin
          try
            subtype_abstract_with_abstract
              ~cause:(Range.dummy "subtype_poly_type1")
              ~address:Alist.empty
              absmodsig1 absmodsig2;
            subtype_abstract_with_abstract
              ~cause:(Range.dummy "subtype_poly_type2")
              ~address:Alist.empty
              absmodsig2 absmodsig1;
            true
          with
          | _ -> false
        end

    | (TypeVar(Bound(bid1)), _) ->
        internbid bid1 pty2

    | (TypeApp(tyid1, ptyargs1), TypeApp(tyid2, ptyargs2)) ->
        TypeID.equal tyid1 tyid2 && aux_list ptyargs1 ptyargs2

    | _ ->
        false

  and aux_list ptys1 ptys2 =
    match List.combine ptys1 ptys2 with
    | exception Invalid_argument(_) ->
        false

    | ptypairs ->
        ptypairs |> List.fold_left (fun bacc (pty1, pty2) ->
          let b = aux pty1 pty2 in
          bacc && b
        ) true

  and aux_domain domain1 domain2 =
    let {ordered = ptydoms1; mandatory = mndlabmap1; optional = poptrow1} = domain1 in
    let {ordered = ptydoms2; mandatory = mndlabmap2; optional = poptrow2} = domain2 in
    let b1 = aux_list ptydoms1 ptydoms2 in
    let bmnd = subtype_label_assoc_with_equal_domain internbid internbrid mndlabmap1 mndlabmap2 in
    let bopt = subtype_row_with_equal_domain internbid internbrid poptrow1 poptrow2 in
    b1 && bmnd && bopt

  and aux_pid (Pid(pty1)) (Pid(pty2)) =
    aux pty1 pty2

  and aux_effect (Effect(pty1)) (Effect(pty2)) =
    aux pty1 pty2
  in
  aux pty1 pty2


(* Checks that `dom plabmap1 ⊆ dom plabmap2` and `∀label ∈ dom plabmap1. plabmap1(label) <: plabmap2(label)`
   by referring and updating `internbid` and `internbrid`. *)
and subtype_label_assoc_inclusive (internbid : type_intern) (internbrid : row_intern) (plabmap1 : poly_type LabelAssoc.t) (plabmap2 : poly_type LabelAssoc.t) : (poly_type LabelAssoc.t) option =
  let merged =
    LabelAssoc.merge (fun label pty1_opt pty2_opt ->
      match (pty1_opt, pty2_opt) with
      | (Some(pty1), Some(pty2)) -> Some(Ok(subtype_poly_type_impl internbid internbrid pty1 pty2))
      | (None, Some(pty2))       -> Some(Error(pty2))
      | _                        -> Some(Ok(false))
    ) plabmap1 plabmap2
  in
  if merged |> LabelAssoc.for_all (fun _label res -> Result.value ~default:true res) then
    let plabmap_diff =
      merged |> LabelAssoc.filter_map (fun _label res ->
        match res with
        | Ok(_)       -> None
        | Error(pty2) -> Some(pty2)
      )
    in
    Some(plabmap_diff)
  else
    None


and subtype_label_assoc_with_equal_domain (internbid : type_intern) (internbrid : row_intern) (plabmap1 : poly_type LabelAssoc.t) (plabmap2 : poly_type LabelAssoc.t) : bool =
  LabelAssoc.merge (fun label pty1_opt pty2_opt ->
    match (pty1_opt, pty2_opt) with
    | (Some(pty1), Some(pty2)) -> Some(subtype_poly_type_impl internbid internbrid pty1 pty2)
    | _                        -> Some(false)
  ) plabmap1 plabmap2 |> LabelAssoc.for_all (fun _label b -> b)


and subtype_row_with_equal_domain (internbid : type_intern) (internbrid : row_intern) (prow1 : poly_row) (prow2 : poly_row) : bool =
(*
  let (sbt1, sbr1, sty1) = TypeConv.show_poly_row TypeConv.DisplayMap.empty prow1 in
  let (sbt2, sbr2, sty2) = TypeConv.show_poly_row TypeConv.DisplayMap.empty prow2 in
  Format.printf "!!! {subtype_row_with_equal_domain> %s <?= %s\n" sty1 sty2;  (* for debug *)
  Format.printf "!!! - %a\n" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Format.pp_print_string) (List.concat [sbt1; sbr1; sbt2; sbr2]);
*)
  let NormalizedRow(plabmap1, rowvar1_opt) = TypeConv.normalize_poly_row prow1 in
  let NormalizedRow(plabmap2, rowvar2_opt) = TypeConv.normalize_poly_row prow2 in

  match (rowvar1_opt, rowvar2_opt) with
  | (None, None) ->
      subtype_label_assoc_with_equal_domain internbid internbrid plabmap1 plabmap2

  | (Some(MonoRow(_)), _) | (_, Some(MonoRow(_))) ->
      assert false

  | (None, Some(BoundRow(_brid2))) ->
      false

  | (Some(BoundRow(brid1)), _) ->
      let opt = subtype_label_assoc_inclusive internbid internbrid plabmap1 plabmap2 in
      begin
        match opt with
        | None               -> false
        | Some(plabmap_diff) -> internbrid brid1 (NormalizedRow(plabmap_diff, rowvar2_opt))
      end


and subtype_poly_type (pty1 : poly_type) (pty2 : poly_type) : bool =
  let bidht = BoundIDHashTable.create 32 in
  let bridht = BoundRowIDHashTable.create 32 in
  let internbid (bid1 : BoundID.t) (pty2 : poly_type) : bool =
    match BoundIDHashTable.find_opt bidht bid1 with
    | None ->
        BoundIDHashTable.add bidht bid1 pty2;
        true

    | Some(pty) ->
        poly_type_equal pty pty2
  in
  let internbrid (brid1 : BoundRowID.t) (nomrow2 : normalized_poly_row) : bool =
    match BoundRowIDHashTable.find_opt bridht brid1 with
    | None ->
        BoundRowIDHashTable.add bridht brid1 nomrow2;
        true

    | Some(nomrow) ->
        normalized_poly_row_equal nomrow nomrow2
  in
  subtype_poly_type_impl internbid internbrid pty1 pty2


(* Checks that `prow1` and `prow2` are exactly the same up to reordering.
   Here, `Mono` and `MonoRow` are not supposed to occur in `prow1` nor `prow2`. *)
and poly_row_equal (prow1 : poly_row) (prow2 : poly_row) : bool =
  normalized_poly_row_equal (TypeConv.normalize_poly_row prow1) (TypeConv.normalize_poly_row prow2)


and normalized_poly_row_equal (nomrow1 : normalized_poly_row) (nomrow2 : normalized_poly_row) : bool =
  let NormalizedRow(plabmap1, rowvar1_opt) = nomrow1 in
  let NormalizedRow(plabmap2, rowvar2_opt) = nomrow2 in
  let bmap =
    LabelAssoc.merge (fun _ ptyopt1 ptyopt2 ->
      match (ptyopt1, ptyopt2) with
      | (None, None)             -> None
      | (Some(pty1), Some(pty2)) -> Some(poly_type_equal pty1 pty2)
      | _                        -> Some(false)
    ) plabmap1 plabmap2 |> LabelAssoc.for_all (fun _ b -> b)
  in
  if bmap then
    match (rowvar1_opt, rowvar2_opt) with
    | (None, None)                                   -> true
    | (Some(BoundRow(brid1)), Some(BoundRow(brid2))) -> BoundRowID.equal brid1 brid2
    | _                                              -> false
  else
    false


(* Checks that `pty1` and `pty2` is exactly equal (up to reordering of records, etc.).
   Here, `Mono` and `MonoRow` are not supposed to occur in `pty1` nor `pty2`. *)
and poly_type_equal (pty1 : poly_type) (pty2 : poly_type) : bool =
  let rec aux (pty1 : poly_type) (pty2 : poly_type) : bool =
    let (_, ptymain1) = pty1 in
    let (_, ptymain2) = pty2 in
    match (ptymain1, ptymain2) with

    | (BaseType(bty1), BaseType(bty2)) ->
        bty1 = bty2

    | (FuncType(pdomain1, pty1cod), FuncType(pdomain2, pty2cod)) ->
        let bdom = aux_domain pdomain1 pdomain2 in
        bdom && aux pty1cod pty2cod

    | (EffType(pdomain1, peff1, ptysub1), EffType(pdomain2, peff2, ptysub2)) ->
        let bdom = aux_domain pdomain1 pdomain2 in
        bdom && aux_effect peff1 peff2 && aux ptysub1 ptysub2

    | (PidType(ppidty1), PidType(ppidty2)) ->
        aux_pid_type ppidty1 ppidty2

    | (ProductType(ptys1), ProductType(ptys2)) ->
        aux_list (ptys1 |> TupleList.to_list) (ptys2 |> TupleList.to_list)

    | (RecordType(prow1), RecordType(prow2)) ->
        poly_row_equal prow1 prow2

    | (PackType(absmodsig1), PackType(absmodsig2)) ->
        begin
          try
            subtype_abstract_with_abstract
              ~cause:(Range.dummy "poly_type_equal1")
              ~address:Alist.empty
              absmodsig1 absmodsig2;
            subtype_abstract_with_abstract
              ~cause:(Range.dummy "poly_type_equal2")
              ~address:Alist.empty
              absmodsig2 absmodsig1;
            true
          with
          | _ ->
              false
        end

    | (TypeApp(vid1, ptyargs1), TypeApp(vid2, ptyargs2)) ->
        TypeID.equal vid1 vid2 && aux_list ptyargs1 ptyargs2

    | (TypeVar(Bound(bid1)), TypeVar(Bound(bid2))) ->
        BoundID.equal bid1 bid2

    | (TypeVar(Mono(_)), _)
    | (_, TypeVar(Mono(_))) ->
        assert false

    | _ ->
        false

  and aux_list tys1 tys2 =
    try
      List.fold_left2 (fun b ty1 ty2 -> b && aux ty1 ty2) true tys1 tys2
    with
    | Invalid_argument(_) -> false

  and aux_domain pdomain1 pdomain2 =
    let {ordered = pty1doms; mandatory = pmndlabmap1; optional = poptrow1} = pdomain1 in
    let {ordered = pty2doms; mandatory = pmndlabmap2; optional = poptrow2} = pdomain2 in
    aux_list pty1doms pty2doms &&
      poly_label_assoc_equal pmndlabmap1 pmndlabmap2 &&
      poly_row_equal poptrow1 poptrow2

  and aux_effect (Effect(pty1)) (Effect(pty2)) =
    aux pty1 pty2

  and aux_pid_type (Pid(pty1)) (Pid(pty2)) =
    aux pty1 pty2

  in
  aux pty1 pty2


and poly_label_assoc_equal plabmap1 plabmap2 =
  let merged =
    LabelAssoc.merge (fun _ ptyopt1 ptyopt2 ->
      match (ptyopt1, ptyopt2) with
      | (None, None)             -> None
      | (None, Some(_))          -> Some(false)
      | (Some(_), None)          -> Some(false)
      | (Some(pty1), Some(pty2)) -> Some(poly_type_equal pty1 pty2)
    ) plabmap1 plabmap2
  in
  merged |> LabelAssoc.for_all (fun _ b -> b)


and subtype_base_kind (bkd1 : base_kind) (bkd2 : base_kind) =
  match (bkd1, bkd2) with
  | (TypeKind, TypeKind) ->
      true

  | (RowKind(labset1), RowKind(labset2)) ->
      failwith "TODO: subtype_base_kind, (RowKind, RowKind)"

  | _ ->
      false


and subtype_type_scheme (tyscheme1 : type_scheme) (tyscheme2 : type_scheme) : bool * BoundID.t BoundIDMap.t =
  let (bids1, pty_body1) = tyscheme1 in
  let (bids2, pty_body2) = tyscheme2 in
  match List.combine bids1 bids2 with
  | exception Invalid_argument(_) ->
      (false, BoundIDMap.empty)

  | zipped ->
      let bidmap =
        zipped |> List.fold_left (fun bidmap (bid1, bid2) ->
          bidmap |> BoundIDMap.add bid1 bid2
        ) BoundIDMap.empty
      in
      let internbid = internbidf bidmap in
      let internbrid = internbridf bidmap in
      let b = subtype_poly_type_impl internbid internbrid pty_body1 pty_body2 in
      (b, bidmap)


and lookup_type_entry (tynm : type_name) (tentry1 : type_entry) (tentry2 : type_entry) : substitution option =
  let Kind(pbkds1, _) = tentry1.type_kind in
  let Kind(pbkds2, _) = tentry2.type_kind in
  if List.length pbkds1 = List.length pbkds2 then
    let subst =
      match TypeConv.get_opaque_type tentry2.type_scheme with
      | None        -> SubstMap.empty
      | Some(tyid2) -> SubstMap.empty |> SubstMap.add tyid2 tentry1.type_scheme
    in
    Some(subst)
  else
    None


and lookup_record (rng : Range.t) (modsig1 : module_signature) (modsig2 : module_signature) : substitution =
  let take_left = (fun _tyid to1 _to2 -> Some(to1)) in
  match (modsig1, modsig2) with
  | (ConcStructure(sigr1), ConcStructure(sigr2)) ->
      sigr2 |> SigRecord.fold
          ~v:(fun _x2 _ventry2 subst ->
            subst
          )
          ~c:(fun ctornm2 _centry2 subst ->
            subst
          )
          ~f:(fun tynm2 _pty2 subst ->
            subst
          )
          ~t:(fun tynm2 tentry2 subst ->
            match sigr1 |> SigRecord.find_type tynm2 with
            | None ->
                raise_error (MissingRequiredTypeName(rng, tynm2, tentry2))

            | Some(tentry1) ->
                begin
                  match lookup_type_entry tynm2 tentry1 tentry2 with
                  | None ->
                      raise_error (NotASubtypeTypeDefinition(rng, tynm2, tentry1, tentry2))

                  | Some(subst0) ->
                      SubstMap.union take_left subst0 subst
                end
          )
          ~m:(fun modnm2 mentry2 subst ->
            let modsig2 = mentry2.mod_signature in
            match sigr1 |> SigRecord.find_module modnm2 with
            | None ->
                raise_error (MissingRequiredModuleName(rng, modnm2, modsig2))

            | Some(mentry1) ->
                let modsig1 = mentry1.mod_signature in
                let subst0 = lookup_record rng modsig1 modsig2 in
                SubstMap.union take_left subst0 subst
          )
          ~s:(fun _ _ subst ->
            subst
          )
          SubstMap.empty

  | _ ->
      SubstMap.empty


and subtype_abstract_with_abstract ~(cause : Range.t) ~(address : address) (absmodsig1 : module_signature abstracted) (absmodsig2 : module_signature abstracted) : unit =
  let (_, modsig1) = absmodsig1 in
  let _ = subtype_concrete_with_abstract ~cause ~address modsig1 absmodsig2 in
  ()


(* `subtype_concrete_with_concrete address rng modsig1 modsig2` asserts that `modsig1 <= modsig2` holds. *)
and subtype_concrete_with_concrete ~(cause : Range.t) ~(address : address) (modsig1 : module_signature) (modsig2 : module_signature) : unit =
  match (modsig1, modsig2) with
  | (ConcFunctor(sigftor1), ConcFunctor(sigftor2)) ->
      let (quant1, Domain(sigr1), absmodsigcod1) = (sigftor1.opaques, sigftor1.domain, sigftor1.codomain) in
      let (quant2, Domain(sigr2), absmodsigcod2) = (sigftor2.opaques, sigftor2.domain, sigftor2.codomain) in
      let subst =
        let modsigdom1 = ConcStructure(sigr1) in
        let modsigdom2 = ConcStructure(sigr2) in
        subtype_concrete_with_abstract ~cause ~address modsigdom2 (quant1, modsigdom1)
      in
      let absmodsigcod1 = absmodsigcod1 |> substitute_abstract ~cause ~address subst in
      subtype_abstract_with_abstract ~cause ~address absmodsigcod1 absmodsigcod2

  | (ConcStructure(sigr1), ConcStructure(sigr2)) ->
      sigr2 |> SigRecord.fold
          ~v:(fun x2 ventry2 () ->
            let pty2 = ventry2.val_type in
            match sigr1 |> SigRecord.find_value x2 with
            | None ->
                raise_error (MissingRequiredValName(cause, x2, pty2))

            | Some(ventry1) ->
                let pty1 = ventry1.val_type in
               if subtype_poly_type pty1 pty2 then
                 ()
               else
                 raise_error (PolymorphicContradiction(cause, x2, pty1, pty2))
          )
          ~c:(fun ctornm2 centry2 () ->
            match sigr1 |> SigRecord.find_constructor ctornm2 with
            | None ->
                raise_error (MissingRequiredConstructorName(cause, ctornm2, centry2))

            | Some(centry1) ->
                let tyscheme1 = make_type_scheme_from_constructor_entry centry1 in
                let tyscheme2 = make_type_scheme_from_constructor_entry centry2 in
                let (b, _) = subtype_type_scheme tyscheme1 tyscheme2 in
                if b then
                  ()
                else
                  raise_error (NotASubtypeConstructorDefinition(cause, ctornm2, centry1, centry2))
          )
          ~f:(fun tynm2 pty2 () ->
            match sigr1 |> SigRecord.find_dummy_fold tynm2 with
            | None ->
                begin
                  match sigr2 |> SigRecord.find_type tynm2 with
                  | None          -> assert false
                  | Some(tentry2) -> raise_error (MissingRequiredTypeName(cause, tynm2, tentry2))
                end

            | Some(pty1) ->
                if subtype_poly_type pty1 pty2 then
                  ()
                else
                  begin
                    match (sigr1 |> SigRecord.find_type tynm2, sigr2 |> SigRecord.find_type tynm2) with
                    | (Some(tentry1), Some(tentry2)) ->
                        raise_error (NotASubtypeTypeDefinition(cause, tynm2, tentry1, tentry2))

                    | _ ->
                        assert false
                  end
          )
          ~t:(fun tynm2 tentry2 () ->
            match sigr1 |> SigRecord.find_type tynm2 with
            | None ->
                raise_error (MissingRequiredTypeName(cause, tynm2, tentry2))

            | Some(tentry1) ->
                let tyscheme1 = tentry1.type_scheme in
                let tyscheme2 = tentry2.type_scheme in
                let (b1, bidmap1) = subtype_type_scheme tyscheme1 tyscheme2 in
                let (b2, _) = subtype_type_scheme tyscheme2 tyscheme1 in
                let Kind(_, bkdcod1) = tentry1.type_kind in
                let Kind(_, bkdcod2) = tentry2.type_kind in
                let b0 = subtype_base_kind bkdcod1 bkdcod2 in
                if b1 && b2 && b0 then
                  ()
                else
                  raise_error (NotASubtypeTypeDefinition(cause, tynm2, tentry1, tentry2))
          )
          ~m:(fun modnm2 mentry2 () ->
            let modsig2 = mentry2.mod_signature in
            match sigr1 |> SigRecord.find_module modnm2 with
            | None ->
                raise_error (MissingRequiredModuleName(cause, modnm2, modsig2))

            | Some(mentry1) ->
                let modsig1 = mentry1.mod_signature in
                subtype_concrete_with_concrete ~cause ~address modsig1 modsig2
          )
          ~s:(fun signm2 absmodsig2 () ->
            match sigr1 |> SigRecord.find_signature signm2 with
            | None ->
                raise_error (MissingRequiredSignatureName(cause, signm2, absmodsig2))

            | Some(absmodsig1) ->
                subtype_abstract_with_abstract ~cause ~address absmodsig1 absmodsig2;
                subtype_abstract_with_abstract ~cause ~address absmodsig2 absmodsig1;
                ()
          )
          ()

  | _ ->
      raise_error (NotASubtype(cause, modsig1, modsig2))


and subtype_concrete_with_abstract ~(cause : Range.t) ~(address : address) (modsig1 : module_signature) (absmodsig2 : module_signature abstracted) : substitution =
  let (quant2, modsig2) = absmodsig2 in
  let subst = lookup_record cause modsig1 modsig2 in
  let modsig2 = modsig2 |> substitute_concrete ~cause ~address subst in
  subtype_concrete_with_concrete ~cause ~address modsig1 modsig2;
  subst


and subtype_signature ~(cause : Range.t) ~(address : address) (modsig1 : module_signature) (absmodsig2 : module_signature abstracted) =
  subtype_concrete_with_abstract ~cause ~address modsig1 absmodsig2


and substitute_concrete ~(cause : Range.t) ~(address : address) (subst : substitution) (modsig : module_signature) =
  match modsig with
  | ConcFunctor(sigftor) ->
      let (quant, Domain(sigr), absmodsigcod) = (sigftor.opaques, sigftor.domain, sigftor.codomain) in
      let sigr = sigr |> substitute_structure ~cause ~address subst in
      let absmodsigcod = absmodsigcod |> substitute_abstract ~cause ~address subst in
      let sigftor =
        { sigftor with
          opaques  = quant;
          domain   = Domain(sigr);
          codomain = absmodsigcod;
        }
      in
      ConcFunctor(sigftor)
        (* Strictly speaking, we should assert that `quant` and the domain of `subst` be disjoint. *)

  | ConcStructure(sigr) ->
      let sigr = sigr |> substitute_structure ~cause ~address subst in
      ConcStructure(sigr)


(* Given `modsig1` and `modsig2` which are already known to satisfy `modsig1 <= modsig2`,
   `copy_closure` copies every closure and every global name occurred in `modsig1`
   into the corresponding occurrence in `modsig2`. *)
and copy_closure (modsig1 : module_signature) (modsig2 : module_signature) : module_signature =
  match (modsig1, modsig2) with
  | (ConcStructure(sigr1), ConcStructure(sigr2)) ->
      let sigr2new = copy_closure_in_structure sigr1 sigr2 in
      ConcStructure(sigr2new)

  | (ConcFunctor(sigftor1), ConcFunctor(sigftor2)) ->
      let Domain(sigrdom1) = sigftor1.domain in
      let Domain(sigrdom2) = sigftor2.domain in
      let sigrdom2new = copy_closure_in_structure sigrdom1 sigrdom2 in
      let (_, modsig1) = sigftor1.codomain in
      let (quant2, modsig2) = sigftor2.codomain in
      let modsig2new = copy_closure modsig1 modsig2 in
      ConcFunctor({ sigftor2 with
        domain   = Domain(sigrdom2new);
        codomain = (quant2, modsig2new);
        closure  = sigftor1.closure;
      })

  | _ ->
      assert false


and copy_closure_in_structure (sigr1 : SigRecord.t) (sigr2 : SigRecord.t) : SigRecord.t =
  sigr2 |> SigRecord.map
    ~v:(fun x ventry2 ->
      match sigr1 |> SigRecord.find_value x with
      | None              -> assert false
      | Some(ventry1)     -> { ventry2 with val_global = ventry1.val_global }
    )
    ~c:(fun _ctornm centry2 -> centry2)
    ~f:(fun _tynm pty2 -> pty2)
    ~t:(fun _tynm tentry2 -> tentry2)
    ~m:(fun modnm mentry2 ->
      match sigr1 |> SigRecord.find_module modnm with
      | None ->
          assert false

      | Some(mentry1) ->
          let modsig2 = copy_closure mentry1.mod_signature mentry2.mod_signature in
          { mod_signature = modsig2; mod_name = mentry1.mod_name }
    )
    ~s:(fun _signm sentry -> sentry)


and substitute_type_id (subst : substitution) (tyid_from : TypeID.t) : TypeID.t =
  match subst |> SubstMap.find_opt tyid_from with
  | None ->
      tyid_from

  | Some(tyscheme) ->
      begin
        match TypeConv.get_opaque_type tyscheme with
        | None ->
            assert false

        | Some(tyid_to) ->
            tyid_to
      end


and update_subsignature (modnms : module_name list) (updater : module_signature -> module_signature) (modsig : module_signature) : module_signature =
  match modnms with
  | [] ->
      updater modsig

  | modnm0 :: modnms ->
      begin
        match modsig with
        | ConcFunctor(_) ->
            modsig

        | ConcStructure(sigr) ->
            begin
              let sigr =
                sigr |> SigRecord.map
                  ~v:(fun _x ventry -> ventry)
                  ~c:(fun _ctornm centry -> centry)
                  ~f:(fun _tynm pty -> pty)
                  ~t:(fun _tynm tentry -> tentry)
                  ~m:(fun modnm mentry ->
                    if String.equal modnm modnm0 then
                      let modsig = mentry.mod_signature |> update_subsignature modnms updater in
                      { mentry with mod_signature = modsig }
                    else
                      mentry
                  )
                  ~s:(fun _signm absmodsig -> absmodsig)
              in
              ConcStructure(sigr)
            end
      end


and substitute_structure ~(cause : Range.t) ~(address : address) (subst : substitution) (sigr : SigRecord.t) : SigRecord.t =
  sigr |> SigRecord.map
      ~v:(fun _x ventry ->
        { ventry with val_type = ventry.val_type |> substitute_poly_type ~cause subst }
      )
      ~c:(fun _ctornm centry ->
        { centry with
          belongs         = centry.belongs |> substitute_type_id subst;
          parameter_types = centry.parameter_types |> List.map (substitute_poly_type ~cause subst);
        }
      )
      ~f:(fun _tynm pty ->
        pty |> substitute_poly_type ~cause subst
      )
      ~t:(fun _tynm tentry ->
        let (bids, pty_body) = tentry.type_scheme in
        {
          type_scheme = (bids, pty_body |> substitute_poly_type ~cause subst);
          type_kind   = tentry.type_kind;
        }
      )
      ~m:(fun _ mentry ->
        { mentry with mod_signature = mentry.mod_signature |> substitute_concrete ~cause ~address subst }
      )
      ~s:(fun _ absmodsig ->
        absmodsig |> substitute_abstract ~cause ~address subst
      )


and substitute_abstract ~(cause : Range.t) ~(address : address) (subst : substitution) (absmodsig : module_signature abstracted) : module_signature abstracted =
  let (quant, modsig) = absmodsig in
  let modsig = substitute_concrete ~cause ~address subst modsig in
  (quant, modsig)
    (* Strictly speaking, we should assert that `quant` and the domain of `subst` be disjoint. *)


(* Applies the subtitution `subst` to `pty`. Here, `MonoRow` are not supposed to occur in `pty`. *)
and substitute_poly_type ~(cause : Range.t) (subst : substitution) (pty : poly_type) : poly_type =
  let rec aux (rng, ptymain) =
    let ptymain =
      match ptymain with
      | BaseType(_)               -> ptymain
      | PidType(ppid)             -> PidType(aux_pid ppid)
      | TypeVar(_)                -> ptymain
      | ProductType(ptys)         -> ProductType(ptys |> TupleList.map aux)

      | EffType(pdomain, peff, ptysub) ->
          EffType(aux_domain pdomain, aux_effect peff, aux ptysub)

      | FuncType(pdomain, ptycod) ->
          FuncType(aux_domain pdomain, aux ptycod)

      | RecordType(prow) ->
          RecordType(aux_row prow)

      | TypeApp(tyid_from, ptyargs) ->
          begin
            match subst |> SubstMap.find_opt tyid_from with
            | None ->
                TypeApp(tyid_from, ptyargs |> List.map aux)

            | Some(tyscheme) ->
                begin
                  match TypeConv.apply_type_scheme_poly tyscheme (ptyargs |> List.map aux) with
                  | Error(None) ->
                    (* Arity mismatch; this cannot happen. *)
                      assert false

                  | Error(Some((ptyarg, pbkd))) ->
                      failwith (Format.asprintf "TODO (error): error report (base kind mispatch, %a)" Range.pp cause)

                  | Ok((_, ptymain)) ->
                      ptymain
                end

          end

      | PackType(absmodsig) ->
          let absmodsig = substitute_abstract ~cause ~address:Alist.empty subst absmodsig in
          PackType(absmodsig)
    in
    (rng, ptymain)

  and aux_domain pdomain =
    let {ordered = ptydoms; mandatory = pmndlabmap; optional = poptrow} = pdomain in
    {
      ordered   = ptydoms |> List.map aux;
      mandatory = pmndlabmap |> LabelAssoc.map aux;
      optional  = aux_row poptrow;
    }

  and aux_pid = function
    | Pid(pty) -> Pid(aux pty)

  and aux_effect = function
    | Effect(pty) -> Effect(aux pty)

  and aux_row (prow : poly_row) =
    match prow with
    | RowCons(rlabel, ty, prow) -> RowCons(rlabel, aux ty, aux_row prow)
    | RowVar(_)                 -> prow (* Assumes that `MonoRow` does not occur in rows. *)
    | RowEmpty                  -> RowEmpty
  in
  aux pty


and typecheck_declaration ~(address : address) (tyenv : Typeenv.t) (utdecl : untyped_declaration) : SigRecord.t abstracted =
  let (_, utdeclmain) = utdecl in
  match utdeclmain with
  | DeclVal((_, x), typarams, rowparams, mty) ->
      let pre =
        let pre_init =
          {
            level                 = 0;
            tyenv                 = tyenv;
            local_type_parameters = TypeParameterMap.empty;
            local_row_parameters  = RowParameterMap.empty;
          }
        in
        let (pre, _) = make_type_parameter_assoc pre_init typarams in
        { pre with level = 1 } |> add_local_row_parameter rowparams
      in
      let ty = decode_manual_type pre mty in
      let pty = TypeConv.generalize 0 ty in
      let gname = OutputIdentifier.fresh_global_dummy () in
      let ventry = { val_type = pty; val_global = gname } in
      let sigr = SigRecord.empty |> SigRecord.add_value x ventry in
      (OpaqueIDMap.empty, sigr)

  | DeclTypeOpaque(tyident, kdannot) ->
      let (_, tynm) = tyident in
      let pre_init =
        {
          level                 = 0;
          tyenv                 = tyenv;
          local_type_parameters = TypeParameterMap.empty;
          local_row_parameters  = RowParameterMap.empty;
        }
      in
      let kd =
        match kdannot with
        | None       -> Kind([], TypeKind)
        | Some(mnkd) -> decode_manual_kind pre_init mnkd
      in
      let oid = TypeID.fresh (Alist.to_list address) tynm in
      let Kind(bkds, _) = kd in
      let tentry =
        {
          type_scheme = TypeConv.make_opaque_type_scheme_from_base_kinds bkds oid;
          type_kind   = kd;
        }
      in
      let sigr = SigRecord.empty |> SigRecord.add_type tynm tentry in
      (OpaqueIDMap.singleton oid kd, sigr)

  | DeclModule(modident, utsig) ->
      let (rngm, m) = modident in
      let absmodsig = typecheck_signature ~address tyenv utsig in
      let (quant, modsig) = absmodsig in
      let sname = get_space_name rngm m in
      let mentry = { mod_signature = modsig; mod_name = sname } in
      let sigr = SigRecord.empty |> SigRecord.add_module m mentry in
      (quant, sigr)

  | DeclSig(sigident, utsig) ->
      let (_, signm) = sigident in
      let absmodsig = typecheck_signature ~address tyenv utsig in
      let sigr = SigRecord.empty |> SigRecord.add_signature signm absmodsig in
      (OpaqueIDMap.empty, sigr)

  | DeclInclude(utsig) ->
      let absmodsig = typecheck_signature ~address tyenv utsig in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utsig in
            raise_error (NotAStructureSignature(rng, modsig))

        | ConcStructure(sigr) ->
            (quant, sigr)
      end


and typecheck_declaration_list ~(address : address) (tyenv : Typeenv.t) (utdecls : untyped_declaration list) : SigRecord.t abstracted =
  let (quantacc, sigracc, _) =
    utdecls |> List.fold_left (fun (quantacc, sigracc, tyenv) ((rng, _) as utdecl) ->
      let (quant, sigr) = typecheck_declaration ~address tyenv utdecl in
      let quantacc = merge_quantifier quantacc quant in
      let sigracc =
        match SigRecord.disjoint_union sigracc sigr with
        | Ok(sigr) -> sigr
        | Error(s) -> raise_error (ConflictInSignature(rng, s))
      in
      let tyenv = tyenv |> update_type_environment_by_signature_record sigr in
      (quantacc, sigracc, tyenv)
    ) (OpaqueIDMap.empty, SigRecord.empty, tyenv)
  in
  (quantacc, sigracc)

and copy_abstract_signature ~(cause : Range.t) ~(address : address) (absmodsig_from : module_signature abstracted) : module_signature abstracted =
  let (quant_from, modsig_from) = absmodsig_from in
  let (quant_to, subst) =
    OpaqueIDMap.fold (fun oid_from pkd (quant_to, subst) ->
      let oid_to =
        let s = TypeID.name oid_from in
        TypeID.fresh (Alist.to_list address) s
      in
      let quant_to = quant_to |> OpaqueIDMap.add oid_to pkd in
      let Kind(pbkds, _) = pkd in
      let tyscheme = TypeConv.make_opaque_type_scheme_from_base_kinds pbkds oid_to in
      let subst = subst |> SubstMap.add oid_from tyscheme in
      (quant_to, subst)
    ) quant_from (OpaqueIDMap.empty, SubstMap.empty)
  in
  let modsig_to = modsig_from |> substitute_concrete ~cause ~address subst in
  (quant_to, modsig_to)


and typecheck_signature ~(address : address) (tyenv : Typeenv.t) (utsig : untyped_signature) : module_signature abstracted =
  let (rng, utsigmain) = utsig in
  match utsigmain with
  | SigVar(signm) ->
      begin
        match tyenv |> Typeenv.find_signature signm with
        | None ->
            raise_error (UnboundSignatureName(rng, signm))

        | Some(absmodsig_from) ->
            copy_abstract_signature ~cause:rng ~address absmodsig_from
              (* We need to rename opaque IDs here, since otherwise
                 we would mistakenly make the following program pass:

                 ```
                 signature S = sig
                   type t:: 0
                 end

                 module F = fun(X: S) -> fun(Y: S) -> struct
                   type f(x: X.t): Y.t = x
                 end
                 ```

                 This issue was reported by `@elpinal`:
                 https://twitter.com/elpin1al/status/1269198048967589889?s=20
              *)
      end

  | SigPath(utmod1, sigident2) ->
      let (absmodsig1, _) = typecheck_module Alist.empty tyenv utmod1 in
      let (quant1, modsig1) = absmodsig1 in
      begin
        match modsig1 with
        | ConcFunctor(_) ->
            let (rng1, _) = utmod1 in
            raise_error (NotOfStructureType(rng1, modsig1))

        | ConcStructure(sigr1) ->
            let (rng2, signm2) = sigident2 in
            begin
              match sigr1 |> SigRecord.find_signature signm2 with
              | None ->
                  raise_error (UnboundSignatureName(rng2, signm2))

              | Some((_, modsig2) as absmodsig2) ->
                  if opaque_occurs quant1 modsig2 then
                    raise_error (OpaqueIDExtrudesScopeViaSignature(rng, absmodsig2))
                  else
                    absmodsig2
                    (* Combining typing rules (P-Mod) and (S-Path)
                       in the original paper "F-ing modules" [Rossberg, Russo & Dreyer 2014],
                       we can ignore `quant1` here.
                       However, we CANNOT SIMPLY ignore `quant1`;
                       according to the second premise “Γ ⊢ Σ : Ω” of (P-Mod),
                       we must assert `absmodsig2` do not contain every type variable in `quant1`.
                       (we have again realized this thanks to `@elpinal`.)
                       https://twitter.com/elpin1al/status/1272110415435010048?s=20
                     *)
            end
      end

  | SigDecls(openspecs, utdecls) ->
      let tyenv = tyenv |> add_open_specs_to_type_environment openspecs in
      let (quant, sigr) = typecheck_declaration_list ~address tyenv utdecls in
      (quant, ConcStructure(sigr))

  | SigFunctor(modident, utsigdom, utsigcod) ->
      let (quant, sigdom) = typecheck_signature ~address:Alist.empty tyenv utsigdom in
      let abssigcod =
        let (rngm, m) = modident in
        let sname = get_space_name rngm m in
        let mentry = { mod_signature = sigdom; mod_name = sname } in
        let tyenv = tyenv |> Typeenv.add_module m mentry in
        typecheck_signature ~address:Alist.empty tyenv utsigcod
      in
      begin
        match sigdom with
        | ConcStructure(sigr) ->
            let sigftor =
              {
                opaques  = quant;
                domain   = Domain(sigr);
                codomain = abssigcod;
                closure  = None;
              }
            in
            (OpaqueIDMap.empty, ConcFunctor(sigftor))

        | _ ->
            raise_error (SupportOnlyFirstOrderFunctor(rng))
      end

  | SigWith(utsig0, modidents, tybinds) ->
      let (rng0, _) = utsig0 in
      let absmodsig0 = typecheck_signature ~address tyenv utsig0 in
      let (quant0, modsig0) = absmodsig0 in
      let sigr_last =
        let (rng_last, modsig_last) =
          modidents |> List.fold_left (fun (rngpre, modsig) (rng, modnm) ->
            match modsig with
            | ConcFunctor(_) ->
                raise_error (NotAStructureSignature(rngpre, modsig))

            | ConcStructure(sigr) ->
                begin
                  match sigr |> SigRecord.find_module modnm with
                  | None         -> raise_error (UnboundModuleName(rng, modnm))
                  | Some(mentry) -> (rng, mentry.mod_signature)
                end
          ) (rng0, modsig0)
        in
        match modsig_last with
        | ConcFunctor(_)           -> raise_error (NotAStructureSignature(rng_last, modsig_last))
        | ConcStructure(sigr_last) -> sigr_last
      in
      let (tydefs, ctordefs) = bind_types ~address tyenv tybinds in
      let (subst, quant) =
        tydefs |> List.fold_left (fun (subst, quant) (tynm1, tentry1) ->
          let (tyid0, pkd_expected) =
            match sigr_last |> SigRecord.find_type tynm1 with
            | None ->
                raise_error (UndefinedTypeName(rng, tynm1))

            | Some(tentry0) ->
                begin
                  match TypeConv.get_opaque_type tentry0.type_scheme with
                  | Some(tyid0) ->
                      assert (quant0 |> OpaqueIDMap.mem tyid0);
                      (tyid0, tentry0.type_kind)

                  | None ->
                      raise_error (CannotRestrictTransparentType(rng, tynm1, tentry1))
                end
          in
          let pkd_actual = tentry1.type_kind in
          unify_kind rng tynm1 ~actual:pkd_actual ~expected:pkd_expected;
          let subst = subst |> SubstMap.add tyid0 tentry1.type_scheme in
          let quant = quant |> OpaqueIDMap.remove tyid0 in
          (subst, quant)
        ) (SubstMap.empty, quant0)
      in
      let modsig_ret = modsig0 |> substitute_concrete ~cause:rng ~address subst in
      let modsig_ret =
        modsig_ret |> update_subsignature (modidents |> List.map snd) (fun modsig_last ->
          match modsig_last with
          | ConcFunctor(_) ->
              assert false

          | ConcStructure(sigr_last) ->
              let sigr_last = sigr_last |> add_constructor_definitions ctordefs in
              ConcStructure(sigr_last)
        )
      in
      (quant, modsig_ret)


(* Checks that `kd1` and `kd2` are the same. *)
and unify_kind (rng : Range.t) (tynm : type_name) ~actual:(kd1 : kind) ~expected:(kd2 : kind) : unit =
  let Kind(bkdsdom1, bkdcod1) = kd1 in
  let Kind(bkdsdom2, bkdcod2) = kd2 in
  match List.combine bkdsdom1 bkdsdom2 with
  | exception Invalid_argument(_) ->
      let arity_actual = List.length bkdsdom1 in
      let arity_expected = List.length bkdsdom2 in
      raise_error (InvalidNumberOfTypeArguments(rng, tynm, arity_expected, arity_actual))

  | bkddomzips ->
      let bdom = bkddomzips |> List.for_all (fun (bkd1, bkd2) -> base_kind_equal bkd1 bkd2) in
      if bdom && base_kind_equal bkdcod1 bkdcod2 then
        ()
      else
        raise_error (KindContradiction(rng, tynm, kd1, kd2))


and base_kind_equal (bkd1 : base_kind) (bkd2 : base_kind) : bool =
  match (bkd1, bkd2) with
  | (TypeKind, TypeKind)                 -> true
  | (RowKind(labset1), RowKind(labset2)) -> LabelSet.equal labset1 labset2
  | _                                    -> false


and typecheck_binding ~(address : address) (tyenv : Typeenv.t) (utbind : untyped_binding) : SigRecord.t abstracted * (ModuleAttribute.t * binding list) =
  let (_, utbindmain) = utbind in
  match utbindmain with
  | BindVal(attrs, External(extbind)) ->
      let (valattr, warnings) = ValueAttribute.decode attrs in
      warnings |> List.iter Logging.warn_invalid_attribute;
      let mty = extbind.ext_type_annot in
      let (rngv, x) = extbind.ext_identifier in
      let arity = extbind.ext_arity in
      let pty =
        let pre =
          let pre_init =
            {
              level                 = 0;
              tyenv                 = tyenv;
              local_type_parameters = TypeParameterMap.empty;
              local_row_parameters  = RowParameterMap.empty;
            }
          in
          let (pre, _) = make_type_parameter_assoc pre_init extbind.ext_type_params in
          { pre with level = 1 } |> add_local_row_parameter extbind.ext_row_params
        in
        let ty = decode_manual_type pre mty in
        TypeConv.generalize 0 ty
      in
      let has_option = extbind.ext_has_option in
      let gname =
        let is_test_suite = valattr.is_test_suite in
        generate_global_name ~is_test_suite ~arity:arity ~has_option:has_option rngv x
      in
      let sigr = SigRecord.empty |> SigRecord.add_value x { val_type = pty; val_global = gname } in
      let ibinds = [ IBindVal(IExternal(gname, extbind.ext_code)) ] in
      ((OpaqueIDMap.empty, sigr), (ModuleAttribute.empty, ibinds))

  | BindVal(attrs, Internal(rec_or_nonrec)) ->
      let (valattr, warnings) = ValueAttribute.decode attrs in
      warnings |> List.iter Logging.warn_invalid_attribute;
      let is_test_suite = valattr.is_test_suite in
      let pre_init =
        {
          level                 = 0;
          tyenv                 = tyenv;
          local_type_parameters = TypeParameterMap.empty;
          local_row_parameters  = RowParameterMap.empty;
        }
      in
      let (sigr, i_rec_or_nonrec) =
        match rec_or_nonrec with
        | Rec([]) ->
            assert false

        | Rec(valbinds) ->
            let proj gname = OutputIdentifier.Global(gname) in
            let recbinds = typecheck_letrec_mutual (global_name_scheme is_test_suite) proj pre_init valbinds in
            let (sigr, irecbindacc) =
              recbinds |> List.fold_left (fun (sigr, irecbindacc) (x, pty, gname_outer, _, e) ->
                let sigr = sigr |> SigRecord.add_value x { val_type = pty; val_global = gname_outer } in
                let irecbindacc = Alist.extend irecbindacc (x, gname_outer, pty, e) in
                (sigr, irecbindacc)
              ) (SigRecord.empty, Alist.empty)
            in
            (sigr, IRec(Alist.to_list irecbindacc))

        | NonRec(valbind) ->
            let (pty, gname, e) =
              let arity = List.length valbind.vb_parameters + List.length valbind.vb_mandatories in
              let has_option = (List.length valbind.vb_optionals > 0) in
              let gnamef = generate_global_name ~is_test_suite ~arity:arity ~has_option:has_option in
              typecheck_let gnamef pre_init valbind
            in
            let (_, x) = valbind.vb_identifier in
            let sigr = SigRecord.empty |> SigRecord.add_value x { val_type = pty; val_global = gname } in
            (sigr, INonRec(x, gname, pty, e))
      in
      let ibinds = [ IBindVal(i_rec_or_nonrec) ] in
      ((OpaqueIDMap.empty, sigr), (ModuleAttribute.empty, ibinds))

  | BindType([]) ->
      assert false

  | BindType((_ :: _) as tybinds) ->
      let (tydefs, ctordefs) = bind_types ~address tyenv tybinds in
      let sigr =
        tydefs |> List.fold_left (fun sigr (tynm, tentry) ->
          sigr |> SigRecord.add_type tynm tentry
        ) SigRecord.empty
      in
      let sigr = sigr |> add_constructor_definitions ctordefs in
      ((OpaqueIDMap.empty, sigr), (ModuleAttribute.empty, []))

  | BindModule(modident, utsigopt2, utmod1) ->
      let (rngm, m) = modident in
      let (absmodsig1, (modattrsub, ibindssub)) = typecheck_module ~address:(Alist.extend address m) tyenv utmod1 in
      let (quant, modsig) =
        match utsigopt2 with
        | None ->
            absmodsig1

        | Some(utsig2) ->
            let (_, modsig1) = absmodsig1 in
            let absmodsig2 = typecheck_signature ~address:(Alist.extend address m) tyenv utsig2 in
            coerce_signature ~cause:rngm ~address modsig1 absmodsig2
      in
      let sname = get_space_name rngm m in
      let sigr = SigRecord.empty |> SigRecord.add_module m { mod_signature = modsig; mod_name = sname } in
      let ibinds =
        match ibindssub with
        | []     -> []
        | _ :: _ -> [IBindModule(sname, modattrsub, ibindssub)]
      in
      ((quant, sigr), (ModuleAttribute.empty, ibinds))

  | BindInclude(utmod) ->
      let (absmodsig, (attrs, ibinds)) = typecheck_module ~address tyenv utmod in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utmod in
            raise_error (NotOfStructureType(rng, modsig))

        | ConcStructure(sigr) ->
            ((quant, sigr), (attrs, ibinds))
      end

  | BindSig(sigident, sigbind) ->
      let (_, signm) = sigident in
      let absmodsig = typecheck_signature ~address tyenv sigbind in
      let sigr = SigRecord.empty |> SigRecord.add_signature signm absmodsig in
      ((OpaqueIDMap.empty, sigr), (ModuleAttribute.empty, []))


and bind_types ~(address : address) (tyenv : Typeenv.t) (tybinds : type_binding list) : (type_name * type_entry) list * variant_definition list =
  let pre_init =
    {
      level                 = 0;
      tyenv                 = tyenv;
      local_type_parameters = TypeParameterMap.empty;
      local_row_parameters  = RowParameterMap.empty;
    }
  in

  (* Add the arity of each variant type to the type environment,
     Construct the graph for checking dependency among synonym types. *)
  let (synacc, vntacc, vertices, graph, tyenv) =
    tybinds |> List.fold_left (fun (synacc, vntacc, vertices, graph, tyenv) (tyident, tyvars, syn_or_vnt) ->
      let (rng, tynm) = tyident in
      let kd =
        let bkddoms =
          tyvars |> List.map (fun (_, kdannot) ->
            match kdannot with
            | None        -> TypeKind
            | Some(mnbkd) -> decode_manual_base_kind pre_init mnbkd
          )
        in
        Kind(bkddoms, TypeKind)
      in
      match syn_or_vnt with
      | BindSynonym(synbind) ->
          let syndata =
            DependencyGraph.{
              position        = rng;
              type_variables  = tyvars;
              definition_body = synbind;
              kind            = kd;
            }
          in
          let graph = graph |> DependencyGraph.add_vertex tynm syndata in
          let synacc = Alist.extend synacc (tyident, synbind) in
          let vertices = vertices |> SynonymNameSet.add tynm in
          (synacc, vntacc, vertices, graph, tyenv)

      | BindVariant(vntbind) ->
          let Kind(bkds, _) = kd in
          let tyid = TypeID.fresh (Alist.to_list address) tynm in
          let tentry =
            {
              type_scheme = TypeConv.make_opaque_type_scheme_from_base_kinds bkds tyid;
              type_kind   = kd;
            }
          in
          let tyenv = tyenv |> Typeenv.add_type tynm tentry in
          let vntacc = Alist.extend vntacc (tyident, tyvars, vntbind, tyid, kd, tentry) in
          (synacc, vntacc, vertices, graph, tyenv)
    ) (Alist.empty, Alist.empty, SynonymNameSet.empty, DependencyGraph.empty, tyenv)
  in
  let pre = { pre_init with tyenv = tyenv } in

  (* Traverse the definition of each synonym type
     in order to add to the graph the edges that stand for dependencies between synonym types. *)
  let graph =
    synacc |> Alist.to_list |> List.fold_left (fun graph syn ->
      let (tyident, mtyreal) = syn in
      let (_, tynm) = tyident in
      let dependencies = get_dependency_on_synonym_types vertices pre mtyreal in
      graph |> SynonymNameSet.fold (fun tynm_dep graph ->
        graph |> DependencyGraph.add_edge ~depended:tynm_dep ~depending:tynm
      ) dependencies
    ) graph
  in

  (* Check that no cyclic dependency exists among synonym types
     and make the signature to be returned from the type definitions. *)
  let syns =
    match DependencyGraph.topological_sort graph with
    | Error(cycle) -> raise_error (CyclicSynonymTypeDefinition(cycle))
    | Ok(syns)     -> syns
  in

  (* Add the definition of the synonym types to the type environment. *)
  let (tyenv, tydefacc) =
    syns |> List.fold_left (fun (tyenv, tydefacc) syn ->
      let pre = { pre with tyenv = tyenv } in
      let (tynm, syndata) = syn in
      let
        DependencyGraph.{
          type_variables  = tyvars;
          definition_body = mtyreal;
          kind            = pkd;
          _
        } = syndata
      in
      let (pre, typaramassoc) = make_type_parameter_assoc pre tyvars in
      let bids = typaramassoc |> TypeParameterAssoc.values |> List.map MustBeBoundID.to_bound in
      let ty_body = decode_manual_type pre mtyreal in
      let pty_body = TypeConv.generalize 0 ty_body in
      let tentry = { type_scheme = (bids, pty_body); type_kind = pkd } in
      let tyenv = tyenv |> Typeenv.add_type tynm tentry in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      (tyenv, tydefacc)
    ) (tyenv, Alist.empty)
  in
  let pre = { pre with tyenv } in

  (* Traverse the definition of each variant type. *)
  let (tydefacc, ctordefacc) =
    vntacc |> Alist.to_list |> List.fold_left (fun (tydefacc, ctordefacc) vnt ->
      let (tyident, tyvars, ctorbrs, tyid, pkd, tentry) = vnt in
      let (_, tynm) = tyident in
      let (pre, typaramassoc) = make_type_parameter_assoc pre tyvars in
      let typarams = typaramassoc |> TypeParameterAssoc.values |> List.map MustBeBoundID.to_bound in
      let ctorbrmap = make_constructor_branch_map pre ctorbrs in
      let tydefacc = Alist.extend tydefacc (tynm, tentry) in
      let ctordefacc = Alist.extend ctordefacc (tynm, tyid, typarams, ctorbrmap) in
      (tydefacc, ctordefacc)
    ) (tydefacc, Alist.empty)
  in
  (Alist.to_list tydefacc, Alist.to_list ctordefacc)


and typecheck_module ~(address : address) (tyenv : Typeenv.t) (utmod : untyped_module) : module_signature abstracted * (ModuleAttribute.t * binding list) =
  let (rng, utmodmain) = utmod in
  match utmodmain with
  | ModVar(m) ->
      let mentry = find_module tyenv (rng, m) in
      let modsig = mentry.mod_signature in
      let absmodsig = (OpaqueIDMap.empty, modsig) in
      (absmodsig, (ModuleAttribute.empty, []))

  | ModBinds(attrs, openspecs, utbinds) ->
      let (modattr, warnings) = ModuleAttribute.decode attrs in
      warnings |> List.iter Logging.warn_invalid_attribute;
      let tyenv = tyenv |> add_open_specs_to_type_environment openspecs in
      let (abssigr, (modattr_included, ibinds)) = typecheck_binding_list ~address tyenv utbinds in
      let (quant, sigr) = abssigr in
      let absmodsig = (quant, ConcStructure(sigr)) in
      (absmodsig, (ModuleAttribute.merge modattr modattr_included, ibinds))

  | ModProjMod(utmod, modident) ->
      let (absmodsig, imod) = typecheck_module ~address tyenv utmod in
      let (quant, modsig) = absmodsig in
      begin
        match modsig with
        | ConcFunctor(_) ->
            let (rng, _) = utmod in
            raise_error (NotOfStructureType(rng, modsig))

        | ConcStructure(sigr) ->
            let (rng, m) = modident in
            begin
              match sigr |> SigRecord.find_module m with
              | None ->
                  raise_error (UnboundModuleName(rng, m))

              | Some(mentry) ->
                  let absmodsigp = (quant, mentry.mod_signature) in
                  (absmodsigp, imod)
            end
      end

  | ModFunctor(modident, utsigdom, utmod0) ->
      let (rngm, m) = modident in
      let absmodsigdom = typecheck_signature ~address:(Alist.extend Alist.empty m) tyenv utsigdom in
      let (quant, modsigdom) = absmodsigdom in
      let (absmodsigcod, _) =
        let sname = get_space_name rngm m in
(*
        Printf.printf "MOD-FUNCTOR %s\n" m;  (* for debug *)
        display_signature 0 modsigdom;  (* for debug *)
*)
        let tyenv = tyenv |> Typeenv.add_module m { mod_signature = modsigdom; mod_name = sname } in
        typecheck_module Alist.empty tyenv utmod0
      in
      let absmodsig =
        begin
          match modsigdom with
          | ConcStructure(sigrdom) ->
              let sigftor =
                {
                  opaques  = quant;
                  domain   = Domain(sigrdom);
                  codomain = absmodsigcod;
                  closure  = Some(modident, utmod0, tyenv);
                }
              in
              (OpaqueIDMap.empty, ConcFunctor(sigftor))

          | _ ->
              raise_error (SupportOnlyFirstOrderFunctor(rng))
        end
      in
      (absmodsig, (ModuleAttribute.empty, []))

  | ModApply(modidentchain1, modidentchain2) ->
      let mentry1 = find_module_from_chain tyenv modidentchain1 in
      let modsig1 = mentry1.mod_signature in
      let mentry2 = find_module_from_chain tyenv modidentchain2 in
      let modsig2 = mentry2.mod_signature in
      let sname2 = mentry2.mod_name in
      begin
        match modsig1 with
        | ConcStructure(_) ->
            let rng1 = get_module_name_chain_position modidentchain1 in
            raise_error (NotOfFunctorType(rng1, modsig1))

        | ConcFunctor(sigftor1) ->
            let
                {
                  opaques  = quant;
                  domain   = Domain(sigrdom1);
                  codomain = absmodsigcod1;
                  _
                } = sigftor1
            in
            begin
              match sigftor1.closure with
              | None ->
                  assert false

              | Some(modident0, utmodC, tyenv0) ->
                  (* Check the subtype relation between the signature `modsig2` of the argument module
                     and the domain `modsigdom1` of the applied functor. *)
                  let subst =
                    let ((rng2, _), _) = modidentchain2 in
                    let modsigdom1 = ConcStructure(sigrdom1) in
                    subtype_signature ~cause:rng2 ~address modsig2 (quant, modsigdom1)
                  in
                  let ((_, modsig0), ibinds) =
                    let tyenv0 =
                      let (_, m0) = modident0 in
                      tyenv0 |> Typeenv.add_module m0 { mod_signature = modsig2; mod_name = sname2 }
                    in
                    typecheck_module ~address tyenv0 utmodC
                  in
                  let (quant1subst, modsigcod1subst) = absmodsigcod1 |> substitute_abstract ~cause:rng ~address subst in
                  let absmodsig = (quant1subst, copy_closure modsig0 modsigcod1subst) in
                  (absmodsig, ibinds)
            end
      end

  | ModCoerce(modident1, utsig2) ->
      let mentry1 = find_module tyenv modident1 in
      let modsig1 = mentry1.mod_signature in
      let (rng1, _) = modident1 in
      let absmodsig2 = typecheck_signature ~address tyenv utsig2 in
      let absmodsig = coerce_signature ~cause:rng1 ~address modsig1 absmodsig2 in
      (absmodsig, (ModuleAttribute.empty, []))


and typecheck_binding_list ~(address : address) (tyenv : Typeenv.t) (utbinds : untyped_binding list) : SigRecord.t abstracted * (ModuleAttribute.t * binding list) =
  let (_tyenv, quantacc, sigracc, (modattracc, ibindacc)) =
    utbinds |> List.fold_left (fun (tyenv, quantacc, sigracc, (modattracc, ibindacc)) utbind ->
      let (abssigr, (modattr, ibinds)) = typecheck_binding ~address tyenv utbind in
      let (quant, sigr) = abssigr in
      let tyenv = tyenv |> update_type_environment_by_signature_record sigr in
      let quantacc = merge_quantifier quantacc quant in
      let sigracc =
        match SigRecord.disjoint_union sigracc sigr with
        | Ok(sigr) -> sigr
        | Error(s) -> let (rng, _) = utbind in raise_error (ConflictInSignature(rng, s))
          (* In the original paper "F-ing modules" [Rossberg, Russo & Dreyer 2014],
             this operation is not disjoint union, but union with right-hand side precedence.
             For the sake of clarity, however, we adopt disjoint union here, at least for now.
          *)
      in
      let modattracc = ModuleAttribute.merge modattracc modattr in
      let ibindacc = Alist.append ibindacc ibinds in
      (tyenv, quantacc, sigracc, (modattracc, ibindacc))
    ) (tyenv, OpaqueIDMap.empty, SigRecord.empty, (ModuleAttribute.empty, Alist.empty))
  in
  ((quantacc, sigracc), (modattracc, Alist.to_list ibindacc))


and coerce_signature ~(cause : Range.t) ~(address : address) (modsig1 : module_signature) (absmodsig2 : module_signature abstracted) =
  let _wtmap = subtype_signature ~cause ~address modsig1 absmodsig2 in
  let (quant2, modsig2) = absmodsig2 in
  (quant2, copy_closure modsig1 modsig2)


let main (tyenv : Typeenv.t) (modident : module_name ranged) (absmodsigopt2 : (module_signature abstracted) option) (utmod1 : untyped_module) : Typeenv.t * SigRecord.t abstracted * space_name * (ModuleAttribute.t * binding list) =
  let (rng, modnm) = modident in
  let address = Alist.extend Alist.empty modnm in
  let (absmodsig1, imod) = typecheck_module ~address tyenv utmod1 in
  let sname = get_space_name rng modnm in
  let (quant, modsig) =
    match absmodsigopt2 with
    | None             -> absmodsig1
    | Some(absmodsig2) -> let (_, modsig1) = absmodsig1 in coerce_signature ~cause:rng ~address modsig1 absmodsig2
  in
  match modsig with
  | ConcFunctor(_) ->
      let (rng, _) = utmod1 in
      raise_error (RootModuleMustBeStructure(rng))

  | ConcStructure(sigr) ->
      let tyenv = tyenv |> Typeenv.add_module modnm { mod_signature = modsig; mod_name = sname } in
      (tyenv, (quant, sigr), sname, imod)
