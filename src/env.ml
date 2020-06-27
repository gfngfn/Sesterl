
open MyUtil
open Syntax

type val_entry = {
  typ  : poly_type;
  name : name;
  mutable is_used : bool;
}

type type_entry =
  | Defining       of TypeID.t
  | DefinedVariant of TypeID.Variant.t
  | DefinedSynonym of TypeID.Synonym.t
  | DefinedOpaque  of TypeID.Opaque.t

type variant_entry = {
  v_type_parameters : BoundID.t list;
  v_branches        : constructor_branch_map;
}

type opaque_entry = {
  o_kind : kind;
}

type 'v module_signature =
  | ConcStructure of 'v record_signature
  | ConcFunctor   of 'v functor_signature

and 'v functor_signature = {
  opaques  : OpaqueIDSet.t;
  domain   : 'v functor_domain;
  codomain : abstract_module_signature;
  closure  : (module_name ranged * untyped_module * environment) option;
}

and 'v functor_domain =
  | Domain of 'v record_signature

and module_entry = {
  mod_name      : space_name;
  mod_signature : concrete_module_signature;
}

and signature_entry = {
  sig_signature : abstract_module_signature;
}

and environment = {
  vals         : val_entry ValNameMap.t;
  type_names   : (type_entry * int) TypeNameMap.t;
  opaques      : opaque_entry OpaqueIDMap.t;
  constructors : constructor_entry ConstructorMap.t;
  modules      : module_entry ModuleNameMap.t;
  signatures   : signature_entry SignatureNameMap.t;
}

and 'v record_signature =
  ('v record_signature_entry) Alist.t

and 'v record_signature_entry =
  | SRVal      of identifier * 'v
  | SRRecTypes of (type_name * type_opacity) list
  | SRModule   of module_name * ('v module_signature * space_name)
  | SRSig      of signature_name * abstract_module_signature
  | SRCtor     of constructor_name * constructor_entry

and no_name_module_signature = poly_type module_signature

and abstract_module_signature = no_name_module_signature abstracted

and concrete_module_signature = (poly_type * name) module_signature


module Typeenv = struct

  type t = environment


  let empty = {
    vals         = ValNameMap.empty;
    type_names   = TypeNameMap.empty;
    opaques      = OpaqueIDMap.empty;
    constructors = ConstructorMap.empty;
    modules      = ModuleNameMap.empty;
    signatures   = SignatureNameMap.empty;
  }


  let map
      ~v:(fv : poly_type * name -> poly_type * name)
      ~m:(fm : concrete_module_signature * space_name -> concrete_module_signature * space_name)
      (tyenv : t) : t =
    let vals =
      tyenv.vals |> ValNameMap.map (fun ventry ->
        let (typ, name) = fv (ventry.typ, ventry.name) in
        { ventry with typ = typ; name = name }
      )
    in
    let modules =
      tyenv.modules |> ModuleNameMap.map (fun mentry ->
        let (modsig, sname) = fm (mentry.mod_signature, mentry.mod_name) in
        { mod_signature = modsig; mod_name = sname }
      )
    in
    { tyenv with vals = vals; modules = modules }


  let add_val x pty name tyenv =
    let entry =
      {
        typ  = pty;
        name = name;

        is_used = false;
      }
    in
    let vals = tyenv.vals |> ValNameMap.add x entry in
    { tyenv with vals = vals; }


  let find_val x tyenv =
    tyenv.vals |> ValNameMap.find_opt x |> Option.map (fun entry ->
      entry.is_used <- true;
      (entry.typ, entry.name)
    )


  let is_val_properly_used x tyenv =
    tyenv.vals |> ValNameMap.find_opt x |> Option.map (fun entry ->
      entry.is_used
    )


  let fold_val f tyenv acc =
    ValNameMap.fold (fun x entry acc -> f x entry.typ acc) tyenv.vals acc


  let add_variant_type (tynm : type_name) (vid : TypeID.Variant.t) (arity : int) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedVariant(vid), arity);
    }


  let add_constructor (ctornm : constructor_name) (ctorentry : constructor_entry) (tyenv : t) : t =
    { tyenv with
      constructors = tyenv.constructors |> ConstructorMap.add ctornm ctorentry;
    }


  let add_synonym_type (tynm : type_name) (sid : TypeID.Synonym.t) (arity : int) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedSynonym(sid), arity);
    }


  let add_opaque_type (tynm : type_name) (oid : TypeID.Opaque.t) (kind : kind) (tyenv : t) : t =
    let oentry =
      {
        o_kind = kind;
      }
    in
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedOpaque(oid), kind);
      opaques    = tyenv.opaques |> OpaqueIDMap.add oid oentry;
    }


  let add_type_for_recursion (tynm : type_name) (tyid : TypeID.t) (arity : int) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (Defining(tyid), arity);
    }


  let find_constructor (ctornm : constructor_name) (tyenv : t) =
    tyenv.constructors |> ConstructorMap.find_opt ctornm |> Option.map (fun entry ->
      (entry.belongs, entry.constructor_id, entry.type_variables, entry.parameter_types)
    )


  let find_type (tynm : type_name) (tyenv : t) : (TypeID.t * int) option =
    tyenv.type_names |> TypeNameMap.find_opt tynm |> Option.map (fun (tyentry, arity) ->
      match tyentry with
      | Defining(tyid)      -> (tyid, arity)
      | DefinedVariant(vid) -> (TypeID.Variant(vid), arity)
      | DefinedSynonym(sid) -> (TypeID.Synonym(sid), arity)
      | DefinedOpaque(oid)  -> (TypeID.Opaque(oid), arity)
    )


  let add_module (modnm : module_name) (modsig : concrete_module_signature) (sname : space_name) (tyenv : t) : t =
    let modentry =
      {
        mod_name      = sname;
        mod_signature = modsig;
      }
    in
    { tyenv with
      modules = tyenv.modules |> ModuleNameMap.add modnm modentry;
    }


  let find_module (modnm : module_name) (tyenv : t) : (concrete_module_signature * space_name) option =
    tyenv.modules |> ModuleNameMap.find_opt modnm |> Option.map (fun modentry ->
      (modentry.mod_signature, modentry.mod_name)
    )


  let add_signature (signm : signature_name) (absmodsig : abstract_module_signature) (tyenv : t) : t =
    let sigentry =
      {
        sig_signature = absmodsig;
      }
    in
    { tyenv with
      signatures = tyenv.signatures |> SignatureNameMap.add signm sigentry;
    }


  let find_signature (signm : signature_name) (tyenv : t) : abstract_module_signature option =
    tyenv.signatures |> SignatureNameMap.find_opt signm |> Option.map (fun sigentry ->
      sigentry.sig_signature
    )

end


module type SigRecordS = sig

  type val_entry

  type t = val_entry record_signature

  val empty : t

  val add_val : identifier -> val_entry -> t -> t

  val find_val : identifier -> t -> val_entry option

  val add_types : (type_name * type_opacity) list -> t -> t

  val add_constructors : TypeID.Variant.t -> BoundID.t list -> constructor_branch_map -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val find_type : type_name -> t -> type_opacity option

  val add_opaque_type : type_name -> TypeID.Opaque.t -> kind -> t -> t

  val add_module : module_name -> val_entry module_signature -> space_name -> t -> t

  val find_module : module_name -> t -> (val_entry module_signature * space_name) option

  val add_signature : signature_name -> abstract_module_signature -> t -> t

  val find_signature : signature_name -> t -> abstract_module_signature option

  val fold :
    v:(identifier -> val_entry -> 'a -> 'a) ->
    t:((type_name * type_opacity) list -> 'a -> 'a) ->
    m:(module_name -> val_entry module_signature * space_name -> 'a -> 'a) ->
    s:(signature_name -> abstract_module_signature -> 'a -> 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(val_entry -> 'a -> val_entry * 'a) ->
    t:(type_opacity list -> 'a -> type_opacity list * 'a) ->
    m:(val_entry module_signature * space_name -> 'a -> (val_entry module_signature * space_name) * 'a) ->
    s:(abstract_module_signature -> 'a -> abstract_module_signature * 'a) ->
    c:(constructor_entry -> 'a -> constructor_entry * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(val_entry -> val_entry) ->
    t:(type_opacity list -> type_opacity list) ->
    m:(val_entry module_signature * space_name -> val_entry module_signature * space_name) ->
    s:(abstract_module_signature -> abstract_module_signature) ->
    c:(constructor_entry -> constructor_entry) ->
    t -> t

  val disjoint_union : Range.t -> t -> t -> t

end



module SigRecordScheme(M : sig type v end) = struct

  type val_entry = M.v

  type t = M.v record_signature

  let empty : t =
    Alist.empty


  let add_val (x : identifier) (ventry : val_entry) (sigr : t) : t =
    Alist.extend sigr (SRVal(x, ventry))


  let find_val (x0 : identifier) (sigr : t) : val_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRVal(x, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                -> None
    )


  let add_types (tydefs : (type_name * type_opacity) list) (sigr : t) : t =
    Alist.extend sigr (SRRecTypes(tydefs))


  let add_constructors (vid : TypeID.Variant.t) (typarams : BoundID.t list) (ctorbrs : constructor_branch_map) (sigr : t) : t =
    ConstructorMap.fold (fun ctornm (ctorid, ptys) sigr ->
      let ctorentry =
        {
          belongs         = vid;
          constructor_id  = ctorid;
          type_variables  = typarams;
          parameter_types = ptys;
        }
      in
      Alist.extend sigr (SRCtor(ctornm, ctorentry))
    ) ctorbrs sigr


  let find_constructor (ctornm0 : constructor_name) (sigr : t) : constructor_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRCtor(ctornm, entry) -> if String.equal ctornm ctornm0 then Some(entry) else None
    | _                     -> None
    )


  let find_type (tynm0 : type_name) (sigr : t) : type_opacity option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRRecTypes(tydefs) ->
        tydefs |> List.find_map (fun (tynm, tyopac) ->
          if String.equal tynm tynm0 then Some(tyopac) else None
        )

    | _ ->
        None
    )


  let add_opaque_type (tynm : type_name) (oid : TypeID.Opaque.t) (kd : kind) (sigr : t) : t =
    Alist.extend sigr (SRRecTypes[ (tynm, (TypeID.Opaque(oid), kd)) ])


  let add_module (modnm : module_name) (modsig : val_entry module_signature) (sname : space_name) (sigr : t) : t =
    Alist.extend sigr (SRModule(modnm, (modsig, sname)))


  let find_module (modnm0 : module_name) (sigr : t) : (val_entry module_signature * space_name) option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRModule(modnm, mentry) -> if String.equal modnm modnm0 then Some(mentry) else None
    | _                       -> None
    )


  let add_signature (signm : signature_name) (absmodsig : abstract_module_signature) (sigr : t) : t =
    Alist.extend sigr (SRSig(signm, absmodsig))


  let find_signature (signm0 : signature_name) (sigr : t) : abstract_module_signature option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRSig(signm, absmodsig) -> if String.equal signm signm0 then Some(absmodsig) else None
    | _                       -> None
    )


  let fold (type a)
      ~v:(fv : identifier -> val_entry -> a -> a)
      ~t:(ft : (type_name * type_opacity) list -> a -> a)
      ~m:(fm : module_name -> val_entry module_signature * space_name -> a -> a)
      ~s:(fs : signature_name -> abstract_module_signature -> a -> a)
      ~c:(fc : constructor_name -> constructor_entry -> a -> a)
      (init : a) (sigr : t) : a =
    sigr |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SRVal(x, ventry)        -> fv x ventry acc
      | SRRecTypes(tydefs)      -> ft tydefs acc
      | SRModule(modnm, mentry) -> fm modnm mentry acc
      | SRSig(signm, absmodsig) -> fs signm absmodsig acc
      | SRCtor(ctor, ctorentry) -> fc ctor ctorentry acc
    ) init


  let map_and_fold (type a)
      ~v:(fv : val_entry -> a -> val_entry * a)
      ~t:(ft : type_opacity list -> a -> type_opacity list * a)
      ~m:(fm : val_entry module_signature * space_name -> a -> (val_entry module_signature * space_name) * a)
      ~s:(fs : abstract_module_signature -> a -> abstract_module_signature * a)
      ~c:(fc : constructor_entry -> a -> constructor_entry * a)
      (init : a) (sigr : t) : t * a =
      sigr |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SRVal(x, ventry) ->
            let (ventry, acc) = fv ventry acc in
            (Alist.extend sigracc (SRVal(x, ventry)), acc)

        | SRRecTypes(tydefs) ->
            let tynms = tydefs |> List.map fst in
            let (tyopacs, acc) = ft (tydefs |> List.map snd) acc in
            (Alist.extend sigracc (SRRecTypes(List.combine tynms tyopacs)), acc)

        | SRModule(modnm, mentry) ->
            let (mentry, acc) = fm mentry acc in
            (Alist.extend sigracc (SRModule(modnm, mentry)), acc)

        | SRSig(signm, absmodsig) ->
            let (absmodsig, acc) = fs absmodsig acc in
            (Alist.extend sigracc (SRSig(signm, absmodsig)), acc)

        | SRCtor(ctor, ctorentry) ->
            let (ctorentry, acc) = fc ctorentry acc in
            (Alist.extend sigracc (SRCtor(ctor, ctorentry)), acc)
      ) (Alist.empty, init)


  let map (type a)
      ~v:(fv : val_entry -> val_entry)
      ~t:(ft : type_opacity list -> type_opacity list)
      ~m:(fm : val_entry module_signature * space_name -> val_entry module_signature * space_name)
      ~s:(fs : abstract_module_signature -> abstract_module_signature)
      ~c:(fc : constructor_entry -> constructor_entry)
      (sigr : t) : t =
    let (sigr, ()) =
      sigr |> map_and_fold
          ~v:(fun v () -> (fv v, ()))
          ~t:(fun t () -> (ft t, ()))
          ~m:(fun m () -> (fm m, ()))
          ~s:(fun s () -> (fs s, ()))
          ~c:(fun c () -> (fc c, ()))
          ()
    in
    sigr

(*
  let overwrite (superior : t) (inferior : t) : t =
    let left _ x _ = Some(x) in
    let sr_vals    = ValNameMap.union       left superior.sr_vals    inferior.sr_vals in
    let sr_types   = TypeNameMap.union      left superior.sr_types   inferior.sr_types in
    let sr_modules = ModuleNameMap.union    left superior.sr_modules inferior.sr_modules in
    let sr_sigs    = SignatureNameMap.union left superior.sr_sigs    inferior.sr_sigs in
    let sr_ctors   = ConstructorMap.union   left superior.sr_ctors   inferior.sr_ctors in
    { sr_vals; sr_types; sr_modules; sr_sigs; sr_ctors }
*)

  let disjoint_union (rng : Range.t) (sigr1 : t) (sigr2 : t) : t =
    let check_none s opt =
      match opt with
      | None    -> ()
      | Some(_) -> raise (ConflictInSignature(rng, s))
    in
    sigr2 |> Alist.to_list |> List.fold_left (fun sigracc entry ->
      let () =
        match entry with
        | SRVal(x, _)        -> check_none x (find_val x sigr1)
        | SRRecTypes(tydefs) -> tydefs |> List.iter (fun (tynm, _) -> check_none tynm (find_type tynm sigr1))
        | SRModule(modnm, _) -> check_none modnm (find_module modnm sigr1)
        | SRSig(signm, _)    -> check_none signm (find_signature signm sigr1)
        | SRCtor(ctor, _)    -> check_none ctor (find_constructor ctor sigr1)
      in
      Alist.extend sigracc entry
    ) sigr1

end


module NamedSigRecord = SigRecordScheme(struct
  type v = poly_type * name
end)

module NoNameSigRecord = SigRecordScheme(struct
  type v = poly_type
end)
