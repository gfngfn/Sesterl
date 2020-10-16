
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
  o_kind : poly_kind;
}

type module_signature =
  | ConcStructure of record_signature
  | ConcFunctor   of functor_signature

and functor_signature = {
  opaques  : OpaqueIDSet.t;
    [@printer pp_opaque_id_set]
  domain   : functor_domain;
  codomain : OpaqueIDSet.t * module_signature;
    [@printer (fun ppf (oidset, modsig) -> Format.fprintf ppf "(%a, _)" pp_opaque_id_set oidset)]
  closure  : (module_name ranged * untyped_module * environment) option;
}

and functor_domain =
  | Domain of record_signature

and module_entry = {
  mod_name      : space_name;
  mod_signature : module_signature;
}

and signature_entry = {
  sig_signature : module_signature abstracted;
    [@printer (fun ppf (oidset, _) -> Format.fprintf ppf "(%a, _)" pp_opaque_id_set oidset)]
}

and environment = {
  vals         : val_entry ValNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<vals>")]
  type_names   : (type_entry * poly_kind) TypeNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<type_names>")]
  opaques      : opaque_entry OpaqueIDMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<opaques>")]
  constructors : constructor_entry ConstructorMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<constructors>")]
  modules      : module_entry ModuleNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<modules>")]
  signatures   : signature_entry SignatureNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<signatures>")]
}

and record_signature =
  record_signature_entry Alist.t
[@printer (fun ppf acc ->
  Format.fprintf ppf "%a" (Format.pp_print_list pp_record_signature_entry) (Alist.to_list acc)
)]

and record_signature_entry =
  | SRVal      of identifier * (poly_type * global_name)
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRVal>")]
  | SRRecTypes of (type_name * type_opacity) list
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRRecTypes>")]
  | SRModule   of module_name * (module_signature * space_name)
  | SRSig      of signature_name * module_signature abstracted
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRSig>")]
  | SRCtor     of constructor_name * constructor_entry
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRCtor>")]
[@@deriving show { with_path = false }]


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
      ~m:(fm : module_signature * space_name -> module_signature * space_name)
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


  let add_variant_type (tynm : type_name) (vid : TypeID.Variant.t) (pkd : poly_kind) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedVariant(vid), pkd);
    }


  let add_constructor (ctornm : constructor_name) (ctorentry : constructor_entry) (tyenv : t) : t =
    { tyenv with
      constructors = tyenv.constructors |> ConstructorMap.add ctornm ctorentry;
    }


  let add_synonym_type (tynm : type_name) (sid : TypeID.Synonym.t) (pkd : poly_kind) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedSynonym(sid), pkd);
    }


  let add_opaque_type (tynm : type_name) (oid : TypeID.Opaque.t) (pkd : poly_kind) (tyenv : t) : t =
    let oentry =
      {
        o_kind = pkd;
      }
    in
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (DefinedOpaque(oid), pkd);
      opaques    = tyenv.opaques |> OpaqueIDMap.add oid oentry;
    }


  let add_type_for_recursion (tynm : type_name) (tyid : TypeID.t) (pkd : poly_kind) (tyenv : t) : t =
    { tyenv with
      type_names = tyenv.type_names |> TypeNameMap.add tynm (Defining(tyid), pkd);
    }


  let find_constructor (ctornm : constructor_name) (tyenv : t) =
    tyenv.constructors |> ConstructorMap.find_opt ctornm |> Option.map (fun entry ->
      (entry.belongs, entry.constructor_id, entry.type_variables, entry.parameter_types)
    )


  let find_type (tynm : type_name) (tyenv : t) : (TypeID.t * poly_kind) option =
    tyenv.type_names |> TypeNameMap.find_opt tynm |> Option.map (fun (tyentry, pkd) ->
      match tyentry with
      | Defining(tyid)      -> (tyid, pkd)
      | DefinedVariant(vid) -> (TypeID.Variant(vid), pkd)
      | DefinedSynonym(sid) -> (TypeID.Synonym(sid), pkd)
      | DefinedOpaque(oid)  -> (TypeID.Opaque(oid), pkd)
    )


  let add_module (modnm : module_name) (modsig : module_signature) (sname : space_name) (tyenv : t) : t =
    let modentry =
      {
        mod_name      = sname;
        mod_signature = modsig;
      }
    in
    { tyenv with
      modules = tyenv.modules |> ModuleNameMap.add modnm modentry;
    }


  let find_module (modnm : module_name) (tyenv : t) : (module_signature * space_name) option =
    tyenv.modules |> ModuleNameMap.find_opt modnm |> Option.map (fun modentry ->
      (modentry.mod_signature, modentry.mod_name)
    )


  let add_signature (signm : signature_name) (absmodsig : module_signature abstracted) (tyenv : t) : t =
    let sigentry =
      {
        sig_signature = absmodsig;
      }
    in
    { tyenv with
      signatures = tyenv.signatures |> SignatureNameMap.add signm sigentry;
    }


  let find_signature (signm : signature_name) (tyenv : t) : (module_signature abstracted) option =
    tyenv.signatures |> SignatureNameMap.find_opt signm |> Option.map (fun sigentry ->
      sigentry.sig_signature
    )

end


module SigRecord = struct

  type t = record_signature

  let empty : t =
    Alist.empty


  let add_val (x : identifier) (pty : poly_type) (gname : global_name) (sigr : t) : t =
    Alist.extend sigr (SRVal(x, (pty, gname)))


  let find_val (x0 : identifier) (sigr : t) : (poly_type * global_name) option =
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


  let add_opaque_type (tynm : type_name) (oid : TypeID.Opaque.t) (pkd : poly_kind) (sigr : t) : t =
    Alist.extend sigr (SRRecTypes[ (tynm, (TypeID.Opaque(oid), pkd)) ])


  let add_module (modnm : module_name) (modsig : module_signature) (sname : space_name) (sigr : t) : t =
    Alist.extend sigr (SRModule(modnm, (modsig, sname)))


  let find_module (modnm0 : module_name) (sigr : t) : (module_signature * space_name) option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRModule(modnm, mentry) -> if String.equal modnm modnm0 then Some(mentry) else None
    | _                       -> None
    )


  let add_signature (signm : signature_name) (absmodsig : module_signature abstracted) (sigr : t) : t =
    Alist.extend sigr (SRSig(signm, absmodsig))


  let find_signature (signm0 : signature_name) (sigr : t) : (module_signature abstracted) option =
    sigr |> Alist.to_list |> List.find_map (function
    | SRSig(signm, absmodsig) -> if String.equal signm signm0 then Some(absmodsig) else None
    | _                       -> None
    )


  let fold (type a)
      ~v:(fv : identifier -> poly_type * global_name -> a -> a)
      ~t:(ft : (type_name * type_opacity) list -> a -> a)
      ~m:(fm : module_name -> module_signature * space_name -> a -> a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> a)
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
      ~v:(fv : identifier -> poly_type * global_name -> a -> (poly_type * global_name) * a)
      ~t:(ft : (type_name * type_opacity) list -> a -> type_opacity list * a)
      ~m:(fm : module_name -> module_signature * space_name -> a -> (module_signature * space_name) * a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> module_signature abstracted * a)
      ~c:(fc : constructor_name -> constructor_entry -> a -> constructor_entry * a)
      (init : a) (sigr : t) : t * a =
      sigr |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SRVal(x, ventry) ->
            let (ventry, acc) = fv x ventry acc in
            (Alist.extend sigracc (SRVal(x, ventry)), acc)

        | SRRecTypes(tydefs) ->
            let tynms = tydefs |> List.map fst in
            let (tyopacs, acc) = ft tydefs acc in
            (Alist.extend sigracc (SRRecTypes(List.combine tynms tyopacs)), acc)

        | SRModule(modnm, mentry) ->
            let (mentry, acc) = fm modnm mentry acc in
            (Alist.extend sigracc (SRModule(modnm, mentry)), acc)

        | SRSig(signm, absmodsig) ->
            let (absmodsig, acc) = fs signm absmodsig acc in
            (Alist.extend sigracc (SRSig(signm, absmodsig)), acc)

        | SRCtor(ctornm, ctorentry) ->
            let (ctorentry, acc) = fc ctornm ctorentry acc in
            (Alist.extend sigracc (SRCtor(ctornm, ctorentry)), acc)
      ) (Alist.empty, init)


  let map (type a)
      ~v:(fv : identifier -> poly_type * global_name -> poly_type * global_name)
      ~t:(ft : (type_name * type_opacity) list -> type_opacity list)
      ~m:(fm : module_name -> module_signature * space_name -> module_signature * space_name)
      ~s:(fs : signature_name -> module_signature abstracted -> module_signature abstracted)
      ~c:(fc : constructor_name -> constructor_entry -> constructor_entry)
      (sigr : t) : t =
    let (sigr, ()) =
      sigr |> map_and_fold
          ~v:(fun x ventry () -> (fv x ventry, ()))
          ~t:(fun tydefs () -> (ft tydefs, ()))
          ~m:(fun modnm mentry () -> (fm modnm mentry, ()))
          ~s:(fun signm sentry () -> (fs signm sentry, ()))
          ~c:(fun ctornm centry () -> (fc ctornm centry, ()))
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

  exception Conflict of string


  let disjoint_union (sigr1 : t) (sigr2 : t) : (t, string) result =
    let check_none s opt =
      match opt with
      | None    -> ()
      | Some(_) -> raise (Conflict(s))
    in
    try
      let sigr =
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
      in
      Ok(sigr)
    with
    | Conflict(s) -> Error(s)
end


let pp_comma ppf () =
  Format.fprintf ppf ", "


let pp_bound_type_id ppf bid =
  let pkd = KindStore.get_bound_id bid in
  match pkd with
  | UniversalKind ->
      Format.fprintf ppf "%a" BoundID.pp bid

  | _ ->
      let (_, _, skd) = TypeConv.show_poly_base_kind pkd in
      Format.fprintf ppf "%a :: %s" BoundID.pp bid skd


let pp_type_parameters ppf typarams =
  match typarams with
  | [] ->
      ()

  | _ :: _ ->
      Format.fprintf ppf "<%a>"
        (Format.pp_print_list ~pp_sep:pp_comma pp_bound_type_id) typarams


let display_poly_type pty =
  let (sbids, sbrids, sty) = TypeConv.show_poly_type pty in
  let ssub =
    let ss = List.append sbids sbrids in
    if List.length ss = 0 then
      ""
    else
      "<" ^ (String.concat ", " ss) ^ ">"
  in
  (ssub, sty)


let rec display_signature (depth : int) (modsig : module_signature) : unit =
  let indent = String.make (depth * 2) ' ' in
  match modsig with
  | ConcStructure(sigr) ->
      Format.printf "%ssig\n" indent;
      display_structure (depth + 1) sigr;
      Format.printf "%send\n" indent

  | ConcFunctor(sigftor) ->
      let (oidset1, Domain(sigr1), (oidset2, modsigcod)) = (sigftor.opaques, sigftor.domain, sigftor.codomain) in
      let modsigdom = ConcStructure(sigr1) in
      let sx1 = stringify_opaque_id_set oidset1 in
      let sx2 = stringify_opaque_id_set oidset2 in
      Format.printf "%s(forall%s) fun(\n" indent sx1;
      display_signature (depth + 1) modsigdom;
      Format.printf "%s) -> (exists%s)\n" indent sx2;
      display_signature (depth + 1) modsigcod


and display_structure (depth : int) (sigr : SigRecord.t) : unit =
  let indent = String.make (depth * 2) ' ' in
  sigr |> SigRecord.fold
      ~v:(fun x (pty, _) () ->
        let (ssub, sty) = display_poly_type pty in
        Format.printf "%sval %s%s : %s\n" indent x ssub sty
      )
      ~t:(fun tydefs () ->
        tydefs |> List.iter (fun (tynm, tyopac) ->
          let (tyid, pkd) = tyopac in
          match tyid with
          | TypeID.Synonym(sid) ->
              let (typarams, ptyreal) = TypeDefinitionStore.find_synonym_type sid in
              let (_, sty) = display_poly_type ptyreal in
              Format.printf "%stype %a%a = %s\n"
                indent
                TypeID.Synonym.pp sid
                pp_type_parameters typarams
                sty

          | TypeID.Variant(vid) ->
              let (typarams, _ctorbrs) = TypeDefinitionStore.find_variant_type vid in
              Format.printf "%stype %a%a = (variant)\n"
                indent
                TypeID.Variant.pp vid
                pp_type_parameters typarams

          | TypeID.Opaque(oid) ->
              let (_, _, skd) = TypeConv.show_poly_kind pkd in
              Format.printf "%stype %a :: %s\n"
                indent
                TypeID.Opaque.pp oid
                skd
        )
      )
      ~m:(fun modnm (modsig, _) () ->
        Format.printf "%smodule %s :\n" indent modnm;
        display_signature (depth + 1) modsig
      )
      ~s:(fun signm (oidset, modsig) () ->
        let sx = stringify_opaque_id_set oidset in
        Format.printf "%ssignature %s =\n" indent signm;
        Format.printf "%s  (exists%s)\n" indent sx;
        display_signature (depth + 2) modsig
      )
      ~c:(fun ctornm _ () ->
        Format.printf "%sconstructor %s\n" indent ctornm
      )
      ()


let display_top_structure ((_, modnm) : module_name ranged) (sigr : SigRecord.t) =
  Format.printf "  --------------------------------\n";
  Format.printf "  module %s =\n" modnm;
  display_structure 2 sigr;
  Format.printf "  --------------------------------\n"
