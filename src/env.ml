
open MyUtil
open Syntax


type 'a abstracted = OpaqueIDSet.t * 'a

type ('a, 'b) typ =
  (('a, 'b) typ_main) ranged

and ('a, 'b) typ_main =
  | BaseType    of base_type
  | FuncType    of ('a, 'b) domain_type * ('a, 'b) typ
  | PidType     of ('a, 'b) pid_type
  | EffType     of ('a, 'b) domain_type * ('a, 'b) effect * ('a, 'b) typ
  | TypeVar     of 'a
  | ProductType of (('a, 'b) typ) TupleList.t
  | TypeApp     of TypeID.t * (('a, 'b) typ) list
  | RecordType  of (('a, 'b) typ) LabelAssoc.t
  | PackType    of module_signature abstracted
      [@printer (fun ppf (oidset, modsig) -> Format.fprintf ppf "PackType(%a, _)" pp_opaque_id_set oidset)]

and ('a, 'b) domain_type = {
  ordered   : (('a, 'b) typ) list;
  mandatory : (('a, 'b) typ) LabelAssoc.t;
  optional  : ('a, 'b) row;
}

and ('a, 'b) effect =
  | Effect of ('a, 'b) typ

and ('a, 'b) pid_type =
  | Pid of ('a, 'b) typ

and ('a, 'b) row =
  | FixedRow of (('a, 'b) typ) LabelAssoc.t
  | RowVar   of 'b

and ('a, 'b) base_kind =
  | UniversalKind
  | RecordKind    of (('a, 'b) typ) LabelAssoc.t

and module_signature =
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

and env_value_entry = {
  typ  : poly_type;
  name : name;
  mutable is_used : bool;
}

and value_entry = {
  val_type   : poly_type;
  val_global : global_name;
}

and type_entry = {
  type_scheme : BoundID.t list * poly_type;
  type_kind   : poly_kind;
}

and module_entry = {
  mod_signature : module_signature;
  mod_name      : space_name;
}

and constructor_entry = {
  belongs         : TypeID.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

and opaque_entry = {
  opaque_kind : poly_kind;
}

and environment = {
  values       : env_value_entry ValNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<values>")]
  constructors : constructor_entry ConstructorMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<constructors>")]
  types        : type_entry TypeNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<types>")]
  opaques      : poly_kind OpaqueIDMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<opaques>")]
  modules      : module_entry ModuleNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<modules>")]
  signatures   : (module_signature abstracted) SignatureNameMap.t;
    [@printer (fun ppf _ -> Format.fprintf ppf "<signatures>")]
}

and record_signature =
  record_signature_entry Alist.t
[@printer (fun ppf acc ->
  Format.fprintf ppf "%a" (Format.pp_print_list pp_record_signature_entry) (Alist.to_list acc)
)]

and record_signature_entry =
  | SRVal      of identifier * value_entry
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRVal>")]
  | SRCtor     of constructor_name * constructor_entry
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRCtor>")]
  | SRFold     of type_name * poly_type
  | SRType     of type_name * type_entry
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRType>")]
  | SRModule   of module_name * module_entry
  | SRSig      of signature_name * module_signature abstracted
      [@printer (fun ppf _ -> Format.fprintf ppf "<SRSig>")]
[@@deriving show { with_path = false }]

and ('a, 'b) kind =
  | Kind of (('a, 'b) base_kind) list * ('a, 'b) base_kind
      (* Handles order-0 or order-1 kind only, *)

and mono_type_var_updatable =
  | Free of FreeID.t
  | Link of mono_type

and mono_type_var =
  | Updatable   of mono_type_var_updatable ref
  | MustBeBound of MustBeBoundID.t

and mono_row_var_updatable =
  | FreeRow of FreeRowID.t
  | LinkRow of mono_type LabelAssoc.t

and mono_row_var =
  | UpdatableRow   of mono_row_var_updatable ref
  | MustBeBoundRow of MustBeBoundRowID.t

and mono_type = (mono_type_var, mono_row_var) typ

and mono_row = (mono_type_var, mono_row_var) row

and mono_kind = (mono_type_var, mono_row_var) kind

and mono_base_kind = (mono_type_var, mono_row_var) base_kind

and mono_effect = (mono_type_var, mono_row_var) effect

and mono_domain_type = (mono_type_var, mono_row_var) domain_type

and poly_type_var =
  | Mono  of mono_type_var
  | Bound of BoundID.t

and poly_row_var =
  | MonoRow  of mono_row_var
  | BoundRow of BoundRowID.t

and poly_type = (poly_type_var, poly_row_var) typ

and poly_row = (poly_type_var, poly_row_var) row

and poly_kind = (poly_type_var, poly_row_var) kind

and poly_base_kind = (poly_type_var, poly_row_var) base_kind

and poly_domain_type = (poly_type_var, poly_row_var) domain_type

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorMap.t

type local_row_parameter_map = (MustBeBoundRowID.t * poly_type LabelAssoc.t) RowParameterMap.t

type variant_entry = {
  v_type_parameters : BoundID.t list;
  v_branches        : constructor_branch_map;
}


module Typeenv = struct

  type t = environment


  let empty = {
    values       = ValNameMap.empty;
    types        = TypeNameMap.empty;
    opaques      = OpaqueIDMap.empty;
    constructors = ConstructorMap.empty;
    modules      = ModuleNameMap.empty;
    signatures   = SignatureNameMap.empty;
  }


  let map
      ~v:(fv : poly_type * name -> poly_type * name)
      ~m:(fm : module_signature * space_name -> module_signature * space_name)
      (tyenv : t) : t =
    let values =
      tyenv.values |> ValNameMap.map (fun ventry ->
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
    { tyenv with values = values; modules = modules }


  let add_value (x : identifier) (pty : poly_type) (name : name) (tyenv : t) : t =
    let entry =
      {
        typ  = pty;
        name = name;

        is_used = false;
      }
    in
    let values = tyenv.values |> ValNameMap.add x entry in
    { tyenv with values = values; }


  let find_value (x : identifier) (tyenv : t) =
    tyenv.values |> ValNameMap.find_opt x |> Option.map (fun entry ->
      entry.is_used <- true;
      (entry.typ, entry.name)
    )


  let is_val_properly_used (x : identifier) (tyenv : t) : bool option =
    tyenv.values |> ValNameMap.find_opt x |> Option.map (fun entry ->
      entry.is_used
    )


  let fold_value f tyenv acc =
    ValNameMap.fold (fun x entry acc -> f x entry.typ acc) tyenv.values acc


  let add_constructor (ctornm : constructor_name) (ctorentry : constructor_entry) (tyenv : t) : t =
    { tyenv with
      constructors = tyenv.constructors |> ConstructorMap.add ctornm ctorentry;
    }


  let find_constructor (ctornm : constructor_name) (tyenv : t) =
    tyenv.constructors |> ConstructorMap.find_opt ctornm


  let add_type (tynm : type_name) (tentry : type_entry) (tyenv : t) : t =
    { tyenv with
      types = tyenv.types |> TypeNameMap.add tynm tentry;
    }


  let add_opaque_id (tynm : type_name) (oid : TypeID.t) (pkd : poly_kind) (tyenv : t) : t =
    { tyenv with
      opaques = tyenv.opaques |> OpaqueIDMap.add oid pkd;
    }


  let find_type (tynm : type_name) (tyenv : t) : type_entry option =
    tyenv.types |> TypeNameMap.find_opt tynm


  let add_module (modnm : module_name) (mentry : module_entry) (tyenv : t) : t =
    { tyenv with
      modules = tyenv.modules |> ModuleNameMap.add modnm mentry;
    }


  let find_module (modnm : module_name) (tyenv : t) : module_entry option =
    tyenv.modules |> ModuleNameMap.find_opt modnm


  let add_signature (signm : signature_name) (absmodsig : module_signature abstracted) (tyenv : t) : t =
    { tyenv with
      signatures = tyenv.signatures |> SignatureNameMap.add signm absmodsig;
    }


  let find_signature (signm : signature_name) (tyenv : t) : (module_signature abstracted) option =
    tyenv.signatures |> SignatureNameMap.find_opt signm

end


module SigRecord = struct

  type t = record_signature

  let empty : t =
    Alist.empty


  let add_value (x : identifier) (ventry : value_entry) (sigr : t) : t =
    Alist.extend sigr (SRVal(x, ventry))


  let find_value (x0 : identifier) (sigr : t) : value_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRVal(x, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                -> None
    )


  let add_type (tynm : type_name) (tentry : type_entry) (sigr : t) : t =
    Alist.extend sigr (SRType(tynm, tentry))


  let find_type (tynm0 : type_name) (sigr : t) : type_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRType(tynm, tentry) -> if String.equal tynm tynm0 then Some(tentry) else None
    | _                    -> None
    )


  let add_constructor (ctornm : constructor_name) (centry : constructor_entry) (sigr : t) : t =
    Alist.extend sigr (SRCtor(ctornm, centry))


  let find_constructor (ctornm : constructor_name) (sigr : t) : constructor_entry option =
    sigr |> Alist.to_rev_list |> List.find_map (function
    | SRCtor(ctornm, centry) -> Some(centry)
    | _                      -> None
    )


  let add_dummy_fold (tynm : type_name) (pty : poly_type) (sigr : t) : t =
    Alist.extend sigr (SRFold(tynm, pty))


  let add_module (modnm : module_name) (mentry : module_entry) (sigr : t) : t =
    Alist.extend sigr (SRModule(modnm, mentry))


  let find_module (modnm0 : module_name) (sigr : t) : module_entry option =
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
      ~v:(fv : identifier -> value_entry -> a -> a)
      ~c:(fc : constructor_name -> constructor_entry -> a -> a)
      ~f:(ff : type_name -> poly_type -> a -> a)
      ~t:(ft : type_name -> type_entry -> a -> a)
      ~m:(fm : module_name -> module_entry -> a -> a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> a)
      (init : a) (sigr : t) : a =
    sigr |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SRVal(x, ventry)        -> fv x ventry acc
      | SRCtor(ctornm, centry)  -> fc ctornm centry acc
      | SRFold(tynm, pty)       -> ff tynm pty acc
      | SRType(tynm, tentry)    -> ft tynm tentry acc
      | SRModule(modnm, mentry) -> fm modnm mentry acc
      | SRSig(signm, absmodsig) -> fs signm absmodsig acc
    ) init


  let map_and_fold (type a)
      ~v:(fv : identifier -> value_entry -> a -> value_entry * a)
      ~c:(fc : constructor_name -> constructor_entry -> a -> constructor_entry * a)
      ~f:(ff : type_name -> poly_type -> a -> poly_type * a)
      ~t:(ft : type_name -> type_entry -> a -> type_entry * a)
      ~m:(fm : module_name -> module_entry -> a -> module_entry * a)
      ~s:(fs : signature_name -> module_signature abstracted -> a -> module_signature abstracted * a)
      (init : a) (sigr : t) : t * a =
      sigr |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SRVal(x, ventry) ->
            let (ventry, acc) = fv x ventry acc in
            (Alist.extend sigracc (SRVal(x, ventry)), acc)

        | SRCtor(ctornm, centry) ->
            let (centry, acc) = fc ctornm centry acc in
            (Alist.extend sigracc (SRCtor(ctornm, centry)), acc)

        | SRFold(tynm, pty) ->
            let (pty, acc) = ff tynm pty acc in
            (Alist.extend sigracc (SRFold(tynm, pty)), acc)

        | SRType(tynm, tentry) ->
            let (tentry, acc) = ft tynm tentry acc in
            (Alist.extend sigracc (SRType(tynm, tentry)), acc)

        | SRModule(modnm, mentry) ->
            let (mentry, acc) = fm modnm mentry acc in
            (Alist.extend sigracc (SRModule(modnm, mentry)), acc)

        | SRSig(signm, absmodsig) ->
            let (absmodsig, acc) = fs signm absmodsig acc in
            (Alist.extend sigracc (SRSig(signm, absmodsig)), acc)

      ) (Alist.empty, init)


  let map (type a)
      ~v:(fv : identifier -> value_entry -> value_entry)
      ~c:(fc : constructor_name -> constructor_entry -> constructor_entry)
      ~f:(ff : type_name -> poly_type -> poly_type)
      ~t:(ft : type_name -> type_entry -> type_entry)
      ~m:(fm : module_name -> module_entry -> module_entry)
      ~s:(fs : signature_name -> module_signature abstracted -> module_signature abstracted)
      (sigr : t) : t =
    let (sigr, ()) =
      sigr |> map_and_fold
          ~v:(fun x ventry () -> (fv x ventry, ()))
          ~c:(fun ctornm centry () -> (fc ctornm centry, ()))
          ~f:(fun tynm pty () -> (ff tynm pty, ()))
          ~t:(fun tynm tentry () -> (ft tynm tentry, ()))
          ~m:(fun modnm mentry () -> (fm modnm mentry, ()))
          ~s:(fun signm sentry () -> (fs signm sentry, ()))
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
            | SRVal(x, _)        -> check_none x (find_value x sigr1)
            | SRCtor(ctornm, _)  -> check_none ctornm (find_constructor ctornm sigr1)
            | SRFold(_, _)       -> ()
            | SRType(tynm, _)    -> check_none tynm (find_type tynm sigr1)
            | SRModule(modnm, _) -> check_none modnm (find_module modnm sigr1)
            | SRSig(signm, _)    -> check_none signm (find_signature signm sigr1)
          in
          Alist.extend sigracc entry
        ) sigr1
      in
      Ok(sigr)
    with
    | Conflict(s) -> Error(s)
end

(*
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


let display_poly_type_params (ptys : poly_type list) =
  match ptys with
  | [] ->
      ""

  | _ :: _ ->
      let ss = ptys |> List.map display_poly_type |> List.map (fun (_, sty) -> sty) in
      Printf.sprintf "(%s)" (String.concat ", " ss)


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
              let (typarams, ctorbrs) = TypeDefinitionStore.find_variant_type vid in
              Format.printf "%stype %a%a =\n"
                indent
                TypeID.Variant.pp vid
                pp_type_parameters typarams;
              ctorbrs |> ConstructorMap.iter (fun ctor (ctorid, ptyparams) ->
                let sparam = display_poly_type_params ptyparams in
                Format.printf "%s  | %s%s\n"
                  indent
                  ctor
                  sparam
              )

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
      ()


let display_top_structure ((_, modnm) : module_name ranged) (sigr : SigRecord.t) =
  Format.printf "  --------------------------------\n";
  Format.printf "  module %s =\n" modnm;
  display_structure 2 sigr;
  Format.printf "  --------------------------------\n"
*)
