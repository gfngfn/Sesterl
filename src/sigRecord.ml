
open MyUtil
open Syntax


type signature_record =
  signature_record_entry Alist.t

and signature_record_entry =
  | SRVal      of identifier * (poly_type * name)
  | SRRecTypes of (type_name * type_opacity) list
  | SRModule   of module_name * (signature_record module_signature_ * name)
  | SRSig      of signature_name * (signature_record module_signature_) abstracted
  | SRCtor     of constructor_name * constructor_entry

type t = signature_record

type module_signature = signature_record module_signature_


let empty : t =
  Alist.empty


let add_val (x : identifier) (pty : poly_type) (name : name) (sigr : t) : t =
  Alist.extend sigr (SRVal(x, (pty, name)))


let find_val (x0 : identifier) (sigr : t) : (poly_type * name) option =
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


let add_module (modnm : module_name) (modsig : module_signature) (name : name) (sigr : t) : t =
  Alist.extend sigr (SRModule(modnm, (modsig, name)))


let find_module (modnm0 : module_name) (sigr : t) : (module_signature * name) option =
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
    ~v:(fv : identifier -> poly_type * name -> a -> a)
    ~t:(ft : (type_name * type_opacity) list -> a -> a)
    ~m:(fm : module_name -> module_signature * name -> a -> a)
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
    ~v:(fv : poly_type * name -> a -> (poly_type * name) * a)
    ~t:(ft : type_opacity list -> a -> type_opacity list * a)
    ~m:(fm : module_signature * name -> a -> (module_signature * name) * a)
    ~s:(fs : module_signature abstracted -> a -> module_signature abstracted * a)
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
