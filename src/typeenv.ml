
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

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type module_entry = {
  mod_name      : name;
  mod_signature : module_signature;
}

type signature_entry = {
  sig_signature : module_signature abstracted;
}

type t = {
  vals         : val_entry ValNameMap.t;
  type_names   : (type_entry * int) TypeNameMap.t;
  variants     : variant_entry VariantIDMap.t;
  opaques      : opaque_entry OpaqueIDMap.t;
  constructors : constructor_entry ConstructorMap.t;
  modules      : module_entry ModuleNameMap.t;
  signatures   : signature_entry SignatureNameMap.t;
}


let empty = {
  vals         = ValNameMap.empty;
  type_names   = TypeNameMap.empty;
  variants     = VariantIDMap.empty;
  opaques      = OpaqueIDMap.empty;
  constructors = ConstructorMap.empty;
  modules      = ModuleNameMap.empty;
  signatures   = SignatureNameMap.empty;
}


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


let find_val_opt x tyenv =
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


let add_variant_type (tynm : type_name) (vid : TypeID.Variant.t) (typarams : BoundID.t list) (brmap : constructor_branch_map) (tyenv : t) : t =
  let ventry =
    {
      v_type_parameters = typarams;
      v_branches        = brmap;
    }
  in
  let ctors =
    ConstructorMap.fold (fun ctornm (ctorid, ptys) ctors ->
      let entry =
        {
          belongs         = vid;
          constructor_id  = ctorid;
          type_variables  = typarams;
          parameter_types = ptys;
        }
      in
      ctors |> ConstructorMap.add ctornm entry
    ) brmap tyenv.constructors
  in
  let arity = List.length typarams in
  { tyenv with
    type_names   = tyenv.type_names |> TypeNameMap.add tynm (DefinedVariant(vid), arity);
    variants     = tyenv.variants |> VariantIDMap.add vid ventry;
    constructors = ctors;
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


let add_module (modnm : module_name) (modsig : module_signature) (name : name) (tyenv : t) : t =
  let modentry =
    {
      mod_name      = name;
      mod_signature = modsig;
    }
  in
  { tyenv with
    modules = tyenv.modules |> ModuleNameMap.add modnm modentry;
  }


let find_module_opt (modnm : module_name) (tyenv : t) : (module_signature * name) option =
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


let find_signature_opt (signm : signature_name) (tyenv : t) : (module_signature abstracted) option =
  tyenv.signatures |> SignatureNameMap.find_opt signm |> Option.map (fun sigentry ->
    sigentry.sig_signature
  )
