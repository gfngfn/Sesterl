
open Syntax

type environment

type record_signature

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
  | RecordType  of ('a, 'b) row
  | PackType    of module_signature abstracted

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
  | RowCons of label ranged * (('a, 'b) typ) * ('a, 'b) row
  | RowVar  of 'b
  | RowEmpty

and base_kind =
  | TypeKind
  | RowKind  of LabelSet.t

and module_signature_main =
  | ConcStructure of record_signature
  | ConcFunctor   of functor_signature

and module_signature =
  signature_source * module_signature_main

and signature_source =
  | ISigVar     of Address.t * signature_name
  | ISigPath    of signature_name (* TODO *)
  | ISigWith    of signature_source * (type_name * type_entry) list
  | ISigFunctor of signature_name * signature_source * signature_source
  | ISigDecls   of record_signature

and functor_signature = {
  opaques  : quantifier;
  domain   : functor_domain;
  codomain : module_signature abstracted;
  closure  : (module_name ranged * untyped_module * environment) option;
}

and functor_domain =
  | Domain of signature_source * record_signature

and kind =
  | Kind of (base_kind) list * base_kind
      (* Handles order-0 or order-1 kind only. *)

and mono_type_var_updatable =
  | Free of FreeID.t
  | Link of mono_type

and mono_type_var =
  | Updatable   of mono_type_var_updatable ref
  | MustBeBound of MustBeBoundID.t

and mono_row_var_updatable =
  | FreeRow of FreeRowID.t
  | LinkRow of mono_row

and mono_row_var =
  | UpdatableRow   of mono_row_var_updatable ref
  | MustBeBoundRow of MustBeBoundRowID.t

and mono_type = (mono_type_var, mono_row_var) typ

and mono_row = (mono_type_var, mono_row_var) row

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

and poly_domain_type = (poly_type_var, poly_row_var) domain_type

and quantifier = kind OpaqueIDMap.t

and 'a abstracted = quantifier * 'a

and type_entry = {
  type_scheme : type_scheme_with_entity;
  type_kind   : kind;
  type_doc    : string option;
}
[@@deriving show { with_path = false }]

and type_scheme_with_entity = BoundID.t list * poly_type * type_entity

and type_entity =
  | Opaque  of TypeID.t
  | Synonym
  | Variant of constructor_map

and constructor_map = (ConstructorID.t * poly_type list) ConstructorMap.t

val pp_module_signature : Format.formatter -> module_signature -> unit

type ('a, 'b) normalized_row =
  | NormalizedRow of (('a, 'b) typ) LabelAssoc.t * 'b option

type normalized_mono_row = (mono_type_var, mono_row_var) normalized_row

type normalized_poly_row = (poly_type_var, poly_row_var) normalized_row

type value_entry = {
  val_type   : poly_type;
  val_global : global_name;
  val_doc    : string option;
}

type type_scheme = BoundID.t list * poly_type

type module_entry = {
  mod_signature : module_signature;
  mod_name      : space_name;
  mod_doc       : string option;
}

type signature_entry = {
  sig_signature : module_signature abstracted;
  sig_doc       : string option;
  sig_address   : Address.t;
}

type constructor_entry = {
  belongs         : TypeID.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type local_row_parameter_map = (MustBeBoundRowID.t * LabelSet.t) RowParameterMap.t

module Typeenv : sig

  type t = environment

  val empty : t

  val map :
    v:(poly_type * name -> poly_type * name) ->
    m:(module_signature * space_name -> module_signature * space_name) ->
    t -> t

  val add_value : identifier -> poly_type -> name -> t -> t

  val find_value : identifier -> t -> (poly_type * name) option

  val is_val_properly_used : identifier -> t -> bool option

  val fold_value : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val add_type : type_name -> type_entry -> t -> t

  val add_opaque_id : type_name -> TypeID.t -> kind -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature_entry -> t -> t

  val find_signature : signature_name -> t -> signature_entry option

end

module SigRecord : sig

  type t = record_signature

  val empty : t

  val add_value : identifier -> value_entry -> t -> t

  val find_value : identifier -> t -> value_entry option

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val add_dummy_fold : type_name -> poly_type -> t -> t

  val find_dummy_fold : type_name -> t -> poly_type option

  val add_type : type_name -> type_entry -> t -> t

  val find_type : type_name -> t -> type_entry option

  val add_module : module_name -> module_entry -> t -> t

  val find_module : module_name -> t -> module_entry option

  val add_signature : signature_name -> signature_entry -> t -> t

  val find_signature : signature_name -> t -> signature_entry option

  val fold :
    v:(identifier -> value_entry -> 'a -> 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
    f:(type_name -> poly_type -> 'a -> 'a) ->
    t:(type_name -> type_entry -> 'a -> 'a) ->
    m:(module_name -> module_entry -> 'a -> 'a) ->
    s:(signature_name -> signature_entry -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(identifier -> value_entry -> 'a -> value_entry * 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> constructor_entry * 'a) ->
    f:(type_name -> poly_type -> 'a -> poly_type * 'a) ->
    t:(type_name -> type_entry -> 'a -> type_entry * 'a) ->
    m:(module_name -> module_entry -> 'a -> module_entry * 'a) ->
    s:(signature_name -> signature_entry -> 'a -> signature_entry * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(identifier -> value_entry -> value_entry) ->
    c:(constructor_name -> constructor_entry -> constructor_entry) ->
    f:(type_name -> poly_type -> poly_type) ->
    t:(type_name -> type_entry -> type_entry) ->
    m:(module_name -> module_entry -> module_entry) ->
    s:(signature_name -> signature_entry -> signature_entry) ->
    t -> t

  val disjoint_union : t -> t -> (t, string) result

end
(*
val display_signature : int -> module_signature -> unit

val display_structure : int -> SigRecord.t -> unit

val display_top_structure : module_name ranged -> SigRecord.t -> unit
*)
