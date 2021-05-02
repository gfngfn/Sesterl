
open Syntax

type 'a abstracted = OpaqueIDSet.t * 'a

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
  | DataType    of TypeID.t * (('a, 'b) typ) list
  | RecordType  of (('a, 'b) typ) LabelAssoc.t
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
  domain   : functor_domain;
  codomain : OpaqueIDSet.t * module_signature;
  closure  : (module_name ranged * untyped_module * environment) option;
}

and functor_domain =
  | Domain of record_signature

val pp_module_signature : Format.formatter -> module_signature -> unit

type ('a, 'b) kind =
  | Kind of (('a, 'b) base_kind) list * ('a, 'b) base_kind
      (* Handles order-0 or order-1 kind only, *)

type mono_type_var_updatable =
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

type mono_row = (mono_type_var, mono_row_var) row

type mono_kind = (mono_type_var, mono_row_var) kind

type mono_base_kind = (mono_type_var, mono_row_var) base_kind

type mono_effect = (mono_type_var, mono_row_var) effect

type mono_domain_type = (mono_type_var, mono_row_var) domain_type

type poly_type_var =
  | Mono  of mono_type_var
  | Bound of BoundID.t

type poly_row_var =
  | MonoRow  of mono_row_var
  | BoundRow of BoundRowID.t

and poly_type = (poly_type_var, poly_row_var) typ

type poly_row = (poly_type_var, poly_row_var) row

type poly_kind = (poly_type_var, poly_row_var) kind

type poly_base_kind = (poly_type_var, poly_row_var) base_kind

type poly_domain_type = (poly_type_var, poly_row_var) domain_type

type type_opacity = TypeID.t * poly_kind

type constructor_entry = {
  belongs         : TypeID.Variant.t;
  constructor_id  : ConstructorID.t;
  type_variables  : BoundID.t list;
  parameter_types : poly_type list;
}

type constructor_branch_map = (ConstructorID.t * poly_type list) ConstructorMap.t

type local_row_parameter_map = (MustBeBoundRowID.t * poly_type LabelAssoc.t) RowParameterMap.t

module Typeenv : sig

  type t = environment

  val empty : t

  val map :
    v:(poly_type * name -> poly_type * name) ->
    m:(module_signature * space_name -> module_signature * space_name) ->
    t -> t

  val add_val : identifier -> poly_type -> name -> t -> t

  val find_val : identifier -> t -> (poly_type * name) option

  val is_val_properly_used : identifier -> t -> bool option

  val fold_val : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a

  val add_variant_type : type_name -> TypeID.Variant.t -> poly_kind -> t -> t

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val add_synonym_type : type_name -> TypeID.Synonym.t -> poly_kind -> t -> t

  val add_opaque_type : type_name -> TypeID.Opaque.t -> poly_kind -> t -> t

  val add_type_for_recursion : type_name -> TypeID.t -> poly_kind -> t -> t

  val find_constructor : constructor_name -> t -> (TypeID.Variant.t * ConstructorID.t * BoundID.t list * poly_type list) option

  val find_type : type_name -> t -> (TypeID.t * poly_kind) option

  val add_module : module_name -> module_signature -> space_name -> t -> t

  val find_module : module_name -> t -> (module_signature * space_name) option

  val add_signature : signature_name -> module_signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (module_signature abstracted) option

end

module SigRecord : sig

  type t = record_signature

  val empty : t

  val add_val : identifier -> poly_type -> global_name -> t -> t

  val find_val : identifier -> t -> (poly_type * global_name) option

  val add_types : (type_name * type_opacity) list -> t -> t

  val find_constructor : (TypeID.Variant.t -> BoundID.t list * constructor_branch_map) -> constructor_name -> t -> constructor_entry option

  val find_type : type_name -> t -> type_opacity option

  val add_opaque_type : type_name -> TypeID.Opaque.t -> poly_kind -> t -> t

  val add_module : module_name -> module_signature -> space_name -> t -> t

  val find_module : module_name -> t -> (module_signature * space_name) option

  val add_signature : signature_name -> module_signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (module_signature abstracted) option

  val fold :
    v:(identifier -> poly_type * global_name -> 'a -> 'a) ->
    t:((type_name * type_opacity) list -> 'a -> 'a) ->
    m:(module_name -> module_signature * space_name -> 'a -> 'a) ->
    s:(signature_name -> module_signature abstracted -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(identifier -> poly_type * global_name -> 'a -> (poly_type * global_name) * 'a) ->
    t:((type_name * type_opacity) list -> 'a -> type_opacity list * 'a) ->
    m:(module_name -> module_signature * space_name -> 'a -> (module_signature * space_name) * 'a) ->
    s:(signature_name -> module_signature abstracted -> 'a -> module_signature abstracted * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(identifier -> poly_type * global_name -> poly_type * global_name) ->
    t:((type_name * type_opacity) list -> type_opacity list) ->
    m:(module_name -> module_signature * space_name -> module_signature * space_name) ->
    s:(signature_name -> module_signature abstracted -> module_signature abstracted) ->
    t -> t

  val disjoint_union : t -> t -> (t, string) result

end
(*
val display_signature : int -> module_signature -> unit

val display_structure : int -> SigRecord.t -> unit

val display_top_structure : module_name ranged -> SigRecord.t -> unit
*)
