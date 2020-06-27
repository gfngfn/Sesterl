
open Syntax

type environment

type 'v record_signature

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

and concrete_module_signature = (poly_type * name) module_signature

and no_name_module_signature = poly_type module_signature

and abstract_module_signature = no_name_module_signature abstracted

module Typeenv : sig

  type t = environment

  val empty : t

  val map :
    v:(poly_type * name -> poly_type * name) ->
    m:(concrete_module_signature * space_name -> concrete_module_signature * space_name) ->
    t -> t

  val add_val : identifier -> poly_type -> name -> t -> t

  val find_val : identifier -> t -> (poly_type * name) option

  val is_val_properly_used : identifier -> t -> bool option

  val fold_val : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a

  val add_variant_type : type_name -> TypeID.Variant.t -> int -> t -> t

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val add_synonym_type : type_name -> TypeID.Synonym.t -> int -> t -> t

  val add_opaque_type : type_name -> TypeID.Opaque.t -> kind -> t -> t

  val add_type_for_recursion : type_name -> TypeID.t -> int -> t -> t

  val find_constructor : constructor_name -> t -> (TypeID.Variant.t * ConstructorID.t * BoundID.t list * poly_type list) option

  val find_type : type_name -> t -> (TypeID.t * int) option

  val add_module : module_name -> concrete_module_signature -> space_name -> t -> t

  val find_module : module_name -> t -> (concrete_module_signature * space_name) option

  val add_signature : signature_name -> abstract_module_signature -> t -> t

  val find_signature : signature_name -> t -> abstract_module_signature option

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

module NamedSigRecord : (SigRecordS with type val_entry = poly_type * name)

module NoNameSigRecord : (SigRecordS with type val_entry = poly_type)
