
open Syntax

type environment

type record_signature

type module_signature =
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

  val add_variant_type : type_name -> TypeID.Variant.t -> int -> t -> t

  val add_constructor : constructor_name -> constructor_entry -> t -> t

  val add_synonym_type : type_name -> TypeID.Synonym.t -> int -> t -> t

  val add_opaque_type : type_name -> TypeID.Opaque.t -> kind -> t -> t

  val add_type_for_recursion : type_name -> TypeID.t -> int -> t -> t

  val find_constructor : constructor_name -> t -> (TypeID.Variant.t * ConstructorID.t * BoundID.t list * poly_type list) option

  val find_type : type_name -> t -> (TypeID.t * int) option

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

  val add_constructors : TypeID.Variant.t -> BoundID.t list -> constructor_branch_map -> t -> t

  val find_constructor : constructor_name -> t -> constructor_entry option

  val find_type : type_name -> t -> type_opacity option

  val add_opaque_type : type_name -> TypeID.Opaque.t -> kind -> t -> t

  val add_module : module_name -> module_signature -> space_name -> t -> t

  val find_module : module_name -> t -> (module_signature * space_name) option

  val add_signature : signature_name -> module_signature abstracted -> t -> t

  val find_signature : signature_name -> t -> (module_signature abstracted) option

  val fold :
    v:(identifier -> poly_type * global_name -> 'a -> 'a) ->
    t:((type_name * type_opacity) list -> 'a -> 'a) ->
    m:(module_name -> module_signature * space_name -> 'a -> 'a) ->
    s:(signature_name -> module_signature abstracted -> 'a -> 'a) ->
    c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
    'a -> t -> 'a

  val map_and_fold :
    v:(poly_type * global_name -> 'a -> (poly_type * global_name) * 'a) ->
    t:(type_opacity list -> 'a -> type_opacity list * 'a) ->
    m:(module_signature * space_name -> 'a -> (module_signature * space_name) * 'a) ->
    s:(module_signature abstracted -> 'a -> module_signature abstracted * 'a) ->
    c:(constructor_entry -> 'a -> constructor_entry * 'a) ->
    'a -> t -> t * 'a

  val map :
    v:(poly_type * global_name -> poly_type * global_name) ->
    t:(type_opacity list -> type_opacity list) ->
    m:(module_signature * space_name -> module_signature * space_name) ->
    s:(module_signature abstracted -> module_signature abstracted) ->
    c:(constructor_entry -> constructor_entry) ->
    t -> t

  val disjoint_union : t -> t -> (t, string) result

end

val display_signature : int -> module_signature -> unit

val display_structure : int -> SigRecord.t -> unit
