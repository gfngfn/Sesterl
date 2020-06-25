
open Syntax

type t

type module_signature = t module_signature_

val empty : t

val add_val : identifier -> poly_type -> name -> t -> t

val find_val : identifier -> t -> (poly_type * name) option

val add_types : (type_name * type_opacity) list -> t -> t

val add_constructors : TypeID.Variant.t -> BoundID.t list -> constructor_branch_map -> t -> t

val find_constructor : constructor_name -> t -> constructor_entry option

val find_type : type_name -> t -> type_opacity option

val add_opaque_type : type_name -> TypeID.Opaque.t -> kind -> t -> t

val add_module : module_name -> module_signature -> name -> t -> t

val find_module : module_name -> t -> (module_signature * name) option

val add_signature : signature_name -> module_signature abstracted -> t -> t

val find_signature : signature_name -> t -> (module_signature abstracted) option

val fold :
  v:(identifier -> poly_type * name -> 'a -> 'a) ->
  t:((type_name * type_opacity) list -> 'a -> 'a) ->
  m:(module_name -> module_signature * name -> 'a -> 'a) ->
  s:(signature_name -> module_signature abstracted -> 'a -> 'a) ->
  c:(constructor_name -> constructor_entry -> 'a -> 'a) ->
  'a -> t -> 'a

val map_and_fold :
  v:(poly_type * name -> 'a -> (poly_type * name) * 'a) ->
  t:(type_opacity list -> 'a -> type_opacity list * 'a) ->
  m:(module_signature * name -> 'a -> (module_signature * name) * 'a) ->
  s:(module_signature abstracted -> 'a -> module_signature abstracted * 'a) ->
  c:(constructor_entry -> 'a -> constructor_entry * 'a) ->
  'a -> t -> t * 'a

val disjoint_union : Range.t -> t -> t -> t
