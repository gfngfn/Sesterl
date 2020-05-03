
open Syntax

type t

val empty : t

val add_val : identifier -> poly_type -> name -> t -> t

val find_val_opt : identifier -> t -> (poly_type * name) option

val is_val_properly_used : identifier -> t -> bool option

val fold_val : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a

val add_variant_type : type_name -> TypeID.Variant.t -> BoundID.t list -> constructor_branch_map -> t -> t

val add_synonym_type : type_name -> TypeID.Synonym.t -> BoundID.t list -> poly_type -> t -> t

val add_type_for_recursion : type_name -> TypeID.t -> int -> t -> t

val find_constructor : constructor_name -> t -> (TypeID.Variant.t * ConstructorID.t * BoundID.t list * poly_type list) option

val find_type : type_name -> t -> (TypeID.t * int) option

val find_synonym_type : TypeID.Synonym.t -> t -> (BoundID.t list * poly_type) option

val add_module : module_name -> module_signature -> name -> t -> t

val find_module_opt : module_name -> t -> (module_signature * name) option
