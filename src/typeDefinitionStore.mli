
open Syntax

val find_synonym_type : TypeID.Synonym.t -> BoundID.t list * poly_type

val add_synonym_type : TypeID.Synonym.t -> BoundID.t list -> poly_type -> unit

val find_variant_type : TypeID.Variant.t -> BoundID.t list * constructor_branch_map

val add_variant_type : TypeID.Variant.t -> BoundID.t list -> constructor_branch_map -> unit
