
open Syntax

val find_synonym_type : TypeID.Synonym.t -> BoundID.t list * poly_type

val add_synonym_type : TypeID.Synonym.t -> BoundID.t list -> poly_type -> unit
