
open Syntax

val register_free_row : FreeRowID.t -> mono_type LabelAssoc.t -> unit

val get_free_row : FreeRowID.t -> mono_type LabelAssoc.t

val register_bound_row : BoundRowID.t -> poly_type LabelAssoc.t -> unit

val get_bound_row : BoundRowID.t -> poly_type LabelAssoc.t
