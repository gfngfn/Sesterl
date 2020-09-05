
open Syntax

val register_free_row : FreeRowID.t -> mono_type LabelAssoc.t -> unit

val get_free_row : FreeRowID.t -> mono_type LabelAssoc.t

val register_bound_row : BoundRowID.t -> poly_type LabelAssoc.t -> unit

val get_bound_row : BoundRowID.t -> poly_type LabelAssoc.t

val register_free_id : FreeID.t -> mono_base_kind -> unit

val get_free_id : FreeID.t -> mono_base_kind

val register_bound_id : BoundID.t -> poly_base_kind -> unit

val get_bound_id : BoundID.t -> poly_base_kind
