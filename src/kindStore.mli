
open Syntax
open Env

val register_free_row : FreeRowID.t -> LabelSet.t -> unit

val get_free_row : FreeRowID.t -> LabelSet.t

val register_bound_row : BoundRowID.t -> LabelSet.t -> unit

val get_bound_row : BoundRowID.t -> LabelSet.t
