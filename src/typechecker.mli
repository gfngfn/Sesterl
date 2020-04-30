
open Syntax

exception UnboundVariable            of Range.t * identifier
exception ContradictionError         of mono_type * mono_type
exception InclusionError             of FreeID.t * mono_type * mono_type
exception BoundMoreThanOnceInPattern of Range.t * identifier

val main : untyped_declaration list -> Typeenv.t * declaration list
