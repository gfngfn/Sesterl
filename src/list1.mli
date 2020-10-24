
type 'a t
(** ['a t] is the type for lists (of values of type ['a]) the length of which is more than or equal to 1. *)

val make : 'a -> 'a list -> 'a t
(** [make e1 es] corresponds to [e1 :: es]. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map_and_fold : ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t

val to_list : 'a t -> 'a list
(** [to_list] forgets the constraint of the length. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
