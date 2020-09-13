
type 'a t
(** ['a t] is the type for lists (of values of type ['a]) the length of which is more than or equal to 2. *)

val make : 'a -> 'a -> 'a list -> 'a t
(** [make e1 e2 es] corresponds to [e1 :: e2 :: es]. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val to_list : 'a t -> 'a list
(** [to_list] forgets the constraint of the length. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
