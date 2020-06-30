
module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val to_rev_list : 'a t -> 'a list
  val from_list : 'a list -> 'a t
  val is_empty : 'a t -> bool
end = struct

  type 'a t = 'a list

  let empty = []

  let extend acc x = x :: acc

  let append acc xs = List.rev_append xs acc

  let to_list = List.rev

  let to_rev_list acc = acc

  let from_list = List.rev

  let is_empty acc =
    match acc with
    | []     -> true
    | _ :: _ -> false

end
