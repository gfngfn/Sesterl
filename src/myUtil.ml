module Alist : sig
  type 'a t

  val empty : 'a t

  val extend : 'a t -> 'a -> 'a t

  val to_list : 'a t -> 'a list
end = struct
  type 'a t = 'a list

  let empty = []

  let extend acc x = x :: acc

  let to_list = List.rev
end
