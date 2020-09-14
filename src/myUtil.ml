
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


type absolute_path = string

type absolute_dir = string


module Utf : sig
  val uchar_of_utf8 : string -> Uchar.t list
end = struct

  let uchar_of_utf8 (s : string) =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(s)) in
    let rec iter acc =
      match Uutf.decode decoder with
      | `End          -> Alist.to_list acc
      | `Uchar(u)     -> iter (Alist.extend acc u)
      | `Await        -> iter acc
      | `Malformed(_) -> iter (Alist.extend acc Uutf.u_rep)
          (* Silently replaces malformed sequences with `Uutf.u_rep`. *)
    in
    iter Alist.empty

end
