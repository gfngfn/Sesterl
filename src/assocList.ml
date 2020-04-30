

module type EQ = sig
  type t
  val equal : t -> t -> bool
end


module Make(Key : EQ) : sig
  type elem
  type 'v t
  val empty : 'v t
  val add_last : elem -> 'v -> 'v t -> 'v t
  val find_opt : elem -> 'v t -> 'v option
  val fold_left : ('a -> elem -> 'v -> 'a) -> 'a -> 'v t -> 'a
  val length : 'v t -> int
end
  with type elem = Key.t
= struct

  type elem = Key.t

  type 'v t = (elem * 'v) list


  let empty = []


  let add_last k v assoc =
    let rec aux acc xs =
      match xs with
      | [] ->
          List.rev ((k, v) :: acc)

      | ((kx, _) as x) :: tail ->
          if Key.equal k kx then
            List.rev_append acc ((k, v) :: tail)
          else
            aux (x :: acc) tail
    in
    aux [] assoc


  let rec find_opt k assoc =
    match assoc with
    | [] ->
        None

    | (kx, vx) :: tail ->
        if Key.equal k kx then
          Some(vx)
        else
          find_opt k tail


  let fold_left f init assoc =
    List.fold_left (fun acc (k, v) -> f acc k v) init assoc


  let length =
    List.length

end
