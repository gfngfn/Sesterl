

module type EQ = sig
  type t
  val equal : t -> t -> bool
end


module Make(Key : EQ) : sig
  type elem
  type 'v t
  val empty : 'a t
  val add_last : elem -> 'a -> 'a t -> 'a t
  val find_opt : elem -> 'a t -> 'a option
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

end
