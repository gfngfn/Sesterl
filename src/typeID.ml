
module type S = sig

  type t

  val fresh : string -> t

  val hash : t -> int

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val name : t -> string

  val pp : Format.formatter -> t -> unit

end


module Internal(A : sig val suffix : string end) = struct

  type t = {
    number : int;
    name   : string;
  }

  let fresh =
    let current_max = ref 0 in
    (fun name ->
      incr current_max;
      {
        number = !current_max;
        name   = name;
      }
    )

  let hash =
    Hashtbl.hash

  let compare tyid1 tyid2 =
    tyid2.number - tyid1.number

  let equal tyid1 tyid2 =
    tyid1.number = tyid2.number

  let name tyid =
    tyid.name

  let pp ppf tyid =
    Format.fprintf ppf "%s/%d%s" tyid.name tyid.number A.suffix

end


module Variant = Internal(struct let suffix = "!" end)

module Synonym = Internal(struct let suffix = "*" end)

module Opaque = Internal(struct let suffix = "@" end)

type t =
  | Variant of Variant.t
  | Synonym of Synonym.t
  | Opaque  of Opaque.t


let pp ppf tyid =
  match tyid with
  | Variant(vid) -> Variant.pp ppf vid
  | Synonym(sid) -> Synonym.pp ppf sid
  | Opaque(oid)  -> Opaque.pp ppf oid


let equal tyid1 tyid2 =
  match (tyid1, tyid2) with
  | (Variant(vid1), Variant(vid2)) -> Variant.equal vid1 vid2
  | (Synonym(sid1), Synonym(sid2)) -> Synonym.equal sid1 sid2
  | (Opaque(oid1) , Opaque(oid2) ) -> Opaque.equal oid1 oid2
  | _                              -> false
