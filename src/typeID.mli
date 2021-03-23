
module type S = sig

  type t

  val fresh : string list -> string -> t

  val hash : t -> int

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val name : t -> string

  val pp : Format.formatter -> t -> unit

  val pp_raw : Format.formatter -> t -> unit
end

module Variant : S

module Synonym : S

module Opaque : S

type t =
  | Variant of Variant.t
  | Synonym of Synonym.t
  | Opaque  of Opaque.t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
