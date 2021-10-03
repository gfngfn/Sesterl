module StringSet = Set.Make (String)

module Alist : sig
  type 'a t

  val empty : 'a t

  val extend : 'a t -> 'a -> 'a t

  val append : 'a t -> 'a list -> 'a t

  val length : 'a t -> int

  val to_list : 'a t -> 'a list

  val to_rev_list : 'a t -> 'a list

  val from_list : 'a list -> 'a t

  val is_empty : 'a t -> bool
end = struct
  type 'a t = 'a list

  let empty = []

  let extend acc x = x :: acc

  let append acc xs = List.rev_append xs acc

  let length acc = List.length acc

  let to_list = List.rev

  let to_rev_list acc = acc

  let from_list = List.rev

  let is_empty acc = match acc with [] -> true | _ :: _ -> false
end

module ResultMonad : sig
  val return : 'a -> ('a, 'e) result

  val err : 'e -> ('a, 'e) result

  val map_err : ('e1 -> 'e2) -> ('a, 'e1) result -> ('a, 'e2) result

  val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
end = struct
  let return v = Ok v

  let err e = Error e

  let ( >>= ) v f = match v with Ok x -> f x | Error e -> Error e

  let map_err f v = match v with Ok x -> Ok x | Error e -> Error (f e)
end

module OptionMonad : sig
  val return : 'a -> 'a option

  val none : 'a option

  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
end = struct
  let return x = Some x

  let none = None

  let ( >>= ) = Option.bind
end

type absolute_path = string

type absolute_dir = string

type relative_path = RelativePath of string

type relative_dir = RelativeDir of string

let make_absolute_path ?(canonicalize = false) (absdir : absolute_dir)
    (fpath : string) : absolute_path =
  let f = if canonicalize then Core.Filename.realpath else fun s -> s in
  if Filename.is_relative fpath then f (Filename.concat absdir fpath)
  else f fpath

let append_dir (absdir : absolute_dir) (RelativeDir reldir : relative_dir) :
    absolute_dir =
  Filename.concat absdir reldir

let append_path (absdir : absolute_dir) (RelativePath relpath : relative_path) :
    absolute_path =
  Filename.concat absdir relpath

let canonicalize_path (abspath : absolute_path) : absolute_path option =
  try Some (Core.Filename.realpath abspath) with Unix.Unix_error _ -> None

let is_existing_directory (abspath : absolute_path) : bool =
  let abspath0 = Core.Filename.concat abspath Filename.current_dir_name in
  try
    Option.equal String.equal
      (canonicalize_path abspath)
      (canonicalize_path abspath0)
  with _ -> false

module Utf : sig
  val uchar_of_utf8 : string -> Uchar.t list
end = struct
  let uchar_of_utf8 (s : string) =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
    let rec iter acc =
      match Uutf.decode decoder with
      | `End -> Alist.to_list acc
      | `Uchar u -> iter (Alist.extend acc u)
      | `Await -> iter acc
      | `Malformed _ -> iter (Alist.extend acc Uutf.u_rep)
      (* Silently replaces malformed sequences with `Uutf.u_rep`. *)
    in
    iter Alist.empty
end
