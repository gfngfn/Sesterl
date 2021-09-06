
type t = Semver.t


let parse (s : string) : t option =
  Option.bind (Core.String.chop_prefix s ~prefix:"v") Semver.of_string


let is_compatible ~(before : t) ~(after : t) =
  let open Semver in
  match (before.major, after.major) with
  | (0, 0) ->
      before.minor = after.minor && before.patch <= after.patch

  | _ ->
      before.major = after.major &&
        ((before.minor < after.minor) ||
          (before.minor == after.minor && before.patch <= after.patch))


let is_supported (specified_language_version : string) : bool =
  match (parse specified_language_version, parse Constants.semantic_version) with
  | (_, None)                         -> assert false
  | (None, _)                         -> false
  | (Some(specified), Some(required)) -> is_compatible ~before:specified ~after:required
