(** `IdentifierScheme` is a module that abstracts identifiers
    for equating the snake case and the upper camel case.
*)

type t

val from_snake_case : string -> t option
(** [from_snake_case s] converts the original identifier string [s] into its corresponding list of word fragments.
    Here, [s] should match [<lower-or-digit> <lower>* ('_' <lower>+)*].

    {[
      from_snake_case "foo_bar"  (* ==> Some{ fragments = ["foo"; "bar"]; ... } *)
      from_snake_case "foo_Bar"  (* ==> None *)
      from_snake_case "foo__bar" (* ==> None *)
      from_snake_case "foo_bar_" (* ==> None *)
      from_snake_case "x86_64"   (* ==> Some{ fragments = ["x86"; "64"]; ... } *)
    ]}
*)

val from_upper_camel_case : string -> t option

val original : t -> string

val to_snake_case : t -> string

val to_lower_camel_case : t -> string

val to_upper_camel_case : t -> string
(** {[
      to_upper_camel_case { fragments = ["foo"; "bar"]; ... } (* ==> "FooBar" *)
      to_upper_camel_case { fragments = ["x86"; "64"]; ... } (* ==> "X86_64" *)
    ]}
*)

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
