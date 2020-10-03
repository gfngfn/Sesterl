
type error

type 'a decoder

val run : 'a decoder -> string -> ('a, error) result

val succeed : 'a -> 'a decoder

val failure : string -> 'a decoder

val bind : 'a decoder -> ('a -> 'b decoder) -> 'b decoder

val get : string -> 'a decoder -> 'a decoder

val number : float decoder

val string : string decoder

val list : 'a decoder -> ('a list) decoder

val map : ('a -> 'b) -> 'a decoder -> 'b decoder

val map2 : ('a1 -> 'a2 -> 'b) -> 'a1 decoder -> 'a2 decoder -> 'b decoder

val map3 : ('a1 -> 'a2 -> 'a3 -> 'b) -> 'a1 decoder -> 'a2 decoder -> 'a3 decoder -> 'b decoder
