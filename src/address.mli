type element = Member of string | FunctorBody of { arg : string }

type t

val root : t

val append_member : string -> t -> t

val append_functor_body : arg:string -> t -> t

val to_list : t -> element list

val subtract : long:t -> short:t -> t

val show : t -> string

val pp : Format.formatter -> t -> unit
