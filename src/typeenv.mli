
open Syntax

type t

val empty : t

val add_val : identifier -> poly_type -> name -> t -> t

val find_val_opt : identifier -> t -> (poly_type * name) option

val is_val_properly_used : identifier -> t -> bool option

val fold_val : (identifier -> poly_type -> 'a -> 'a) -> t -> 'a -> 'a

val add_type : type_name -> TypeID.t -> type_parameter_assoc -> constructor_branch_map -> t -> t

val find_constructor : constructor_name -> t -> (TypeID.t * ConstructorID.t * type_parameter_assoc * poly_type list) option
