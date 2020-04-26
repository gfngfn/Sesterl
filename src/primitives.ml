
open Syntax


let initial_type_environment =
  let dr = Range.dummy "primitives" in
  let b = (dr, BaseType(BoolType)) in
  let i = (dr, BaseType(IntType)) in
  let ( @-> ) tydoms tycod = (dr, FuncType(tydoms, tycod)) in
  let tylogic = [b; b] @-> b in
  let tycomp = [i; i] @-> b in
  let tyarith = [i; i] @-> i in

  List.fold_left (fun tyenv (x, ty) ->
    tyenv |> Typeenv.add x ty
  ) Typeenv.empty [
    ("&&", tylogic);
    ("||", tylogic);
    ("==", tycomp);
    ("<=", tycomp);
    (">=", tycomp);
    ("<", tycomp);
    (">", tycomp);
    ("*", tyarith);
    ("/", tyarith);
    ("+", tyarith);
    ("-", tyarith);
  ]
