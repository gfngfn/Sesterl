
open Syntax


let main fname =
  let inc = open_in fname in
  let lexbuf = Lexing.from_channel inc in
  let utdecls = ParserInterface.process lexbuf in
  let (tyenv, decls) = Typechecker.main utdecls in
  let scode = OutputErlangCode.main decls in
  Format.printf "%s\n" scode;
  Typeenv.fold_val (fun x pty () ->
    Format.printf "%s : %a\n" x pp_poly_type pty
  ) tyenv ()


let () =
  try
    Arg.parse [] main ""
  with
  | ParserInterface.Error(rng) ->
      Format.printf "%a: syntax error\n" Range.pp rng

  | UnidentifiedToken(rng, s) ->
      Format.printf "%a: unidentified token '%s'\n" Range.pp rng s

  | SeeEndOfFileInComment(rng) ->
      Format.printf "%a: unclosed comment\n" Range.pp rng

  | Typechecker.UnboundVariable(rng, x) ->
      Format.printf "%a: unbound variable '%s'\n" Range.pp rng x

  | Typechecker.ContradictionError(ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a but is expected of type %a\n"
        Range.pp rng1
        pp_mono_type ty1
        pp_mono_type ty2

  | Typechecker.InclusionError(fid, ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a and type %a at the same time, but these types are inconsistent as to the occurrence of type variable %a\n"
        Range.pp rng1
        pp_mono_type ty1
        pp_mono_type ty2
        FreeID.pp fid

  | Typechecker.BoundMoreThanOnceInPattern(rng, x) ->
      Format.printf "%a: this pattern binds '%s' more than once.\n"
        Range.pp rng
        x

  | Typechecker.UnboundTypeParameter(rng, tyvar) ->
      Format.printf "%a: unbound type variable '$%s'\n"
        Range.pp rng
        tyvar

  | Typechecker.UndefinedConstructor(rng, ctor) ->
      Format.printf "%a: undefined constructor '%s'\n"
        Range.pp rng
        ctor

  | Typechecker.InvalidNumberOfConstructorArguments(rng, ctor, len_expected, len_actual) ->
      Format.printf "%a: constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        Range.pp rng
        ctor
        len_expected
        len_actual

  | Typechecker.UndefinedTypeName(rng, tynm) ->
      Format.printf "%a: undefined type or type constructor '%s'"
        Range.pp rng
        tynm

  | Typechecker.InvalidNumberOfTypeArguments(rng, tynm, len_expected, len_actual) ->
      Format.printf "%a: type constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        Range.pp rng
        tynm
        len_expected
        len_actual

  | Typechecker.TypeParameterBoundMoreThanOnce(rng, tyvar) ->
      Format.printf "%a: type variable '%s' is bound more than once\n"
        Range.pp rng
        tyvar
