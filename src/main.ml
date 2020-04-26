
open Syntax


let main fname =
  let inc = open_in fname in
  let lexbuf = Lexing.from_channel inc in
  let decls = ParserInterface.process lexbuf in
  let tyenv = Typechecker.main decls in
  Format.printf "%a\n" (Format.pp_print_list pp_declaration) decls;
  Typeenv.fold (fun x pty () ->
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
      Format.printf "%a: this expression has type %a but is expected of type %a"
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
