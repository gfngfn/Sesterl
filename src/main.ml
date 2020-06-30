
open MyUtil
open Syntax
open Env


let main fpath_in fpath_out =
  try
    let inc = open_in fpath_in in
    let lexbuf = Lexing.from_channel inc in
    let (modident, utmod) = ParserInterface.process lexbuf in
    close_in inc;
    let ((_, sigr), binds) = Typechecker.main utmod in
    display_structure 0 sigr;
    let scode = OutputErlangCode.main modident binds in
    let outc = open_out fpath_out in
    output_string outc scode;
    close_out outc;
    Format.printf "output written on '%s'\n" fpath_out;
  with
  | Failure(msg) ->
      Format.printf "unsupported \"%s\"\n" msg

  | ParserInterface.Error(rng) ->
      Format.printf "%a: syntax error\n" Range.pp rng

  | UnidentifiedToken(rng, s) ->
      Format.printf "%a: unidentified token '%s'\n" Range.pp rng s

  | SeeEndOfFileInComment(rngL) ->
      Format.printf "%a: unclosed comment begins here\n" Range.pp rngL

  | SeeEndOfFileInStringLiteral(rngL) ->
      Format.printf "%a: unclosed string literal begins here\n" Range.pp rngL

  | ConflictInSignature(rng, x) ->
      Format.printf "%a: '%s' is already defined in the signature\n"
        Range.pp rng
        x

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
      Format.printf "%a: undefined type or type constructor '%s'\n"
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

  | Typechecker.InvalidByte(rng) ->
      Format.printf "%a: invalid byte\n"
        Range.pp rng

  | Typechecker.CyclicSynonymTypeDefinition(tyidents) ->
      Format.printf "cyclic type definitions:\n";
      tyidents |> List.iter (fun (rng, tynm) ->
        Format.printf "%s (%a)\n" tynm Range.pp rng
      )

  | Typechecker.UnboundModuleName(rng, modnm) ->
      Format.printf "%a: unbound module name '%s'\n"
        Range.pp rng
        modnm

  | Typechecker.NotOfStructureType(rng, modsig) ->
      Format.printf "%a: this module expression is not of a structure signature\n"
        Range.pp rng

  | Typechecker.NotOfFunctorType(rng, modsig) ->
      Format.printf "%a: this module expression is not of a functor signature\n"
        Range.pp rng

  | Typechecker.NotAStructureSignature(rng, modsig) ->
      Format.printf "%a: this signature expression is not a structure\n"
        Range.pp rng

  | Typechecker.NotAFunctorSignature(rng, modsig) ->
      Format.printf "%a: this signature expression is not a functor\n"
        Range.pp rng

  | Typechecker.UnboundSignatureName(rng, signm) ->
      Format.printf "%a: unbound signature name '%s'\n"
        Range.pp rng
        signm

  | Typechecker.CannotRestrictTransparentType(rng, _) ->
      Format.printf "%a: the specified type is already transparent\n"
        Range.pp rng

  | Typechecker.PolymorphicContradiction(rng, x, pty1, pty2) ->
      Format.printf "%a: not a subtype; as to value '%s', type %a cannot be encapsulated by type %a\n"
        Range.pp rng
        x
        pp_poly_type pty1
        pp_poly_type pty2

  | Typechecker.PolymorphicInclusion(rng, fid, pty1, pty2) ->
      Format.printf "%a: type %a is inconsistent with type %a as to type variable %a\n"
        Range.pp rng
        pp_poly_type pty1
        pp_poly_type pty2
        FreeID.pp fid

  | Typechecker.MissingRequiredValName(rng, x, pty) ->
      Format.printf "%a: missing required value '%s' of type %a\n"
        Range.pp rng
        x
        pp_poly_type pty

  | Typechecker.MissingRequiredTypeName(rng, tynm, (_, arity)) ->
      Format.printf "%a: missing required type name '%s' of arity %d\n"
        Range.pp rng
        tynm
        arity

  | Typechecker.MissingRequiredModuleName(rng, modnm, _modsign) ->
      Format.printf "%a: missing required module name '%s'\n"
        Range.pp rng
        modnm

  | Typechecker.MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      Format.printf "%a: missing required module name '%s'\n"
        Range.pp rng
        signm

  | Typechecker.NotASubtype(rng, modsig1, modsig2) ->
      Format.printf "%a: not a subtype (TODO: detailed explanation)\n"
        Range.pp rng

  | Typechecker.NotASubtypeTypeOpacity(rng, tynm, _tyopac1, _tyopac2) ->
      Format.printf "%a: not a subtype; type '%s' cannot be encapsulated (TODO: detailed explanation)\n"
        Range.pp rng
        tynm

  | Typechecker.NotASubtypeVariant(rng, vid1, vid2, ctor) ->
      Format.printf "%a: not a subtype about constructor '%s' (TODO: detailed explanation)\n"
        Range.pp rng
        ctor

  | InvalidIdentifier(rng, s) ->
      Format.printf "%a: invalid identifier '%s'\n"
        Range.pp rng
        s


let flag_output =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(required (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let arg_in =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let command_term : unit Cmdliner.Term.t =
  let open Cmdliner in
  Term.(const main $ arg_in $ flag_output)


let command_info : Cmdliner.Term.info =
  let open Cmdliner in
  Term.info
    ~version: "0.0.0"
    "sesterl"


let () =
  let open Cmdliner in
  Term.(exit (eval (command_term, command_info)))
