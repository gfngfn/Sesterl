
open Syntax


let main fpath_in fpath_out =
  try
    let inc = open_in fpath_in in
    let lexbuf = Lexing.from_channel inc in
    let utdecls = ParserInterface.process lexbuf in
    close_in inc;
    let ((_, sigr), decls) = Typechecker.main utdecls in
    display_structure 0 sigr;
    let scode =
      let modname =
        Filename.remove_extension (Filename.basename fpath_out)
      in
      OutputErlangCode.main modname decls
    in
    let outc = open_out fpath_out in
    output_string outc scode;
    close_out outc;
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

  | Typechecker.CannotRestrictTransparentType(rng, typarams, syn_or_vnt) ->
      Format.printf "%a: the specified type is already transparent\n"
        Range.pp rng


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
