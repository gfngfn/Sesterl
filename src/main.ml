
open Syntax


let rec display_signature (depth : int) (modsig : module_signature) : unit =
  let indent = String.make (depth * 2) ' ' in
  match modsig with
  | ConcStructure(sigr) ->
      Format.printf "%ssig\n" indent;
      display_structure (depth + 1) sigr;
      Format.printf "%send\n" indent

  | ConcFunctor(_oidset, _modsigdom, _absmodsigcod) ->
      Format.printf "%s: fun ...\n" indent


and display_structure (depth : int) (sigr : SigRecord.t) : unit =
  let indent = String.make (depth * 2) ' ' in
  sigr |> SigRecord.fold
      ~v:(fun x (pty, _) () ->
        Format.printf "%sval %s : %a\n" indent x pp_poly_type pty
      )
      ~t:(fun tynm tyopacity () ->
        match tyopacity with
        | Transparent(typarams, ISynonym(_sid, ptyreal)) ->
            Format.printf "%stype %s<%a> = %a\n"
              indent
              tynm
              (Format.pp_print_list BoundID.pp) typarams
              pp_poly_type ptyreal

        | Transparent(typarams, IVariant(_vid, _ctorbrs)) ->
            Format.printf "%stype %s<%a> = (variant)\n"
              indent
              tynm
              (Format.pp_print_list BoundID.pp) typarams

        | Opaque(kind, _) ->
            Format.printf "%stype %s :: %d\n" indent tynm kind
      )
      ~m:(fun modnm (modsig, _) () ->
        Format.printf "%smodule %s :\n" indent modnm;
        display_signature (depth + 1) modsig;
      )
      ~s:(fun signm _ () ->
        Format.printf "signature %s\n" signm
      )
      ()


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
