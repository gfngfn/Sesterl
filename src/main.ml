
open MyUtil
open Syntax


let pp_comma ppf () =
  Format.fprintf ppf ", "


let stringify_opaque_id_set oidset =
  OpaqueIDSet.fold (fun oid acc ->
    Alist.extend acc (Format.asprintf "%a" TypeID.Opaque.pp oid)
  ) oidset Alist.empty |> Alist.to_list |> List.map (fun s -> " " ^ s) |> String.concat ","


let rec display_signature (depth : int) (modsig : module_signature) : unit =
  let indent = String.make (depth * 2) ' ' in
  match modsig with
  | ConcStructure(sigr) ->
      Format.printf "%ssig\n" indent;
      display_structure (depth + 1) sigr;
      Format.printf "%send\n" indent

  | ConcFunctor(oidset1, modsigdom, (oidset2, modsigcod)) ->
      let sx1 = stringify_opaque_id_set oidset1 in
      let sx2 = stringify_opaque_id_set oidset2 in
      Format.printf "%s(forall%s) fun(\n" indent sx1;
      display_signature (depth + 1) modsigdom;
      Format.printf "%s) -> (exists%s)\n" indent sx2;
      display_signature (depth + 1) modsigcod


and display_structure (depth : int) (sigr : SigRecord.t) : unit =
  let indent = String.make (depth * 2) ' ' in
  sigr |> SigRecord.fold
      ~v:(fun x (pty, _) () ->
        Format.printf "%sval %s: %a\n" indent x pp_poly_type pty
      )
      ~t:(fun tynm tyopac () ->
        let (tyid, arity) = tyopac in
        match tyid with
        | TypeID.Synonym(sid) ->
            let (typarams, ptyreal) = TypeSynonymStore.find_synonym_type sid in
            Format.printf "%stype %a<%a> = %a\n"
              indent
              TypeID.Synonym.pp sid
              (Format.pp_print_list ~pp_sep:pp_comma BoundID.pp) typarams
              pp_poly_type ptyreal

        | TypeID.Variant(vid) ->
            let (typarams, _ctorbrs) = TypeSynonymStore.find_variant_type vid in
            Format.printf "%stype %a<%a> = (variant)\n"
              indent
              TypeID.Variant.pp vid
              (Format.pp_print_list ~pp_sep:pp_comma BoundID.pp) typarams

        | TypeID.Opaque(oid) ->
            Format.printf "%stype %a:: %d\n"
              indent
              TypeID.Opaque.pp oid
              arity
      )
      ~m:(fun modnm (modsig, _) () ->
        Format.printf "%smodule %s:\n" indent modnm;
        display_signature (depth + 1) modsig;
      )
      ~s:(fun signm _ () ->
        Format.printf "signature %s\n" signm
      )
      ~c:(fun ctornm _ () ->
        Format.printf "constructor %s\n" ctornm
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

  | Typechecker.MissingRequiredConstructor(rng, ctornm, _) ->
      Format.printf "%a: missing required constructor '%s'\n"
        Range.pp rng
        ctornm

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

  | Typechecker.MismatchedNumberOfConstructorParameters(rng, ctornm, ctorentry1, ctorentry2) ->
      Format.printf "%a: not a subtype about constructor '%s' (TODO: detailed explanation)\n"
        Range.pp rng
        ctornm


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
