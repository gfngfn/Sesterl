
open MyUtil
open Syntax
open Errors


let warn_val_not_used (rng : Range.t) (x : identifier) =
  Format.printf "* [Warning] %a: variable '%s' is unused.\n"
    Range.pp rng
    x


let output_written (fpath : string) =
  Format.printf "  output written on '%s'.\n"
    fpath


let begin_to_parse (abspath : absolute_path) =
  Format.printf "  parsing '%s' ...\n"
    abspath


let begin_to_typecheck (abspath : absolute_path) =
  Format.printf "  type checking '%s' ...\n"
    abspath


let report_unsupported_feature (msg : string) =
  Format.printf "! [Unsupported] \"%s\"\n" msg


let report_system_error (msg : string) =
  Format.printf "! [Error] system error: %s\n" msg


let report_parser_error (rng : Range.t) =
  Format.printf "%a: syntax error\n" Range.pp rng


let report_lexer_error (e : lexer_error) : unit =
  Format.printf "! [Syntax error] ";
  match e with
  | UnidentifiedToken(rng, s) ->
      Format.printf "%a: unidentified token '%s'\n"
        Range.pp rng
        s

  | SeeEndOfFileInComment(rngL) ->
      Format.printf "%a: an unclosed comment begins here\n"
        Range.pp rngL

  | SeeEndOfFileInStringLiteral(rngL) ->
      Format.printf "%a: an unclosed string literal begins here\n"
        Range.pp rngL

  | SeeBreakInStringLiteral(rngL) ->
      Format.printf "%a: a string literal that contains a break begins here\n"
        Range.pp rngL

  | BlockClosedWithTooManyBackQuotes(rngR) ->
      Format.printf "%a: a string block ends with too many back quotes\n"
        Range.pp rngR

  | NotASingleCodePoint(rng) ->
      Format.printf "%a: not a single code point\n"
        Range.pp rng


let report_config_error (e : config_error) : unit =
  Format.printf "! [Build error] ";
  match e with
  | ConfigFileError(e) ->
      Format.printf "malformed config file; %a\n"
        YamlDecoder.pp_error e

  | CyclicFileDependencyFound(cycle) ->
      begin
        match cycle with
        | Loop(abspath) ->
            Format.printf "file '%s' is dependent on itself.\n"
              abspath

        | Cycle(abspaths) ->
            Format.printf "cyclic file dependency found among:\n";
            abspaths |> TupleList.to_list |> List.iter (fun abspath ->
              Format.printf "  - '%s'\n" abspath
            )
      end

  | MultipleModuleOfTheSameName(modnm, abspath1, abspath2) ->
      Format.printf "multiple module bound with the same name '%s':\n  - %s\n  - %s\n"
        modnm abspath1 abspath2

  | ModuleNotFound(rng, modnm) ->
      Format.printf "%a: module '%s' not found\n"
        Range.pp rng
        modnm

  | InvalidPackageName(s) ->
      Format.printf "invalid package name '%s'\n"
        s

  | CannotSpecifyDependency ->
      Format.printf "cannot specify dependency at standalone file\n"

  | MainModuleNotFound(pkgname, modnm) ->
      Format.printf "main module '%s' not found in package '%s'\n"
        modnm pkgname


let report_package_error (e : package_error) : unit =
  Format.printf "! [Build error] ";
  match e with
  | DuplicatedPackageName(pkgname, abspath1, abspath2) ->
      Format.printf "multiple package have the same name '%s':\n  - %s\n  - %s\n"
        pkgname abspath1 abspath2


let report_type_error (e : type_error) : unit =
  Format.printf "! [Type error] ";
  match e with
  | UnboundVariable(rng, x) ->
      Format.printf "%a: unbound variable '%s'\n" Range.pp rng x

  | ContradictionError(ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a but is expected of type %a\n"
        Range.pp rng1
        TypeConv.pp_mono_type ty1
        TypeConv.pp_mono_type ty2

  | InclusionError(fid, ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a and type %a at the same time, but these types are inconsistent as to the occurrence of type variable %a\n"
        Range.pp rng1
        TypeConv.pp_mono_type ty1
        TypeConv.pp_mono_type ty2
        FreeID.pp fid

  | InclusionRowError(frid, ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a and type %a at the same time, but these types are inconsistent as to the occurrence of row variable %a\n"
        Range.pp rng1
        TypeConv.pp_mono_type ty1
        TypeConv.pp_mono_type ty2
        FreeRowID.pp frid

  | BoundMoreThanOnceInPattern(rng, x) ->
      Format.printf "%a: this pattern binds '%s' more than once.\n"
        Range.pp rng
        x

  | UnboundTypeParameter(rng, tyvar) ->
      Format.printf "%a: unbound type variable '$%s'\n"
        Range.pp rng
        tyvar

  | UnboundRowParameter(rng, rowvar) ->
      Format.printf "%a: unbound row variable '?$%s'\n"
        Range.pp rng
        rowvar

  | UndefinedConstructor(rng, ctor) ->
      Format.printf "%a: undefined constructor '%s'\n"
        Range.pp rng
        ctor

  | InvalidNumberOfConstructorArguments(rng, ctor, len_expected, len_actual) ->
      Format.printf "%a: constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        Range.pp rng
        ctor
        len_expected
        len_actual

  | UndefinedTypeName(rng, tynm) ->
      Format.printf "%a: undefined type or type constructor '%s'\n"
        Range.pp rng
        tynm

  | UndefinedKindName(rng, kdnm) ->
      Format.printf "%a: undefined kind '%s'\n"
        Range.pp rng
        kdnm

  | InvalidNumberOfTypeArguments(rng, tynm, len_expected, len_actual) ->
      Format.printf "%a: type constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        Range.pp rng
        tynm
        len_expected
        len_actual

  | TypeParameterBoundMoreThanOnce(rng, tyvar) ->
      Format.printf "%a: type variable '%s' is bound more than once\n"
        Range.pp rng
        tyvar

  | RowParameterBoundMoreThanOnce(rng, rowvar) ->
      Format.printf "%a: row variable '%s' is bound more than once\n"
        Range.pp rng
        rowvar

  | InvalidByte(rng) ->
      Format.printf "%a: invalid byte\n"
        Range.pp rng

  | CyclicSynonymTypeDefinition(cycle) ->
      let tyidents =
        match cycle with
        | Loop((_, tyident)) -> [ tyident ]
        | Cycle(vs)          -> vs |> TupleList.to_list |> List.map (fun (_, tyident) -> tyident)
      in
      Format.printf "cyclic type definitions:\n";
      tyidents |> List.iter (fun (rng, tynm) ->
        Format.printf "  - %s (%a)\n" tynm Range.pp rng
      )

  | CyclicTypeParameter(rng, cycle, pty) ->
      let bbids =
        match cycle with
        | Loop(bbid)   -> [ bbid ]
        | Cycle(bbids) -> bbids |> TupleList.to_list
      in
      Format.printf "%a: cyclic type variables:\n"
        Range.pp rng;
      bbids |> List.iter (fun bbid ->
        Format.printf "  - %a\n"
          BoundBothID.pp bbid
      );
      Format.printf "  in:\n";
      let (sbids, sbrids, smain) = TypeConv.show_poly_type pty in
      let ss = List.append sbids sbrids in
      let sb = if List.length ss = 0 then "" else "<" ^ String.concat ", " ss ^ ">" in
      Format.printf "  %s %s\n" sb smain

  | UnboundModuleName(rng, modnm) ->
      Format.printf "%a: unbound module name '%s'\n"
        Range.pp rng
        modnm

  | NotOfStructureType(rng, modsig) ->
      Format.printf "%a: this module expression is not of a structure signature\n"
        Range.pp rng

  | NotOfFunctorType(rng, modsig) ->
      Format.printf "%a: this module expression is not of a functor signature\n"
        Range.pp rng

  | NotAStructureSignature(rng, modsig) ->
      Format.printf "%a: this signature expression is not a structure\n"
        Range.pp rng

  | NotAFunctorSignature(rng, modsig) ->
      Format.printf "%a: this signature expression is not a functor\n"
        Range.pp rng

  | UnboundSignatureName(rng, signm) ->
      Format.printf "%a: unbound signature name '%s'\n"
        Range.pp rng
        signm

  | CannotRestrictTransparentType(rng, _) ->
      Format.printf "%a: the specified type is already transparent\n"
        Range.pp rng

  | PolymorphicContradiction(rng, x, pty1, pty2) ->
      Format.printf "%a: not a subtype; as to value '%s', type %a cannot be encapsulated by type %a\n"
        Range.pp rng
        x
        TypeConv.pp_poly_type pty1
        TypeConv.pp_poly_type pty2

  | PolymorphicInclusion(rng, fid, pty1, pty2) ->
      Format.printf "%a: type %a is inconsistent with type %a as to type variable %a\n"
        Range.pp rng
        TypeConv.pp_poly_type pty1
        TypeConv.pp_poly_type pty2
        FreeID.pp fid

  | MissingRequiredValName(rng, x, pty) ->
      Format.printf "%a: missing required value '%s' of type %a\n"
        Range.pp rng
        x
        TypeConv.pp_poly_type pty

  | MissingRequiredTypeName(rng, tynm, (_, pkd)) ->
      Format.printf "%a: missing required type name '%s' of arity %d\n"
        Range.pp rng
        tynm
        (TypeConv.arity_of_kind pkd)

  | MissingRequiredModuleName(rng, modnm, _modsign) ->
      Format.printf "%a: missing required module name '%s'\n"
        Range.pp rng
        modnm

  | MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      Format.printf "%a: missing required module name '%s'\n"
        Range.pp rng
        signm

  | NotASubtype(rng, modsig1, modsig2) ->
      Format.printf "%a: not a subtype (TODO: detailed explanation)\n"
        Range.pp rng

  | NotASubtypeTypeOpacity(rng, tynm, _tyopac1, _tyopac2) ->
      Format.printf "%a: not a subtype; type '%s' cannot be encapsulated (TODO: detailed explanation)\n"
        Range.pp rng
        tynm

  | NotASubtypeVariant(rng, _vid1, _vid2, ctor) ->
      Format.printf "%a: not a subtype about constructor '%s' (TODO: detailed explanation)\n"
        Range.pp rng
        ctor

  | NotASubtypeSynonym(rng, sid1, sid2) ->
      let (bids1, pty1) = TypeDefinitionStore.find_synonym_type sid1 in
      let (bids2, pty2) = TypeDefinitionStore.find_synonym_type sid2 in
      Format.printf "%a: type %a is not a subtype of %a;\n  - %a<%a> = %a\n  - %a<%a> = %a\n"
        Range.pp rng
        TypeID.Synonym.pp sid1
        TypeID.Synonym.pp sid2
        TypeID.Synonym.pp sid1
        (Format.pp_print_list BoundID.pp) bids1
        TypeConv.pp_poly_type pty1
        TypeID.Synonym.pp sid2
        (Format.pp_print_list BoundID.pp) bids2
        TypeConv.pp_poly_type pty2

  | OpaqueIDExtrudesScopeViaValue(rng, _pty) ->
      Format.printf "%a: an abstract type extrudes its scope via value (TODO: detailed explanation)\n"
        Range.pp rng

  | OpaqueIDExtrudesScopeViaType(rng, _tyopac) ->
      Format.printf "%a: an abstract type extrudes its scope via type (TODO: detailed explanation)\n"
        Range.pp rng

  | OpaqueIDExtrudesScopeViaSignature(rng, _absmodsig) ->
      Format.printf "%a: an abstract type extrudes its scope via signature (TODO: detailed explanation)\n"
        Range.pp rng

  | SupportOnlyFirstOrderFunctor(rng) ->
      Format.printf "%a: only first-order functors are supported\n"
        Range.pp rng

  | RootModuleMustBeStructure(rng) ->
      Format.printf "%a: root modules must be structures\n"
        Range.pp rng

  | InvalidIdentifier(rng, s) ->
      Format.printf "%a: invalid identifier '%s'\n"
        Range.pp rng
        s

  | ConflictInSignature(rng, x) ->
      Format.printf "%a: '%s' is already defined in the signature\n"
        Range.pp rng
        x

  | DuplicatedLabel(rng, label) ->
      Format.printf "%a: label '%s' is used more than once in a binding\n"
        Range.pp rng
        label

  | NullaryFormatString(rng) ->
      Format.printf "%a: nullary format string\n"
        Range.pp rng

  | CannotFreezeNonGlobalName(rng, x) ->
      Format.printf "%a: cannot freeze non-top-level identifier '%s'\n"
        Range.pp rng
        x
