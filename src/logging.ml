
open MyUtil
open Syntax
open Env
open Errors


let warn_val_not_used (rng : Range.t) (x : identifier) =
  Format.printf "* [Warning] %a: variable '%s' is unused\n"
    Range.pp rng
    x


let warn_invalid_attribute (warning : attribute_warning) =
  Format.printf "* [Warning] %a: tag '%s': %s\n"
    Range.pp warning.position
    warning.tag
    warning.message


let warn_old_config_file_name (fpath : string) =
  Format.printf "* [Warning] old config file name detected: please rename '%s' to 'sesterl.yaml'\n"
    fpath

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


let report_invalid_external_spec (s : string) =
  Format.printf "! [Error] invalid external spec: \"%s\"\n" s


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

  | UnknownEscapeSequence(rngL) ->
      Format.printf "%a: unknown escape sequence \n"
        Range.pp rngL


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
            abspaths |> List2.to_list |> List.iter (fun abspath ->
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

  | UnrecognizableExtension(ext) ->
      Format.printf "unrecognizable extension '%s' for a source file\n"
        ext

  | ConfigFileNotFound(abspath) ->
      Format.printf "config file '%s' not found\n"
        abspath

  | SourceFileDependsOnTestFile(mod_src, mod_test) ->
      Format.printf "source module '%s' depends on test module '%s'\n"
        mod_src
        mod_test

  | NoOutputSpecForSingleSource ->
      Format.printf "no output spec ('--output' or '-o') for single source file\n"

  | UnsupportedLanguageVersion(language_version) ->
      Format.printf "unsupported language version '%s' (the version of this compiler is '%s')\n"
        language_version
        Constants.semantic_version


let report_package_error (e : package_error) : unit =
  Format.printf "! [Build error] ";
  match e with
  | DuplicatedPackageName(pkgname, abspath1, abspath2) ->
      Format.printf "multiple package have the same name '%s':\n  - %s\n  - %s\n"
        pkgname abspath1 abspath2

  | PackageDirNotFound(absdir) ->
      Format.printf "package directory '%s' not found\n"
        absdir

  | NotFoundInExternalMap(pkgname, external_map) ->
      let knowns = external_map |> ExternalMap.bindings in
      Format.printf "package '%s' not found in:\n" pkgname;
      knowns |> List.iter (fun (name, path) ->
        Format.printf "  - %s (%s)\n" name path
      )


let pp_type_parameter_list dispmap ppf bids =
  match bids with
  | [] ->
      ()

  | _ :: _ ->
      let pp_bound_id ppf bid = Format.fprintf ppf "%s" (dispmap |> DisplayMap.find_bound_id bid) in
      let pp_sep ppf () = Format.fprintf ppf ", " in
      Format.fprintf ppf "<%a>" (Format.pp_print_list ~pp_sep pp_bound_id) bids


let make_display_map_from_mono_types =
  DisplayMap.empty |> List.fold_left (fun dispmap ty -> dispmap |> TypeConv.collect_ids_mono ty)


let make_display_map_from_poly_types =
  DisplayMap.empty |> List.fold_left (fun dispmap pty -> dispmap |> TypeConv.collect_ids_poly pty)


let print_free_rows_and_base_kinds (dispmap : DisplayMap.t) =
  let row_names =
    dispmap |> DisplayMap.fold_free_row_id (fun frid row_name acc ->
      let labset = KindStore.get_free_row frid in
      let s = labset |> LabelSet.elements |> String.concat ", " in
      Alist.extend acc (row_name, s)
    ) Alist.empty |> Alist.to_list
  in
  match row_names with
  | [] ->
      ()

  | _ :: _ ->
      Format.printf "  where\n";
      row_names |> List.iter (fun (row_name, skd) ->
        Format.printf "  - %s :: (%s)\n" row_name skd
      )


let print_bound_ids (ss : string list) =
  match ss with
  | [] ->
      ()

  | _ :: _ ->
      Format.printf "  where\n";
      ss |> List.iter (fun s ->
        Format.printf "  - %s\n" s
      )


let report_unification_error ~actual:(ty1 : mono_type) ~expected:(ty2 : mono_type) (e : unification_error) : unit =
  match e with
  | Contradiction ->
      let dispmap = make_display_map_from_mono_types [ty1; ty2] in
      let (rng1, _) = ty1 in
      Format.printf "%a:\n"
        Range.pp rng1;
      Format.printf "  this expression has type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty1;
      Format.printf "  but is expected of type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty2;
      print_free_rows_and_base_kinds dispmap

  | Inclusion(fid) ->
      let dispmap = make_display_map_from_mono_types [ty1; ty2] in
      let (rng1, _) = ty1 in
      Format.printf "%a:"
        Range.pp rng1;
      Format.printf "  this expression has type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty1;
      Format.printf "  and type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty2;
      Format.printf "  at the same time, but these types are inconsistent as to the occurrence of type variable %s\n"
        (dispmap |> DisplayMap.find_free_id fid);
      print_free_rows_and_base_kinds dispmap

  | InclusionRow(frid) ->
      let dispmap = make_display_map_from_mono_types [ty1; ty2] in
      let (rng1, _) = ty1 in
      Format.printf "%a:\n"
        Range.pp rng1;
      Format.printf "  this expression has type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty1;
      Format.printf "  and type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty2;
      Format.printf "  at the same time, but these types are inconsistent as to the occurrence of row variable %s\n"
        (dispmap |> DisplayMap.find_free_row_id frid);
      print_free_rows_and_base_kinds dispmap

  | InsufficientRowConstraint(r) ->
      let dispmap = make_display_map_from_mono_types [ty1; ty2] in
      let (rng1, _) = ty1 in
      Format.printf "%a:\n"
        Range.pp rng1;
      Format.printf "  this expression has type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty1;
      Format.printf "  but is expected of type\n";
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty2;
      print_free_rows_and_base_kinds dispmap;
      Format.printf "  The row parameter %a is specified so that it does not contain the following label(s):\n"
        MustBeBoundRowID.pp_rich r.id;
      Format.printf "    %s\n"
        (r.given |> LabelSet.elements |> String.concat ", ");
      Format.printf "  but the following label(s) should also be specified:\n";
      Format.printf "    %s\n"
        (r.required |> LabelSet.elements |> String.concat ", ")


let report_type_error (e : type_error) : unit =
  Format.printf "! [Type error] ";
  match e with
  | UnboundVariable(rng, x) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  unbound variable '%s'\n"
        x

  | UnificationError(r) ->
      report_unification_error ~actual:r.actual ~expected:r.expected r.detail

  | BoundMoreThanOnceInPattern(rng, x) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  this pattern binds '%s' more than once.\n"
        x

  | UnboundTypeParameter(rng, tyvar) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  unbound type variable '$%s'\n"
        tyvar

  | UnboundRowParameter(rng, rowvar) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  unbound row variable '?$%s'\n"
        rowvar

  | UndefinedConstructor(rng, ctor) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  undefined constructor '%s'\n"
        ctor

  | InvalidNumberOfConstructorArguments(rng, ctor, len_expected, len_actual) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        ctor
        len_expected
        len_actual

  | UndefinedTypeName(rng, tynm) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  undefined type or type constructor '%s'\n"
        tynm

  | UndefinedKindName(rng, kdnm) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  undefined kind '%s'\n"
        kdnm

  | InvalidNumberOfTypeArguments(rng, tynm, len_expected, len_actual) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  type constructor '%s' expects %d argument(s), but is here applied to %d argument(s)\n"
        tynm
        len_expected
        len_actual

  | KindContradiction(rng, tynm, kd_expected, kd_actual) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  type constructor '%s' has kind %s, but is expected of kind %s\n"
        tynm
        (TypeConv.show_kind kd_actual)
        (TypeConv.show_kind kd_expected)

  | TypeParameterBoundMoreThanOnce(rng, tyvar) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  type variable '%s' is bound more than once\n"
        tyvar

  | RowParameterBoundMoreThanOnce(rng, rowvar) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  row variable '%s' is bound more than once\n"
        rowvar

  | InvalidByte(rng) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  invalid byte\n"

  | CyclicSynonymTypeDefinition(cycle) ->
      let tyidents =
        match cycle with
        | Loop(tyident)   -> [ tyident ]
        | Cycle(tyidents) -> tyidents |> List2.to_list
      in
      Format.printf "cyclic type definitions:\n";
      tyidents |> List.iter (fun (rng, tynm) ->
        Format.printf "  - %s (%a)\n" tynm Range.pp rng
      )

  | CyclicTypeParameter(rng, cycle, pty) ->
      let dispmap = make_display_map_from_poly_types [pty] in
      let bbids =
        match cycle with
        | Loop(bbid)   -> [ bbid ]
        | Cycle(bbids) -> bbids |> List2.to_list
      in
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  cyclic type variables:\n";
      bbids |> List.iter (fun bbid ->
        let sb =
          match bbid with
          | BoundBothID.Type(bid) -> dispmap |> DisplayMap.find_bound_id bid
          | BoundBothID.Row(brid) -> dispmap |> DisplayMap.find_bound_row_id brid
        in
        Format.printf "  - %s\n"
          sb
      );
      Format.printf "  in:\n";
      let (sbids, sbrids, smain) = TypeConv.show_poly_type dispmap pty in
      let ss = List.append sbids sbrids in
      let sb = if List.length ss = 0 then "" else "<" ^ String.concat ", " ss ^ ">" in
      Format.printf "  %s %s\n" sb smain

  | UnboundModuleName(rng, modnm) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  unbound module name '%s'\n"
        modnm

  | NotOfStructureType(rng, modsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  this module expression is not of a structure signature\n"

  | NotOfFunctorType(rng, modsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  this module expression is not of a functor signature\n"

  | NotAStructureSignature(rng, modsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  this signature expression is not a structure\n"

  | NotAFunctorSignature(rng, modsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  this signature expression is not a functor\n"

  | UnboundSignatureName(rng, signm) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  unbound signature name '%s'\n"
        signm

  | CannotRestrictTransparentType(rng, tynm, _) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  the specified type '%s' is already transparent\n"
        tynm

  | PolymorphicContradiction(rng, x, pty1, pty2) ->
      let dispmap = make_display_map_from_poly_types [pty1; pty2] in
      let (sbids1, sbrids1, smain1) = TypeConv.show_poly_type dispmap pty1 in
      let (sbids2, sbrids2, smain2) = TypeConv.show_poly_type dispmap pty2 in
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  as to value '%s', type\n"
        x;
      Format.printf "    %s\n"
        smain1;
      Format.printf "  is not a subtype of\n";
      Format.printf "    %s\n"
        smain2;
      print_bound_ids (List.concat [sbids1; sbrids1; sbids2; sbrids2])

  | PolymorphicInclusion(rng, fid, pty1, pty2) ->
      let dispmap = make_display_map_from_poly_types [pty1; pty2] in
      let (sbids1, sbrids1, smain1) = TypeConv.show_poly_type dispmap pty1 in
      let (sbids2, sbrids2, smain2) = TypeConv.show_poly_type dispmap pty2 in
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  type\n";
      Format.printf "    %s\n"
        smain1;
      Format.printf " is inconsistent with type\n";
      Format.printf "    %s\n"
        smain2;
      Format.printf "  as to type variable %s\n"
        (dispmap |> DisplayMap.find_free_id fid);
      print_bound_ids (List.concat [sbids1; sbrids1; sbids2; sbrids2])

  | MissingRequiredValName(rng, x, pty) ->
      let dispmap = make_display_map_from_poly_types [pty] in
      let (sbids, sbrids, smain) = TypeConv.show_poly_type dispmap pty in
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  missing required value '%s' of type\n"
        x;
      Format.printf "    %s\n"
        smain;
      print_bound_ids (List.concat [sbids; sbrids])

  | MissingRequiredConstructorName(rng, ctornm, _centry) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  missing required constructor '%s'\n"
        ctornm

  | MissingRequiredTypeName(rng, tynm, tentry) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  missing required type name '%s' of arity %d\n"
        tynm
        (TypeConv.arity_of_kind tentry.type_kind)

  | MissingRequiredModuleName(rng, modnm, _modsign) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  missing required module name '%s'\n"
        modnm

  | MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  missing required module name '%s'\n"
        signm

  | NotASubtype(rng, modsig1, modsig2) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  not a subtype (TODO: detailed explanation)\n"

  | NotASubtypeTypeDefinition(rng, tynm, _tentry1, _tentry2) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  not a subtype; type '%s' cannot be encapsulated (TODO: detailed explanation)\n"
        tynm

  | NotASubtypeConstructorDefinition(rng, ctornm, _centry1, _centry2) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  not a subtype; constructor '%s' cannot be encapsulated (TODO: detailed explanation)\n"
        ctornm

  | NotASubtypeVariant(rng, _vid1, _vid2, ctor) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  not a subtype about constructor '%s' (TODO: detailed explanation)\n"
        ctor

  | OpaqueIDExtrudesScopeViaValue(rng, _pty) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  an abstract type extrudes its scope via value (TODO: detailed explanation)\n"

  | OpaqueIDExtrudesScopeViaType(rng, _tyopac) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  an abstract type extrudes its scope via type (TODO: detailed explanation)\n"

  | OpaqueIDExtrudesScopeViaSignature(rng, _absmodsig) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  an abstract type extrudes its scope via signature (TODO: detailed explanation)\n"

  | SupportOnlyFirstOrderFunctor(rng) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  only first-order functors are supported\n"

  | RootModuleMustBeStructure(rng) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  root modules must be structures\n"

  | InvalidIdentifier(rng, s) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  invalid identifier '%s'\n"
        s

  | ConflictInSignature(rng, x) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  '%s' is already defined in the signature\n"
        x

  | DuplicatedLabel(rng, label) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  label '%s' is used more than once in a binding\n"
        label

  | BadArityOfOrderedArguments(info) ->
      Format.printf "%a:\n"
        Range.pp info.range;
      Format.printf "  the function expects %d ordered argument(s), but is applied to %d ordered argument(s) here\n"
        info.expected
        info.got

  | MissingMandatoryLabel(info) ->
      let ty = info.typ in
      let dispmap = make_display_map_from_mono_types [ty] in
      Format.printf "%a:\n"
        Range.pp info.range;
      Format.printf "  missing mandatory label '-%s' with an argument of type\n"
        info.label;
      Format.printf "    %a\n"
        (TypeConv.pp_mono_type dispmap) ty;
      print_free_rows_and_base_kinds dispmap

  | UnexpectedMandatoryLabel(info) ->
      Format.printf "%a:\n"
        Range.pp info.range;
      Format.printf "  unexpected mandatory label '-%s'\n"
        info.label

  | UnexpectedOptionalLabel(info) ->
      Format.printf "%a:\n"
        Range.pp info.range;
      Format.printf "  unexpected optional label '?%s'\n"
        info.label

  | NullaryFormatString(rng) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  nullary format string\n"

  | CannotFreezeNonGlobalName(rng, x) ->
      Format.printf "%a:\n"
        Range.pp rng;
      Format.printf "  cannot freeze non-top-level identifier '%s'\n"
        x
