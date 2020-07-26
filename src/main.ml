
open MyUtil
open Syntax
open Env

exception CyclicFileDependencyFound


let report_lexer_error (e : lexer_error) : unit =
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



let report_type_error (e : Typechecker.error) : unit =
  let open Typechecker in
  begin
    match e with
    | UnboundVariable(rng, x) ->
        Format.printf "%a: unbound variable '%s'\n" Range.pp rng x

    | ContradictionError(ty1, ty2) ->
        let (rng1, _) = ty1 in
        Format.printf "%a: this expression has type %a but is expected of type %a\n"
          Range.pp rng1
          pp_mono_type ty1
          pp_mono_type ty2

    | InclusionError(fid, ty1, ty2) ->
        let (rng1, _) = ty1 in
        Format.printf "%a: this expression has type %a and type %a at the same time, but these types are inconsistent as to the occurrence of type variable %a\n"
          Range.pp rng1
          pp_mono_type ty1
          pp_mono_type ty2
          FreeID.pp fid

    | BoundMoreThanOnceInPattern(rng, x) ->
        Format.printf "%a: this pattern binds '%s' more than once.\n"
          Range.pp rng
          x

    | UnboundTypeParameter(rng, tyvar) ->
        Format.printf "%a: unbound type variable '$%s'\n"
          Range.pp rng
          tyvar

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

    | InvalidByte(rng) ->
        Format.printf "%a: invalid byte\n"
          Range.pp rng

    | CyclicSynonymTypeDefinition(tyidents) ->
        Format.printf "cyclic type definitions:\n";
      tyidents |> List.iter (fun (rng, tynm) ->
        Format.printf "%s (%a)\n" tynm Range.pp rng
      )

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
          pp_poly_type pty1
          pp_poly_type pty2

    | PolymorphicInclusion(rng, fid, pty1, pty2) ->
        Format.printf "%a: type %a is inconsistent with type %a as to type variable %a\n"
          Range.pp rng
          pp_poly_type pty1
          pp_poly_type pty2
          FreeID.pp fid

    | MissingRequiredValName(rng, x, pty) ->
        Format.printf "%a: missing required value '%s' of type %a\n"
          Range.pp rng
          x
          pp_poly_type pty

    | MissingRequiredTypeName(rng, tynm, (_, arity)) ->
        Format.printf "%a: missing required type name '%s' of arity %d\n"
          Range.pp rng
          tynm
          arity

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

    | NotASubtypeSynonym(rng, _sid1, _sid2) ->
        Format.printf "%a: not a subtype type synonym (TODO: detailed explanation)\n"
          Range.pp rng

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
  end


let make_absolute_path (dir : absolute_dir) (fpath : string) : absolute_path =
  if Filename.is_relative fpath then
    Core.Filename.realpath (Filename.concat dir fpath)
  else
    fpath


let read_source (fpath_in : absolute_path) : absolute_path list * (module_name ranged * untyped_module) =
  let inc = open_in fpath_in in
  let lexbuf = Lexing.from_channel inc in
  let (deps_raw, modident, utmod) = ParserInterface.process lexbuf in
  let deps =
    let dir = Filename.dirname fpath_in in
    deps_raw |> List.map (make_absolute_path dir)
  in
  close_in inc;
  (deps, (modident, utmod))


module ContentMap = Map.Make(String)

type reading_state = {
  loaded : (module_name ranged * untyped_module) ContentMap.t;
  graph  : FileDependencyGraph.t;
}


let read_source_recursively (abspath : absolute_path) : (absolute_path * (module_name ranged * untyped_module)) list =
  let rec aux (state : reading_state) (vertex : FileDependencyGraph.vertex) (abspath : absolute_path) : reading_state =
    let (deps, content) = read_source abspath in
    let loaded = state.loaded |> ContentMap.add abspath content in
    deps |> List.fold_left (fun state abspath_sub ->
      let graph = state.graph in
      if graph |> FileDependencyGraph.mem abspath_sub then
        state
      else
        let () = Format.printf "######## '%s' ---> '%s'\n" abspath abspath_sub in
        let (graph, vertex_sub) = graph |> FileDependencyGraph.add_vertex abspath_sub in
        let graph = graph |> FileDependencyGraph.add_edge ~depending:vertex ~depended:vertex_sub in
        aux { state with graph = graph } vertex_sub abspath_sub
    ) { state with loaded = loaded }
  in
  let state =
    let (graph, vertex) = FileDependencyGraph.empty |> FileDependencyGraph.add_vertex abspath in
    let state = { graph = graph; loaded = ContentMap.empty } in
    aux state vertex abspath
  in
  match FileDependencyGraph.topological_sort state.graph with
  | None ->
      raise CyclicFileDependencyFound

  | Some(sources) ->
      sources |> List.map (fun abspath ->
        match state.loaded |> ContentMap.find_opt abspath with
        | None          -> assert false
        | Some(content) -> (abspath, content)
      )


let main (fpath_in : string) (dir_out : string) (is_verbose : bool) =
  try
    let abspath_in =
      let dir = Sys.getcwd () in
      make_absolute_path dir fpath_in
    in
    let sources = read_source_recursively abspath_in in
    let (_, outacc) =
      let (tyenv, _) = Primitives.initial_environment in
      sources |> List.fold_left (fun (tyenv, outacc) (abspath, (modident, utmod)) ->
        Format.printf "type checking '%s' ...\n" abspath;
        let (tyenv, (oidset, sigr), sname, binds) = Typechecker.main tyenv modident utmod in
        if is_verbose then display_structure 0 sigr;
        let outacc = Alist.extend outacc (sname, binds) in
        (tyenv, outacc)
      ) (tyenv, Alist.empty)
    in
    let (_, gmap) = Primitives.initial_environment in
    outacc |> Alist.to_list |> List.fold_left (fun gmap (sname, binds) ->
      OutputErlangCode.main dir_out gmap sname binds
    ) gmap |> ignore
  with
  | Sys_error(msg) ->
      Format.printf "system error: %s\n" msg;
      exit 1

  | Failure(msg) ->
      Format.printf "unsupported \"%s\"\n" msg;
      exit 1

  | LexerError(e) ->
      report_lexer_error e;
      exit 1

  | ParserInterface.Error(rng) ->
      Format.printf "%a: syntax error\n" Range.pp rng;
      exit 1

  | CyclicFileDependencyFound ->
      Format.printf "cyclic file dependency found (TODO: detailed explanation)\n";
      exit 1

  | ConflictInSignature(rng, x) ->
      Format.printf "%a: '%s' is already defined in the signature\n"
        Range.pp rng
        x;
      exit 1

  | Typechecker.Error(e) ->
      report_type_error e;
      exit 1


let flag_output : string Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Specify output path." in
  Arg.(required (opt (some string) None (info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)))


let flag_verbose : bool Cmdliner.Term.t =
  let open Cmdliner in
  let doc = "Makes reports more detailed." in
  Arg.(value (flag (info [ "verbose" ] ~doc)))


let arg_in : string Cmdliner.Term.t =
  let open Cmdliner in
  Arg.(required (pos 0 (some file) None (info [])))


let command_term : unit Cmdliner.Term.t =
  let open Cmdliner in
  Term.(const main $ arg_in $ flag_output $ flag_verbose)


let command_info : Cmdliner.Term.info =
  let open Cmdliner in
  Term.info
    ~version: "0.0.1"
    "sesterl"


let () =
  let open Cmdliner in
  Term.(exit (eval (command_term, command_info)))
