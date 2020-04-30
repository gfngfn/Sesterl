
open Syntax


let unit_atom = "ok"


let stringify_base_constant (bc : base_constant) =
  match bc with
  | Unit        -> unit_atom
  | Bool(true)  -> "true"
  | Bool(false) -> "false"
  | Int(n)      -> string_of_int n


let output_normal name =
  match OutputIdentifier.output name with
  | OutputIdentifier.Normal(s) -> s
  | _                          -> assert false


let rec stringify_ast (ast : ast) =
  match ast with
  | IVar(name) ->
      output_normal name

  | IBaseConst(bc) ->
      stringify_base_constant bc

  | ILambda(recopt, names, ast0) ->
      let snames = names |> List.map output_normal in
      let s0 = stringify_ast ast0 in
      let srec =
        match recopt with
        | None          -> ""
        | Some(namerec) -> " " ^ output_normal namerec
      in
      Printf.sprintf "fun%s(%s) -> %s end" srec (String.concat ", " snames) s0

  | IApply(name, astargs) ->
      let sargs = astargs |> List.map stringify_ast in
      begin
        match (OutputIdentifier.output name, sargs) with
        | (Normal(sname), _) ->
            Printf.sprintf "%s(%s)" sname (String.concat ", " sargs)

        | (Operator(sop), [sarg1; sarg2]) ->
            Printf.sprintf "(%s %s %s)" sarg1 sop sarg2

        | _ ->
            assert false
      end

  | ILetIn(name, ast1, ast2) ->
      let sname = output_normal name in
      let s1 = stringify_ast ast1 in
      let s2 = stringify_ast ast2 in
      Printf.sprintf "begin %s = %s, %s end" sname s1 s2

  | ICase(ast1, [ IBranch(ipat, None, ast2) ]) ->
      let spat = stringify_pattern ipat in
      let s1 = stringify_ast ast1 in
      let s2 = stringify_ast ast2 in
      Printf.sprintf "begin %s = %s, %s end" spat s1 s2

  | ICase(ast0, branches) ->
      let s0 = stringify_ast ast0 in
      let sbrs = branches |> List.map stringify_branch in
      Printf.sprintf "case %s of %s end" s0 (String.concat "; " sbrs)

  | IReceive(branches) ->
      let sbrs = branches |> List.map stringify_branch in
      Printf.sprintf "receive %s end" (String.concat "; " sbrs)

  | ITuple(es) ->
      let ss = es |> TupleList.to_list |> List.map stringify_ast in
      Printf.sprintf "{%s}" (String.concat ", " ss)

  | IListNil ->
      "[]"

  | IListCons(e1, e2) ->
      let s1 = stringify_ast e1 in
      let s2 = stringify_ast e2 in
      Printf.sprintf "[%s | %s]" s1 s2


and stringify_branch (br : branch) =
  match br with
  | IBranch(pat, ast0opt, ast1) ->
      let spat = stringify_pattern pat in
      let swhen =
        match ast0opt with
        | None ->
            ""

        | Some(ast0) ->
            let s0 = stringify_ast ast0 in
            Printf.sprintf " when %s" s0
      in
      let s1 = stringify_ast ast1 in
      Printf.sprintf "%s%s -> %s" spat swhen s1


and stringify_pattern (ipat : pattern) =
  match ipat with
  | IPUnit        -> unit_atom
  | IPBool(true)  -> "true"
  | IPBool(false) -> "false"
  | IPInt(n)      -> string_of_int n
  | IPVar(name)   -> output_normal name
  | IPWildCard    -> "_"
  | IPListNil     -> "[]"

  | IPListCons(ipat1, ipat2) ->
      let s1 = stringify_pattern ipat1 in
      let s2 = stringify_pattern ipat2 in
      Printf.sprintf "[%s | %s]" s1 s2

  | IPTuple(ipats) ->
      let ss = ipats |> TupleList.to_list |> List.map stringify_pattern in
      Printf.sprintf "{%s}" (String.concat ", " ss)


let stringify_declaration (decl : declaration) =
  match decl with
  | IValDecl(namefun, ast) ->
      begin
        match ast with
        | ILambda(None, nameparams, ast0) ->
            let sfun = output_normal namefun in
            let sparams = nameparams |> List.map output_normal in
            let s0 = stringify_ast ast0 in
            Printf.sprintf "%s(%s) -> %s." sfun (String.concat ", " sparams) s0

        | _ ->
            assert false
      end


let main (decls : declaration list) =
  let sdecls = decls |> List.map stringify_declaration in
  let lines =
    List.append [
      "-module(autogen).";
      "-export([main/0]).";
      "thunk_return(X) -> fun() -> X end.";
      "thunk_spawn(X) -> fun() -> erlang:spawn(X) end.";
      "thunk_send(X, Y) -> fun() -> X ! Y, ok end.";
      "thunk_self() -> erlang:self().";
      "print_debug(X) -> io:format(\"~p~n\", [X]), ok.";
    ] sdecls
  in
  lines |> List.map (fun s -> s ^ "\n") |> String.concat ""
