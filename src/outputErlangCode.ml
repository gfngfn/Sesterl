
open Syntax


let unit_atom = "ok"


let stringify_base_constant (bc : base_constant) =
  match bc with
  | Unit        -> unit_atom
  | Bool(true)  -> "true"
  | Bool(false) -> "false"
  | Int(n)      -> string_of_int n


let rec stringify_ast (ast : ast) =
  match ast with
  | IVar(name) ->
      OutputIdentifier.to_string name

  | IBaseConst(bc) ->
      stringify_base_constant bc

  | ILambda(recopt, names, ast0) ->
      let snames = names |> List.map OutputIdentifier.to_string in
      let s0 = stringify_ast ast0 in
      let srec =
        match recopt with
        | None          -> ""
        | Some(namerec) -> " " ^ OutputIdentifier.to_string namerec
      in
      Printf.sprintf "fun%s(%s) -> %s end" srec (String.concat ", " snames) s0

  | IApply(name, astargs) ->
      let sname = OutputIdentifier.to_string name in
      let sargs = astargs |> List.map stringify_ast in
      Printf.sprintf "%s(%s)" sname (String.concat ", " sargs)

  | ILetIn(name, ast1, ast2) ->
      let sname = OutputIdentifier.to_string name in
      let s1 = stringify_ast ast1 in
      let s2 = stringify_ast ast2 in
      Printf.sprintf "%s = %s, %s" sname s1 s2

  | ICase(ast0, branches) ->
      let s0 = stringify_ast ast0 in
      let sbrs = branches |> List.map stringify_branch in
      Printf.sprintf "case %s of %s end" s0 (String.concat "; " sbrs)

  | IReceive(branches) ->
      let sbrs = branches |> List.map stringify_branch in
      Printf.sprintf "receive %s end" (String.concat "; " sbrs)

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

and stringify_pattern (pat : pattern) =
  match pat with
  | IPUnit        -> unit_atom
  | IPBool(true)  -> "true"
  | IPBool(false) -> "false"
  | IPInt(n)      -> string_of_int n
  | IPVar(name)   -> OutputIdentifier.to_string name
  | IPWildCard    -> "_"


let stringify_declaration (decl : declaration) =
  match decl with
  | IValDecl(namefun, ast) ->
      begin
        match ast with
        | ILambda(None, nameargs, ast0) ->
            let sfun = OutputIdentifier.to_string namefun in
            let sargs = nameargs |> List.map OutputIdentifier.to_string in
            let s0 = stringify_ast ast0 in
            Printf.sprintf "%s(%s) -> %s." sfun (String.concat ", " sargs) s0

        | _ ->
            assert false
      end


let main (decls : declaration list) =
  decls |> List.map stringify_declaration |> String.concat "\n"
