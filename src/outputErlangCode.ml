
open Syntax


let unit_atom = "ok"


let stringify_base_constant (bc : base_constant) =
  match bc with
  | Unit              -> unit_atom
  | Bool(true)        -> "true"
  | Bool(false)       -> "false"
  | Int(n)            -> string_of_int n
  | BinaryByString(s) -> Printf.sprintf "<<\"%s\">>" (String.escaped s)
  | BinaryByInts(ns)  -> Printf.sprintf "<<%s>>" (ns |> List.map string_of_int |> String.concat ", ")


let output_module_prefix = function
  | [] -> ""
  | ss -> String.concat "_" ss ^ ":"


let output_single = function
  | OutputIdentifier.Local(lname) ->
      OutputIdentifier.output_local lname

  | OutputIdentifier.Global(gname) ->
      let r = OutputIdentifier.output_global gname in
      let sparam =
        List.init r.arity (fun _ ->
          let lname = OutputIdentifier.fresh () in
          OutputIdentifier.output_local lname
        ) |> String.concat ", "
      in
      let smod = output_module_prefix r.module_names in
      let sfun = r.function_name in
      Printf.sprintf "(fun(%s) -> %s%s(%s) end)" sparam smod sfun sparam
        (*  Performs the eta expansion for global function names
            in order to avoid being confused with atoms.
            Note that we cannot simply use `(fun ?MODULE:F/A)` here
            because it is not valid for private functions. *)

  | OutputIdentifier.Operator(oname) ->
      let sop = OutputIdentifier.output_operator oname in
      let gensym () =
        let lname = OutputIdentifier.fresh () in
        OutputIdentifier.output_local lname
      in
      let s1 = gensym () in
      let s2 = gensym () in
      Printf.sprintf "(fun(%s, %s) -> %s %s %s end)" s1 s2 s1 sop s2


let rec stringify_ast (ast : ast) =
  match ast with
  | IVar(name) ->
      output_single name

  | IBaseConst(bc) ->
      stringify_base_constant bc

  | ILambda(recopt, names, ast0) ->
      let snames = names |> List.map OutputIdentifier.output_local in
      let s0 = stringify_ast ast0 in
      let srec =
        match recopt with
        | None          -> ""
        | Some(namerec) -> " " ^ OutputIdentifier.output_local namerec
      in
      Printf.sprintf "fun%s(%s) -> %s end" srec (String.concat ", " snames) s0

  | IApply(name, astargs) ->
      let sargs = astargs |> List.map stringify_ast in
      begin
        match (name, sargs) with
        | (OutputIdentifier.Local(lname), _) ->
            let s = OutputIdentifier.output_local lname in
            Printf.sprintf "%s(%s)" s (String.concat ", " sargs)

        | (OutputIdentifier.Global(gname), _) ->
            let r = OutputIdentifier.output_global gname in
            let smod = output_module_prefix r.module_names in
            let sfun = r.function_name in
            Printf.sprintf "%s%s(%s)" smod sfun (String.concat ", " sargs)

        | (OutputIdentifier.Operator(op), [sarg1; sarg2]) ->
            let sop = OutputIdentifier.output_operator op in
            Printf.sprintf "(%s %s %s)" sarg1 sop sarg2

        | _ ->
            assert false
      end

  | ILetIn(lname, ast1, ast2) ->
      let s0 = OutputIdentifier.output_local lname in
      let s1 = stringify_ast ast1 in
      let s2 = stringify_ast ast2 in
      Printf.sprintf "begin %s = %s, %s end" s0 s1 s2

  | ICase(ast1, [ IBranch(ipat, None, ast2) ]) ->
    (* -- slight optimization of case-expressions into pattern-matching let-expressions -- *)
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

  | IConstructor(ctorid, es) ->
      let sctor = ConstructorID.output ctorid in
      begin
        match es with
        | [] ->
            sctor

        | _ :: _ ->
            let ss = es |> List.map stringify_ast in
            Printf.sprintf "{%s, %s}" sctor (String.concat ", " ss)
      end


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
  | IPVar(lname)  -> OutputIdentifier.output_local lname
  | IPWildCard    -> "_"
  | IPListNil     -> "[]"

  | IPListCons(ipat1, ipat2) ->
      let s1 = stringify_pattern ipat1 in
      let s2 = stringify_pattern ipat2 in
      Printf.sprintf "[%s | %s]" s1 s2

  | IPTuple(ipats) ->
      let ss = ipats |> TupleList.to_list |> List.map stringify_pattern in
      Printf.sprintf "{%s}" (String.concat ", " ss)

  | IPConstructor(ctorid, ipats) ->
      let atom = ConstructorID.output ctorid in
      begin
        match ipats with
        | [] ->
            atom

        | _ :: _ ->
            let ss = ipats |> List.map stringify_pattern in
            Printf.sprintf "{%s, %s}" atom (String.concat ", " ss)
      end


let rec stringify_binding (ibind : binding) : string list =
  let val_single (_, gnamefun, _, ast) =
    match ast with
    | ILambda(None, nameparams, ast0) ->
        let r = OutputIdentifier.output_global gnamefun in
        let sparams = nameparams |> List.map OutputIdentifier.output_local in
        let s0 = stringify_ast ast0 in
        Printf.sprintf "%s(%s) -> %s." r.function_name (String.concat ", " sparams) s0

    | _ ->
        assert false
  in
  match ibind with
  | IBindVal(INonRec(valbind)) ->
      [ val_single valbind ]

  | IBindVal(IRec(valbinds)) ->
      valbinds |> List.map val_single

  | IBindModule(space, ibinds) ->
      let sspace = OutputIdentifier.output_space space in
      let ss = ibinds |> List.map stringify_binding |> List.concat in
      List.concat [
        [ Printf.sprintf "%% module %s = {" sspace ];
        ss |> List.map (fun s -> "% " ^ s);
        [ "% }" ];
      ]


let main (modname : string) (binds : binding list) : string =

  Format.printf "@[<v>%a@]" (Format.pp_print_list pp_binding) binds;

  let sbinds = binds |> List.map stringify_binding |> List.concat in
  let lines =
    List.append [
      Printf.sprintf "-module(%s)." modname;
      "-export([main/0]).";
      "thunk_return(X) -> fun() -> X end.";
      "thunk_spawn(X) -> fun() -> erlang:spawn(X) end.";
      "thunk_send(X, Y) -> fun() -> X ! Y, ok end.";
      "thunk_self() -> erlang:self().";
      "print_debug(X) -> io:format(\"~p~n\", [X]), ok.";
    ] sbinds
  in
  lines |> List.map (fun s -> s ^ "\n") |> String.concat ""
