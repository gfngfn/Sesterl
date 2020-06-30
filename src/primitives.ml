
open Syntax
open Env


let primitive_module_name =
  "sesterl_internal_prim"


let fresh_bound () =
  let bid = BoundID.fresh () in
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let dr = Range.dummy "primitives"
let u = (dr, BaseType(UnitType))
let b = (dr, BaseType(BoolType))
let i = (dr, BaseType(IntType))
let ( @-> ) tydoms tycod = (dr, FuncType(tydoms, tycod))
let eff tyrcv ty0 = (dr, EffType(Effect(tyrcv), ty0))
let pid tyrcv = (dr, PidType(Pid(tyrcv)))

let tylogic : poly_type = [b; b] @-> b
let tycomp  : poly_type = [i; i] @-> b
let tyarith : poly_type = [i; i] @-> i

let tyspawn : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvnew = fresh_bound () in
  [eff tyrecvnew u] @-> eff tyrecv (pid tyrecvnew)

let tysend : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvremote = fresh_bound () in
  [pid tyrecvremote; tyrecvremote] @-> eff tyrecv u

let tyreturn : poly_type =
  let tyrecv = fresh_bound () in
  let tyres = fresh_bound () in
  [tyres] @-> eff tyrecv tyres

let tyself : poly_type =
  let tyrecv = fresh_bound () in
  eff tyrecv (pid tyrecv)

let typrintdebug : poly_type =
  let typaram = fresh_bound () in
  [typaram] @-> u


type primitive_definition = {
  identifier  : string;
  typ         : poly_type;
  target_name : string;
  parameters  : string list;
  code        : string;
}


let primitive_definitions = [
  {
    identifier  = "spawn";
    typ         = tyspawn;
    target_name = "thunk_spawn";
    parameters  = ["X"];
    code        = "fun() -> erlang:spawn(X) end";
  };
  {
    identifier  = "send";
    typ         = tysend;
    target_name = "thunk_send";
    parameters  = ["X"; "Y"];
    code        = "fun() -> X ! Y, ok end";
  };
  {
    identifier  = "return";
    typ         = tyreturn;
    target_name = "thunk_return";
    parameters  = ["X"];
    code        = "fun() -> X end";
  };
  {
    identifier  = "self";
    typ         = tyself;
    target_name = "thunk_self";
    parameters  = [];
    code        = "erlang:self()";
  };
  {
    identifier  = "print_debug";
    typ         = typrintdebug;
    target_name = "print_debug";
    parameters  = ["X"];
    code        = "io:format(\"~p~n\", [X]), ok";
  };
]


let initial_environment =
  let add_operators (ops : (string * poly_type * string) list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    let tyenv =
      ops |> List.fold_left (fun tyenv (x, pty, target) ->
        let name = OutputIdentifier.Operator(OutputIdentifier.operator target) in
        tyenv |> Typeenv.add_val x pty name
      ) tyenv
    in
    (tyenv, gmap)
  in
  let add_primitives (prims : primitive_definition list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    prims |> List.fold_left (fun (tyenv, gmap) primdef ->
      let gname =
        let arity = List.length primdef.parameters in
        match OutputIdentifier.generate_global primdef.target_name arity with
        | None        -> assert false
        | Some(gname) -> gname
      in
      let tyenv = tyenv |> Typeenv.add_val primdef.identifier primdef.typ (OutputIdentifier.Global(gname)) in
      let gmap = gmap |> GlobalNameMap.add gname primitive_module_name in
      (tyenv, gmap)
    ) (tyenv, gmap)
  in

  (Typeenv.empty, GlobalNameMap.empty)
    |> add_operators [
      ("&&", tylogic, "and");
      ("||", tylogic, "or" );
      ("==", tycomp , "==" );
      ("<=", tycomp , "=<" );
      (">=", tycomp , ">=" );
      ("<" , tycomp , "<"  );
      (">" , tycomp , ">"  );
      ("*" , tyarith, "*"  );
      ("/" , tyarith, "div");
      ("+" , tyarith, "+"  );
      ("-" , tyarith, "-"  );
    ]
    |> add_primitives primitive_definitions
