
open Syntax
open Env


let primitive_module_name =
  "sesterl_internal_prim"


let fresh_bound () =
  let bid = BoundID.fresh () in
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let initial_environment =
  let dr = Range.dummy "primitives" in
  let u = (dr, BaseType(UnitType)) in
  let b = (dr, BaseType(BoolType)) in
  let i = (dr, BaseType(IntType)) in
  let ( @-> ) tydoms tycod = (dr, FuncType(tydoms, tycod)) in
  let eff tyrcv ty0 = (dr, EffType(Effect(tyrcv), ty0)) in
  let pid tyrcv = (dr, PidType(Pid(tyrcv))) in

  let tylogic : poly_type = [b; b] @-> b in
  let tycomp  : poly_type = [i; i] @-> b in
  let tyarith : poly_type = [i; i] @-> i in
  let tyspawn : poly_type =
    let tyrecv = fresh_bound () in
    let tyrecvnew = fresh_bound () in
    [eff tyrecvnew u] @-> eff tyrecv (pid tyrecvnew)
  in
  let tysend : poly_type =
    let tyrecv = fresh_bound () in
    let tyrecvremote = fresh_bound () in
    [pid tyrecvremote; tyrecvremote] @-> eff tyrecv u
  in
  let tyreturn : poly_type =
    let tyrecv = fresh_bound () in
    let tyres = fresh_bound () in
    [tyres] @-> eff tyrecv tyres
  in
  let tyself : poly_type =
    let tyrecv = fresh_bound () in
    eff tyrecv (pid tyrecv)
  in

  let typrintdebug : poly_type =
    let typaram = fresh_bound () in
    [typaram] @-> u
  in

  let add_operators (ops : (string * poly_type * string) list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    let tyenv =
      ops |> List.fold_left (fun tyenv (x, pty, target) ->
        let name = OutputIdentifier.Operator(OutputIdentifier.operator target) in
        tyenv |> Typeenv.add_val x pty name
      ) tyenv
    in
    (tyenv, gmap)
  in
  let add_primitives (prims : (string * poly_type * string * int) list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    prims |> List.fold_left (fun (tyenv, gmap) (x, pty, target, arity) ->
      let gname =
        match OutputIdentifier.generate_global target arity with
        | None        -> assert false
        | Some(gname) -> gname
      in
      let tyenv = tyenv |> Typeenv.add_val x pty (OutputIdentifier.Global(gname)) in
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
    |> add_primitives [
      ("spawn" , tyspawn , "thunk_spawn" , 1);
      ("send"  , tysend  , "thunk_send"  , 2);
      ("return", tyreturn, "thunk_return", 1);
      ("self"  , tyself  , "thunk_self"  , 0);

      ("print_debug", typrintdebug, "print_debug", 1);
    ]
