
open Syntax

let fresh_bound () =
  let bid = BoundID.fresh () in
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let initial_type_environment =
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

  let op = OutputIdentifier.global_operator in
  let normal = OutputIdentifier.global in
  List.fold_left (fun tyenv (x, ty, name) ->
    tyenv |> Typeenv.add x ty name
  ) Typeenv.empty [
    ("&&", tylogic, op "and");
    ("||", tylogic, op "or" );
    ("==", tycomp , op "==" );
    ("<=", tycomp , op "=<" );
    (">=", tycomp , op ">=" );
    ("<" , tycomp , op "<"  );
    (">" , tycomp , op ">"  );
    ("*" , tyarith, op "*"  );
    ("/" , tyarith, op "/"  );
    ("+" , tyarith, op "+"  );
    ("-" , tyarith, op "-"  );

    ("spawn" , tyspawn , normal "thunk_spawn" );
    ("send"  , tysend  , normal "thunk_send"  );
    ("return", tyreturn, normal "thunk_return");
    ("self"  , tyself  , normal "thunk_self"  );

    ("print_debug", typrintdebug, normal "print_debug");
  ]
