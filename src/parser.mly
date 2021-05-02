%{
  open Syntax
  open MyUtil

  type 'a range_spec =
    | Token of Range.t
    | Ranged of (Range.t * 'a)

  let make_range rs1 rs2 =
    let aux = function
      | Token(rng)       -> rng
      | Ranged((rng, _)) -> rng
    in
    let rng1 = aux rs1 in
    let rng2 = aux rs2 in
      Range.unite rng1 rng2


  let chop_last modchain =
    let (uident, uidents) = modchain in
    let (tokL, _) = uident in
    let (modidents, ctor) =
      match List.rev (uident :: uidents) with
      | []                   -> assert false
      | ctor :: revmodidents -> (List.rev revmodidents, ctor)
    in
    (tokL, modidents, ctor)


  let fold_module_chain modchainraw =
    let (modident, projs) = modchainraw in
    let utmod =
      let (rng, modnm) = modident in
      (rng, ModVar(modnm))
    in
    projs |> List.fold_left (fun utmod proj ->
      let rng = make_range (Ranged(utmod)) (Ranged(proj)) in
      (rng, ModProjMod(utmod, proj))
    ) utmod


  let binary e1 op e2 =
    let rng = make_range (Ranged(e1)) (Ranged(e2)) in
    let (rngop, vop) = op in
    (rng, Apply((rngop, Var(vop)), ([e1; e2], [], [])))

(*
  let syntax_sugar_module_application : Range.t -> untyped_module -> untyped_module -> untyped_module =
    let fresh =
      let r = ref 0 in
      (fun () -> incr r; Printf.sprintf "SesterlInternalModule%d" !r)
    in
    (* TODO: sophisticate how to generate dummy module identifiers *)
    fun rng utmod1 utmod2 ->
    let modident1 = (Range.dummy "appident1", fresh ()) in
    let modident2 = (Range.dummy "appident2", fresh ()) in
    let modidentA = (Range.dummy "appidentA", fresh ()) in
    let utbinds =
      [
        (Range.dummy "appbind1", BindModule(modident1, utmod1));
        (Range.dummy "appbind2", BindModule(modident2, utmod2));
        (Range.dummy "appbindA", BindModule(modidentA, (Range.dummy "appA", ModApply(modident1, modident2))));
      ]
    in
    (rng, ModProjMod((Range.dummy "appB", ModBinds(utbinds)), modidentA))
*)

  let base_kind_o =
    (Range.dummy "base_kind_o", MKindName("o"))
      (* TODO: fix such an ad-hoc insertion of kinds *)


  let decl_type_transparent tokL tybinds : untyped_declaration =
    let rng = Range.dummy "decl_type_transparent" in  (* TODO: give appropriate code ranges *)
    let dr = Range.dummy "decl_type_transparent" in
    let decls : untyped_declaration list =
      tybinds |> List.map (fun (tyident, tyvars, syn_or_vnt) ->
        let mnbkddoms =
          tyvars |> List.map (function
          | (_, None)        -> base_kind_o
          | (_, Some(mnbkd)) -> mnbkd
          )
        in
        let mnkd = (dr, MKind(mnbkddoms, base_kind_o)) in
        (dr, DeclTypeOpaque(tyident, Some(mnkd)))
      )
    in
    (rng, DeclInclude((dr, SigWith((dr, SigDecls(decls)), [], tybinds))))
%}

%token<Range.t> LET REC AND IN LAMBDA IF THEN ELSE TRUE FALSE DO RECEIVE ACT END CASE OF TYPE VAL MODULE STRUCT SIGNATURE SIG WITH EXTERNAL INCLUDE IMPORT FREEZE PACK
%token<Range.t> LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE
%token<Range.t> DEFEQ COMMA ARROW REVARROW BAR UNDERSCORE CONS COLON COERCE
%token<Range.t> GT_SPACES GT_NOSPACE LTLT LT_EXACT
%token<Range.t * string> LOWER DOTLOWER UPPER DOTUPPER TYPARAM ROWPARAM MNDLABEL OPTLABEL
%token<Range.t * string> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * int> INT
%token<Range.t * float> FLOAT
%token<Range.t * string> BINARY STRING STRING_BLOCK
%token<Range.t * Syntax.format_element list> FORMAT
%token<Range.t * Uchar.t> CHAR
%token EOI

%start main
%type<Syntax.untyped_binding> bindtop
%type<(Syntax.module_name Syntax.ranged) list * Syntax.module_name Syntax.ranged * Syntax.untyped_signature option * Syntax.untyped_module> main
%type<Syntax.manual_type> ty
%type<Syntax.binder list * (Syntax.labeled_binder list * Syntax.labeled_optional_binder list)> params
%type<Syntax.labeled_binder list * Syntax.labeled_optional_binder list> labparams
%type<Syntax.labeled_optional_binder list> optparams
%type<Syntax.untyped_ast list * (Syntax.labeled_untyped_ast list * Syntax.labeled_untyped_ast list)> args
%type<Syntax.labeled_untyped_ast list * Syntax.labeled_untyped_ast list> labargs
%type<Syntax.labeled_untyped_ast list> optargs
%type<Syntax.manual_type list * (Syntax.labeled_manual_type list * Syntax.manual_row)> tydoms
%type<Syntax.labeled_manual_type list * Syntax.manual_row> labtydoms
%type<Syntax.manual_row> opttydoms
%type<Syntax.labeled_manual_type list> opttydomsfixed
%type<Syntax.type_variable_binder list * ((Range.t * Syntax.row_variable_name) * Syntax.labeled_manual_type list) list> typarams
%type<((Range.t * Syntax.row_variable_name) * Syntax.labeled_manual_type list) list> rowparams
%type<Syntax.untyped_let_binding> bindvalsingle
%type<Range.t * Syntax.internal_or_external> bindvaltop
%type<Syntax.rec_or_nonrec> bindvallocal
%type<Syntax.untyped_module> modexprbot

%%
main:
  | deps=list(dep); bindmod=bindmod; EOI {
      let (_, modident, utsigopt, utmod) = bindmod in
      (deps, modident, utsigopt, utmod)
    }
;
dep:
  | IMPORT; modident=UPPER { modident }
;
ident:
  | ident=LOWER { ident }
;
bindtop:
  | TYPE; tybind=bindtypesingle; tybinds=list(bindtypesub) {
      let rng = Range.dummy "bindtop-1" in  (* TODO: give appropriate code range *)
      (rng, BindType(tybind :: tybinds))
    }
  | bindval=bindvaltop {
      let rng = Range.dummy "bindtop-1" in  (* TODO: give appropriate code range *)
      let (_, valbinding) = bindval in
      (rng, BindVal(valbinding))
    }
  | bindmod=bindmod {
      let (rng, modident, utsigopt, utmod) = bindmod in
      (rng, BindModule(modident, utsigopt, utmod))
    }
  | tokL=SIGNATURE; sigident=UPPER; DEFEQ; utsig=sigexpr {
      let rng = make_range (Token(tokL)) (Ranged(utsig)) in
      (rng, BindSig(sigident, utsig))
    }
  | tokL=INCLUDE; utmod=modexpr {
      let rng = make_range (Token(tokL)) (Ranged(utmod)) in
      (rng, BindInclude(utmod))
    }
;
bindmod:
  | tokL=MODULE; modident=UPPER; utsigopt=option(coercion); DEFEQ; utmod=modexpr {
      let rng = make_range (Token(tokL)) (Ranged(utmod)) in
      (rng, modident, utsigopt, utmod)
    }
;
coercion:
  | COERCE; utsig=sigexpr { utsig }
;
bindtypesingle:
  | ident=LOWER; tyrowparams=typarams; DEFEQ; ctorbrs=ctorbranches {
      let (typarams, _) = tyrowparams in
        (* TODO: restrict that the second entry is `[]` *)
      (ident, typarams, BindVariant(ctorbrs))
    }
  | ident=LOWER; tyrowparams=typarams; DEFEQ; mty=ty {
      let (typarams, _) = tyrowparams in
        (* TODO: restrict that the second entry is `[]` *)
      (ident, typarams, BindSynonym(mty))
    }
;
bindtypesub:
  | AND; tybind=bindtypesingle { tybind }
;
typarams:
  |                                         { ([], []) }
  | tylparen; typarams=typaramssub tyrparen { typarams }
;
typaramssub:
  | rowparams=rowparams {
      ([], rowparams)
    }
  | typaram=TYPARAM {
      ([ (typaram, None) ], [])
    }
  | typaram=TYPARAM; CONS; mnbkd=bkd {
      ([ (typaram, Some(mnbkd)) ], [])
    }
  | typaram=TYPARAM; COMMA; tail=typaramssub {
      let (typarams, rowparams) = tail in
      ((typaram, None) :: typarams, rowparams)
    }
  | typaram=TYPARAM; CONS; mnbkd=bkd COMMA; tail=typaramssub {
      let (typarams, rowparams) = tail in
      ((typaram, Some(mnbkd)) :: typarams, rowparams)
    }
;
rowparams:
  |                                                                                        { [] }
  | rowparam=ROWPARAM; CONS; LPAREN; rowkind=opttydomsfixed; RPAREN                        { [ (rowparam, rowkind) ] }
  | rowparam=ROWPARAM; CONS; LPAREN; rowkind=opttydomsfixed; RPAREN; COMMA; tail=rowparams { (rowparam, rowkind) :: tail }
;
bindvallocal:
  | valbinding=bindvalsingle                           { NonRec(valbinding) }
  | REC; valbinding=bindvalsingle; tail=list(recbinds) { Rec(valbinding :: tail) }
;
bindvaltop:
  | tokL=VAL; rec_or_nonrec=bindvallocal {
      (tokL, Internal(rec_or_nonrec))
        (* TODO: give appropriate range *)
    }
  | tokL=VAL; ident=LOWER; tyrowparams=typarams; COLON; mty=ty; DEFEQ; EXTERNAL; inttok=INT; has_option=has_option; strblock=STRING_BLOCK {
      let (typarams, rowparams) = tyrowparams in
      let (tokR, erlang_bind) = strblock in
      let (_, arity) = inttok in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      let extbind =
        {
          ext_identifier  = ident;
          ext_type_params = typarams;
          ext_row_params  = rowparams;
          ext_type_annot  = mty;
          ext_arity       = arity;
          ext_has_option  = has_option;
          ext_code        = erlang_bind;
        }
      in
      (rng, External(extbind))
    }
;
has_option:
  |            { false }
  | BINOP_PLUS { true }  (* TODO: fix this ad-hoc implementation *)
;
recbinds:
  | AND; valbinding=bindvalsingle { valbinding }
;
bindvalsingle:
  | ident=LOWER; tyrowparams=typarams; LPAREN; params=params; RPAREN; ret=bindvalret {
      let (typarams, rowparams) = tyrowparams in
      let (ordparams, (mndparams, optparams)) = params in
      {
        vb_identifier  = ident;
        vb_forall      = typarams;
        vb_forall_row  = rowparams;
        vb_parameters  = ordparams;
        vb_mandatories = mndparams;
        vb_optionals   = optparams;
        vb_return      = ret;
      }
    }
;
bindvalret:
  | DEFEQ; e=exprlet {
      Pure(None, e)
    }
  | COLON; mty=ty DEFEQ; e=exprlet {
      Pure(Some(mty), e)
    }
  | DEFEQ; ACT; c=comp {
      Effectful(None, c)
    }
  | COLON; LSQUARE; mty1=ty; RSQUARE; mty2=ty DEFEQ; ACT; c=comp {
      Effectful(Some(mty1, mty2), c)
    }
;
ctorbranches:
  | ctorbr=ctorbranchtop; ctorbrs=list(ctorbranchsub) { ctorbr :: ctorbrs }
  | ctorbrs=nonempty_list(ctorbranchsub)              { ctorbrs }
;
ctorbranchsub:
  | BAR; ctorbr=ctorbranchtop { ctorbr }
;
ctorbranchtop:
  | ctor=UPPER                               { ConstructorBranch(ctor, []) }
  | ctor=UPPER; LPAREN; paramtys=tys; RPAREN { ConstructorBranch(ctor, paramtys) }
;
params:
  | labparams=labparams {
      ([], labparams)
    }
  | ident=LOWER; tyannot=tyannot {
      ([ (ident, tyannot) ], ([], []))
    }
  | ident=LOWER; tyannot=tyannot; COMMA; tail=params {
      let (ordparams, labparams) = tail in
      ((ident, tyannot) :: ordparams, labparams)
    }
;
labparams:
  | optparams=optparams {
      ([], optparams)
    }
  | rlabel=MNDLABEL; ident=LOWER; tyannot=tyannot {
      ([ (rlabel, (ident, tyannot)) ], [])
    }
  | rlabel=MNDLABEL; ident=LOWER; tyannot=tyannot; COMMA; tail=labparams {
      let (mndparams, optparams) = tail in
      ((rlabel, (ident, tyannot)) :: mndparams, optparams)
    }
;
optparams:
  |                                          { [] }
  | optparam=optparam                        { [ optparam ] }
  | optparam=optparam; COMMA; tail=optparams { optparam :: tail }
;
optparam:
  | rlabel=OPTLABEL; ident=LOWER; tyannot=tyannot {
      ((rlabel, (ident, tyannot)), None)
    }
  | rlabel=OPTLABEL; ident=LOWER; tyannot=tyannot; DEFEQ; utast=exprlet {
      ((rlabel, (ident, tyannot)), Some(utast))
    }
;
tyannot:
  |               { None }
  | COLON; mty=ty { Some(mty) }
;
decl:
  | tokL=VAL; ident=LOWER; tyrowparams=typarams; COLON; mty=ty {
      let (typarams, rowparams) = tyrowparams in
      let rng = make_range (Token(tokL)) (Ranged(mty)) in
      (rng, DeclVal(ident, typarams, rowparams, mty))
    }
  | tokL=TYPE; tyident=LOWER; CONS; kd=kd {
      let rng = make_range (Token(tokL)) (Ranged(kd)) in
      (rng, DeclTypeOpaque(tyident, Some(kd)))
    }
  | tokL=TYPE; tyident=LOWER {
      let rng = make_range (Token(tokL)) (Ranged(tyident)) in
      (rng, DeclTypeOpaque(tyident, None))
    }
  | tokL=TYPE; tybind=bindtypesingle; tybinds=list(bindtypesub) {
      decl_type_transparent tokL (tybind :: tybinds)
    }
  | tokL=MODULE; modident=UPPER; COLON; utsig=sigexpr {
      let rng = make_range (Token(tokL)) (Ranged(utsig)) in
      (rng, DeclModule(modident, utsig))
    }
  | tokL=SIGNATURE; sigident=UPPER; DEFEQ; utsig=sigexpr {
      let rng = make_range (Token(tokL)) (Ranged(utsig)) in
      (rng, DeclSig(sigident, utsig))
    }
;
modexpr:
  | tokL=LAMBDA; LPAREN; modident=UPPER; COLON; utsig=sigexpr; RPAREN; ARROW; utmod=modexpr {
      let rng = make_range (Token(tokL)) (Ranged(utmod)) in
      (rng, ModFunctor(modident, utsig, utmod))
    }
  | modident=UPPER; COERCE; utsig=sigexprbot {
      let rng = make_range (Ranged(modident)) (Ranged(utsig)) in
      (rng, ModCoerce(modident, utsig))
    }
  | utmod=modapp { utmod }
;
modapp:
  | modchain1=modchainraw; LPAREN; modchain2=modchainraw; tokR=RPAREN {
      let (modident1, _) = modchain1 in
      let rng = make_range (Ranged(modident1)) (Token(tokR)) in
      (rng, ModApply(modchain1, modchain2))
    }
  | utmod=modexprbot { utmod }
;
modexprbot:
  | utmod=modexprunit { utmod }
  | utmod=modchain    { utmod }
;
modexprunit:
  | tokL=STRUCT; utbinds=list(bindtop); tokR=END {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, ModBinds(utbinds))
    }
  | tokL=LPAREN; utmod=modexpr; tokR=RPAREN {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      let (_, utmodmain) = utmod in
      (rng, utmodmain)
    }
;
modchain:
  | modchainraw=modchainraw { fold_module_chain modchainraw }
;
modchainraw:
  | modident=UPPER; projs=list(DOTUPPER) { (modident, projs) }
;
sigexpr:
  | tokL=LAMBDA; LPAREN; sigident=UPPER; COLON; utsig1=sigexpr; RPAREN; ARROW; utsig2=sigexpr {
      let rng = make_range (Token(tokL)) (Ranged(utsig2)) in
      (rng, SigFunctor(sigident, utsig1, utsig2))
    }
  | utsig=sigexprwith { utsig }
;
sigexprwith:
  | utsig=sigexprbot; WITH; modidents=withproj; TYPE; tybind=bindtypesingle; tybinds=list(bindtypesub) {
      let rng = Range.dummy "sigexpr" in  (* TODO: give appropriate code ranges *)
      (rng, SigWith(utsig, modidents, tybind :: tybinds))
    }
  | utsig=sigexprbot { utsig }
;
withproj:
  |                                        { [] }
  | modident=UPPER; modidents=list(DOTUPPER) { modident :: modidents }
;
sigexprbot:
  | utmod=modexprunit; sigident=DOTUPPER {
      let rng = make_range (Ranged(utmod)) (Ranged(sigident)) in
      (rng, SigPath(utmod, sigident))
    }
  | modchain=modchainraw {
      let (tokL, modidents, sigident) = chop_last modchain in
      let rng = make_range (Token(tokL)) (Ranged(sigident)) in
      match modidents with
      | [] ->
          let (_, signm) = sigident in
          (rng, SigVar(signm))

      | modident :: projs ->
          let utmod = fold_module_chain (modident, projs) in
          (rng, SigPath(utmod, sigident))
    }
  | tokL=SIG; utdecls=list(decl); tokR=END {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, SigDecls(utdecls))
    }
  | tokL=LPAREN; utsig=sigexpr; tokR=RPAREN {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      let (_, utsigmain) = utsig in
      (rng, utsigmain)
    }
;
comp:
  | tokL=DO; ident=LOWER; tyannot=tyannot; REVARROW; c1=comp; IN; c2=comp {
      let rng = make_range (Token(tokL)) (Ranged(c2)) in
      (rng, CompDo(Some((ident, tyannot)), c1, c2))
    }
  | tokL=DO; c1=comp; IN; c2=comp {
      let rng = make_range (Token(tokL)) (Ranged(c2)) in
      (rng, CompDo(None, c1, c2))
    }
  | tokL=RECEIVE; branches=nonempty_list(receive_branch); tokR=END {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, CompReceive(branches))
    }
  | tokL=LET; rec_or_nonrec=bindvallocal; IN; c2=comp {
      let rng = make_range (Token(tokL)) (Ranged(c2)) in
      (rng, CompLetIn(rec_or_nonrec, c2))
    }
  | tokL=LET; pat=patcons; DEFEQ; e1=exprlet; IN; c2=comp {
      let rng = make_range (Token(tokL)) (Ranged(c2)) in
      (rng, CompLetPatIn(pat, e1, c2))
    }
  | tokL=IF; e0=exprlet; THEN; c1=comp; ELSE c2=comp {
      let rng = make_range (Token(tokL)) (Ranged(c2)) in
      (rng, CompIf(e0, c1, c2))
    }
  | tokL=CASE; e=exprlet; OF; branches=nonempty_list(comp_case_branch); tokR=END {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, CompCase(e, branches))
    }
  | efun=exprapp; LPAREN; args=args; tokR=RPAREN {
      let (ordargs, (mndargs, optargs)) = args in
      let rng = make_range (Ranged(efun)) (Token(tokR)) in
      (rng, CompApply(efun, (ordargs, mndargs, optargs)))
    }
;
exprlet:
  | tokL=LET; rec_or_nonrec=bindvallocal; IN; e2=exprlet {
      let rng = make_range (Token(tokL)) (Ranged(e2)) in
      (rng, LetIn(rec_or_nonrec, e2))
    }
  | tokL=LET; pat=patcons; DEFEQ; e1=exprlet; IN; e2=exprlet {
      let rng = make_range (Token(tokL)) (Ranged(e2)) in
      (rng, LetPatIn(pat, e1, e2))
    }
  | tokL=IF; e0=exprlet; THEN; e1=exprlet; ELSE; e2=exprlet {
      let rng = make_range (Token(tokL)) (Ranged(e2)) in
      (rng, If(e0, e1, e2))
    }
  | e=exprfun { e }
;
exprfun:
  | tokL=LAMBDA; LPAREN; params=params; RPAREN; ARROW; cod=exprcod; tokR=END {
      let (ordparams, (mndparams, optparams)) = params in
      let lamparams = (ordparams, mndparams, optparams) in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      match cod with
      | Pure(e)      -> (rng, Lambda(lamparams, e))
      | Effectful(c) -> (rng, LambdaEff(lamparams, c))
    }
  | tokL=CASE; e=exprlet; OF; branches=nonempty_list(case_branch); tokR=END {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, Case(e, branches))
    }
  | e=exprland { e }
;
exprcod:
  | e=exprlet   { Pure(e) }
  | ACT; c=comp { Effectful(c) }
;
exprland:
  | e1=exprlor; op=BINOP_AMP; e2=exprland { binary e1 op e2 }
  | e=exprlor                             { e }
;
exprlor:
  | e1=exprcomp; op=BINOP_BAR; e2=exprlor { binary e1 op e2 }
  | e=exprcomp                            { e }
;
exprcomp:
  | e1=exprcons; op=BINOP_EQ; e2=exprcomp { binary e1 op e2 }
  | e1=exprcons; op=oplt;     e2=exprcomp { binary e1 op e2 }
  | e1=exprcons; op=opgt;     e2=exprcomp { binary e1 op e2 }
  | e=exprcons                            { e }
;
oplt:
  | op=BINOP_LT  { op }
  | rng=LT_EXACT { (rng, "<") }
;
opgt:
  | op=BINOP_GT    { op }
  | rng=GT_SPACES  { (rng, ">") }
  | rng=GT_NOSPACE { (rng, ">") }
;
exprcons:
  | e1=exprtimes; CONS; e2=exprcons {
      let rng = make_range (Ranged(e1)) (Ranged(e2)) in
      (rng, ListCons(e1, e2))
    }
  | e=exprtimes { e }
;
exprtimes:
  | e1=exprplus; op=BINOP_TIMES; e2=exprtimes   { binary e1 op e2 }
  | e1=exprplus; op=BINOP_DIVIDES; e2=exprtimes { binary e1 op e2 }
  | e=exprplus                                  { e }
;
exprplus:
  | e1=exprapp; op=BINOP_PLUS; e2=exprplus  { binary e1 op e2 }
  | e1=exprapp; op=BINOP_MINUS; e2=exprplus { binary e1 op e2 }
  | e=exprapp                               { e }
;
exprapp:
  | efun=exprapp; LPAREN; args=args; tokR=RPAREN {
      let (ordargs, (mndargs, optargs)) = args in
      let rng = make_range (Ranged(efun)) (Token(tokR)) in
      (rng, Apply(efun, (ordargs, mndargs, optargs)))
    }
  | tokL=PACK; modchain=modchainraw; COLON; utsig=sigexprbot {
      let rng = make_range (Token(tokL)) (Ranged(utsig)) in
      (rng, Pack(modchain, utsig))
    }
  | tokL=FREEZE; modchain=modchainraw; ident=DOTLOWER; LPAREN; args=freezeargs; tokR=RPAREN {
      let (ordargs, rngs) = args in
      let ((rng1, _), _) = modchain in
      let rngapp = make_range (Token(rng1)) (Token(tokR)) in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, Freeze(rngapp, FrozenModFun(modchain, ident), ordargs, rngs))
    }
  | tokL=FREEZE; ident=LOWER; LPAREN; args=freezeargs; tokR=RPAREN {
      let (ordargs, rngs) = args in
      let rngapp = make_range (Ranged(ident)) (Token(tokR)) in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, Freeze(rngapp, FrozenFun(ident), ordargs, rngs))
    }
  | tokL=FREEZE; LPAREN; e=exprlet; RPAREN; WITH; LPAREN; args=freezeargs; tokR=RPAREN {
      let (ordargs, rngs) = args in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, FreezeUpdate(e, ordargs, rngs))
    }
  | modchain=modchainraw; LPAREN; args=args; tokR=RPAREN {
      let (tokL, modidents, ctor) = chop_last modchain in
      let (ordargs, optargs) = args in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      match modidents with
      | []     -> let (_, ctornm) = ctor in (rng, Constructor(ctornm, ordargs))
      | _ :: _ -> (rng, ModProjCtor(modidents, ctor, ordargs))
        (* TODO: emit errors when `optargs` is not nil *)
    }
  | modchain=modchainraw {
      let (tokL, modidents, ctor) = chop_last modchain in
      let rng = make_range (Token(tokL)) (Ranged(ctor)) in
      match modidents with
      | []     -> let (_, ctornm) = ctor in (rng, Constructor(ctornm, []))
      | _ :: _ -> (rng, ModProjCtor(modidents, ctor, []))
    }
  | modchain=modchainraw; ident=DOTLOWER {
      let (modident, modidents) = modchain in
      let rng = make_range (Ranged(modident)) (Ranged(ident)) in
      (rng, ModProjVal(modident :: modidents, ident))
    }
  | e=exprbot { e }
;
args:
  | labargs=labargs             { ([], labargs) }
  | e=exprlet                   { ([ e ], ([], [])) }
  | e=exprlet; COMMA; tail=args { let (ordargs, labargs) = tail in (e :: ordargs, labargs) }
;
labargs:
  | optargs=optargs                                 { ([], optargs) }
  | rlabel=MNDLABEL; e=exprlet                      { ([ (rlabel, e) ], []) }
  | rlabel=MNDLABEL; e=exprlet; COMMA; tail=labargs { let (mndargs, optargs) = tail in ((rlabel, e) :: mndargs, optargs) }
;
optargs:
  |                                                 { [] }
  | rlabel=OPTLABEL; e=exprlet                      { [ (rlabel, e) ] }
  | rlabel=OPTLABEL; e=exprlet; COMMA; tail=optargs { (rlabel, e) :: tail }
;
freezeargs:
  | rngs=holeargs                     { ([], rngs) }
  | e=exprlet                         { ([ e ], []) }
  | e=exprlet; COMMA; tail=freezeargs { let (ordargs, rngs) = tail in (e :: ordargs, rngs) }
;
holeargs:
  |                                      { [] }
  | tok=UNDERSCORE                       { [ tok ] }
  | tok=UNDERSCORE; COMMA; tail=holeargs { tok :: tail }
;
record:
  |                                                    { [] }
  | rlabel=LOWER; DEFEQ; e=exprlet                     { [ (rlabel, e) ] }
  | rlabel=LOWER; DEFEQ; e=exprlet; COMMA; tail=record { (rlabel, e) :: tail }
;
exprs:
  |                              { [] }
  | e=exprlet                    { [ e ] }
  | e=exprlet; COMMA; tail=exprs { e :: tail }
;
exprbot:
  | rng=TRUE                  { (rng, BaseConst(Bool(true))) }
  | rng=FALSE                 { (rng, BaseConst(Bool(false))) }
  | tokL=LBRACE; tokR=RBRACE  { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, BaseConst(Unit)) }
  | c=INT                     { let (rng, n) = c in (rng, BaseConst(Int(n))) }
  | c=FLOAT                   { let (rng, r) = c in (rng, BaseConst(Float(r))) }
  | ident=ident               { let (rng, x) = ident in (rng, Var(x)) }
  | LPAREN; e=exprlet; RPAREN { e }

  | tokL=LBRACE; e1=exprlet; es=list(tuplesub); tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, Tuple(TupleList.make e1 es))
    }
  | tokL=LSQUARE; es=exprs; tokR=RSQUARE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      let dr = Range.dummy "list" in
      let (_, emain) =
        List.fold_right (fun e tail -> (dr, ListCons(e, tail))) es (dr, ListNil)
      in
      (rng, emain)
    }
  | tokL=LTLT; ns=bytes tokR=gtgt {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, BinaryByList(ns))
    }
  | strlit=BINARY {
      let (rng, s) = strlit in
      (rng, BaseConst(BinaryByString(s)))
    }
  | strlit=STRING {
      let (rng, s) = strlit in
      (rng, BaseConst(String(s)))
    }
  | fmtlit=FORMAT {
      let (rng, fmtelems) = fmtlit in
      (rng, BaseConst(FormatString(fmtelems)))
    }
  | charlit=CHAR {
      let (rng, uchar) = charlit in
      (rng, BaseConst(Char(uchar)))
    }
  | tokL=LBRACE; les=record; tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, Record(les))
    }
  | tokL=LBRACE; e1=exprbot; BAR; les=record; tokR=RBRACE {
      let (_, eaccmain) =
        List.fold_left (fun eacc (rlabel, e2) ->
          let rng = make_range (Token(tokL)) (Ranged(e2)) in
          (rng, RecordUpdate(eacc, rlabel, e2))
        ) e1 les
      in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, eaccmain)
    }
  | e=exprbot; rlabel=DOTLOWER {
      let rng = make_range (Ranged(e)) (Ranged(rlabel)) in
      (rng, RecordAccess(e, rlabel))
    }
;
bytes:
  |                            { [] }
  | tok=INT                    { tok :: [] }
  | tok=INT; COMMA; tail=bytes { tok :: tail }
;
gtgt:
  | GT_NOSPACE; tokR=GT_NOSPACE { tokR }
  | GT_NOSPACE; tokR=GT_SPACES  { tokR }
;
tuplesub:
  COMMA; e=exprlet { e }
;
receive_branch:
  | BAR; pat=patcons; ARROW; c=comp { ReceiveBranch(pat, c) }
;
case_branch:
  | BAR; pat=patcons; ARROW; e=exprlet { CaseBranch(pat, e) }
;
comp_case_branch:
  | BAR; pat=patcons; ARROW; c=comp { CompCaseBranch(pat, c) }
;
patcons:
  | p1=patbot; CONS; p2=patcons { let rng = make_range (Ranged(p1)) (Ranged(p2)) in (rng, PListCons(p1, p2)) }
  | p=patbot                    { p }
;
patbot:
  | rng=TRUE                   { (rng, PBool(true)) }
  | rng=FALSE                  { (rng, PBool(true)) }
  | tokL=LPAREN; tokR=RPAREN   { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, PUnit) }
  | c=INT                      { let (rng, n) = c in (rng, PInt(n)) }
  | charlit=CHAR               { let (rng, uchar) = charlit in (rng, PChar(uchar)) }
  | ident=ident                { let (rng, x) = ident in (rng, PVar(x)) }
  | rng=UNDERSCORE             { (rng, PWildCard) }
  | tokL=LSQUARE; tokR=RSQUARE { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, PListNil) }

  | tokL=LBRACE; p1=patcons; pats=list(pattuplesub); tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, PTuple(TupleList.make p1 pats))
    }
  | ctor=UPPER {
      let (rng, ctornm) = ctor in
      (rng, PConstructor(ctornm, []))
    }
  | ctor=UPPER; LPAREN; pats=pats; tokR=RPAREN {
      let (tokL, ctornm) = ctor in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, PConstructor(ctornm, pats))
    }
;
pats:
  |                               { [] }
  | pat=patcons                   { pat :: [] }
  | pat=patcons; COMMA; tail=pats { pat :: tail }
;
pattuplesub:
  | COMMA; p=patcons { p }
;
tys:
  |                         { [] }
  | mty=ty                  { mty :: [] }
  | mty=ty; COMMA; tail=tys { mty :: tail }
;
tydoms:
  | labmtydoms=labtydoms       { ([], labmtydoms) }
  | mty=ty                     { ([ mty ], ([], MFixedRow([]))) }
  | mty=ty; COMMA; tail=tydoms { let (ordmtydoms, labmtydoms) = tail in (mty :: ordmtydoms, labmtydoms) }
;
labtydoms:
  | optmtydoms=opttydoms {
      ([], optmtydoms)
    }
  | rlabel=MNDLABEL; mty=ty {
      ([ (rlabel, mty) ], MFixedRow([]))
    }
  | rlabel=MNDLABEL; mty=ty; COMMA; tail=labtydoms {
      let (mndmtydoms, optmtydoms) = tail in
      ((rlabel, mty) :: mndmtydoms, optmtydoms)
    }
;
opttydoms:
  | tok=ROWPARAM            { let (rng, rowparam) = tok in MRowVar(rng, rowparam) }
  | fixedrow=opttydomsfixed { MFixedRow(fixedrow) }
;
opttydomsfixed:
  |                                                     { [] }
  | rlabel=OPTLABEL; mty=ty                             { [ (rlabel, mty) ] }
  | rlabel=OPTLABEL; mty=ty; COMMA; tail=opttydomsfixed { (rlabel, mty) :: tail }
;
kd:
  | tokL=LPAREN; bkddoms=bkds; RPAREN; ARROW; bkdcod=bkd {
      let rng = make_range (Token(tokL)) (Ranged(bkdcod)) in
      (rng, MKind(bkddoms, bkdcod))
    }
  | bkd=bkd {
      let (rng, _) = bkd in
      (rng, MKind([], bkd))
    }
;
bkds:
  | bkd=bkd                   { [ bkd ] }
  | bkd=bkd; COMMA            { [ bkd ] }
  | bkd=bkd; COMMA; tail=bkds { bkd :: tail }
;
bkd:
  | ident=LOWER {
      let (rng, kdnm) = ident in
      (rng, MKindName(kdnm))
    }
  | tokL=LBRACE; tyrecord=tyrecord; tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, MRecordKind(tyrecord))
    }
;
ty:
  | utmod=modchain; tyident=DOTLOWER {
      let rng = make_range (Ranged(utmod)) (Ranged(tyident)) in
      (rng, MModProjType(utmod, tyident, []))
    }
  | utmod=modchain; tyident=DOTLOWER; tylparen; mtyargs=tys; tokR=tyrparen {
      let rng = make_range (Ranged(utmod)) (Token(tokR)) in
      (rng, MModProjType(utmod, tyident, mtyargs))
    }
  | mty=tybot { mty }
;
tybot:
  | tok=TYPARAM {
      let (rng, typaram) = tok in
      (rng, MTypeVar(typaram))
    }
  | ident=LOWER {
      let (rng, tynm) = ident in
      (rng, MTypeName(tynm, []))
    }
  | ident=LOWER; tylparen; mtyargs=tys; tokR=tyrparen {
      let (tokL, tynm) = ident in
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, MTypeName(tynm, mtyargs))
    }
  | tokL=LAMBDA; LPAREN; tydoms=tydoms; RPAREN; ARROW; cod=tycod {
      let (ordmtydoms, (mndmtydoms, optmtydoms)) = tydoms in
      match cod with
      | Pure(mtycod) ->
          let rng = make_range (Token(tokL)) (Ranged(mtycod)) in
          (rng, MFuncType((ordmtydoms, mndmtydoms, optmtydoms), mtycod))

      | Effectful(rngL, mty1, mty2) ->
          let rng = make_range (Token(tokL)) (Token(rngL)) in
          (rng, MEffType((ordmtydoms, mndmtydoms, optmtydoms), mty1, mty2))
    }
  | tokL=LBRACE; mty1=ty; mtys=list(tytuplesub) tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, MProductType(TupleList.make mty1 mtys))
    }
  | tokL=LBRACE; tyrecord=tyrecord; tokR=RBRACE {
      let rng = make_range (Token(tokL)) (Token(tokR)) in
      (rng, MRecordType(tyrecord))
    }
;
tycod:
  | mtycod=ty {
      Pure(mtycod)
    }
  | tokL=LSQUARE; mty1=ty; RSQUARE; mty2=ty {
      let rng = make_range (Token(tokL)) (Ranged(mty2)) in
      Effectful(rng, mty1, mty2)
    }
;
tyrecord:
  |                                                   { [] }
  | rlabel=LOWER; COLON; mty=ty                       { [ (rlabel, mty) ] }
  | rlabel=LOWER; COLON; mty=ty; COMMA; tail=tyrecord { (rlabel, mty) :: tail }
;
tytuplesub:
  | COMMA; mty=ty { mty }
;
tylparen:
  | tok=LT_EXACT { tok }
;
tyrparen:
  | tok=GT_NOSPACE { tok }
  | tok=GT_SPACES  { tok }
;
