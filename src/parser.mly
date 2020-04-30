%{
  open Syntax

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


  let make_lambda rng binders e =
    (rng, Lambda(binders, e))


  let binary e1 op e2 =
    let rng = make_range (Ranged(e1)) (Ranged(e2)) in
    let (rngop, vop) = op in
    (rng, Apply((rngop, Var(vop)), [e1; e2]))
%}

%token<Range.t> LET LETREC DEFEQ IN LAMBDA ARROW IF THEN ELSE LPAREN RPAREN LSQUARE RSQUARE TRUE FALSE COMMA DO REVARROW RECEIVE BAR WHEN END UNDERSCORE CONS CASE OF TYPE
%token<Range.t * string> IDENT CTOR TYPARAM BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * string> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token<Range.t * int> INT
%token EOI

%start main
%type<Syntax.untyped_binding list> main
%type<Syntax.manual_type> ty

%%
main:
  | binds=list(bindtop); EOI { binds }
;
ident:
  | ident=IDENT { ident }
;
bindtop:
  | TYPE; ident=IDENT; typarams=typarams; DEFEQ; ctorbrs=nonempty_list(ctorbranch) {
        BindType(ident, typarams, ctorbrs)
      }
  | bindval=bindvaltop {
        let (_, isrec, ident, e1) = bindval in
        BindVal(isrec, ident, e1)
      }
;
typarams:
  |                                     { [] }
  | LPAREN; typarams=typaramssub RPAREN { typarams }
;
typaramssub:
  |                                          { [] }
  | typaram=TYPARAM                          { typaram :: [] }
  | typaram=TYPARAM; COMMA; tail=typaramssub { typaram :: tail }
;
bindvaltop:
  | tok=LET; ident=IDENT; args=args; DEFEQ; e1=exprlet {
        (tok, false, ident, make_lambda (Range.dummy "let") args e1)
      }
  | tok=LETREC; ident=IDENT; args=args; DEFEQ; e1=exprlet {
        (tok, true, ident, make_lambda (Range.dummy "letrec") args e1)
      }
;
ctorbranch:
  | BAR; ctor=CTOR; {
        let (_, ctornm) = ctor in
        ConstructorBranch(ctornm, [])
      }
  | BAR; ctor=CTOR; LPAREN; paramtys=tys; RPAREN {
        let (_, ctornm) = ctor in
        ConstructorBranch(ctornm, paramtys)
      }
;
args:
  | LPAREN; args=argssub { args }
;
argssub:
  | RPAREN                           { [] }
  | ident=IDENT; RPAREN              { ident :: [] }
  | ident=IDENT; COMMA; tail=argssub { ident :: tail }
;
exprlet:
  | bindval=bindvaltop; IN; e2=exprlet {
        let (tok1, isrec, ident, e1) = bindval in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        if isrec then
          (rng, LetRecIn(ident, e1, e2))
        else
          (rng, LetIn(ident, e1, e2))
      }
  | tok1=LET; pat=patcons; DEFEQ; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, LetPatIn(pat, e1, e2))
      }
  | tok1=IF; e0=exprlet; THEN; e1=exprlet; ELSE; e2=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, If(e0, e1, e2))
      }
  | tok1=DO; ident=IDENT; REVARROW; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, Do(Some(ident), e1, e2))
      }
  | tok1=DO; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, Do(None, e1, e2))
      }
  | e=exprfun { e }
;
exprfun:
  | tok1=LAMBDA; args=args; ARROW; e=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e)) in
        make_lambda rng args e
      }
  | tok1=RECEIVE; branches=nonempty_list(branch); tok2=END {
        let rng = make_range (Token(tok1)) (Token(tok2)) in
        (rng, Receive(branches))
      }
  | tok1=CASE; e=exprlet; OF; branches=nonempty_list(branch); tok2=END {
        let rng = make_range (Token(tok1)) (Token(tok2)) in
        (rng, Case(e, branches))
      }
  | e=exprland { e }
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
  | e1=exprcons; op=BINOP_LT; e2=exprcomp { binary e1 op e2 }
  | e1=exprcons; op=BINOP_GT; e2=exprcomp { binary e1 op e2 }
  | e=exprcons                            { e }
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
  | efun=exprapp; LPAREN; args=exprargs {
        let (rtok, eargs) = args in
        let rng = make_range (Ranged(efun)) (Token(rtok)) in
        (rng, Apply(efun, eargs))
      }
  | ctor=CTOR; LPAREN; args=exprargs {
        let (ltok, ctornm) = ctor in
        let (rtok, eargs) = args in
        let rng = make_range (Token(ltok)) (Token(rtok)) in
        (rng, Constructor(ctornm, eargs))
      }
  | e=exprbot { e }
;
exprargs:
  | rtok=RPAREN                     { (rtok, []) }
  | e=exprlet; rtok=RPAREN          { (rtok, e :: []) }
  | e=exprlet; COMMA; rest=exprargs { let (rtok, tail) = rest in (rtok, e :: tail) }
;
exprbot:
  | rng=TRUE                  { (rng, BaseConst(Bool(true))) }
  | rng=FALSE                 { (rng, BaseConst(Bool(false))) }
  | tok1=LPAREN; tok2=RPAREN  { let rng = make_range (Token(tok1)) (Token(tok2)) in (rng, BaseConst(Unit)) }
  | c=INT                     { let (rng, n) = c in (rng, BaseConst(Int(n))) }
  | ident=ident               { let (rng, x) = ident in (rng, Var(x)) }
  | LPAREN; e=exprlet; RPAREN { e }
  | rngl=LPAREN; e1=exprlet; COMMA; e2=exprlet; es=list(tuplesub); rngr=RPAREN {
        let rng = make_range (Token(rngl)) (Token(rngr)) in
        (rng, Tuple(TupleList.make e1 e2 es))
      }
  | tok1=LSQUARE; tok2=RSQUARE {
        let rng = make_range (Token(tok1)) (Token(tok2)) in
        (rng, ListNil)
      }
  | ctor=CTOR {
        let (rng, ctornm) = ctor in
        (rng, Constructor(ctornm, []))
      }
;
tuplesub:
  COMMA; e=exprlet { e }
;
branch:
  | BAR; pat=patcons; ARROW; e=exprlet {
        Branch(pat, None, e)
      }
  | BAR; pat=patcons; WHEN; ew=exprlet; ARROW; e=exprlet {
        Branch(pat, Some(ew), e)
      }
;
patcons:
  | p1=patbot; CONS; p2=patcons { let rng = make_range (Ranged(p1)) (Ranged(p2)) in (rng, PListCons(p1, p2)) }
  | p=patbot                    { p }
;
patbot:
  | rng=TRUE                 { (rng, PBool(true)) }
  | rng=FALSE                { (rng, PBool(true)) }
  | tok1=LPAREN; tok2=RPAREN { let rng = make_range (Token(tok1)) (Token(tok2)) in (rng, PUnit) }
  | c=INT                    { let (rng, n) = c in (rng, PInt(n)) }
  | ident=ident              { let (rng, x) = ident in (rng, PVar(x)) }
  | rng=UNDERSCORE           { (rng, PWildCard) }
  | tok1=LSQUARE; tok2=RSQUARE { let rng = make_range (Token(tok1)) (Token(tok2)) in (rng, PListNil) }
  | rngl=LPAREN; p1=patcons; COMMA; p2=patcons; pats=list(pattuplesub); rngr=RPAREN {
        let rng = make_range (Token(rngl)) (Token(rngr)) in
        (rng, PTuple(TupleList.make p1 p2 pats))
      }
;
pattuplesub:
  | COMMA; p=patcons { p }
;
tys:
  |                         { [] }
  | mty=ty                  { mty :: [] }
  | mty=ty; COMMA; tail=tys { mty :: tail }
;
ty:
  | mty=tybot { mty }
;
tybot:
  | tok=TYPARAM {
        let (rng, typaram) = tok in
        (rng, MTypeVar(typaram))
      }
  | ident=IDENT {
        let (rng, tynm) = ident in
        (rng, MTypeName(tynm, []))
      }
  | ident=IDENT; LPAREN; mtyargs=tys; tokR=RPAREN {
        let (tokL, tynm) = ident in
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, MTypeName(tynm, mtyargs))
      }
  | tokL=LAMBDA; LPAREN; mtydoms=tys; RPAREN; ARROW; mtycod=ty {
        let rng = make_range (Token(tokL)) (Ranged(mtycod)) in
        (rng, MFuncType(mtydoms, mtycod))
      }
;
