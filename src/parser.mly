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

%token<Range.t> LET LETREC DEFEQ IN LAMBDA ARROW IF THEN ELSE LPAREN RPAREN TRUE FALSE COMMA DO REVARROW RECEIVE BAR WHEN END UNDERSCORE
%token<Range.t * Syntax.identifier> IDENT BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * Syntax.identifier> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token<Range.t * int> INT
%token EOI

%start main
%type<Syntax.declaration list> main

%%
main:
  | decls=decls { decls }
;
decls:
  | dec=letdec; tail=decls {
        let (_, ident, isrec, e1) = dec in
        ValDecl(isrec, ident, e1) :: tail
      }
  | EOI { [] }
;
ident:
  | ident=IDENT { ident }
;
letdec:
  | tok1=LET; ident=IDENT; args=args; DEFEQ; e1=exprlet {
        (tok1, ident, false, make_lambda (Range.dummy "let") args e1)
      }
  | tok1=LETREC; ident=IDENT; args=args; DEFEQ; e1=exprlet {
        (tok1, ident, true, make_lambda (Range.dummy "letrec") args e1)
      }
  | tok1=LET; ident=IDENT; DEFEQ; e1=exprlet {
        (tok1, ident, false, e1)
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
  | dec=letdec; IN; e2=exprlet {
        let (tok1, ident, isrec, e1) = dec in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        if isrec then
          (rng, LetRecIn(ident, e1, e2))
        else
          (rng, LetIn(ident, e1, e2))
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
  | e1=exprtimes; op=BINOP_EQ; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_LT; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_GT; e2=exprcomp { binary e1 op e2 }
  | e=exprtimes                            { e }
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
;
branch:
  | BAR; pat=pattern; ARROW; e=exprlet {
        Branch(pat, None, e)
      }
  | BAR; pat=pattern; WHEN; ew=exprlet; ARROW; e=exprlet {
        Branch(pat, Some(ew), e)
      }
;
pattern:
  | rng=TRUE                 { (rng, PBool(true)) }
  | rng=FALSE                { (rng, PBool(true)) }
  | tok1=LPAREN; tok2=RPAREN { let rng = make_range (Token(tok1)) (Token(tok2)) in (rng, PUnit) }
  | c=INT                    { let (rng, n) = c in (rng, PInt(n)) }
  | ident=ident              { let (rng, x) = ident in (rng, PVar(x)) }
  | rng=UNDERSCORE           { (rng, PWildCard) }
;
