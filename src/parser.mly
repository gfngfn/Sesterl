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


  let make_lambda rngopt args e =
    let (_, elammain) as elam =
      List.fold_right (fun ident e ->
        (Range.dummy "make_lambda", Lambda(ident, e))
      ) args e
    in
    match rngopt with
    | None      -> elam
    | Some(rng) -> (rng, elammain)


  let binary e1 op e2 =
    let rng = make_range (Ranged(e1)) (Ranged(e2)) in
    let (rngop, vop) = op in
    (rng, Apply((Range.dummy "binary", Apply((rngop, Var(vop)), e1)), e2))
%}

%token<Range.t> LET LETREC DEFEQ IN LAMBDA ARROW IF THEN ELSE LPAREN RPAREN TRUE FALSE
%token<Range.t * Syntax.identifier> IDENT BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * Syntax.identifier> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token<Range.t * int> INT
%token EOI

%start main
%type<Syntax.untyped_ast> main

%%

main:
  | dec=letdec; e2=main {
        let (tok1, ident, isrec, e1) = dec in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        if isrec then
          (rng, LetRecIn(ident, e1, e2))
        else
          (rng, LetIn(ident, e1, e2))
      }
  | IN; e=exprfun; EOI { e }
;
ident:
  | ident=IDENT { ident }
;
letdec:
  | tok1=LET; ident=IDENT; args=list(ident); DEFEQ; e1=exprlet {
        (tok1, ident, false, make_lambda None args e1)
      }
  | tok1=LETREC; ident=IDENT; args=list(ident); DEFEQ; e1=exprlet {
        (tok1, ident, true, make_lambda None args e1)
      }
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
  | e=exprfun { e }
;
exprfun:
  | tok1=LAMBDA; args=nonempty_list(ident); ARROW; e=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e)) in
        make_lambda (Some(rng)) args e
      }
  | e=exprland { e }
;
exprland:
  | e1=exprlor; op=BINOP_AMP; e2=exprland { binary e1 op e2 }
  | e=exprlor { e }
;
exprlor:
  | e1=exprcomp; op=BINOP_BAR; e2=exprlor { binary e1 op e2 }
  | e=exprcomp { e }
;
exprcomp:
  | e1=exprtimes; op=BINOP_EQ; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_LT; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_GT; e2=exprcomp { binary e1 op e2 }
  | e=exprtimes { e }
;
exprtimes:
  | e1=exprplus; op=BINOP_TIMES; e2=exprtimes { binary e1 op e2 }
  | e1=exprplus; op=BINOP_DIVIDES; e2=exprtimes { binary e1 op e2 }
  | e=exprplus { e }
;
exprplus:
  | e1=exprapp; op=BINOP_PLUS; e2=exprplus { binary e1 op e2 }
  | e1=exprapp; op=BINOP_MINUS; e2=exprplus { binary e1 op e2 }
  | e=exprapp { e }
;
exprapp:
  | e1=exprapp; e2=exprbot {
        let rng = make_range (Ranged(e1)) (Ranged(e2)) in
        (rng, Apply(e1, e2))
      }
  | e=exprbot { e }
;
exprbot:
  | rng=TRUE { (rng, Bool(true)) }
  | rng=FALSE { (rng, Bool(false)) }
  | c=INT { let (rng, n) = c in (rng, Int(n)) }
  | ident=ident { let (rng, x) = ident in (rng, Var(x)) }
  | LPAREN; e=exprlet; RPAREN { e }
;
