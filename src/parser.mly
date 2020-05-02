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


  let binary e1 op e2 =
    let rng = make_range (Ranged(e1)) (Ranged(e2)) in
    let (rngop, vop) = op in
    (rng, Apply((rngop, Var(vop)), [e1; e2]))
%}

%token<Range.t> LET LETREC DEFEQ IN LAMBDA ARROW IF THEN ELSE LPAREN RPAREN LSQUARE RSQUARE TRUE FALSE COMMA DO REVARROW RECEIVE BAR WHEN END UNDERSCORE CONS CASE OF TYPE COLON ANDREC
%token<Range.t> GT_SPACES GT_NOSPACE LTLT LT_EXACT
%token<Range.t * string> IDENT CTOR TYPARAM BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * string> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token<Range.t * int> INT
%token EOI

%start main
%type<Syntax.untyped_binding list> main
%type<Syntax.manual_type> ty
%type<Syntax.binder list> params
%type<Syntax.untyped_let_binding> bindvalsingle
%type<Range.t * Syntax.rec_or_nonrec> bindvaltop

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
        let (_, valbinding) = bindval in
        BindVal(valbinding)
      }
;
typarams:
  |                                       { [] }
  | LSQUARE; typarams=typaramssub RSQUARE { typarams }
;
typaramssub:
  |                                          { [] }
  | typaram=TYPARAM                          { typaram :: [] }
  | typaram=TYPARAM; COMMA; tail=typaramssub { typaram :: tail }
;
bindvaltop:
  | tok=LET; valbinding=bindvalsingle {
        (tok, NonRec(valbinding))
      }
  | tok=LETREC; valbinding=bindvalsingle; tail=list(recbinds) {
        (tok, Rec(valbinding :: tail))
      }
;
recbinds:
  | ANDREC; valbinding=bindvalsingle { valbinding }
;
bindvalsingle:
  | ident=IDENT; bids=typarams; LPAREN; params=params; RPAREN; tyannot=tyannot; DEFEQ; e0=exprlet {
        {
          vb_identifier = ident;
          vb_forall     = bids;
          vb_parameters = params;
          vb_return_type = tyannot;
          vb_body       = e0;
        }
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
params:
  |                                                  { [] }
  | ident=IDENT; tyannot=tyannot                     { (ident, tyannot) :: [] }
  | ident=IDENT; tyannot=tyannot; COMMA; tail=params { (ident, tyannot) :: tail }
;
tyannot:
  |               { None }
  | COLON; mty=ty { Some(mty) }
;
exprlet:
  | bindval=bindvaltop; IN; e2=exprlet {
        let (tokL, valbind) = bindval in
        let rng = make_range (Token(tokL)) (Ranged(e2)) in
        (rng, LetIn(valbind, e2))
      }
  | tokL=LET; pat=patcons; DEFEQ; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tokL)) (Ranged(e2)) in
        (rng, LetPatIn(pat, e1, e2))
      }
  | tokL=IF; e0=exprlet; THEN; e1=exprlet; ELSE; e2=exprlet {
        let rng = make_range (Token(tokL)) (Ranged(e2)) in
        (rng, If(e0, e1, e2))
      }
  | tokL=DO; ident=IDENT; tyannot=tyannot; REVARROW; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tokL)) (Ranged(e2)) in
        (rng, Do(Some((ident, tyannot)), e1, e2))
      }
  | tokL=DO; e1=exprlet; IN; e2=exprlet {
        let rng = make_range (Token(tokL)) (Ranged(e2)) in
        (rng, Do(None, e1, e2))
      }
  | e=exprfun { e }
;
exprfun:
  | tokL=LAMBDA; LPAREN; params=params; RPAREN; ARROW; e=exprlet {
        let rng = make_range (Token(tokL)) (Ranged(e)) in
        (rng, Lambda(params, e))
      }
  | tokL=RECEIVE; branches=nonempty_list(branch); tokR=END {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, Receive(branches))
      }
  | tokL=CASE; e=exprlet; OF; branches=nonempty_list(branch); tokR=END {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
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
  | efun=exprapp; LPAREN; eargs=exprargs; tokR=RPAREN {
        let rng = make_range (Ranged(efun)) (Token(tokR)) in
        (rng, Apply(efun, eargs))
      }
  | ctor=CTOR; LPAREN; eargs=exprargs; tokR=RPAREN {
        let (tokL, ctornm) = ctor in
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, Constructor(ctornm, eargs))
      }
  | e=exprbot { e }
;
exprargs:
  |                                 { [] }
  | e=exprlet;                      { e :: [] }
  | e=exprlet; COMMA; tail=exprargs { e :: tail }
;
exprbot:
  | rng=TRUE                  { (rng, BaseConst(Bool(true))) }
  | rng=FALSE                 { (rng, BaseConst(Bool(false))) }
  | tokL=LPAREN; tokR=RPAREN  { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, BaseConst(Unit)) }
  | c=INT                     { let (rng, n) = c in (rng, BaseConst(Int(n))) }
  | ident=ident               { let (rng, x) = ident in (rng, Var(x)) }
  | LPAREN; e=exprlet; RPAREN { e }
  | tokL=LPAREN; e1=exprlet; COMMA; e2=exprlet; es=list(tuplesub); tokR=RPAREN {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, Tuple(TupleList.make e1 e2 es))
      }
  | tokL=LSQUARE; es=exprargs; tokR=RSQUARE {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        let dr = Range.dummy "list" in
        let (_, emain) =
          List.fold_right (fun e tail -> (dr, ListCons(e, tail))) es (dr, ListNil)
        in
        (rng, emain)
      }
  | ctor=CTOR {
        let (rng, ctornm) = ctor in
        (rng, Constructor(ctornm, []))
      }
  | tokL=LTLT; ns=bytes tokR=gtgt {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, BinaryByList(ns))
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
  | tokL=LPAREN; tokR=RPAREN { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, PUnit) }
  | c=INT                    { let (rng, n) = c in (rng, PInt(n)) }
  | ident=ident              { let (rng, x) = ident in (rng, PVar(x)) }
  | rng=UNDERSCORE           { (rng, PWildCard) }
  | tokL=LSQUARE; tokR=RSQUARE { let rng = make_range (Token(tokL)) (Token(tokR)) in (rng, PListNil) }
  | tokL=LPAREN; p1=patcons; COMMA; p2=patcons; pats=list(pattuplesub); tokR=RPAREN {
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, PTuple(TupleList.make p1 p2 pats))
      }
  | ctor=CTOR {
        let (rng, ctornm) = ctor in
        (rng, PConstructor(ctornm, []))
      }
  | ctor=CTOR; LPAREN; pats=pats; tokR=RPAREN {
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
  | ident=IDENT; LSQUARE; mtyargs=tys; tokR=RSQUARE {
        let (tokL, tynm) = ident in
        let rng = make_range (Token(tokL)) (Token(tokR)) in
        (rng, MTypeName(tynm, mtyargs))
      }
  | tokL=LAMBDA; LPAREN; mtydoms=tys; RPAREN; ARROW; mtycod=ty {
        let rng = make_range (Token(tokL)) (Ranged(mtycod)) in
        (rng, MFuncType(mtydoms, mtycod))
      }
;
