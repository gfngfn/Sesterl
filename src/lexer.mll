{
  open Syntax
  open Parser
}

let space = [' ' '\t']
let break = ['\n' '\r']
let nzdigit = ['1'-'9']
let digit = (nzdigit | "0")
let hex   = (digit | ['A'-'F'])
let capital = ['A'-'Z']
let small = ['a'-'z']
let latin = (small | capital)
let identifier = (small (digit | latin | "_")*)
let constructor = (capital (digit | latin | "_")*)

rule token = parse
  | space { token lexbuf }
  | break { Lexing.new_line lexbuf; token lexbuf }
  | identifier {
        let s = Lexing.lexeme lexbuf in
        let pos = Range.from_lexbuf lexbuf in
          match s with
          | "let"    -> LET(pos)
          | "letrec" -> LETREC(pos)
          | "in"     -> IN(pos)
          | "fun"    -> LAMBDA(pos)
          | "if"     -> IF(pos)
          | "then"   -> THEN(pos)
          | "else"   -> ELSE(pos)
          | "true"   -> TRUE(pos)
          | "false"  -> FALSE(pos)
          | _        -> IDENT(pos, s)
      }
  | ("0" | nzdigit (digit*) | ("0x" | "0X") hex+) {
        let s = Lexing.lexeme lexbuf in
        let rng = Range.from_lexbuf lexbuf in
          INT(rng, int_of_string s)
      }
  | "="  { DEFEQ(Range.from_lexbuf lexbuf) }
  | "->" { ARROW(Range.from_lexbuf lexbuf) }
  | "("  { LPAREN(Range.from_lexbuf lexbuf) }
  | ")"  { RPAREN(Range.from_lexbuf lexbuf) }
  | "/*" { comment (Range.from_lexbuf lexbuf) lexbuf; token lexbuf }
  | eof  { EOI }
  | _ as c { raise (UnidentifiedToken(Range.from_lexbuf lexbuf, String.make 1 c)) }

and comment rng = parse
  | "/*" { comment (Range.from_lexbuf lexbuf) lexbuf; comment rng lexbuf }
  | "*/" { () }
  | break { Lexing.new_line lexbuf; comment rng lexbuf }
  | eof  { raise (SeeEndOfFileInComment(rng)) }
  | _    { comment rng lexbuf }
