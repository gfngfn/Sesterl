{
  open MyUtil
  open Syntax
  open Parser
  open Errors


  exception Error of lexer_error


  let raise_error e =
    raise (Error(e))


  let hole_of_char = function
    | 'c' -> HoleC
    | 'f' -> HoleF
    | 'e' -> HoleE
    | 'g' -> HoleG
    | 's' -> HoleS
    | 'p' -> HoleP
    | 'w' -> HoleW
    | _   -> assert false


  let int_of_string_or_empty = function
    | "" -> None
    | s  -> Some(int_of_string s)


  let flush_buffer strbuf =
    let s = Buffer.contents strbuf in
    Buffer.clear strbuf;
    FormatConst(s)

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
let nssymbol = ['&' '|' '=' '/' '+' '-' '.']
let fmtdigits = (("-" digit+) | (digit*))
let hole = ['c' 'f' 'e' 'g' 's' 'p' 'w']

rule token = parse
  | space { token lexbuf }
  | break { Lexing.new_line lexbuf; token lexbuf }
  | identifier {
        let s = Lexing.lexeme lexbuf in
        let pos = Range.from_lexbuf lexbuf in
          match s with
          | "let"       -> LET(pos)
          | "letrec"    -> LETREC(pos)
          | "andrec"    -> ANDREC(pos)
          | "in"        -> IN(pos)
          | "fun"       -> LAMBDA(pos)
          | "if"        -> IF(pos)
          | "then"      -> THEN(pos)
          | "else"      -> ELSE(pos)
          | "true"      -> TRUE(pos)
          | "false"     -> FALSE(pos)
          | "do"        -> DO(pos)
          | "receive"   -> RECEIVE(pos)
          | "when"      -> WHEN(pos)
          | "end"       -> END(pos)
          | "case"      -> CASE(pos)
          | "of"        -> OF(pos)
          | "val"       -> VAL(pos)
          | "type"      -> TYPE(pos)
          | "module"    -> MODULE(pos)
          | "struct"    -> STRUCT(pos)
          | "signature" -> SIGNATURE(pos)
          | "sig"       -> SIG(pos)
          | "with"      -> WITH(pos)
          | "external"  -> EXTERNAL(pos)
          | "include"   -> INCLUDE(pos)
          | "require"   -> REQUIRE(pos)
          | "freeze"    -> FREEZE(pos)
          | _           -> LOWER(pos, s)
      }
  | ("$" (identifier as s)) {
        let pos = Range.from_lexbuf lexbuf in
        TYPARAM(pos, s)
      }
  | ("?$" (identifier as s)) {
        let pos = Range.from_lexbuf lexbuf in
        ROWPARAM(pos, s)
      }
  | constructor {
        let s = Lexing.lexeme lexbuf in
        let pos = Range.from_lexbuf lexbuf in
        UPPER(pos, s)
      }
  | ("." (constructor as s)) {
        let pos = Range.from_lexbuf lexbuf in
        DOTUPPER(pos, s)
      }
  | ("." (identifier as s)) {
        let pos = Range.from_lexbuf lexbuf in
        DOTLOWER(pos, s)
      }
  | ("-" (identifier as s)) {
        let pos = Range.from_lexbuf lexbuf in
        MNDLABEL(pos, s)
      }
  | ("?" (identifier as s)) {
        let pos = Range.from_lexbuf lexbuf in
        OPTLABEL(pos, s)
      }
  | ("0" | nzdigit (digit*) | ("0x" | "0X") hex+) {
        let s = Lexing.lexeme lexbuf in
        let pos = Range.from_lexbuf lexbuf in
        INT(pos, int_of_string s)
      }
  | (("0" | nzdigit (digit*)) "." (digit*)) {
        let s = Lexing.lexeme lexbuf in
        let pos = Range.from_lexbuf lexbuf in
        FLOAT(pos, float_of_string s)
      }
  | "_"  { UNDERSCORE(Range.from_lexbuf lexbuf) }
  | ","  { COMMA(Range.from_lexbuf lexbuf) }
  | "("  { LPAREN(Range.from_lexbuf lexbuf) }
  | ")"  { RPAREN(Range.from_lexbuf lexbuf) }
  | "["  { LSQUARE(Range.from_lexbuf lexbuf) }
  | "]"  { RSQUARE(Range.from_lexbuf lexbuf) }
  | "{"  { LBRACE(Range.from_lexbuf lexbuf) }
  | "}"  { RBRACE(Range.from_lexbuf lexbuf) }

  | "::" { CONS(Range.from_lexbuf lexbuf) }
  | ":"  { COLON(Range.from_lexbuf lexbuf) }
  | ":>" { COERCE(Range.from_lexbuf lexbuf) }

  | ("&" (nssymbol*)) { BINOP_AMP(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "|"               { BAR(Range.from_lexbuf lexbuf) }
  | ("|" (nssymbol+)) { BINOP_BAR(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "="               { DEFEQ(Range.from_lexbuf lexbuf) }
  | ("=" (nssymbol+)) { BINOP_EQ(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "<-"                   { REVARROW(Range.from_lexbuf lexbuf) }
  | "<<"                   { LTLT(Range.from_lexbuf lexbuf) }
  | "<"                    { LT_EXACT(Range.from_lexbuf lexbuf) }
  | ("<" (nssymbol+))      { BINOP_LT(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | (">" space)            { GT_SPACES(Range.from_lexbuf lexbuf) }
  | (">" break)            { Lexing.new_line lexbuf; GT_SPACES(Range.from_lexbuf lexbuf) }
  | ">"                    { GT_NOSPACE(Range.from_lexbuf lexbuf) }
  | (">" (nssymbol+))      { BINOP_GT(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | ("*" (nssymbol*)) { BINOP_TIMES(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "/*"              { comment (Range.from_lexbuf lexbuf) lexbuf; token lexbuf }
  | ("/" (nssymbol*)) { BINOP_DIVIDES(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | ("+" (nssymbol*)) { BINOP_PLUS(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "->"              { ARROW(Range.from_lexbuf lexbuf) }
  | ("-" (nssymbol*)) { BINOP_MINUS(Range.from_lexbuf lexbuf, Lexing.lexeme lexbuf) }

  | "\"" {
      let posL = Range.from_lexbuf lexbuf in
      let strbuf = Buffer.create 128 in
      let (rng, s) = binary_literal posL strbuf lexbuf in
      BINARY(rng, s)
    }

  | "\'" {
      let posL = Range.from_lexbuf lexbuf in
      let strbuf = Buffer.create 128 in
      let (rng, s) = string_literal posL strbuf lexbuf in
      STRING(rng, s)
    }

  | "f\'" {
      let posL = Range.from_lexbuf lexbuf in
      let strbuf = Buffer.create 128 in
      let (rng, fmtelemacc) = format_literal posL strbuf Alist.empty lexbuf in
      FORMAT(rng, Alist.to_list fmtelemacc)
    }

  | "$\'" {
      let posL = Range.from_lexbuf lexbuf in
      let strbuf = Buffer.create 16 in
      let (rng, s) = string_literal posL strbuf lexbuf in
      match MyUtil.Utf.uchar_of_utf8 s with
      | [ uchar ] -> CHAR(rng, uchar)
      | _         -> raise_error (NotASingleCodePoint(rng))
    }

  | ("`" +) {
      let posL = Range.from_lexbuf lexbuf in
      let num_start = String.length (Lexing.lexeme lexbuf) in
      let strbuf = Buffer.create 128 in
      string_block num_start posL strbuf lexbuf
    }

  | eof  { EOI }
  | _ as c { raise_error (UnidentifiedToken(Range.from_lexbuf lexbuf, String.make 1 c)) }

and binary_literal posL strbuf = parse
  | break  { raise_error (SeeBreakInStringLiteral(posL)) }
  | eof    { raise_error (SeeEndOfFileInStringLiteral(posL)) }
  | "\""   { let posR = Range.from_lexbuf lexbuf in (Range.unite posL posR, Buffer.contents strbuf) }
  | "\\\"" { Buffer.add_char strbuf '"'; binary_literal posL strbuf lexbuf }
  | _ as c { Buffer.add_char strbuf c; binary_literal posL strbuf lexbuf }

and string_literal posL strbuf = parse
  | break  { raise_error (SeeBreakInStringLiteral(posL)) }
  | eof    { raise_error (SeeEndOfFileInStringLiteral(posL)) }
  | "\'"   { let posR = Range.from_lexbuf lexbuf in (Range.unite posL posR, Buffer.contents strbuf) }
  | "\\\'" { Buffer.add_char strbuf '\''; string_literal posL strbuf lexbuf }
  | _ as c { Buffer.add_char strbuf c; string_literal posL strbuf lexbuf }

and format_literal posL strbuf acc = parse
  | break  { raise_error (SeeBreakInStringLiteral(posL)) }
  | eof    { raise_error (SeeEndOfFileInStringLiteral(posL)) }

  | "\'" {
      let posR = Range.from_lexbuf lexbuf in
      let elem = flush_buffer strbuf in
      (Range.unite posL posR, Alist.extend acc elem)
    }

  | "\\\'" { Buffer.add_char strbuf '\''; format_literal posL strbuf acc lexbuf }

  | "~~" {
      let elem = flush_buffer strbuf in
      format_literal posL strbuf (Alist.append acc [elem; FormatTilde]) lexbuf
    }

  | "~n" {
      let elem = flush_buffer strbuf in
      format_literal posL strbuf (Alist.append acc [elem; FormatBreak]) lexbuf
    }
  | ("~" (fmtdigits as s1) (hole as c)) {
      let elem = flush_buffer strbuf in
      let hole = hole_of_char c in
      let control =
        {
          field_width = int_of_string_or_empty s1;
          precision   = None;
          padding     = None;
        }
      in
      format_literal posL strbuf (Alist.append acc [elem; FormatHole(hole, control)]) lexbuf
    }

  | ("~" (fmtdigits as s1) "." (fmtdigits as s2) (hole as c)) {
      let elem = flush_buffer strbuf in
      let hole = hole_of_char c in
      let control =
        {
          field_width = int_of_string_or_empty s1;
          precision   = int_of_string_or_empty s2;
          padding     = None;
        }
      in
      format_literal posL strbuf (Alist.append acc [elem; FormatHole(hole, control)]) lexbuf
    }

  | "\\\"" {
      let elem = flush_buffer strbuf in
      format_literal posL strbuf (Alist.append acc [elem; FormatDQuote]) lexbuf
    }

  | _ as c { Buffer.add_char strbuf c; format_literal posL strbuf acc lexbuf }

and string_block num_start posL strbuf = parse
  | ("`" +) {
      let posR = Range.from_lexbuf lexbuf in
      let s = Lexing.lexeme lexbuf in
      let num_end = String.length s in
      if num_end > num_start then
        raise_error (BlockClosedWithTooManyBackQuotes(posR))
      else if num_end = num_start then
        STRING_BLOCK(Range.unite posL posR, Buffer.contents strbuf)
      else begin
        Buffer.add_string strbuf s;
        string_block num_start posL strbuf lexbuf
      end
    }
  | break {
      let s = Lexing.lexeme lexbuf in
      Lexing.new_line lexbuf;
      Buffer.add_string strbuf s;
      string_block num_start posL strbuf lexbuf
    }
  | eof    { raise_error (SeeEndOfFileInStringLiteral(posL)) }
  | _ as c { Buffer.add_char strbuf c; string_block num_start posL strbuf lexbuf }

and comment rng = parse
  | "/*"  { comment (Range.from_lexbuf lexbuf) lexbuf; comment rng lexbuf }
  | "*/"  { () }
  | break { Lexing.new_line lexbuf; comment rng lexbuf }
  | eof   { raise_error (SeeEndOfFileInComment(rng)) }
  | _     { comment rng lexbuf }
