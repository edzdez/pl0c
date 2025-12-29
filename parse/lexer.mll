{
open! Core
module T = Parser
module Mark = Util.Mark
module Ident = Util.Ident

exception Syntax_error of string Mark.t

let keywords =
  Hashtbl.of_alist_exn
    (module String)
    [ "const", T.CONST
    ; "var", T.VAR
    ; "procedure", T.PROCEDURE
    ; "CALL", T.CALL
    ; "begin", T.BEGIN
    ; "end", T.END
    ; "if", T.IF
    ; "then", T.THEN
    ; "while", T.WHILE
    ; "do", T.DO
    ; "odd", T.ODD
    ]

let raise_error lexbuf msg =
  let src_span = Mark.of_lexbuf lexbuf in
  raise Syntax_error (Mark.create msg src_span)

let number lexbuf =
  let lexeme = Lexing.lexeme lexbuf in
  let value = try Int32.of_string lexeme with Failure _ ->
      raise_error lexbuf @@ sprintf "Failed to parse numeric constant: %s." n
  in T.NUMBER value

let keyword_or_ident lexbuf =
  let lexeme = Lexing.lexeme lexbuf in
  match Hashtbl.find keywords (String.lowercase lexeme) with
  | Some t -> t
  | None -> T.IDENT (Ident.add lexeme)
}

let ws    = [' ' '\t']+
let nl    = '\r' | '\n' | "\r\n"

let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

let num   = '0' | ['1'-'9'] digit*
let ident = alpha (alpha | digit)*

rule read =
  parse
  | ws+           { read lexbuf }
  | nl            { Lexing.new_line lexbuf; read lexbuf}

  | '('           { T.LPAREN }
  | ')'           { T.RPAREN }

  | '.'           { T.DOT }
  | ','           { T.COMMA }
  | ';'           { T.SEMICOLON }

  | '='           { T.EQ }
  | "=/="         { T.NEQ }
  | '<'           { T.LT }
  | "<="          { T.LEQ }
  | '>'           { T.GT }
  | ">="          { T.GEQ }

  | '+'           { T.PLUS }
  | '-'           { T.MINUS }
  | '*'           { T.STAR }
  | '/'           { T.SLASH }

  | ":="          { T.WALRUS }
  | '?'           { T.QUESTION }
  | '!'           { T.BANG }

  | num           { number lexbuf }
  | ident         { keyword_or_ident lexbuf }

  | '{'           { consume_comment 0 lexbuf }
  | eof           { T.EOF }
  | _             { raise_error lexbuf
                    @@ sprintf "Unexpected character: %s." (Lexing.lexeme lexbuf) }

and consume_comment nesting =
  parse
  | nl            { Lexing.new_line lexbuf; consume_comment nesting lexbuf }
  | '{'           { consume_comment (nesting + 1) lexbuf }
  | '}'           { if nesting = 0 then
                      read lexbuf
                    else consume_comment (nesting - 1) lexbuf }
  | eof           { raise_error lexbuf "Unterminated comment." }
  | _             { consume_comment nesting lexbuf }

