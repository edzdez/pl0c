open! Core
module Ident = Util.Ident

type token =
  | WHILE
  | WALRUS
  | VAR
  | THEN
  | STAR
  | SLASH
  | SEMICOLON
  | RPAREN
  | QUESTION
  | PROCEDURE
  | PLUS
  | ODD
  | NUMBER of Int32.t
  | NOT
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LEQ
  | IF
  | IDENT of Ident.t
  | GT
  | GEQ
  | EQ
  | EOF
  | END
  | DOT
  | DO
  | CONST
  | COMMA
  | CALL
  | BEGIN
  | BANG

let to_string token =
  match token with
  | WHILE -> "while"
  | WALRUS -> ":="
  | VAR -> "var"
  | THEN -> "then"
  | STAR -> "*"
  | SLASH -> "/"
  | SEMICOLON -> ";"
  | RPAREN -> ")"
  | QUESTION -> "?"
  | PROCEDURE -> "procedure"
  | PLUS -> "+"
  | ODD -> "odd"
  | NUMBER n -> sprintf "num[%ld]" n
  | NOT -> "not"
  | NEQ -> "=/="
  | MINUS -> "-"
  | LT -> "<"
  | LPAREN -> "("
  | LEQ -> "<="
  | IF -> "if"
  | IDENT x -> sprintf "ident[%s]" (Ident.get_exn x)
  | GT -> ">"
  | GEQ -> ">="
  | EQ -> "="
  | EOF -> "EOF"
  | END -> "end"
  | DOT -> "."
  | DO -> "do"
  | CONST -> "const"
  | COMMA -> ","
  | CALL -> "call"
  | BEGIN -> "begin"
  | BANG -> "!"
;;
