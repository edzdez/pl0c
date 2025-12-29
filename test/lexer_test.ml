open! Core
module L = Parse.Lexer
module T = Parse.Token
module Mark = Util.Mark
module Ident = Util.Ident

let rec lex lexbuf =
  try
    let token = L.read lexbuf in
    printf "%s " (T.to_string token);
    match token with
    | T.EOF -> ()
    | _ -> lex lexbuf
  with
  | L.Syntax_error e -> printf "\n[%s]: %s\n" (Mark.to_string e.span) e.data
;;

let%expect_test "lexes all keywords" =
  let lexbuf =
    Lexing.from_string "const var procedure call begin end if then while do odd"
  in
  lex lexbuf;
  [%expect {| const var procedure call begin end if then while do odd EOF |}]
;;

let%expect_test "keywords are case-insensitive" =
  let lexbuf = Lexing.from_string "const CONST cOnst CONst" in
  lex lexbuf;
  [%expect {| const const const const EOF |}]
;;

let%expect_test "correctly handles comments" =
  In_channel.with_file "../examples/lex_comments.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect {| begin end . EOF |}])
;;

let%expect_test "reports an error on unterminated comments" =
  In_channel.with_file "../examples/lex_comments1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect {| [1:1-3:0]: Unterminated comment. |}])
;;

let%expect_test "reports an error on unterminated nested comments" =
  In_channel.with_file "../examples/lex_comments2.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect {| [1:1-7:0]: Unterminated comment. |}])
;;

let%expect_test "reports an error on integers that are too large" =
  In_channel.with_file "../examples/lex_integers.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect
      {|
      const ident[num] = num[1234567890] , ident[bignum] =
      [1:33-1:43]: Failed to parse numeric constant: 4294967297.
      |}])
;;

let%expect_test "correctly ignores whitespace" =
  In_channel.with_file "../examples/lex_whitespace.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect
      {| const ident[a] = num[10] , ident[b] = num[20] ; var ident[x] , ident[y] ; begin ident[x] := ident[a] + ident[b] ; ident[y] := ident[x] * num[2] ; ! ident[y] end . EOF |}])
;;

let%expect_test "properly lexes a complex program" =
  In_channel.with_file "../examples/fibonacci.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    lex lexbuf;
    [%expect
      {| var ident[a] , ident[b] , ident[t] , ident[n] ; begin ident[a] := num[0] ; ident[b] := num[1] ; ? ident[n] ; while ident[n] > num[0] do begin ! ident[a] ; ident[t] := ident[a] + ident[b] ; ident[a] := ident[b] ; ident[b] := ident[t] ; ident[n] := ident[n] - num[1] end end . EOF |}])
;;
