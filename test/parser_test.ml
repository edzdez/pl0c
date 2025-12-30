open! Core
module P = Parse.Parser
module L = Parse.Lexer
module M = Parse.Parser_messages
module Mark = Util.Mark
module Ident = Util.Ident

let parse lexbuf =
  try P.program L.read lexbuf with
  | P.Error state ->
    printf "%d\n" state;
    printf "[%s]: %s\n" (Mark.of_lexbuf lexbuf |> Mark.to_string) (M.message state);
    None
;;

let%expect_test "parses the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect {| begin end. |}])
;;

let%expect_test "parses simple programs" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        const a = 10;
        var x;
        begin
          x := (a) + (10)
        end.
        |}])
;;

let%expect_test "parses programs with multiple procedures" =
  In_channel.with_file "../examples/parse_multipleprocedures.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        procedure p;
        begin end;
        procedure q;
        begin end;
        begin end.
        |}])
;;

let%expect_test "parses programs with nested blocks" =
  In_channel.with_file "../examples/parse_nestedblocks.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          begin
            begin
              x := 1
            end
          end;
          !(x)
        end.
        |}])
;;

let%expect_test "parses a program with valid declaration ordering" =
  In_channel.with_file "../examples/parse_declarationordering1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        const a = 1;
        var x;
        procedure p;
        begin
          x := a
        end;
        begin
          call p;
          !(x)
        end.
        |}])
;;

let%expect_test "emits an error when illegal tokens appear after a var declaration" =
  In_channel.with_file "../examples/parse_declarationordering2.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        19
        [3:0-3:5]: Illegal token after [var] declaration.
        |}]
    | Some _ -> assert false)
;;

let%expect_test "declaration order rules should still be enforced inside of nested blocks"
  =
  In_channel.with_file "../examples/parse_declarationordering3.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        19
        [4:0-4:5]: Illegal token after [var] declaration.
        |}]
    | Some _ -> assert false)
;;

let%expect_test "emits an error when illegal tokens appear after a procedure declaration" =
  In_channel.with_file "../examples/parse_declarationordering4.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        24
        [4:0-4:3]: Illegal token after [procedure] declaration.
        |}]
    | Some _ -> assert false)
;;

let%expect_test "emits an error when a top-level begin...end is not terminated by a ." =
  In_channel.with_file "../examples/parse_declarationordering5.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        91
        [2:9-2:10]: A top-level statement must be terminated by a [.].
        |}]
    | Some _ -> assert false)
;;

let%expect_test "emits an error when other top-level statements are not terminated by ." =
  In_channel.with_file "../examples/parse_declarationordering6.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        91
        [4:6-4:7]: A top-level statement must be terminated by a [.].
        |}]
    | Some _ -> assert false)
;;

let%expect_test "emits an error when illegal tokens appear after a [begin]" =
  In_channel.with_file "../examples/parse_declarationordering7.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        76
        [3:2-3:11]: Illegal token after [begin].
        |}]
    | Some _ -> assert false)
;;

let%expect_test "procedures are allowed to be nested" =
  In_channel.with_file "../examples/parse_declarationordering8.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        procedure outer;
        var y;
        procedure inner;
        begin
          x := (x) + (1);
          y := (y) + (1)
        end;
        begin
          y := 0;
          call inner;
          !(y)
        end;
        begin
          x := 0;
          call outer;
          !(x)
        end.
        |}])
;;

let%expect_test "emits an error when tokens appear after a top-level [.]" =
  In_channel.with_file "../examples/parse_declarationordering9.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        92
        [2:0-2:5]: A top-level [.] should end the program.
        |}]
    | Some _ -> assert false)
;;

let%expect_test "respects operator precedence" =
  In_channel.with_file "../examples/parse_operatorprecedence.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := (2) + ((3) * (4));
          !(x)
        end.
        |}])
;;

let%expect_test "respects parentheses" =
  In_channel.with_file "../examples/parse_parens.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := ((2) + (3)) * (4);
          !(x)
        end.
        |}])
;;

let%expect_test "correctly parses unary minus" =
  In_channel.with_file "../examples/parse_unaryminus.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := (-(5)) + (2);
          !(x)
        end.
        |}])
;;

let%expect_test "correctly parses [if] statements" =
  In_channel.with_file "../examples/parse_if.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := 10;
          if (x) > (5) then
            !(x)
        end.
        |}])
;;

let%expect_test "correctly parses [while] statements" =
  In_channel.with_file "../examples/parse_while.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := 0;
          while (x) < (5) do
            begin
              !(x);
              x := (x) + (1)
            end
        end.
        |}])
;;

let%expect_test "correctly parses logical operators" =
  In_channel.with_file "../examples/parse_logicaloperators.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Ast.to_string ast);
      [%expect
        {|
        var x;
        begin
          x := 10;
          if not(odd(x)) then
            begin
              if (x) =/= (10) then
                !((x) + (1));
              if (x) < (10) then
                !((x) + (2));
              if (x) >= (10) then
                !(x)
            end
        end.
        |}])
;;

let%expect_test "emits an error on an unterminated declaration" =
  In_channel.with_file "../examples/parse_missingsemicolon.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = parse lexbuf in
    match ast with
    | None ->
      [%expect
        {|
        14
        [2:0-2:5]: Illegal token in [var] declaration. Expected [,] or [;].
        |}]
    | Some _ -> assert false)
;;
