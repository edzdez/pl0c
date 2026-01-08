open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Fold_constants
module S = Semant
module Mark = Util.Mark

let lower lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast -> S.semant ast |> C.fold |> Lower.lower
;;

let%expect_test "lowers the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      proc _main {
      L0:
        ret
      }
      |}])
;;

let%expect_test "lowers a simple program" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        store i32 20, ptr<i32> @x
        %t0 := load ptr<i32> @x
        write i32 %t0
        ret
      }
      |}])
;;

let%expect_test "lowers a program with procedures" =
  In_channel.with_file "../examples/parse_declarationordering1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        call p
        %t0 := load ptr<i32> @x
        write i32 %t0
        ret
      }

      proc p {
      L0:
        store i32 1, ptr<i32> @x
        ret
      }
      |}])
;;

let%expect_test "lowers a program with nested procedures" =
  In_channel.with_file "../examples/parse_declarationordering8.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc outer {
      L0:
        @y := alloca i32
        store i32 0, ptr<i32> @y
        call inner
        %t0 := load ptr<i32> @y
        write i32 %t0
        ret
      }

      proc _main {
      L0:
        store i32 0, ptr<i32> @x
        call outer
        %t0 := load ptr<i32> @x
        write i32 %t0
        ret
      }

      proc inner {
      L0:
        %t0 := load ptr<i32> @x
        %t1 := add i32 %t0, 1
        store i32 %t1, ptr<i32> @x
        %t2 := load ptr<i32> @y
        %t3 := add i32 %t2, 1
        store i32 %t3, ptr<i32> @y
        ret
      }
      |}])
;;

let%expect_test "lowers if statements" =
  In_channel.with_file "../examples/parse_if.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        store i32 10, ptr<i32> @x
        %t0 := load ptr<i32> @x
        %t1 := gt i32 %t0, 5
        br i1 %t1, label L1, label L2

      L1:
        %t2 := load ptr<i32> @x
        write i32 %t2
        jmp label L2

      L2:
        ret
      }
      |}])
;;

let%expect_test "lowers relations" =
  In_channel.with_file "../examples/parse_logicaloperators.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        store i32 10, ptr<i32> @x
        %t0 := load ptr<i32> @x
        %t1 := odd i32 %t0
        %t2 := not i1 %t1
        br i1 %t2, label L1, label L8

      L1:
        %t3 := load ptr<i32> @x
        %t4 := neq i32 %t3, 10
        br i1 %t4, label L2, label L3

      L2:
        %t5 := load ptr<i32> @x
        %t6 := add i32 %t5, 1
        write i32 %t6
        jmp label L3

      L3:
        %t7 := load ptr<i32> @x
        %t8 := lt i32 %t7, 10
        br i1 %t8, label L4, label L5

      L4:
        %t9 := load ptr<i32> @x
        %t10 := add i32 %t9, 2
        write i32 %t10
        jmp label L5

      L5:
        %t11 := load ptr<i32> @x
        %t12 := geq i32 %t11, 10
        br i1 %t12, label L6, label L7

      L6:
        %t13 := load ptr<i32> @x
        write i32 %t13
        jmp label L7

      L7:
        jmp label L8

      L8:
        ret
      }
      |}])
;;

let%expect_test "lowers nested scopes" =
  In_channel.with_file "../examples/parse_nestedblocks.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        store i32 1, ptr<i32> @x
        %t0 := load ptr<i32> @x
        write i32 %t0
        ret
      }
      |}])
;;

let%expect_test "lowers while loops" =
  In_channel.with_file "../examples/parse_while.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

      proc _main {
      L0:
        store i32 0, ptr<i32> @x
        jmp label L1

      L1:
        %t0 := load ptr<i32> @x
        %t1 := lt i32 %t0, 5
        br i1 %t1, label L2, label L3

      L2:
        %t2 := load ptr<i32> @x
        write i32 %t2
        %t3 := load ptr<i32> @x
        %t4 := add i32 %t3, 1
        store i32 %t4, ptr<i32> @x
        jmp label L1

      L3:
        ret
      }
      |}])
;;
