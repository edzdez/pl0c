open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Fold_constants
module S = Semant
module M = Opt.Mem2reg
module Mark = Util.Mark

let opt lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast -> S.semant ast |> C.fold |> Lower.lower |> M.mem2reg
;;

let%expect_test "doesn't promote globals" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
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
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      |}])
;;

let%expect_test "does not promote variables referenced in nested scopes" =
  In_channel.with_file "../examples/parse_declarationordering8.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      @x := global i32

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

      proc outer {
      L0:
        @y := alloca i32
        store i32 0, ptr<i32> @y
        call inner
        %t0 := load ptr<i32> @y
        write i32 %t0
        ret
      }
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      proc: inner
        succs:
        dom_tree:
        dom_frontiers:
      proc: outer
        succs:
        dom_tree:
        dom_frontiers:
      |}])
;;

let%expect_test "works with linear code" =
  In_channel.with_file "../examples/mem2reg_straightline.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      proc _main {
      L0:
        call p
        ret
      }

      proc p {
      L0:
        @z := alloca i32
        store i32 1, ptr<i32> @z
        %t0 := load ptr<i32> @z
        %t1 := add i32 %t0, 1
        store i32 %t1, ptr<i32> @z
        %t2 := load ptr<i32> @z
        write i32 %t2
        ret
      }
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      proc: p
        succs:
        dom_tree:
        dom_frontiers:
      |}])
;;

let%expect_test "works on ifs" =
  In_channel.with_file "../examples/mem2reg_if.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      proc _main {
      L0:
        call p
        ret
      }

      proc p {
      L0:
        @x := alloca i32
        store i32 0, ptr<i32> @x
        %t0 := load ptr<i32> @x
        %t1 := lt i32 %t0, 5
        br i1 %t1, label L1, label L2

      L1:
        store i32 1, ptr<i32> @x
        jmp label L2

      L2:
        %t3 := phi i32

        store i32 2, ptr<i32> @x
        %t2 := load ptr<i32> @x
        write i32 %t2
        ret
      }
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      proc: p
        succs:
          L0 -> [L2, L1]
          L1 -> [L2]
        dom_tree:
          L0 -> [L1, L2]
        dom_frontiers:
          L0 -> []
          L1 -> [L2]
      |}])
;;

let%expect_test "works on while loops" =
  In_channel.with_file "../examples/mem2reg_while.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      proc p {
      L0:
        @i := alloca i32
        store i32 0, ptr<i32> @i
        jmp label L1

      L1:
        %t5 := phi i32

        %t0 := load ptr<i32> @i
        %t1 := lt i32 %t0, 10
        br i1 %t1, label L2, label L3

      L2:
        %t2 := load ptr<i32> @i
        %t3 := add i32 %t2, 1
        store i32 %t3, ptr<i32> @i
        jmp label L1

      L3:
        %t4 := load ptr<i32> @i
        write i32 %t4
        ret
      }

      proc _main {
      L0:
        call p
        ret
      }
      proc: p
        succs:
          L0 -> [L1]
          L2 -> [L1]
          L1 -> [L3, L2]
        dom_tree:
          L0 -> [L1]
          L1 -> [L3, L2]
        dom_frontiers:
          L0 -> []
          L2 -> [L1]
          L1 -> [L1]
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      |}])
;;

let%expect_test "works with complex control flow" =
  In_channel.with_file "../examples/mem2reg_complex.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = opt lexbuf in
    printf "%s\n" (Ir.to_string ir);
    [%expect
      {|
      proc _main {
      L0:
        call p
        ret
      }

      proc p {
      L0:
        @x := alloca i32
        @y := alloca i32
        store i32 0, ptr<i32> @x
        store i32 0, ptr<i32> @y
        jmp label L1

      L1:
        %t10 := phi i32
        %t8 := phi i32

        %t0 := load ptr<i32> @y
        %t1 := lt i32 %t0, 10
        br i1 %t1, label L2, label L5

      L2:
        %t2 := load ptr<i32> @y
        %t3 := lt i32 %t2, 5
        br i1 %t3, label L3, label L4

      L3:
        %t4 := load ptr<i32> @x
        %t5 := add i32 %t4, 1
        store i32 %t5, ptr<i32> @x
        jmp label L4

      L4:
        %t9 := phi i32

        %t6 := load ptr<i32> @y
        %t7 := add i32 %t6, 1
        store i32 %t7, ptr<i32> @y
        jmp label L1

      L5:
        ret
      }
      proc: _main
        succs:
        dom_tree:
        dom_frontiers:
      proc: p
        succs:
          L0 -> [L1]
          L4 -> [L1]
          L2 -> [L3, L4]
          L3 -> [L4]
          L1 -> [L2, L5]
        dom_tree:
          L0 -> [L1]
          L2 -> [L3, L4]
          L1 -> [L2, L5]
        dom_frontiers:
          L0 -> []
          L4 -> [L1]
          L2 -> [L1]
          L3 -> [L4]
          L1 -> [L1]
      |}])
;;
