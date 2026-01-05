open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Constants
module S = Semant
module Mark = Util.Mark

let lower lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast ->
    let ast = S.semant ast in
    let ast = C.fold ast in
    Ir.lower ast
;;

let%expect_test "lowers the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals:

      proc _main:
        locals:

      L0:
        return

      end
      |}])
;;

let%expect_test "lowers a simple program" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        store 20 -> x
        t0 <- load x
        write t0
        return

      end
      |}])
;;

let%expect_test "lowers a program with procedures" =
  In_channel.with_file "../examples/parse_declarationordering1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        call p
        t0 <- load x
        write t0
        return

      end

      proc p:
        locals:

      L0:
        store 1 -> x
        return

      end
      |}])
;;

let%expect_test "lowers a program with nested procedures" =
  In_channel.with_file "../examples/parse_declarationordering8.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc outer:
        locals: y

      L0:
        store 0 -> y
        call inner
        t0 <- load y
        write t0
        return

      end

      proc _main:
        locals:

      L0:
        store 0 -> x
        call outer
        t0 <- load x
        write t0
        return

      end

      proc inner:
        locals:

      L0:
        t0 <- load x
        t1 <- t0 + 1
        store t1 -> x
        t2 <- load y
        t3 <- t2 + 1
        store t3 -> y
        return

      end
      |}])
;;

let%expect_test "lowers if statements" =
  In_channel.with_file "../examples/parse_if.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        store 10 -> x
        t0 <- load x
        t1 <- t0 > 5
        cjump t1 L1 L2


      L1:
        t2 <- load x
        write t2
        jump L2


      L2:
        return

      end
      |}])
;;

let%expect_test "lowers relations" =
  In_channel.with_file "../examples/parse_logicaloperators.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        store 10 -> x
        t0 <- load x
        t1 <- odd t0
        t2 <- not t1
        cjump t2 L1 L8


      L1:
        t3 <- load x
        t4 <- t3 =/= 10
        cjump t4 L2 L3


      L2:
        t5 <- load x
        t6 <- t5 + 1
        write t6
        jump L3


      L3:
        t7 <- load x
        t8 <- t7 < 10
        cjump t8 L4 L5


      L4:
        t9 <- load x
        t10 <- t9 + 2
        write t10
        jump L5


      L5:
        t11 <- load x
        t12 <- t11 >= 10
        cjump t12 L6 L7


      L6:
        t13 <- load x
        write t13
        jump L7


      L7:
        jump L8


      L8:
        return

      end
      |}])
;;

let%expect_test "lowers nested scopes" =
  In_channel.with_file "../examples/parse_nestedblocks.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        store 1 -> x
        t0 <- load x
        write t0
        return

      end
      |}])
;;

let%expect_test "lowers while loops" =
  In_channel.with_file "../examples/parse_while.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ir = lower lexbuf in
    printf "%s\n" (Ir.Tac.to_string ir);
    [%expect
      {|
      globals: x

      proc _main:
        locals:

      L0:
        store 0 -> x
        jump L1


      L1:
        t0 <- load x
        t1 <- t0 < 5
        cjump t1 L2 L3


      L2:
        t2 <- load x
        write t2
        t3 <- load x
        t4 <- t3 + 1
        store t4 -> x
        jump L1


      L3:
        return

      end
      |}])
;;
