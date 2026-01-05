open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Constants
module M = Opt.Coalesce_mir
module S = Semant
module Mark = Util.Mark

let opt lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast ->
    let ast = S.semant ast in
    let ast = C.fold ast in
    let tac = Ir.lower ast in
    let instrs = Isel.isel tac in
    M.coalesce_mov instrs
;;

let%expect_test "works for the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string instrs);
    [%expect
      {|
        .data

        .text
        .globl main
      main:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret
      |}])
;;

let%expect_test "coalesces moves in a simple program" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string instrs);
    [%expect
      {|
        .data
      x: .long 0

        .text
        .globl main
      main:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        movl $20, x(%rip)
        mov x(%rip), %edi
        push %rbp
        call _write
        add $8, %rsp
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret
      |}])
;;

let%expect_test "nested procedures" =
  In_channel.with_file "../examples/parse_declarationordering8.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string instrs);
    [%expect
      {|
        .data
      x: .long 0

        .text
        .globl main
      .outerL0:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        movl $0, -4(%rbp)
        push %rbp
        call .innerL0
        add $8, %rsp
        mov -4(%rbp), %edi
        push %rbp
        call _write
        add $8, %rsp
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret

      .innerL0:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        mov x(%rip), %v1
        add $1, %v1
        mov %v1, x(%rip)
        mov %rbp, %rdi
        mov 16(%rdi), %rdi
        mov -4(%rbp), %v3
        add $1, %v3
        mov %rbp, %rdi
        mov 16(%rdi), %rdi
        mov %v3, -4(%rbp)
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret

      main:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        movl $0, x(%rip)
        push %rbp
        call .outerL0
        add $8, %rsp
        mov x(%rip), %edi
        push %rbp
        call _write
        add $8, %rsp
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret
      |}])
;;

let%expect_test "works for if statements" =
  In_channel.with_file "../examples/parse_if.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string instrs);
    [%expect
      {|
        .data
      x: .long 0

        .text
        .globl main
      ._mainL1:
        mov x(%rip), %edi
        push %rbp
        call _write
        add $8, %rsp
        jmp ._mainL2

      ._mainL2:
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret

      main:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        movl $10, x(%rip)
        mov x(%rip), %v1
        cmp $5, %v1
        jg ._mainL1
        jmp ._mainL2
      |}])
;;

let%expect_test "works for while loops" =
  In_channel.with_file "../examples/parse_while.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string instrs);
    [%expect
      {|
        .data
      x: .long 0

        .text
        .globl main
      ._mainL1:
        mov x(%rip), %v1
        cmp $5, %v1
        jl ._mainL2
        jmp ._mainL3

      ._mainL2:
        mov x(%rip), %edi
        push %rbp
        call _write
        add $8, %rsp
        mov x(%rip), %v4
        add $1, %v4
        mov %v4, x(%rip)
        jmp ._mainL1

      ._mainL3:
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret

      main:
        push %rbp
        mov %rsp, %rbp
        sub $8, %rsp
        movl $0, x(%rip)
        jmp ._mainL1
      |}])
;;
