open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Constants
module S = Semant
module Mark = Util.Mark

let isel lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast ->
    let ast = S.semant ast in
    let ast = C.fold ast in
    let tac = Ir.lower ast in
    Isel.isel tac
;;

let%expect_test "isel for the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = isel lexbuf in
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

let%expect_test "works for a simple program" =
  In_channel.with_file "../examples/parse_simple.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = isel lexbuf in
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
        mov x(%rip), %v0
        mov %v0, %edi
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
    let instrs = isel lexbuf in
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
        mov -4(%rbp), %v0
        mov %v0, %edi
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
        mov x(%rip), %v0
        mov %v0, %v1
        add $1, %v1
        mov %v1, x(%rip)
        mov %rbp, %rdi
        mov 16(%rdi), %rdi
        mov -4(%rbp), %v2
        mov %v2, %v3
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
        mov x(%rip), %v0
        mov %v0, %edi
        push %rbp
        call _write
        add $8, %rsp
        add $8, %rsp
        pop %rbp
        movl $0, %eax
        ret
      |}])
;;
