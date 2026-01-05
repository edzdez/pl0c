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
    ast
    |> S.semant
    |> C.fold
    |> Ir.lower
    |> Isel.isel
    |> M.coalesce_mov
    |> Liveness.populate_liveness
;;

let%expect_test "works for the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string ~liveness:true instrs);
    [%expect
      {|
        .data

        .text
        .globl main
      main:
        push %rbp		[ defs: { %rsp }; uses: { %rsp, %rbp }; clobbers: { %rsp } ]
        mov %rsp, %rbp		[ defs: { %rbp }; uses: { %rsp }; clobbers: { %rbp } ]
        sub $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        add $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        pop %rbp		[ defs: { %rsp, %rbp }; uses: { %rsp }; clobbers: { %rsp, %rbp } ]
        movl $0, %eax		[ defs: { %eax }; uses: {  }; clobbers: { %eax } ]
        ret		[ defs: {  }; uses: {  }; clobbers: {  } ]
      |}])
;;

let%expect_test "works for a complex program" =
  In_channel.with_file "../examples/fibonacci.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let instrs = opt lexbuf in
    printf "%s\n" (Mir.to_string ~liveness:true instrs);
    [%expect
      {|
        .data
      a: .long 0
      b: .long 0
      t: .long 0
      n: .long 0

        .text
        .globl main
      ._mainL1:
        mov n(%rip), %v1		[ defs: { %v1 }; uses: {  }; clobbers: { %v1 } ]
        cmp $0, %v1		[ defs: { FLAGS }; uses: { %v1 }; clobbers: { FLAGS } ]
        jg ._mainL2		[ defs: {  }; uses: { FLAGS }; clobbers: {  } ]
        jmp ._mainL3		[ defs: {  }; uses: {  }; clobbers: {  } ]

      ._mainL2:
        mov a(%rip), %edi		[ defs: { %edi }; uses: {  }; clobbers: { %edi } ]
        push %rbp		[ defs: { %rsp }; uses: { %rsp, %rbp }; clobbers: { %rsp } ]
        call _write		[ defs: { %eax }; uses: { %edi }; clobbers: { %esi, %ecx, %eax, %edx, %rdi } ]
        add $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        mov a(%rip), %v3		[ defs: { %v3 }; uses: {  }; clobbers: { %v3 } ]
        mov b(%rip), %v4		[ defs: { %v4 }; uses: {  }; clobbers: { %v4 } ]
        mov %v3, %v5		[ defs: { %v5 }; uses: { %v3 }; clobbers: { %v5 } ]
        add %v4, %v5		[ defs: { %v5 }; uses: { %v4 }; clobbers: {  } ]
        mov %v5, t(%rip)		[ defs: {  }; uses: { %v5 }; clobbers: {  } ]
        mov b(%rip), a(%rip)		[ defs: {  }; uses: {  }; clobbers: {  } ]
        mov t(%rip), b(%rip)		[ defs: {  }; uses: {  }; clobbers: {  } ]
        mov n(%rip), %v9		[ defs: { %v9 }; uses: {  }; clobbers: { %v9 } ]
        sub $1, %v9		[ defs: { %v9 }; uses: {  }; clobbers: {  } ]
        mov %v9, n(%rip)		[ defs: {  }; uses: { %v9 }; clobbers: {  } ]
        jmp ._mainL1		[ defs: {  }; uses: {  }; clobbers: {  } ]

      ._mainL3:
        add $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        pop %rbp		[ defs: { %rsp, %rbp }; uses: { %rsp }; clobbers: { %rsp, %rbp } ]
        movl $0, %eax		[ defs: { %eax }; uses: {  }; clobbers: { %eax } ]
        ret		[ defs: {  }; uses: {  }; clobbers: {  } ]

      main:
        push %rbp		[ defs: { %rsp }; uses: { %rsp, %rbp }; clobbers: { %rsp } ]
        mov %rsp, %rbp		[ defs: { %rbp }; uses: { %rsp }; clobbers: { %rbp } ]
        sub $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        movl $0, a(%rip)		[ defs: {  }; uses: {  }; clobbers: {  } ]
        movl $1, b(%rip)		[ defs: {  }; uses: {  }; clobbers: {  } ]
        lea n(%rip), %rdi		[ defs: { %rdi }; uses: {  }; clobbers: { %rdi } ]
        push %rbp		[ defs: { %rsp }; uses: { %rsp, %rbp }; clobbers: { %rsp } ]
        call _read		[ defs: { %eax }; uses: { %edi }; clobbers: { %esi, %ecx, %eax, %edx, %rdi } ]
        add $8, %rsp		[ defs: { %rsp }; uses: {  }; clobbers: {  } ]
        jmp ._mainL1		[ defs: {  }; uses: {  }; clobbers: {  } ]
      |}])
;;
