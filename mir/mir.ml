open! Core
module Symbol = Ir.Tac.Symbol
module Label = Ir.Tac.Label
module Virt_reg = Ir.Tac.Virt_reg

type vreg = Virt_reg.t
type sym = Symbol.t

type label =
  { owner : sym option
  ; label : Label.t
  }

type preg =
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
[@@deriving sexp, compare, equal, hash]

type reg =
  | Phys of preg
  | Virt of vreg

type mem_addr =
  | Static of label
  | Dynamic of
      { base : reg
      ; offset : int
      }

type operand =
  | Imm32 of Int32.t
  | Reg of reg
  | Addr of mem_addr
  | Sym of sym

type liveness =
  { instr : minstr
  ; uses : vreg Hash_set.t
  ; defs : vreg Hash_set.t
  ; clobbers : preg Hash_set.t
  }

and minstr =
  | Mov of
      { dst : vreg
      ; src : operand
      }
  | Add of
      { dst : vreg
      ; src : operand
      }
  | Sub of
      { dst : vreg
      ; src : operand
      }
  | IMul of
      { dst : vreg
      ; src : operand
      }
  | IDiv of
      { quot : vreg
      ; rem : vreg
      ; dividend : operand
      ; divisor : operand
      }
  | Neg of { dst : vreg }
  | Not of { dst : vreg }
  | Call of operand
  | Jmp of label
  | JE of label
  | JNE of label
  | JL of label
  | JLE of label
  | JG of label
  | JGE of label
  | SETE of reg
  | SETNE of reg
  | SETL of reg
  | SETLE of reg
  | SETG of reg
  | SETGE of reg
  | Ret
