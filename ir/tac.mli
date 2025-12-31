(**
 * A simple 3-address IR with virtual registers.
 * We lower the elaborated AST produced by the semant pass to this representation.
 *)

open! Core
module Temp = Util.Temp
module Symbol = Util.Symbol

type sym = Symbol.t

module Virt_reg : Temp.Temp
module Label : Temp.Temp

type operand =
  | Reg of Virt_reg.t
  | Const of Int32.t

type program =
  { globals : sym list
  ; procedures : proc list
  }

and proc =
  { name : sym option
  ; locals : sym list
  ; body : block list
  }

and block =
  { label : Label.t
  ; instrs : instr list
  }

and instr =
  | Load of
      { d : Virt_reg.t
      ; s : sym
      }
  | Store of
      { s : operand
      ; d : sym
      }
  | Move of
      { d : Virt_reg.t
      ; s : operand
      }
  | Bin_op of
      { d : Virt_reg.t
      ; op : bin_op
      ; l : operand
      ; r : operand
      }
  | Un_op of
      { d : Virt_reg.t
      ; op : un_op
      ; s : operand
      }
  | Jump of Label.t
  | Cond_jump of
      { tst : Virt_reg.t
      ; yes : Label.t
      ; no : Label.t
      }
  | Call of sym
  | Return
  | Read of sym
  | Write of operand
  | Nop

and un_op =
  | Plus
  | Minus
  | Odd
  | Not

and bin_op =
  | Plus
  | Minus
  | Star
  | Slash
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

val to_string : program -> string
