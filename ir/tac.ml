open! Core
module Temp = Util.Temp
module Symbol = Util.Symbol

type sym = Symbol.t

module VirtReg = Temp.Make ()
module Label = Temp.Make ()

type operand =
  | Reg of VirtReg.t
  | Const of Int32.t

type program = proc list

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
      { d : VirtReg.t
      ; s : sym
      }
  | Store of
      { d : sym
      ; s : operand
      }
  | Move of
      { d : VirtReg.t
      ; s : operand
      }
  | Bin_op of
      { d : VirtReg.t
      ; op : bin_op
      ; l : operand
      ; r : operand
      }
  | Un_op of
      { d : VirtReg.t
      ; op : un_op
      ; s : operand
      }
  | Jump of Label.t
  | Cond_jump of
      { tst : VirtReg.t
      ; yes : Label.t
      ; no : Label.t
      }
  | Call of sym
  | Return
  | Read of sym
  | Write of operand

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
