(**
 * An SSA 3-address IR.
 *)

open! Core
module Symbol = Util.Symbol

type sym = Symbol.t
type lbl = Util.Temp.t
type tmp = Util.Temp.t

type ty =
  | I1
  | I32
  | Lbl

type kind =
  | Lit of Int32.t
  | Ptr of sym
  | Tmp of tmp

type value =
  { ty : ty
  ; kind : kind
  }

type binary =
  { dst : value
  ; lhs : value
  ; rhs : value
  }

type unary =
  { dst : value
  ; src : value
  }

type program =
  { globals : value list
  ; procedures : procedure list
  }

and procedure =
  { name : sym
  ; blocks : block list
  }

and block =
  { label : lbl
  ; joins : phi list
  ; instrs : instr list
  ; terminator : term
  }

and phi =
  { dst : value
  ; srcs : (lbl, value) Hashtbl.t
  }

and instr =
  | Add of binary
  | Sub of binary
  | Mul of binary
  | Div of binary
  | Eq of binary
  | Neq of binary
  | Lt of binary
  | Leq of binary
  | Gt of binary
  | Geq of binary
  | Not of unary
  | Neg of unary
  | Odd of unary
  | Call of sym
  | Read of value
  | Write of value
  | Alloca of value
  | Store of unary
  | Load of unary

and term =
  | Ret
  | Jmp of lbl
  | Br of
      { cond : value
      ; yes : lbl
      ; no : lbl
      }

val to_string : program -> string
