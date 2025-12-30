(**
 * The elaborated AST.
 *)

module Symbol = Util.Symbol
module Mark = Util.Mark

type sym = Symbol.t
type rel = Ast.rel
type un_op = Ast.un_op
type bin_op = Ast.bin_op

type eblock =
  { consts : (sym * Int32.t) list
  ; vars : sym list
  ; procs : (sym * eblock) list
  ; stmt : estmt
  }

and estmt =
  | Assign of sym * meexpr
  | Call of sym
  | Scope of estmt list
  | If of econd * estmt
  | While of econd * estmt
  | Read of sym
  | Write of meexpr

and econd =
  | Odd of meexpr
  | Not of econd
  | Rel of rel * meexpr * meexpr

and eexpr =
  | Unary of un_op * meexpr
  | Binary of bin_op * meexpr * meexpr
  | Num of Int32.t
  | Sym of sym

and meexpr = eexpr Mark.t
