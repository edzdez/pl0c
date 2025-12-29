(**
 * Abstract Syntax Tree for PL/0.
 * Derived from:
 *   https://cs.wmich.edu/~yang/teach/cs485/pl0/pl0grammar.txt
 *)

open! Core
module Mark = Util.Mark
module Ident = Util.Ident

type block =
  { const_decl : const_decl list Mark.t
  ; var_decl : var_decl list Mark.t
  ; proc_decl : proc_decl list Mark.t
  ; stmt : stmt Mark.t
  }

and const_decl = (Ident.t * Int32.t) Mark.t
and var_decl = Ident.t Mark.t
and proc_decl = (Ident.t * block) Mark.t

and stmt =
  | Assign of (Ident.t * expr) Mark.t
  | Call of Ident.t Mark.t
  | Scope of stmt list Mark.t
  | If of (cond * stmt) Mark.t
  | While of (cond * stmt) Mark.t
  | Read of Ident.t Mark.t
  | Write of expr Mark.t

and cond =
  | Odd of expr Mark.t
  | Rel of (rel * expr * expr) Mark.t

and rel =
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

and expr =
  | Unary of (un_op * expr) Mark.t
  | Binary of (bin_op * expr * expr) Mark.t
  | Num of int Mark.t
  | Id of Ident.t Mark.t

and un_op =
  | Plus
  | Minus

and bin_op =
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving sexp]
