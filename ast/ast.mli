(**
 * Abstract Syntax Tree for PL/0.
 * Derived from:
 *   https://cs.wmich.edu/~yang/teach/cs485/pl0/pl0grammar.txt
 *)

open! Core
module Mark = Util.Mark
module Ident = Util.Ident

type mident = Ident.t Mark.t

type block =
  { const_decl : const_decl list
  ; var_decl : var_decl list
  ; proc_decl : proc_decl list
  ; stmt : stmt
  }

and const_decl = mident * Int32.t
and var_decl = mident
and proc_decl = mident * block

and stmt =
  | Assign of mident * mexpr
  | Call of mident
  | Scope of stmt list
  | If of (cond * stmt)
  | While of (cond * stmt)
  | Read of mident
  | Write of mexpr

and cond =
  | Odd of mexpr
  | Rel of (rel * mexpr * mexpr)

and rel =
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

and expr =
  | Unary of (un_op * mexpr)
  | Binary of (bin_op * mexpr * mexpr)
  | Num of Int32.t
  | Id of mident

and mexpr = expr Mark.t

and un_op =
  | UnPlus
  | UnMinus

and bin_op =
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving sexp]
