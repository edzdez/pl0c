(**
 * Constant Folding.
 *
 * This module evaluates constant expressions at compile time.
 * In particular, we inline references to constant variables and fold arithmetic
 * expressions where all arguments are proven constants, i.e. we don't ever fold vars.
 *)

open! Core
open Semant.East

type constant_folding_error =
  | Overflow
  | Division_by_zero
[@@deriving sexp]

exception Constant_folding_error of constant_folding_error Mark.t

(** Fold constants in the elaborated AST. *)
val fold : eblock -> eblock
