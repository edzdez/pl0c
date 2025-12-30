(**
 * Semantic Elaboration.
 *
 * In this module, we
 *  1. construct a symbol table where each declaration is assigned a unique symbol id and
 *     given some semantic information (kind, type (eventually) owning procedure, etc.).
 *  2. rewrite the AST so that each reference to a logical identifier is replaced with a
 *     reference to the correct symbol id.
 *  3. check some basic `kind` information, e.g. we only assign to variables and call
 *     procedures.
 *
 * This step emits errors for:
 *  - Undeclared identifiers
 *  - Duplicate identifiers
 *  - Invalid kinds
 *)

open! Core
open East

type elab_error =
  | Undeclared_ident of string
  | Duplicate_ident of string
  | Not_lvalue of string
  | Not_procedure of string
[@@deriving sexp]

exception Elab_error of elab_error Mark.t

(** Construct an elaborated AST from the original *)
val elab : Ast.block -> eblock
