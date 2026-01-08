(**
 * Semantic analysis.
 *
 * This phase accepts the parse tree as input, and consists of the following processes:
 *  - Elaboration: annotate the AST with semantic information.
 *  - Typechecking: check expression types.
 *)

module Elab = Elab
module Typecheck = Typecheck

(** Given the raw AST, perform semantic analysis and output the elaborated AST. *)
val semant : Ast.block -> East.eblock
