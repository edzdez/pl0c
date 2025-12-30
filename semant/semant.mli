(**
 * Semantic analysis.
 *
 * This phase accepts the parse tree as input, and consists of the following processes:
 *  - Elaboration: annotate the AST with semantic information.
 *  - Typechecking: check expression types.
 *  - Constant Folding: evaluate constant expressions at compile time.
 *)

module Elab = Elab
module Typecheck = Typecheck
module Constants = Constants

(** Given the raw AST, perform semantic analysis and output the elaborated AST. *)
val semant : Ast.block -> East.eblock

val to_string : East.eblock -> string
