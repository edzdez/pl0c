(**
 * Semantic analysis.
 *
 * This phase accepts the parse tree as input, and consists of the following processes:
 *  - Elaboration: annotate the AST with semantic information.
 *  - Typechecking: check expression types.
 *  - Constant Folding: evaluate constant expressions at compile time.
 *)

module Elab = Elab
