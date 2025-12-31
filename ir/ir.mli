(**
 * IRs
 *)

module Ast = Semant.East
module Tac : module type of Tac

(** Lowers the elaborated AST to 3-address code.
    Expects constant propagation to have been run. *)
val lower : Ast.eblock -> Tac.program
