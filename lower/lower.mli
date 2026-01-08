(**
 * AST lowering.
 * Initially, we will treat every local variable as if it had a slot in memory.
 * After this pass, we will implement a mem2reg pass that promotes locals (that don't escape)
 * to registers and makes the resulting program SSA.
 *)

(** Lowers the elaborated AST to 3-address code.
    Expects constant propagation to have been run. *)
val lower : East.eblock -> Ir.program
