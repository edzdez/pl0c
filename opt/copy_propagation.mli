(**
 * SSA copy-propagation.
 * The goal of this pass is to replace redundant copies in SSA code.
 *)

val propagate : Ir.program -> Ir.program
