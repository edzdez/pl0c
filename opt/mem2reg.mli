(**
 * Mem2reg
 * This pass promotes variables to registers whenever possible, and in doing so makes the
 * IR SSA.
 *)

val mem2reg : Ir.program -> Ir.program
