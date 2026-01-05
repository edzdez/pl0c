(**
 * Liveness analysis.
 * This module provides functionality for working wih the liveness info in the MIR.
 *)

val populate_liveness : Mir.program -> Mir.program
