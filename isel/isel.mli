(**
 * Instruction selection.
 * Generate a pseudo-x86-64 assembly from the TAC IR.
 * This pass consists of three phases:
 *  1. select the x86 instructions to use and
 *  2. resolve stack symbols.
 *)

open Ir

(** convert the 3-address code IR into machine IR.
    Fully resolves symbols, but does not populate liveness information. *)
val isel : Tac.program -> Mir.program
