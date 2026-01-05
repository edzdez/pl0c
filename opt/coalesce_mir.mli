(**
 * Coalesce mov mov chains in the MIR.
 * There are a lot of examples of code that looks like
 *  mov a, b
 *  mov b, c
 * in the currently generated code.
 * Since we currently don't use the former registers for anything else, it should be
 * perfectly fine to collapse these into
 *  mov a, c
 *)

(** Coalesce mov chains in the MIR. Discards liveness information. *)
val coalesce_mov : Mir.program -> Mir.program
