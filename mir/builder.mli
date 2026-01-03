(**
 * A more convenient API for building MIR programs.
 * Similar to the Tac_builder interface.
 *)

open Instr

type t

(** Produce the program currently stored in [t]. *)
val build : t -> program

(** Create a new [t] with the provided global variables. *)
val create : sym list -> t

(** Append an instruction to the provided [label]. *)
val add_instr : t -> label:label -> minstr -> unit

(** Append multiple instructions to the provided [label]. *)
val add_instrs : t -> label:label -> minstr list -> unit
