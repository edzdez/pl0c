(**
 * A convenient builder interface for constructing IR code.
 *)

module Symbol = Util.Symbol
module Label = Util.Temp

type t

(** Produce the program currently stored in [t]. *)
val build : t -> Instr.program

(** Create a new [t] with the provided global variables. *)
val create : Instr.value list -> t

(** Create a new procedure with the provided name and local variables, and then makes it
    the current procedure.
    Throws an exception if such a procedure already exists *)
val init_proc : t -> Symbol.t -> unit

(** Create a new basic block in the current procedure. *)
val init_block : t -> Label.t

(** Append an instruction to the basic block with the provided [label].
    Throws an exception if no such block exists. *)
val add_instr : t -> label:Label.t -> Instr.instr -> unit

(** Set the terminator for the basic block with the provided [label].
    Throws an exception if no such block exists. *)
val set_terminator : t -> label:Label.t -> Instr.term -> unit

(** Generate a fresh temporary value. *)
val fresh_tmp : t -> Util.Temp.t
