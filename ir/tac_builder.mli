(**
 * A more convenient API for building TAC code, for use in lowering the eAST.
 *)

module Symbol = Util.Symbol
module Label = Tac.Label

type t

(** Produce the program currently stored in [t]. *)
val build : t -> Tac.program

(** Create a new [t] with the provided global variables. *)
val create : Symbol.t list -> t

(** Create a new procedure with the provided name and local variables, and then makes it
    the current procedure.
    Throws an exception if such a procedure already exists *)
val init_proc : t -> name:Symbol.t -> locals:Symbol.t list -> unit

(** Create a new basic block in the current procedure. *)
val init_block : t -> Label.t

(** Append an instruction to the basic block with the provided [label].
    Throws an exception if no such block exists. *)
val add_instr : t -> label:Label.t -> instr:Tac.instr -> unit
