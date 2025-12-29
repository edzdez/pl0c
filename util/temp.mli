(**
 * A module for producing temporary values.
 * Also includes a functor for producing such modules
 *)

module type Temp = sig
  type t = int

  (** Reset the internal state.
      Note that subsequent calls to [fresh] may produce values that have been previously assigned. *)
  val reset : unit -> unit

  (** Generate a fresh temporary value *)
  val fresh : unit -> t
end

module Make () : Temp
include Temp
