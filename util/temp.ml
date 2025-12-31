module type Temp = sig
  type t = int [@@deriving sexp_of, compare, equal, hash]

  (** Reset the internal state.
      Note that subsequent calls to [fresh] may produce values that have been previously assigned. *)
  val reset : unit -> unit

  (** Generate a fresh temporary value *)
  val fresh : unit -> t
end

module Make () = struct
  open! Core

  type t = int [@@deriving sexp_of, compare, equal, hash]

  let counter = ref 0
  let reset () = counter := 0

  let fresh () =
    let n = !counter in
    counter := n + 1;
    n
  ;;
end

include Make ()
