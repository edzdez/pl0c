(**
 * A global symbol table that stores identifiers.
 *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

include Comparator.S with type t := t

(** Add an identifier to the symbol table.
    If this identifier already exists, then return the corresponding identifier id. *)
val add : string -> t

(** Find the identifier in the symbol table with the given id.
    If no such identifier exists, returns [None]. *)
val get : t -> string option

(** Find the identifier in the symbol table with the given id.
    Throws an exception if no such identifier exists. *)
val get_exn : t -> string
