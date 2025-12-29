(**
 * An implementation of a symbol table.
 *)

open! Core

type t [@@deriving sexp]

type entry =
  { name : string
  ; kind : kind
  ; owner : t option
  ; value : Int32.t option
  }

and kind =
  | Const
  | Var
  | Proc
[@@deriving sexp]

(** Add an entry to the symbol table. *)
val add : entry -> t

(** Find the entry in the symbol table with the given id.
    If no such entry exists, returns [None]. *)
val get : t -> entry option

(** Find the entry in the symbol table with the given id.
    Throws an error if no such entry exists. *)
val get_exn : t -> entry
