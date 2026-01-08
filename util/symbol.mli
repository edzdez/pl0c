(**
 * An implementation of a symbol table.
 *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

val to_string : t -> string
val to_string_hum : t -> string

type entry =
  { name : string
  ; kind : kind
  ; owner : t option
  }

and kind =
  | Const of { value : Int32.t }
  | Var of { slot : Int32.t option }
  | Proc of
      { temps : (module Temp.Temp)
      ; labels : (module Temp.Temp)
      }

(** Create an entry *)
val create : ?owner:t -> string -> kind -> entry

(** Add an entry to the symbol table. *)
val add : entry -> t

(** Set the stack slot for a symbol in the symbol table.
    Throws an exception if no such entry exists *)
val set_slot : t -> Int32.t -> unit

(** Set the owner for a symbol in the symbol table.
    Throws an exception if no such entry exists *)
val set_owner : t -> t -> unit

(** Find the entry in the symbol table with the given id.
    If no such entry exists, returns [None]. *)
val get : t -> entry option

(** Find the entry in the symbol table with the given id.
    Throws an error if no such entry exists. *)
val get_exn : t -> entry
