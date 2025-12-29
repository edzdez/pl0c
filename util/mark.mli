(**
 * This module provides a wrapper type that includes additional source-file information
 * that will be used for error reporting.
 *)

open! Core

type src_loc =
  { filename : string option
  ; line_no : int
  ; col_no : int
  }
[@@deriving sexp]

val of_position : Lexing.position -> src_loc

type src_span =
  { begin_loc : src_loc
  ; end_loc : src_loc
  }
[@@deriving sexp]

val of_positions : Lexing.position -> Lexing.position -> src_span
val of_lexbuf : Lexing.lexbuf -> src_span
val to_string : src_span -> string

type 'a t =
  { data : 'a
  ; span : src_span
  }
[@@deriving sexp]

(** Create an instance of ['a t] *)
val create : 'a -> src_span -> 'a t
