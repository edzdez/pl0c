open! Core

type src_loc =
  { filename : string option
  ; line_no : int
  ; col_no : int
  }
[@@deriving sexp]

let of_position ({ pos_fname; pos_lnum; pos_cnum; _ } : Lexing.position) =
  { filename = (if String.is_empty pos_fname then None else Some pos_fname)
  ; line_no = pos_lnum
  ; col_no = pos_cnum
  }
;;

let to_string { filename; line_no; col_no } =
  sprintf "%s%d:%d" (Option.value ~default:"" filename) line_no col_no
;;

type src_span =
  { begin_loc : src_loc
  ; end_loc : src_loc
  }
[@@deriving sexp]

let of_lexbuf lexbuf =
  { begin_loc = of_position @@ Lexing.lexeme_start_p lexbuf
  ; end_loc = of_position @@ Lexing.lexeme_end_p lexbuf
  }
;;

type 'a t =
  { data : 'a
  ; span : src_span
  }
[@@deriving sexp]

let create data span = { data; span }
