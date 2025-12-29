open! Core

type src_loc =
  { filename : string option
  ; line_no : int
  ; col_no : int
  }
[@@deriving sexp]

let of_position ({ pos_fname; pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  { filename = (if String.is_empty pos_fname then None else Some pos_fname)
  ; line_no = pos_lnum
  ; col_no = pos_cnum - pos_bol
  }
;;

type src_span =
  { begin_loc : src_loc
  ; end_loc : src_loc
  }
[@@deriving sexp]

let of_positions begin_loc end_loc =
  { begin_loc = of_position begin_loc; end_loc = of_position end_loc }
;;

let of_lexbuf lexbuf =
  { begin_loc = of_position @@ Lexing.lexeme_start_p lexbuf
  ; end_loc = of_position @@ Lexing.lexeme_end_p lexbuf
  }
;;

let to_string { begin_loc; end_loc } =
  (* assume the filename is the same *)
  sprintf
    "%s%d:%d-%d:%d"
    (Option.value ~default:"" begin_loc.filename)
    begin_loc.line_no
    begin_loc.col_no
    end_loc.line_no
    end_loc.col_no
;;

type 'a t =
  { data : 'a
  ; span : src_span
  }
[@@deriving sexp]

let create data span = { data; span }
