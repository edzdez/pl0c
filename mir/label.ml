open! Core
module Symbol = Ir.Tac.Symbol

type sym = Symbol.t [@@deriving sexp_of, compare, equal, hash]
type t = string [@@deriving sexp_of, compare, equal, hash]

let of_label ?owner label =
  match owner with
  | None -> sprintf ".L%d" label
  | Some p -> sprintf ".%sL%d" (Symbol.get_exn p).name label
;;

let of_global sym = sprintf "%s" (Symbol.get_exn sym).name
