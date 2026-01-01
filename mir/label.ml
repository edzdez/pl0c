open! Core
module Symbol = Ir.Tac.Symbol

type sym = Symbol.t [@@deriving sexp_of, compare, equal, hash]

type t =
  { owner : sym option
  ; label : Ir.Tac.Label.t
  }
[@@deriving sexp_of, compare, equal, hash]
