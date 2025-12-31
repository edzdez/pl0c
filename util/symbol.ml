open! Core

type t = int [@@deriving sexp, compare, equal, hash]

let to_string t = sprintf "sym$%d" t

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

let create ?owner ?value name kind = { name; kind; owner; value }

module Temp = Temp.Make ()

let table = Hashtbl.create (module Int)

let add entry =
  let symb = Temp.fresh () in
  Hashtbl.add_exn table ~key:symb ~data:entry;
  symb
;;

let get key = Hashtbl.find table key
let get_exn key = Hashtbl.find_exn table key
let to_string_hum t = Option.value_map ~default:"<unknown>" ~f:(fun e -> e.name) @@ get t
