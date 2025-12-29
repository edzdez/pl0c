open! Core

type t = int [@@deriving sexp]

let by_value = Hashtbl.create (module String)
let by_id = Hashtbl.create (module Int)
let next_ident = ref 0

let incr_ident () =
  let ident = !next_ident in
  next_ident := ident + 1;
  ident
;;

let add value =
  match Hashtbl.find by_value value with
  | Some v -> v
  | None ->
    let ident = incr_ident () in
    Hashtbl.add_exn by_value ~key:value ~data:ident;
    Hashtbl.add_exn by_id ~key:ident ~data:value;
    ident
;;

let get key = Hashtbl.find by_id key
let get_exn key = Hashtbl.find_exn by_id key
