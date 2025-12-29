open! Core

module T = struct
  type t = int [@@deriving sexp, compare, equal]

  let by_value = Hashtbl.create (module String)
  let by_id = Hashtbl.create (module Int)

  module Temp = Temp.Make ()

  let add value =
    match Hashtbl.find by_value value with
    | Some v -> v
    | None ->
      let ident = Temp.fresh () in
      Hashtbl.add_exn by_value ~key:value ~data:ident;
      Hashtbl.add_exn by_id ~key:ident ~data:value;
      ident
  ;;

  let get key = Hashtbl.find by_id key
  let get_exn key = Hashtbl.find_exn by_id key
end

include T
include Comparator.Make (T)
