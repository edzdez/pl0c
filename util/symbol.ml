open! Core

type t = int [@@deriving sexp, compare, equal, hash]

let to_string t = sprintf "sym$%d" t

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

let create ?owner name kind = { name; kind; owner }

module Temp = Temp.Make ()

let table = Hashtbl.create (module Int)

let add entry =
  let symb = Temp.fresh () in
  Hashtbl.add_exn table ~key:symb ~data:entry;
  symb
;;

let set_slot sym slot =
  Hashtbl.update table sym ~f:(function
    | None -> failwith "no such symbol."
    | Some entry ->
      (match entry.kind with
       | Var _ -> { entry with kind = Var { slot = Some slot } }
       | _ -> failwith "invalid kind."))
;;

let set_owner sym owner =
  Hashtbl.update table sym ~f:(function
    | None -> failwith "no such symbol."
    | Some entry -> { entry with owner = Some owner })
;;

let get key = Hashtbl.find table key
let get_exn key = Hashtbl.find_exn table key
let to_string_hum t = Option.value_map ~default:"<unknown>" ~f:(fun e -> e.name) @@ get t
