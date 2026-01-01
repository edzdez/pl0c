open! Core
open Instr

type t =
  { data : sym list
  ; blocks : (label, minstr list) Hashtbl.t
  }

let build { data; blocks } =
  let code =
    Hashtbl.to_alist blocks
    |> List.map ~f:(fun (label, instrs) ->
      { label
      ; data =
          List.rev instrs
          |> List.map ~f:(fun instr ->
            { instr
            ; uses = Hash_set.create (module Reg)
            ; defs = Hash_set.create (module Reg)
            ; clobbers = Hash_set.create (module Reg)
            })
      })
  in
  { data; code }
;;

let create globals = { data = globals; blocks = Hashtbl.create (module Label) }

let add_instr { blocks; _ } ~label ~instr =
  Hashtbl.update blocks label ~f:(function
    | None -> [ instr ]
    | Some xs -> instr :: xs)
;;
