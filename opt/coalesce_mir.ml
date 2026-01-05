open! Core
open Mir

let rec go = function
  | [] -> []
  | [ x ] -> [ x ]
  | Mov m1 :: (Mov m2 :: rest as rest2) ->
    (match m1.dst, m2.src with
     | Reg r1, Reg r2 ->
       if Reg.equal r1 r2
       then go @@ (Mov { dst = m2.dst; src = m1.src } :: rest)
       else Mov m1 :: (go @@ rest2)
     | _ -> Mov m1 :: (go @@ rest2))
  | x :: rest -> x :: go rest
;;

let go_block { label; data } =
  let instrs = List.map data ~f:(fun { instr; _ } -> instr) in
  { label
  ; data =
      List.map ~f:(fun instr ->
        { instr
        ; uses = Hash_set.create (module Reg)
        ; defs = Hash_set.create (module Reg)
        ; clobbers = Hash_set.create (module Reg)
        })
      @@ go instrs
  }
;;

let coalesce_mov { data; code } = { data; code = List.map code ~f:go_block }
