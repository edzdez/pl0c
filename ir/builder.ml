open! Core
open Instr
module Symbol = Util.Symbol
module Label = Util.Temp

type t =
  { globals : value list
    (* instructions within a basic block are stored in reverse for efficiency *)
  ; procedures : (Symbol.t, (Label.t, instr list * term) Hashtbl.t) Hashtbl.t
  ; mutable curr_proc : sym option
  }

let build { globals; procedures; _ } : program =
  let procedures =
    Hashtbl.to_alist procedures
    |> List.map ~f:(fun (name, blocks) ->
      let blocks =
        Hashtbl.to_alist blocks
        |> List.sort ~compare:(fun (l, _) (r, _) -> Label.compare l r)
        |> List.map ~f:(fun (label, (instrs, terminator)) ->
          { label; joins = []; instrs = List.rev instrs; terminator })
      in
      { name; blocks })
  in
  { globals; procedures }
;;

let create globals =
  { globals; procedures = Hashtbl.create (module Symbol); curr_proc = None }
;;

let init_proc t name =
  Hashtbl.add_exn t.procedures ~key:name ~data:(Hashtbl.create (module Label));
  t.curr_proc <- Some name
;;

let unwrap_proc = function
  | None -> failwith "no current procedure"
  | Some proc -> proc
;;

let init_block { curr_proc; procedures; _ } =
  let proc = unwrap_proc curr_proc in
  let label = Label.fresh () in
  let blocks = Hashtbl.find_exn procedures proc in
  Hashtbl.add_exn blocks ~key:label ~data:([], Ret);
  label
;;

let add_instr { curr_proc; procedures; _ } ~label instr =
  let proc = unwrap_proc curr_proc in
  let blocks = Hashtbl.find_exn procedures proc in
  Hashtbl.update blocks label ~f:(function
    | None -> failwith "basic block does not exist"
    | Some (instrs, term) -> instr :: instrs, term)
;;

let set_terminator { curr_proc; procedures; _ } ~label term =
  let proc = unwrap_proc curr_proc in
  let blocks = Hashtbl.find_exn procedures proc in
  Hashtbl.update blocks label ~f:(function
    | None -> failwith "basic block does not exist"
    | Some (instrs, _) -> instrs, term)
;;
