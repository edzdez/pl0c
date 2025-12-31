open! Core
open Tac
module Symbol = Util.Symbol
module Label = Label

type t =
  { globals : sym list
    (* instructions within a basic block are stored in reverse for efficiency *)
  ; procedures : (Symbol.t, Symbol.t list * (Label.t, instr list) Hashtbl.t) Hashtbl.t
  ; mutable curr_proc : sym option
  }

let build { globals; procedures; _ } : program =
  let procedures =
    Hashtbl.to_alist procedures
    |> List.map ~f:(fun (name, (locals, blocks)) ->
      let body =
        Hashtbl.to_alist blocks
        |> List.sort ~compare:(fun (l, _) (r, _) -> Label.compare l r)
        |> List.map ~f:(fun (label, instrs) -> { label; instrs = List.rev instrs })
      in
      { name = Some name; locals; body })
  in
  { globals; procedures }
;;

let create globals =
  { globals; procedures = Hashtbl.create (module Symbol); curr_proc = None }
;;

let init_proc t ~name ~locals =
  Label.reset ();
  Virt_reg.reset ();
  Hashtbl.add_exn t.procedures ~key:name ~data:(locals, Hashtbl.create (module Label));
  t.curr_proc <- Some name
;;

let unwrap_proc = function
  | None -> failwith "no current procedure"
  | Some proc -> proc
;;

let init_block { curr_proc; procedures; _ } =
  let proc = unwrap_proc curr_proc in
  let label = Label.fresh () in
  let _, blocks = Hashtbl.find_exn procedures proc in
  Hashtbl.add_exn blocks ~key:label ~data:[];
  label
;;

let add_instr { curr_proc; procedures; _ } ~label ~instr =
  let proc = unwrap_proc curr_proc in
  let _, blocks = Hashtbl.find_exn procedures proc in
  Hashtbl.update blocks label ~f:(function
    | None -> failwith "basic block does not exist"
    | Some instrs -> instr :: instrs)
;;
