open! Core
open Ir
module Temp = Util.Temp
module Lbl = Temp

(** To compute dominators, we implement the Lengauer-Tarjan algorithm. *)
module Lengauer_tarjan = struct
  let find_idoms ~graph ~preds =
    (* the dfs part *)
    let node2num = Hashtbl.create (module Lbl) in
    let parent = Hashtbl.create (module Lbl) in
    let module Counter = Temp.Make () in
    let rec visit u =
      let num = Counter.fresh () in
      Hashtbl.add_exn node2num ~key:u ~data:num;
      Hashtbl.find graph u
      |> Option.value ~default:[]
      |> List.iter ~f:(fun v ->
        if not @@ Hashtbl.mem node2num v
        then (
          Hashtbl.add_exn parent ~key:v ~data:u;
          visit v))
    in
    visit 0;
    (* the actual algorithm *)
    let idom = Hashtbl.create (module Lbl) in
    let semi = Hashtbl.create (module Lbl) in
    let best = Hashtbl.create (module Lbl) in
    let bucket = Hashtbl.create (module Lbl) in
    let ancestor = Hashtbl.create (module Lbl) in
    Hashtbl.iter_keys node2num ~f:(fun v ->
      Hashtbl.set semi ~key:v ~data:v;
      Hashtbl.set best ~key:v ~data:v;
      Hashtbl.set bucket ~key:v ~data:[]);
    let rec eval v =
      match Hashtbl.find ancestor v with
      | None -> v
      | Some a ->
        let u = eval a in
        Hashtbl.set ancestor ~key:v ~data:u;
        let semi_u = Hashtbl.find_exn semi u in
        let semi_best = Hashtbl.find_exn semi (Hashtbl.find_exn best v) in
        if Hashtbl.find_exn node2num semi_u < Hashtbl.find_exn node2num semi_best
        then Hashtbl.set best ~key:v ~data:u;
        Hashtbl.find_exn best v
    in
    let link v u = Hashtbl.set ancestor ~key:u ~data:v in
    let order =
      Hashtbl.to_alist node2num
      |> List.sort ~compare:(fun (_, l) (_, r) -> Int.compare l r)
      |> Fn.flip List.drop 1
      |> List.rev
    in
    List.iter order ~f:(fun (w, _) ->
      (* compute semi(w) *)
      let preds_w = Hashtbl.find preds w |> Option.value ~default:[] in
      List.iter preds_w ~f:(fun v ->
        let u = eval v in
        let semi_w = Hashtbl.find_exn semi w in
        let semi_u = Hashtbl.find_exn semi u in
        if Hashtbl.find_exn node2num semi_u < Hashtbl.find_exn node2num semi_w
        then Hashtbl.set semi ~key:w ~data:semi_u);
      (* update bucket[semi(w)] *)
      let semi_w = Hashtbl.find_exn semi w in
      Hashtbl.update bucket semi_w ~f:(function
        | None -> [ w ]
        | Some bucket -> w :: bucket);
      (* implicitly define idoms *)
      let p = Hashtbl.find_exn parent w in
      link p w;
      Hashtbl.find_exn bucket p
      |> List.iter ~f:(fun v ->
        let u = eval v in
        let semi_u = Hashtbl.find_exn semi u in
        let semi_v = Hashtbl.find_exn semi v in
        if semi_u = semi_v
        then Hashtbl.set idom ~key:v ~data:p
        else Hashtbl.set idom ~key:v ~data:u);
      Hashtbl.set bucket ~key:p ~data:[]);
    (* explicitly define idoms *)
    Hashtbl.set idom ~key:0 ~data:0;
    List.iter order ~f:(fun (w, _) ->
      let idom_w = Hashtbl.find_exn idom w in
      let semi_w = Hashtbl.find_exn semi w in
      if idom_w <> semi_w
      then Hashtbl.set idom ~key:w ~data:(Hashtbl.find_exn idom idom_w));
    idom
  ;;
end

(* type env = (sym, value) Hashtbl.t *)

let predecessors blocks =
  let preds = Hashtbl.create (module Lbl) in
  let add ~key ~data =
    Hashtbl.update preds key ~f:(function
      | None -> [ data ]
      | Some preds -> data :: preds)
  in
  List.iter blocks ~f:(fun { label; terminator; _ } ->
    match terminator with
    | Ret -> ()
    | Jmp succ -> add ~key:succ ~data:label
    | Br { yes; no; _ } ->
      add ~key:yes ~data:label;
      add ~key:no ~data:label);
  preds
;;

let successors preds =
  let succs = Hashtbl.create (module Lbl) in
  let add ~key ~data =
    Hashtbl.update succs key ~f:(function
      | None -> [ data ]
      | Some succs -> data :: succs)
  in
  Hashtbl.iteri preds ~f:(fun ~key ~data ->
    List.iter data ~f:(fun u -> add ~key:u ~data:key));
  succs
;;

let invert_idoms idoms =
  let dom_tree = Hashtbl.create (module Lbl) in
  Hashtbl.iteri idoms ~f:(fun ~key ~data ->
    if key <> data
    then
      Hashtbl.update dom_tree data ~f:(function
        | None -> [ key ]
        | Some children -> key :: children));
  dom_tree
;;

let find_frontiers ~graph ~idoms ~dom_tree =
  let df = Hashtbl.map graph ~f:(fun _ -> Hash_set.create (module Lbl)) in
  let try_add_to_frontier u v =
    if not @@ Option.equal Lbl.equal (Hashtbl.find idoms v) (Some u)
    then Hash_set.add (Hashtbl.find_exn df u) v
  in
  let rec visit u =
    (* post order traversal of dom_tree *)
    let dom_children = Hashtbl.find dom_tree u |> Option.value ~default:[] in
    List.iter dom_children ~f:visit;
    (* local frontier *)
    Hashtbl.find graph u
    |> Option.value ~default:[]
    |> List.iter ~f:(fun v -> try_add_to_frontier u v);
    (* upward frontier *)
    List.iter dom_children ~f:(fun v ->
      Hashtbl.find df v
      |> Option.value_map ~default:[] ~f:Hash_set.to_list
      |> List.iter ~f:(fun w -> try_add_to_frontier u w))
  in
  visit 0;
  df
;;

let mem2reg_proc ~promotable { name; blocks } =
  let preds = predecessors blocks in
  let graph = successors preds in
  let idoms = Lengauer_tarjan.find_idoms ~graph ~preds in
  let dom_tree = invert_idoms idoms in
  let dom_frontiers = find_frontiers ~graph ~idoms ~dom_tree in
  eprintf "proc: %s\n" (Symbol.get_exn name).name;
  eprintf "  succs:\n";
  Hashtbl.iteri graph ~f:(fun ~key ~data ->
    eprintf
      "    L%d -> [%s]\n"
      key
      (List.map data ~f:(sprintf "L%d") |> String.concat ~sep:", "));
  eprintf "  dom_tree:\n";
  Hashtbl.iteri dom_tree ~f:(fun ~key ~data ->
    eprintf
      "    L%d -> [%s]\n"
      key
      (List.map data ~f:(sprintf "L%d") |> String.concat ~sep:", "));
  eprintf "  dom_frontiers:\n";
  Hashtbl.iteri dom_frontiers ~f:(fun ~key ~data ->
    eprintf
      "    L%d -> [%s]\n"
      key
      (Hash_set.to_list data |> List.map ~f:(sprintf "L%d") |> String.concat ~sep:", "));
  (* TODO *)
  ignore promotable;
  ignore name;
  { name; blocks }
;;

let mem2reg { globals; procedures } =
  let find_promotable procedures =
    let locals = Hash_set.create (module Symbol) in
    let not_promotable = Hash_set.create (module Symbol) in
    let mark_not_promotable ~name x =
      let owner = (Symbol.get_exn x).owner in
      if Option.equal Symbol.equal owner (Some name) then Hash_set.add not_promotable x
    in
    List.iter procedures ~f:(fun { name; blocks } ->
      List.iter blocks ~f:(fun { instrs; _ } ->
        List.iter instrs ~f:(function
          | Alloca { kind = Ptr p; _ } -> Hash_set.add locals p
          | Read { kind = Ptr p; _ } -> mark_not_promotable ~name p
          | Store { dst = { kind = Ptr p; _ }; _ } -> mark_not_promotable ~name p
          | Load { src = { kind = Ptr p; _ }; _ } -> mark_not_promotable ~name p
          | _ -> () (* pointer operations can only occur in the above few places *))));
    Hash_set.diff locals not_promotable
  in
  let promotable = find_promotable procedures in
  let procedures = List.map procedures ~f:(mem2reg_proc ~promotable) in
  { globals; procedures }
;;
