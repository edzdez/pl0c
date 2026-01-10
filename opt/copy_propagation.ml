open! Core
open Ir
module Temp = Util.Temp
module Lbl = Temp

let build_tree blocks =
  let reprs = Hashtbl.create (module Temp) in
  List.iter blocks ~f:(fun { instrs; _ } ->
    List.iter instrs ~f:(function
      | Assign { dst = { kind = Tmp t_dst; _ }; src = { kind = Tmp t_src; _ } } ->
        (* SSA form guarantees only one def *)
        Hashtbl.add_exn reprs ~key:t_dst ~data:t_src
      | _ -> ()));
  reprs
;;

let is_trivial_copy = function
  | Assign { dst; src } ->
    (match dst.kind, src.kind with
     | Tmp x, Tmp y -> x = y
     | _ -> false)
  | _ -> false
;;

let propagate_proc { blocks; _ } =
  let reprs = build_tree blocks in
  let rewrite blocks =
    let changed = ref false in
    let rewrite_value { kind; ty } =
      let kind =
        match kind with
        | Tmp x ->
          (match Hashtbl.find reprs x with
           | None -> Tmp x
           | Some y ->
             changed := true;
             Tmp y)
        | v -> v
      in
      { kind; ty }
    in
    let rewrite_binary { dst; lhs; rhs } =
      let dst = rewrite_value dst in
      let lhs = rewrite_value lhs in
      let rhs = rewrite_value rhs in
      { dst; lhs; rhs }
    in
    let rewrite_unary { dst; src } =
      let dst = rewrite_value dst in
      let src = rewrite_value src in
      { dst; src }
    in
    (* phi nodes *)
    List.iter blocks ~f:(fun block ->
      block.joins
      <- List.map block.joins ~f:(fun { dst; srcs; sym } ->
           { dst = rewrite_value dst; srcs = Hashtbl.map srcs ~f:rewrite_value; sym });
      (* instructions *)
      block.instrs
      <- List.map block.instrs ~f:(function
           | Add binary -> Add (rewrite_binary binary)
           | Sub binary -> Sub (rewrite_binary binary)
           | Mul binary -> Mul (rewrite_binary binary)
           | Div binary -> Div (rewrite_binary binary)
           | Eq binary -> Eq (rewrite_binary binary)
           | Neq binary -> Neq (rewrite_binary binary)
           | Lt binary -> Lt (rewrite_binary binary)
           | Leq binary -> Leq (rewrite_binary binary)
           | Gt binary -> Gt (rewrite_binary binary)
           | Geq binary -> Geq (rewrite_binary binary)
           | Not unary -> Not (rewrite_unary unary)
           | Neg unary -> Neg (rewrite_unary unary)
           | Odd unary -> Odd (rewrite_unary unary)
           | Read value -> Read (rewrite_value value)
           | Write value -> Write (rewrite_value value)
           | Store unary -> Store (rewrite_unary unary)
           | Load unary -> Load (rewrite_unary unary)
           | Assign unary -> Assign (rewrite_unary unary)
           | i -> i);
      (* terminator *)
      block.terminator
      <- (match block.terminator with
          | Br { cond; yes; no } -> Br { cond = rewrite_value cond; yes; no }
          | i -> i));
    !changed
  in
  let rec go blocks =
    let changed = rewrite blocks in
    if changed then go blocks else ()
  in
  go blocks;
  (* remove trivial copies *)
  List.iter blocks ~f:(fun block ->
    block.instrs <- List.filter ~f:(Fn.compose not is_trivial_copy) block.instrs)
;;

let propagate prog =
  List.iter prog.procedures ~f:propagate_proc;
  prog
;;
