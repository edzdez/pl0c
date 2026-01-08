open! Core
open Ir
module Temp = Util.Temp

(** flatten the elaborated AST so that there are no more nested procedures *)
let rec flatten ((p, { consts; vars; procs; stmt }) as inp : Symbol.t * East.eblock) =
  match procs with
  | [] -> [ inp ]
  | _ -> (p, { consts; vars; procs = []; stmt }) :: List.concat_map procs ~f:flatten
;;

let mk_var x = { kind = Ptr x; ty = I32 }
let mk_i32 x = { kind = Lit x; ty = I32 }

let reserve_slots ~builder ~entry vars =
  List.iteri vars ~f:(fun i sym ->
    let v = mk_var sym in
    Symbol.set_slot sym @@ Int32.of_int_exn (i + 1);
    Builder.add_instr builder ~label:entry (Alloca v))
;;

let rec lower_expr ~builder ~label = function
  | East.Unary (op, expr) ->
    let expr = lower_expr ~builder ~label expr.data in
    (match op with
     | Ast.Un_plus -> expr
     | Un_minus ->
       let tmp = Builder.fresh_tmp builder in
       let dst = { kind = Tmp tmp; ty = I32 } in
       Builder.add_instr builder ~label (Neg { dst; src = expr });
       dst)
  | Binary (op, lhs, rhs) ->
    let lhs = lower_expr ~builder ~label lhs.data in
    let rhs = lower_expr ~builder ~label rhs.data in
    let tmp = Builder.fresh_tmp builder in
    let dst = { kind = Tmp tmp; ty = I32 } in
    (match op with
     | Ast.Plus -> Builder.add_instr builder ~label (Add { dst; lhs; rhs })
     | Ast.Minus -> Builder.add_instr builder ~label (Sub { dst; lhs; rhs })
     | Ast.Star -> Builder.add_instr builder ~label (Mul { dst; lhs; rhs })
     | Ast.Slash -> Builder.add_instr builder ~label (Div { dst; lhs; rhs }));
    dst
  | Num n -> mk_i32 n
  | Sym x ->
    let tmp = Builder.fresh_tmp builder in
    let dst = { kind = Tmp tmp; ty = I32 } in
    Builder.add_instr builder ~label (Load { dst; src = mk_var x });
    dst
;;

let rec lower_cond ~builder ~label = function
  | East.Odd expr ->
    let expr = lower_expr ~builder ~label expr.data in
    let tmp = Builder.fresh_tmp builder in
    let dst = { kind = Tmp tmp; ty = I1 } in
    Builder.add_instr builder ~label (Odd { dst; src = expr });
    dst
  | Not cond ->
    let cond = lower_cond ~builder ~label cond in
    let tmp = Builder.fresh_tmp builder in
    let dst = { kind = Tmp tmp; ty = I1 } in
    Builder.add_instr builder ~label (Not { dst; src = cond });
    dst
  | Rel (op, lhs, rhs) ->
    let lhs = lower_expr ~builder ~label lhs.data in
    let rhs = lower_expr ~builder ~label rhs.data in
    let tmp = Builder.fresh_tmp builder in
    let dst = { kind = Tmp tmp; ty = I1 } in
    (match op with
     | Ast.Eq -> Builder.add_instr builder ~label (Eq { dst; lhs; rhs })
     | Ast.Neq -> Builder.add_instr builder ~label (Neq { dst; lhs; rhs })
     | Ast.Lt -> Builder.add_instr builder ~label (Lt { dst; lhs; rhs })
     | Ast.Leq -> Builder.add_instr builder ~label (Leq { dst; lhs; rhs })
     | Ast.Gt -> Builder.add_instr builder ~label (Gt { dst; lhs; rhs })
     | Ast.Geq -> Builder.add_instr builder ~label (Geq { dst; lhs; rhs }));
    dst
  | Bool _ ->
    failwith "bool constants should have been eliminated in constant propagation."
;;

let rec lower_stmt ~builder ~label = function
  | East.Assign (x, expr) ->
    let x = mk_var x in
    let expr = lower_expr ~builder ~label expr.data in
    Builder.add_instr builder ~label (Store { dst = x; src = expr });
    label
  | Call proc ->
    Builder.add_instr builder ~label (Call proc);
    label
  | Scope stmts ->
    List.fold stmts ~init:label ~f:(fun label stmt -> lower_stmt ~builder ~label stmt)
  | If (cond, stmt) ->
    let cond = lower_cond ~builder ~label cond in
    let yes = Builder.init_block builder in
    let yes_exit = lower_stmt ~builder ~label:yes stmt in
    let no = Builder.init_block builder in
    Builder.set_terminator builder ~label (Br { cond; yes; no });
    Builder.set_terminator builder ~label:yes_exit (Jmp no);
    no
  | While (cond, stmt) ->
    let test = Builder.init_block builder in
    Builder.set_terminator builder ~label (Jmp test);
    let cond = lower_cond ~builder ~label cond in
    let yes = Builder.init_block builder in
    let yes_exit = lower_stmt ~builder ~label:yes stmt in
    let no = Builder.init_block builder in
    Builder.set_terminator builder ~label (Br { cond; yes; no });
    Builder.set_terminator builder ~label:yes_exit (Jmp test);
    no
  | Read x ->
    let x = mk_var x in
    Builder.add_instr builder ~label (Read x);
    label
  | Write expr ->
    let expr = lower_expr ~builder ~label expr.data in
    Builder.add_instr builder ~label (Write expr);
    label
  | Nop -> label
;;

let lower_proc ~builder ((name, { vars; stmt; _ }) : Symbol.t * East.eblock) =
  Builder.init_proc builder name;
  let entry = Builder.init_block builder in
  reserve_slots ~builder ~entry vars;
  let exit = lower_stmt ~builder ~label:entry stmt in
  Builder.set_terminator builder ~label:exit Ret
;;

let lower ({ consts; vars; procs; stmt } : East.eblock) : Ir.program =
  let builder = Builder.create (List.map ~f:mk_var vars) in
  let main_sym =
    Symbol.add
      (Symbol.create
         "_main"
         (Proc { temps = (module Temp.Make ()); labels = (module Temp.Make ()) }))
  in
  List.iter procs ~f:(fun (p, _) -> Symbol.set_owner p main_sym);
  let procedures = flatten (main_sym, { consts; vars = []; procs; stmt }) in
  List.iter procedures ~f:(lower_proc ~builder);
  Builder.build builder
;;
