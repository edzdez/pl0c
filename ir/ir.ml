open! Core
module Ast = Semant.East
module Symbol = Util.Symbol

module Tac = struct
  include Tac

  let arith_un_op_conv : Ast.un_op -> un_op = function
    | Un_plus -> Plus
    | Un_minus -> Minus
  ;;

  let arith_bin_op_conv : Ast.bin_op -> bin_op = function
    | Plus -> Plus
    | Minus -> Minus
    | Star -> Star
    | Slash -> Slash
  ;;

  let rel_bin_op_conv : Ast.rel -> bin_op = function
    | Eq -> Eq
    | Neq -> Neq
    | Lt -> Lt
    | Leq -> Leq
    | Gt -> Gt
    | Geq -> Geq
  ;;

  let rec lower_proc ~builder ((p, { vars; stmt; _ }) : Symbol.t * Ast.eblock) =
    Tac_builder.init_proc builder ~name:p ~locals:vars;
    let entry = Tac_builder.init_block builder in
    let exit = lower_stmt ~builder ~label:entry stmt in
    Tac_builder.add_instr builder ~label:exit ~instr:Return

  and lower_stmt ~builder ~label stmt =
    match stmt with
    | Ast.Assign (x, e) ->
      let e = lower_expr ~builder ~label e in
      Tac_builder.add_instr builder ~label ~instr:(Store { d = x; s = e });
      label
    | Ast.Call p ->
      Tac_builder.add_instr builder ~label ~instr:(Call p);
      label
    | Ast.Scope stmts ->
      List.fold stmts ~init:label ~f:(fun label stmt -> lower_stmt ~builder ~label stmt)
    | Ast.If (c, stmt) ->
      let c = lower_cond ~builder ~label c in
      let yes = Tac_builder.init_block builder in
      let yes_exit = lower_stmt ~builder ~label:yes stmt in
      let no = Tac_builder.init_block builder in
      Tac_builder.add_instr builder ~label ~instr:(Cond_jump { tst = c; yes; no });
      Tac_builder.add_instr builder ~label:yes_exit ~instr:(Jump no);
      no
    | Ast.While (c, stmt) ->
      let test = Tac_builder.init_block builder in
      Tac_builder.add_instr builder ~label ~instr:(Jump test);
      let c = lower_cond ~builder ~label:test c in
      let yes = Tac_builder.init_block builder in
      let yes_exit = lower_stmt ~builder ~label:yes stmt in
      let no = Tac_builder.init_block builder in
      Tac_builder.add_instr builder ~label:test ~instr:(Cond_jump { tst = c; yes; no });
      Tac_builder.add_instr builder ~label:yes_exit ~instr:(Jump test);
      no
    | Ast.Read x ->
      Tac_builder.add_instr builder ~label ~instr:(Read x);
      label
    | Ast.Write e ->
      let e = lower_expr ~builder ~label e in
      Tac_builder.add_instr builder ~label ~instr:(Write e);
      label
    | Ast.Nop -> label

  and lower_expr ~builder ~label e =
    match e.data with
    | Ast.Unary (op, e) ->
      let s = lower_expr ~builder ~label e in
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr
        builder
        ~label
        ~instr:(Un_op { d = tmp; op = arith_un_op_conv op; s });
      Reg tmp
    | Ast.Binary (op, l, r) ->
      let l = lower_expr ~builder ~label l in
      let r = lower_expr ~builder ~label r in
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr
        builder
        ~label
        ~instr:(Bin_op { d = tmp; op = arith_bin_op_conv op; l; r });
      Reg tmp
    | Ast.Num n -> Const n
    | Ast.Sym x ->
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr builder ~label ~instr:(Load { d = tmp; s = x });
      Reg tmp

  and lower_cond ~builder ~label c =
    match c with
    | Ast.Odd e ->
      let s = lower_expr ~builder ~label e in
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr builder ~label ~instr:(Un_op { d = tmp; op = Odd; s });
      tmp
    | Ast.Not c ->
      let s = lower_cond ~builder ~label c in
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr builder ~label ~instr:(Un_op { d = tmp; op = Not; s = Reg s });
      tmp
    | Ast.Rel (op, l, r) ->
      let l = lower_expr ~builder ~label l in
      let r = lower_expr ~builder ~label r in
      let tmp = Virt_reg.fresh () in
      Tac_builder.add_instr
        builder
        ~label
        ~instr:(Bin_op { d = tmp; op = rel_bin_op_conv op; l; r });
      tmp
    | Ast.Bool _ -> failwith "eliminated by constant propagation"
  ;;
end

(** flatten the elaborated AST so that there are no more nested procedures *)
let rec flatten ((p, { consts; vars; procs; stmt }) as inp : Symbol.t * Ast.eblock) =
  match procs with
  | [] -> [ inp ]
  | _ -> (p, { consts; vars; procs = []; stmt }) :: List.concat_map procs ~f:flatten
;;

let lower ({ consts; vars; procs; stmt } : Ast.eblock) : Tac.program =
  let builder = Tac_builder.create vars in
  let main_sym = Symbol.add (Symbol.create "_main" Proc) in
  let procedures = flatten (main_sym, { consts; vars = []; procs; stmt }) in
  List.iter procedures ~f:(Tac.lower_proc ~builder);
  Tac_builder.build builder
;;
