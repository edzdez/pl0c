open! Core
module Ast = Semant.East
module Symbol = Util.Symbol

type sym = Util.Symbol.t

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

  let rec lower_proc ((p, { vars; stmt; _ }) : sym * Ast.eblock) =
    Label.reset ();
    Virt_reg.reset ();
    let initial_block = { label = Label.fresh (); instrs = [ Return ] } in
    let _, body = lower_stmt ~body:[ initial_block ] stmt in
    { name = Some p; locals = vars; body }

  (* FIXME: yuck *)
  and lower_stmt ~body stmt =
    match body with
    | [] -> failwith "lower_stmt requires a nonempty body"
    | { label; instrs } :: rest ->
      (match stmt with
       | Ast.Assign (x, e) ->
         let e_op, e_instrs = lower_expr e in
         ( label
         , { label; instrs = e_instrs @ (Store { d = x; s = e_op } :: instrs) } :: rest )
       | Ast.Call p -> label, { label; instrs = Call p :: instrs } :: rest
       | Ast.Scope ss ->
         let body =
           List.fold_right ss ~init:body ~f:(fun stmt body ->
             snd @@ lower_stmt ~body stmt)
         in
         let label = (List.hd_exn body).label in
         label, body
       | Ast.If (c, s) ->
         let c_reg, c_instrs = lower_cond c in
         let yes, body =
           lower_stmt
             ~body:({ label = Label.fresh (); instrs = [ Jump label ] } :: body)
             s
         in
         let test = Label.fresh () in
         ( test
         , { label = test
           ; instrs = c_instrs @ [ Cond_jump { tst = c_reg; yes; no = label } ]
           }
           :: body )
       | Ast.While (c, s) ->
         let c_reg, c_instrs = lower_cond c in
         let test = Label.fresh () in
         let yes, body =
           lower_stmt ~body:({ label = Label.fresh (); instrs = [ Jump test ] } :: body) s
         in
         let fresh = Label.fresh () in
         ( fresh
         , { label = fresh; instrs = [ Jump test ] }
           :: { label = test
              ; instrs = c_instrs @ [ Cond_jump { tst = c_reg; yes; no = label } ]
              }
           :: body )
       | Ast.Read x -> label, { label; instrs = Read x :: instrs } :: rest
       | Ast.Write e ->
         let e_op, e_instrs = lower_expr e in
         label, { label; instrs = e_instrs @ (Write e_op :: instrs) } :: rest
       | Ast.Nop -> label, body)

  and lower_expr e =
    match e.data with
    | Ast.Unary (op, e) ->
      let e_op, e_instrs = lower_expr e in
      let tmp = Virt_reg.fresh () in
      Reg tmp, e_instrs @ [ Un_op { d = tmp; op = arith_un_op_conv op; s = e_op } ]
    | Ast.Binary (op, l, r) ->
      let l_op, l_instrs = lower_expr l in
      let r_op, r_instrs = lower_expr r in
      let tmp = Virt_reg.fresh () in
      ( Reg tmp
      , l_instrs
        @ r_instrs
        @ [ Bin_op { d = tmp; op = arith_bin_op_conv op; l = l_op; r = r_op } ] )
    | Ast.Num n -> Const n, []
    | Ast.Sym s ->
      let tmp = Virt_reg.fresh () in
      Reg tmp, [ Load { d = tmp; s } ]

  and lower_cond c =
    match c with
    | Ast.Odd e ->
      let e_op, e_instrs = lower_expr e in
      let tmp = Virt_reg.fresh () in
      tmp, e_instrs @ [ Un_op { d = tmp; op = Odd; s = e_op } ]
    | Ast.Not c ->
      let c_reg, c_instrs = lower_cond c in
      let tmp = Virt_reg.fresh () in
      tmp, c_instrs @ [ Un_op { d = tmp; op = Not; s = Reg c_reg } ]
    | Ast.Rel (op, l, r) ->
      let l_op, l_instrs = lower_expr l in
      let r_op, r_instrs = lower_expr r in
      let tmp = Virt_reg.fresh () in
      ( tmp
      , l_instrs
        @ r_instrs
        @ [ Bin_op { d = tmp; op = rel_bin_op_conv op; l = l_op; r = r_op } ] )
    | Ast.Bool _ -> failwith "such branches should have been eliminated earlier"
  ;;
end

(** flatten the elaborated AST so that there are no more nested procedures *)
let rec flatten ((p, { consts; vars; procs; stmt }) as inp : sym * Ast.eblock) =
  match procs with
  | [] -> [ inp ]
  | _ -> (p, { consts; vars; procs = []; stmt }) :: List.concat_map procs ~f:flatten
;;

let lower ({ consts; vars; procs; stmt } : Ast.eblock) : Tac.program =
  let main_sym = Symbol.add (Symbol.create "<main>" Proc) in
  let procedures =
    flatten (main_sym, { consts; vars = []; procs; stmt }) |> List.map ~f:Tac.lower_proc
  in
  { globals = vars; procedures }
;;
