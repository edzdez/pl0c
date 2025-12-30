open! Core
open East
module Symbol = Util.Symbol
module Ident = Util.Ident
module Mark = Util.Mark

type elab_error =
  | Undeclared_ident of string
  | Duplicate_ident of string
  | Not_lvalue of string
  | Not_procedure of string
[@@deriving sexp]

exception Elab_error of elab_error Mark.t

let raise_error err ({ data; span } : Ast.mident) =
  raise (Elab_error (Mark.create (err (Ident.get_exn data)) span))
;;

let duplicate_ident x = Duplicate_ident x
let undeclared_ident x = Undeclared_ident x
let not_lvalue x = Not_lvalue x
let not_procedure x = Not_procedure x

module Env = struct
  type t = (Ident.t, sym) Hashtbl.t list

  let open_scope (t : t) = Hashtbl.create (module Ident) :: t

  let add (t : t) ~(key : Ast.mident) ~data =
    match t with
    | [] -> failwith "cannot add a binding to an empty environment"
    | env :: _ ->
      (match Hashtbl.add env ~key:key.data ~data with
       | `Duplicate -> raise_error duplicate_ident key
       | `Ok -> t)
  ;;

  let find (t : t) key =
    let rec go = function
      | [] -> raise_error undeclared_ident key
      | hd :: tl ->
        (match Hashtbl.find hd key.data with
         | None -> go tl
         | Some symb -> symb)
    in
    go t
  ;;

  let empty () : t = open_scope []
end

let rec elab_block ?owner ~env ({ const_decl; var_decl; proc_decl; stmt } : Ast.block) =
  let env, consts = elab_consts ?owner ~env const_decl in
  let env, vars = elab_vars ?owner ~env var_decl in
  let env, procs = elab_procs ?owner ~env proc_decl in
  let stmt = elab_stmt ~env stmt in
  env, { consts; vars; procs; stmt }

and elab_consts ?owner ~env const_decl =
  let env, consts =
    List.rev const_decl
    |> List.fold_right ~init:(env, []) ~f:(fun (c, n) (env, consts) ->
      let entry = Symbol.create ?owner ~value:n (Ident.get_exn c.data) Symbol.Const in
      let symb = Symbol.add entry in
      let env = Env.add env ~key:c ~data:symb in
      env, (symb, n) :: consts)
  in
  env, List.rev consts

and elab_vars ?owner ~env var_decl =
  let env, vars =
    List.rev var_decl
    |> List.fold_right ~init:(env, []) ~f:(fun c (env, vars) ->
      let entry = Symbol.create ?owner (Ident.get_exn c.data) Symbol.Var in
      let symb = Symbol.add entry in
      let env = Env.add env ~key:c ~data:symb in
      env, symb :: vars)
  in
  env, List.rev vars

and elab_procs ?owner ~env proc_decl =
  let env, procs =
    List.rev proc_decl
    |> List.fold_right ~init:(env, []) ~f:(fun (p, body) (env, procs) ->
      let entry = Symbol.create ?owner (Ident.get_exn p.data) Symbol.Proc in
      let symb = Symbol.add entry in
      let env = Env.add env ~key:p ~data:symb in
      let _, body = elab_block ~owner:symb ~env:(Env.open_scope env) body in
      env, (symb, body) :: procs)
  in
  env, List.rev procs

and elab_stmt ~env stmt =
  match stmt with
  | Ast.Assign (x, e) ->
    let x = elab_lvalue ~env x in
    let e = elab_expr ~env e in
    Assign (x, e)
  | Ast.Call p ->
    let p = elab_proc ~env p in
    Call p
  | Ast.Scope s -> elab_scope ~env:(Env.open_scope env) s
  | Ast.If (c, s) ->
    let c = elab_cond ~env c in
    let s = elab_stmt ~env s in
    If (c, s)
  | Ast.While (c, s) ->
    let c = elab_cond ~env c in
    let s = elab_stmt ~env s in
    While (c, s)
  | Ast.Read x ->
    let x = elab_lvalue ~env x in
    Read x
  | Ast.Write e ->
    let e = elab_expr ~env e in
    Write e

and elab_lvalue ~env mident =
  let symb = Env.find env mident in
  let entry = Symbol.get_exn symb in
  match entry.kind with
  | Var -> symb
  | _ -> raise_error not_lvalue mident

and elab_proc ~env mident =
  let symb = Env.find env mident in
  let entry = Symbol.get_exn symb in
  match entry.kind with
  | Proc -> symb
  | _ -> raise_error not_procedure mident

and elab_cond ~env c =
  match c with
  | Ast.Odd e ->
    let e = elab_expr ~env e in
    Odd e
  | Ast.Not c ->
    let c = elab_cond ~env c in
    Not c
  | Ast.Rel (op, l, r) ->
    let l = elab_expr ~env l in
    let r = elab_expr ~env r in
    Rel (op, l, r)

and elab_scope ~env ss = Scope (List.map ss ~f:(elab_stmt ~env))

and elab_expr ~env { data; span } =
  let e =
    match data with
    | Ast.Unary (op, e) ->
      let e = elab_expr ~env e in
      Unary (op, e)
    | Ast.Binary (op, l, r) ->
      let l = elab_expr ~env l in
      let r = elab_expr ~env r in
      Binary (op, l, r)
    | Ast.Num n -> Num n
    | Ast.Id x -> Sym (Env.find env x)
  in
  Mark.create e span
;;

let elab block =
  let _, eblock = elab_block ~env:(Env.empty ()) block in
  eblock
;;

let create_indent indent = String.init (2 * indent) ~f:(const ' ')

let rec block_to_string ~indent { consts; vars; procs; stmt } =
  sprintf
    "%s%s%s%s."
    (consts_to_string ~indent consts)
    (vars_to_string ~indent vars)
    (procs_to_string ~indent procs)
    (stmt_to_string ~indent stmt)

and consts_to_string ~indent decls =
  let pps =
    List.map decls ~f:(fun (x, i) -> sprintf "%s = %ld" (Symbol.to_string x) i)
    |> String.concat ~sep:", "
  in
  match decls with
  | [] -> ""
  | _ -> sprintf "%sconst %s;\n" (create_indent indent) pps

and vars_to_string ~indent decls =
  let pps = List.map decls ~f:Symbol.to_string |> String.concat ~sep:", " in
  match decls with
  | [] -> ""
  | _ -> sprintf "%svar %s;\n" (create_indent indent) pps

and procs_to_string ~indent decls =
  List.map decls ~f:(proc_decl_to_string ~indent) |> String.concat_lines

and proc_decl_to_string ~indent (p, block) =
  sprintf
    "%sprocedure %s;\n%s;"
    (create_indent indent)
    (Symbol.to_string p)
    (block_to_string ~indent block)

and stmt_to_string ~indent s =
  let s_indent = create_indent indent in
  match s with
  | Assign (x, e) ->
    sprintf "%s%s := %s" s_indent (Symbol.to_string x) (expr_to_string e.data)
  | Call p -> sprintf "%scall %s" s_indent (Symbol.to_string p)
  | Scope ss ->
    (match ss with
     | [] -> sprintf "%sbegin end" s_indent
     | _ ->
       sprintf
         "%sbegin\n%s\n%send"
         s_indent
         (List.map ss ~f:(stmt_to_string ~indent:(indent + 1)) |> String.concat ~sep:";\n")
         s_indent)
  | If (c, s) ->
    sprintf
      "%sif %s then\n%s"
      s_indent
      (cond_to_string c)
      (stmt_to_string ~indent:(indent + 1) s)
  | While (c, s) ->
    sprintf
      "%swhile %s do\n%s"
      s_indent
      (cond_to_string c)
      (stmt_to_string ~indent:(indent + 1) s)
  | Read x -> sprintf "%s?%s" s_indent (Symbol.to_string x)
  | Write e -> sprintf "%s!(%s)" s_indent (expr_to_string e.data)

and cond_to_string c =
  match c with
  | Odd e -> sprintf "odd(%s)" (expr_to_string e.data)
  | Not c -> sprintf "not(%s)" (cond_to_string c)
  | Rel (op, l, r) ->
    sprintf
      "(%s) %s (%s)"
      (expr_to_string l.data)
      (rel_to_string op)
      (expr_to_string r.data)

and rel_to_string r =
  match r with
  | Eq -> "="
  | Neq -> "=/="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="

and expr_to_string e =
  match e with
  | Unary (op, e) -> sprintf "%s(%s)" (un_op_to_string op) (expr_to_string e.data)
  | Binary (op, l, r) ->
    sprintf
      "(%s) %s (%s)"
      (expr_to_string l.data)
      (bin_op_to_string op)
      (expr_to_string r.data)
  | Num n -> Int32.to_string n
  | Sym x -> Symbol.to_string x

and un_op_to_string op =
  match op with
  | UnPlus -> "+"
  | UnMinus -> "-"

and bin_op_to_string op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
;;

let to_string = block_to_string ~indent:0
