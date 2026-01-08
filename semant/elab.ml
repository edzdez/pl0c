open! Core
open East
open Util

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
    |> List.fold_right ~init:(env, []) ~f:(fun (c, value) (env, consts) ->
      let entry = Symbol.create ?owner (Ident.get_exn c.data) (Symbol.Const { value }) in
      let symb = Symbol.add entry in
      let env = Env.add env ~key:c ~data:symb in
      env, (symb, value) :: consts)
  in
  env, List.rev consts

and elab_vars ?owner ~env var_decl =
  let env, vars =
    List.rev var_decl
    |> List.fold_right ~init:(env, []) ~f:(fun c (env, vars) ->
      let entry =
        Symbol.create ?owner (Ident.get_exn c.data) (Symbol.Var { slot = None })
      in
      let symb = Symbol.add entry in
      let env = Env.add env ~key:c ~data:symb in
      env, symb :: vars)
  in
  env, List.rev vars

and elab_procs ?owner ~env proc_decl =
  let env, procs =
    List.rev proc_decl
    |> List.fold_right ~init:(env, []) ~f:(fun (p, body) (env, procs) ->
      let entry =
        Symbol.create
          ?owner
          (Ident.get_exn p.data)
          (Symbol.Proc { tmp = (module Temp.Make ()) })
      in
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
  | Var _ -> symb
  | _ -> raise_error not_lvalue mident

and elab_proc ~env mident =
  let symb = Env.find env mident in
  let entry = Symbol.get_exn symb in
  match entry.kind with
  | Proc _ -> symb
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
