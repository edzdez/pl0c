open! Core
module Mark = Util.Mark
module Ident = Util.Ident

type mident = Ident.t Mark.t

type block =
  { const_decl : const_decl list
  ; var_decl : var_decl list
  ; proc_decl : proc_decl list
  ; stmt : stmt
  }

and const_decl = mident * Int32.t
and var_decl = mident
and proc_decl = mident * block

and stmt =
  | Assign of mident * mexpr
  | Call of mident
  | Scope of stmt list
  | If of (cond * stmt)
  | While of (cond * stmt)
  | Read of mident
  | Write of mexpr

and cond =
  | Odd of mexpr
  | Not of cond
  | Rel of (rel * mexpr * mexpr)

and rel =
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

and expr =
  | Unary of (un_op * mexpr)
  | Binary of (bin_op * mexpr * mexpr)
  | Num of Int32.t
  | Id of mident

and mexpr = expr Mark.t

and un_op =
  | UnPlus
  | UnMinus

and bin_op =
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving sexp]

let create_indent indent = String.init (2 * indent) ~f:(const ' ')

let rec block_to_string ~indent { const_decl; var_decl; proc_decl; stmt } =
  sprintf
    "%s%s%s%s."
    (const_decls_to_string ~indent const_decl)
    (var_decls_to_string ~indent var_decl)
    (proc_decls_to_string ~indent proc_decl)
    (stmt_to_string ~indent stmt)

and const_decls_to_string ~indent decls =
  let pps =
    List.map decls ~f:(fun (x, i) -> sprintf "%s = %ld" (Ident.get_exn x.data) i)
    |> String.concat ~sep:", "
  in
  match decls with
  | [] -> ""
  | _ -> sprintf "%sconst %s;\n" (create_indent indent) pps

and var_decls_to_string ~indent decls =
  let pps =
    List.map decls ~f:(fun x -> Ident.get_exn x.data) |> String.concat ~sep:", "
  in
  match decls with
  | [] -> ""
  | _ -> sprintf "%svar %s;\n" (create_indent indent) pps

and proc_decls_to_string ~indent decls =
  List.map decls ~f:(proc_decl_to_string ~indent) |> String.concat_lines

and proc_decl_to_string ~indent (p, block) =
  sprintf
    "%sprocedure %s;\n%s;"
    (create_indent indent)
    (Ident.get_exn p.data)
    (block_to_string ~indent block)

and stmt_to_string ~indent s =
  let s_indent = create_indent indent in
  match s with
  | Assign (x, e) ->
    sprintf "%s%s := %s" s_indent (Ident.get_exn x.data) (expr_to_string e.data)
  | Call p -> sprintf "%scall %s" s_indent (Ident.get_exn p.data)
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
  | Read x -> sprintf "%s?%s" s_indent (Ident.get_exn x.data)
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
  | Id x -> Ident.get_exn x.data

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
