open! Core
module Symbol = Util.Symbol
module Mark = Util.Mark

type sym = Symbol.t
type rel = Ast.rel
type un_op = Ast.un_op
type bin_op = Ast.bin_op

type eblock =
  { consts : (sym * Int32.t) list
  ; vars : sym list
  ; procs : (sym * eblock) list
  ; stmt : estmt
  }

and estmt =
  | Assign of sym * meexpr
  | Call of sym
  | Scope of estmt list
  | If of econd * estmt
  | While of econd * estmt
  | Read of sym
  | Write of meexpr
  | Nop

and econd =
  | Odd of meexpr
  | Not of econd
  | Rel of rel * meexpr * meexpr
  | Bool of bool

and eexpr =
  | Unary of un_op * meexpr
  | Binary of bin_op * meexpr * meexpr
  | Num of Int32.t
  | Sym of sym

and meexpr = eexpr Mark.t

let create_indent indent = String.init (2 * indent) ~f:(const ' ')

let rec block_to_string ~indent { consts; vars; procs; stmt } =
  sprintf
    "%s%s%s%s"
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
  | Nop -> ""

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
  | Bool b -> Bool.to_string b

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

let to_string block = sprintf "%s." (block_to_string ~indent:0 block)
