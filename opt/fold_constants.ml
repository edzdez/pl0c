open! Core
open East

type constant_folding_error =
  | Overflow
  | Division_by_zero
[@@deriving sexp]

exception Constant_folding_error of constant_folding_error Mark.t

let raise_error err span = raise (Constant_folding_error (Mark.create err span))

let rec fold { vars; procs; stmt; _ } =
  { consts = []; vars; procs = List.map procs ~f:fold_proc; stmt = fold_stmt stmt }

and fold_proc (p, block) = p, fold block

and fold_stmt s =
  match s with
  | Assign (x, e) -> Assign (x, fold_expr e)
  | Call _ -> s
  | Scope ss -> Scope (List.map ss ~f:fold_stmt)
  | If (c, s) ->
    (match fold_cond c with
     | Bool true -> s
     | Bool false -> Nop
     | c -> If (c, fold_stmt s))
  | While (c, s) ->
    (match fold_cond c with
     | Bool false -> Nop
     | c -> While (c, fold_stmt s))
  | Read _ -> s
  | Write e -> Write (fold_expr e)
  | Nop -> Nop

and fold_cond c =
  match c with
  | Odd e ->
    let e = fold_expr e in
    (match e.data with
     | Num n -> if Int32.(n % 2l = 1l) then Bool true else Bool false
     | _ -> Odd e)
  | Not c ->
    (match fold_cond c with
     | Bool b -> Bool (not b)
     | c -> Not c)
  | Rel (op, l, r) ->
    let l = fold_expr l in
    let r = fold_expr r in
    (match l.data, r.data with
     | Num l, Num r -> fold_rel op l r
     | _ -> Rel (op, l, r))
  | Bool b -> Bool b

and fold_rel op l r =
  let open Int32 in
  Bool
    (match op with
     | Eq -> l = r
     | Neq -> l <> r
     | Lt -> l < r
     | Leq -> l <= r
     | Gt -> l > r
     | Geq -> l >= r)

and fold_expr { data; span } =
  Mark.create
    (match data with
     | Unary (op, e) ->
       let e = fold_expr e in
       (match e.data with
        | Num n -> fold_unary op n
        | _ -> Unary (op, e))
     | Binary (op, l, r) ->
       let l = fold_expr l in
       let r = fold_expr r in
       (match l.data, r.data with
        | Num l, Num r -> fold_binary op l r span
        | _ -> Binary (op, l, r))
     | Num n -> Num n
     | Sym x ->
       let entry = Symbol.get_exn x in
       (match entry.kind with
        | Const -> Num (Option.value_exn entry.value)
        | _ -> Sym x))
    span

and fold_unary op n =
  Num
    (match op with
     | Un_plus -> n
     | Un_minus -> Int32.(-n))

and fold_binary op l r span =
  Num
    (try
       Int32.of_int64_exn
         (let open Int64 in
          let l = of_int32 l in
          let r = of_int32 r in
          match op with
          | Plus -> l + r
          | Minus -> l - r
          | Star -> l * r
          | Slash ->
            (try l / r with
             | Division_by_zero -> raise_error Division_by_zero span))
     with
     | Failure _ -> raise_error Overflow span)
;;
