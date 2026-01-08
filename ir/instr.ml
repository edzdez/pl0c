open! Core
module Symbol = Util.Symbol

type sym = Symbol.t
type lbl = Util.Temp.t
type tmp = Util.Temp.t

type ty =
  | I1
  | I32
  | Lbl

type kind =
  | Lit of Int32.t
  | Ptr of sym
  | Tmp of tmp

type value =
  { ty : ty
  ; kind : kind
  }

type binary =
  { dst : value
  ; lhs : value
  ; rhs : value
  }

type unary =
  { dst : value
  ; src : value
  }

type program =
  { globals : value list
  ; procedures : procedure list
  }

and procedure =
  { name : sym
  ; blocks : block list
  }

and block =
  { label : lbl
  ; joins : phi list
  ; instrs : instr list
  ; terminator : term
  }

and phi =
  { dst : value
  ; srcs : (lbl, value) Hashtbl.t
  }

and instr =
  | Add of binary
  | Sub of binary
  | Mul of binary
  | Div of binary
  | Eq of binary
  | Neq of binary
  | Lt of binary
  | Leq of binary
  | Gt of binary
  | Geq of binary
  | Not of unary
  | Neg of unary
  | Odd of unary
  | Call of sym
  | Read of value
  | Write of value
  | Alloca of value
  | Store of unary
  | Load of unary

and term =
  | Ret
  | Jmp of lbl
  | Br of
      { cond : value
      ; yes : lbl
      ; no : lbl
      }

let ty_to_string = function
  | I1 -> "i1"
  | I32 -> "i32"
  | Lbl -> "label"
;;

let kind_to_string = function
  | Lit n -> Int32.to_string n
  | Ptr s -> sprintf "@%s" (Symbol.get_exn s).name
  | Tmp tmp -> sprintf "%%t%d" tmp
;;

let value_to_string { ty; kind } =
  match kind with
  | Ptr _ -> sprintf "ptr<%s> %s" (ty_to_string ty) (kind_to_string kind)
  | _ -> sprintf "%s %s" (ty_to_string ty) (kind_to_string kind)
;;

let globals_to_string globals =
  List.map globals ~f:(fun { ty; kind } ->
    match kind with
    | Ptr g -> sprintf "@%s := global %s" (Symbol.get_exn g).name (ty_to_string ty)
    | _ -> failwith "globals must be ptrs!")
  |> String.concat ~sep:"\n"
;;

let lbl_to_string i = sprintf "L%d" i

let join_to_string { dst; srcs } =
  sprintf
    "%s := phi %s %s"
    (kind_to_string dst.kind)
    (ty_to_string dst.ty)
    (Hashtbl.to_alist srcs
     |> List.sort ~compare:(fun (l1, _) (l2, _) -> Int.compare l1 l2)
     |> List.map ~f:(fun (label, value) ->
       sprintf "[%s, %s]" (value_to_string value) (lbl_to_string label))
     |> String.concat ~sep:", ")
;;

let binary_to_string ~op { dst; lhs; rhs } =
  sprintf
    "%s := %s %s, %s"
    (kind_to_string dst.kind)
    op
    (value_to_string lhs)
    (kind_to_string rhs.kind)
;;

let unary_to_string ~op { dst; src } =
  sprintf "%s := %s %s" (kind_to_string dst.kind) op (value_to_string src)
;;

let instr_to_string = function
  | Add body -> binary_to_string ~op:"add" body
  | Sub body -> binary_to_string ~op:"sub" body
  | Mul body -> binary_to_string ~op:"mul" body
  | Div body -> binary_to_string ~op:"div" body
  | Eq body -> binary_to_string ~op:"eq" body
  | Neq body -> binary_to_string ~op:"neq" body
  | Lt body -> binary_to_string ~op:"lt" body
  | Leq body -> binary_to_string ~op:"leq" body
  | Gt body -> binary_to_string ~op:"gt" body
  | Geq body -> binary_to_string ~op:"geq" body
  | Not body -> unary_to_string ~op:"not" body
  | Neg body -> unary_to_string ~op:"neg" body
  | Odd body -> unary_to_string ~op:"odd" body
  | Call s -> sprintf "call %s" (Symbol.get_exn s).name
  | Read v -> sprintf "read %s" (value_to_string v)
  | Write v -> sprintf "write %s" (value_to_string v)
  | Alloca v -> sprintf "%s := alloca %s" (kind_to_string v.kind) (ty_to_string v.ty)
  | Store { dst; src } ->
    sprintf "store %s, %s" (value_to_string src) (value_to_string dst)
  | Load body -> unary_to_string ~op:"load" body
;;

let terminator_to_string = function
  | Ret -> "ret"
  | Jmp lbl -> sprintf "jmp label %s" (lbl_to_string lbl)
  | Br { cond; yes; no } ->
    sprintf
      "br %s, label %s, label %s"
      (value_to_string cond)
      (lbl_to_string yes)
      (lbl_to_string no)
;;

let block_to_string { label; joins; instrs; terminator } =
  sprintf
    "L%d:\n%s"
    label
    ([ List.map joins ~f:join_to_string |> String.concat ~sep:"\n"
     ; String.concat ~sep:"\n"
       @@ List.map instrs ~f:(fun instr -> sprintf "  %s" (instr_to_string instr))
       @ [ sprintf "  %s" (terminator_to_string terminator) ]
     ]
     |> List.filter ~f:(Fn.compose not String.is_empty)
     |> String.concat_lines)
;;

let procedures_to_string procedures =
  List.map procedures ~f:(fun { name; blocks } ->
    sprintf
      "proc %s {\n%s}"
      (Symbol.get_exn name).name
      (List.map blocks ~f:block_to_string |> String.concat ~sep:"\n"))
  |> String.concat ~sep:"\n\n"
;;

let to_string { globals; procedures } =
  sprintf "%s\n\n%s" (globals_to_string globals) (procedures_to_string procedures)
;;
