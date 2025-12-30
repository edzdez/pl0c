open! Core
module Temp = Util.Temp
module Symbol = Util.Symbol

type sym = Symbol.t

module VirtReg = struct
  include Temp.Make ()

  let to_string t = sprintf "t%d" t
end

module Label = struct
  include Temp.Make ()

  let to_string t = sprintf "L%d" t
end

type operand =
  | Reg of VirtReg.t
  | Const of Int32.t

let operand_to_string = function
  | Reg r -> VirtReg.to_string r
  | Const n -> Int32.to_string n
;;

type program = proc list

and proc =
  { name : sym option
  ; locals : sym list
  ; body : block list
  }

and block =
  { label : Label.t
  ; instrs : instr list
  }

and instr =
  | Load of
      { d : VirtReg.t
      ; s : sym
      }
  | Store of
      { s : operand
      ; d : sym
      }
  | Move of
      { d : VirtReg.t
      ; s : operand
      }
  | Bin_op of
      { d : VirtReg.t
      ; op : bin_op
      ; l : operand
      ; r : operand
      }
  | Un_op of
      { d : VirtReg.t
      ; op : un_op
      ; s : operand
      }
  | Jump of Label.t
  | Cond_jump of
      { tst : VirtReg.t
      ; yes : Label.t
      ; no : Label.t
      }
  | Call of sym
  | Return
  | Read of sym
  | Write of operand

and un_op =
  | Plus
  | Minus
  | Odd
  | Not

and bin_op =
  | Plus
  | Minus
  | Star
  | Slash
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

let un_op_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Not -> "not"
  | Odd -> "odd"
;;

let bin_op_to_string : bin_op -> string = function
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Eq -> "="
  | Neq -> "=/="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
;;

let rec procedure_to_string { name; locals; body } =
  sprintf
    "proc %s:\n  locals: %s\n\n%s\nend"
    (match name with
     | None -> "<main>"
     | Some symb -> Symbol.to_string symb)
    (List.map locals ~f:(fun symb -> Symbol.to_string symb) |> String.concat ~sep:" ")
    (List.map body ~f:block_to_string |> String.concat ~sep:"\n\n")

and block_to_string { label; instrs } =
  sprintf
    "%s:\n%s"
    (Label.to_string label)
    (List.map instrs ~f:(fun instr -> sprintf "  %s" (instr_to_string instr))
     |> String.concat_lines)

and instr_to_string instr =
  match instr with
  | Load { d; s } -> sprintf "%s <- load %s" (VirtReg.to_string d) (Symbol.to_string s)
  | Store { s; d } -> sprintf "store %s -> %s" (operand_to_string s) (Symbol.to_string d)
  | Move { d; s } -> sprintf "%s <- %s" (VirtReg.to_string d) (operand_to_string s)
  | Bin_op { d; op; l; r } ->
    sprintf
      "%s <- %s %s %s"
      (VirtReg.to_string d)
      (operand_to_string l)
      (bin_op_to_string op)
      (operand_to_string r)
  | Un_op { d; op; s } ->
    sprintf "%s <- %s %s" (VirtReg.to_string d) (un_op_to_string op) (operand_to_string s)
  | Jump l -> sprintf "jump %s" (Label.to_string l)
  | Cond_jump { tst; yes; no } ->
    sprintf
      "cjump %s %s %s"
      (VirtReg.to_string tst)
      (Label.to_string yes)
      (Label.to_string no)
  | Call p -> sprintf "call %s" (Symbol.to_string p)
  | Return -> "return"
  | Read d -> sprintf "read -> %s" (Symbol.to_string d)
  | Write s -> sprintf "write %s" (operand_to_string s)
;;

let to_string program =
  List.map ~f:procedure_to_string program |> String.concat ~sep:"\n\n"
;;
