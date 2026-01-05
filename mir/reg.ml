open! Core
module Virt_reg = Ir.Tac.Virt_reg

module Phys_reg = struct
  type t =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | RDI
    | RBP
    | RSP
  [@@deriving sexp, compare, equal, hash]
end

type t =
  | Phys of Phys_reg.t
  | Virt of Virt_reg.t
[@@deriving sexp_of, compare, equal, hash]

type vreg = Virt_reg.t
type preg = Phys_reg.t
type reg = t [@@deriving equal]

let to_string = function
  | Virt reg -> sprintf "%%v%d" reg
  | Phys EAX -> "%eax"
  | Phys EBX -> "%ebx"
  | Phys ECX -> "%ecx"
  | Phys EDX -> "%edx"
  | Phys ESI -> "%esi"
  | Phys EDI -> "%edi"
  | Phys RDI -> "%rdi"
  | Phys RBP -> "%rbp"
  | Phys RSP -> "%rsp"
;;
