module Virt_reg = Ir.Tac.Virt_reg

module Phys_reg = struct
  type t =
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | RBP
    | RSP
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving sexp, compare, equal, hash]
end

type t =
  | Phys of Phys_reg.t
  | Virt of Virt_reg.t
[@@deriving sexp_of, compare, equal, hash]

type vreg = Virt_reg.t
type preg = Phys_reg.t
type reg = t
