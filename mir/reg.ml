module Virt_reg = Ir.Tac.Virt_reg

module Phys_reg = struct
  type t =
    | EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | EBP
    | ESP
  [@@deriving sexp, compare, equal, hash]
end

type t =
  | Phys of Phys_reg.t
  | Virt of Virt_reg.t
[@@deriving sexp_of, compare, equal, hash]

type vreg = Virt_reg.t
type preg = Phys_reg.t
type reg = t
