open! Core
open Mir

let go_node { instr; defs; uses; clobbers } =
  match instr with
  | Mov { dst; src } ->
    Option.iter (Mir.get_reg dst) ~f:(Hash_set.add defs);
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses);
    Option.iter (Mir.get_reg dst) ~f:(Hash_set.add clobbers)
  | Lea { dst; src } ->
    Hash_set.add defs dst;
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses);
    Hash_set.add clobbers dst
  | Add { dst; src } ->
    Hash_set.add defs dst;
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses)
  | Sub { dst; src } ->
    Hash_set.add defs dst;
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses)
  | IMul { dst; src } ->
    Hash_set.add defs dst;
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses)
  | IDiv operand ->
    Hash_set.add defs (Phys EAX);
    Hash_set.add defs (Phys EDX);
    Hash_set.add uses (Phys EAX);
    Hash_set.add uses (Phys EDX);
    Option.iter (Mir.get_reg operand) ~f:(Hash_set.add uses);
    Hash_set.add clobbers (Phys EAX);
    Hash_set.add clobbers (Phys EDX)
  | Cdq ->
    Hash_set.add defs (Phys EDX);
    Hash_set.add uses (Phys EAX);
    Hash_set.add clobbers (Phys EDX)
  | Neg reg ->
    Hash_set.add defs reg;
    Hash_set.add uses reg
  | Not reg ->
    Hash_set.add defs reg;
    Hash_set.add uses reg
  | Call operand ->
    Hash_set.add defs (Phys EAX);
    Hash_set.add uses (Phys EDI);
    Option.iter (Mir.get_reg operand) ~f:(Hash_set.add uses);
    Hash_set.add clobbers (Phys EAX);
    Hash_set.add clobbers (Phys ECX);
    Hash_set.add clobbers (Phys EDX);
    Hash_set.add clobbers (Phys ESI);
    Hash_set.add clobbers (Phys RDI)
  | Jmp _ -> ()
  | Cmp { dst; src } ->
    Hash_set.add defs (Phys FLAGS);
    Hash_set.add uses dst;
    Option.iter (Mir.get_reg src) ~f:(Hash_set.add uses);
    Hash_set.add clobbers (Phys FLAGS)
  | Je _ -> Hash_set.add uses (Phys FLAGS)
  | Jne _ -> Hash_set.add uses (Phys FLAGS)
  | Jl _ -> Hash_set.add uses (Phys FLAGS)
  | Jle _ -> Hash_set.add uses (Phys FLAGS)
  | Jg _ -> Hash_set.add uses (Phys FLAGS)
  | Jge _ -> Hash_set.add uses (Phys FLAGS)
  | Sete reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Setne reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Setl reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Setle reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Setg reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Setge reg ->
    Hash_set.add defs reg;
    Hash_set.add uses (Phys FLAGS);
    Hash_set.add clobbers reg
  | Push reg ->
    Hash_set.add defs (Phys RSP);
    Hash_set.add uses (Phys RSP);
    Hash_set.add uses reg;
    Hash_set.add clobbers (Phys RSP)
  | Pop reg ->
    Hash_set.add defs (Phys RSP);
    Hash_set.add defs reg;
    Hash_set.add uses (Phys RSP);
    Hash_set.add clobbers (Phys RSP);
    Hash_set.add clobbers reg
  | Ret -> ()
;;

let populate_liveness program =
  List.iter program.code ~f:(fun { data; _ } -> List.iter data ~f:go_node);
  program
;;
