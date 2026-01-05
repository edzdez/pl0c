open! Core
open Ir
module Builder = Mir.Builder
module Symbol = Util.Symbol

module Mir = struct
  include Mir

  let resolve_label ~(proc : sym option) ~(label : Tac.Label.t) : Mir.label =
    Label.of_label ?owner:proc label
  ;;

  let should_push_rbp ~proc owner =
    Option.is_some owner && (not @@ Option.equal Symbol.equal proc owner)
  ;;

  let resolve_addr ~builder ~label ~(proc : sym option) sym : Mir.mem_addr =
    let rec traverse_links ~target curr =
      if not @@ Symbol.equal target curr
      then (
        Builder.add_instr
          builder
          ~label
          (Mov
             { dst = Reg (Phys RDI)
             ; src = Addr (Dynamic { base = Phys RDI; offset = 16l })
             });
        let { owner; _ } : Symbol.entry = Symbol.get_exn curr in
        traverse_links ~target @@ Option.value_exn owner)
    in
    let entry = Symbol.get_exn sym in
    match entry.owner with
    | None -> Static (Label.of_global sym) (* global *)
    | Some target ->
      if should_push_rbp ~proc entry.owner
      then
        Builder.add_instr
          builder
          ~label
          (Mov { dst = Reg (Phys RDI); src = Reg (Phys RBP) });
      traverse_links ~target @@ Option.value_exn proc;
      Dynamic { base = Phys RBP; offset = Int32.(Option.value_exn entry.slot * -4l) }
  ;;

  let munch_operand = function
    | Tac.Const n -> Imm32 n
    | Reg r -> Reg (Reg.Virt r)
  ;;

  let push_access_link ~builder ~label ~caller callee =
    let rec traverse_links ~target curr =
      if not @@ Option.equal Symbol.equal target curr
      then (
        Builder.add_instr
          builder
          ~label
          (Mov
             { dst = Reg (Phys RDI)
             ; src = Addr (Dynamic { base = Phys RDI; offset = 16l })
             });
        let { owner; _ } : Symbol.entry = Symbol.get_exn (Option.value_exn curr) in
        traverse_links ~target @@ owner)
    in
    let entry = Symbol.get_exn callee in
    if Option.equal Symbol.equal entry.owner caller
    then Builder.add_instr builder ~label (Push (Phys RBP))
    else (
      Builder.add_instr
        builder
        ~label
        (Mov { dst = Reg (Phys RDI); src = Reg (Phys RBP) });
      traverse_links ~target:entry.owner caller;
      Builder.add_instr builder ~label (Push (Phys RDI)))
  ;;

  let rec munch_block ~builder ~proc ~framesize ~label = function
    | [] -> ()
    | Tac.Nop :: rest -> munch_block ~builder ~proc ~framesize ~label rest
    | [ Tac.Return ] ->
      Builder.add_instrs
        builder
        ~label
        [ Add { dst = Phys RSP; src = Imm32 framesize }
        ; Pop (Phys RBP)
        ; Mov { dst = Reg (Phys EAX); src = Imm32 0l }
        ; Ret
        ]
    | Tac.Move { d; s } :: rest ->
      Builder.add_instr builder ~label (Mov { dst = Reg (Virt d); src = munch_operand s });
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Load { d; s } :: rest ->
      let s = resolve_addr ~builder ~label ~proc s in
      Builder.add_instr builder ~label (Mov { dst = Reg (Virt d); src = Addr s });
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Store { s; d } :: rest ->
      let d = resolve_addr ~builder ~label ~proc d in
      let s = munch_operand s in
      Builder.add_instr builder ~label (Mov { dst = Addr d; src = s });
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Jump l :: rest ->
      let target = resolve_label ~proc ~label:l in
      Builder.add_instr builder ~label (Jmp target);
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Bin_op { d; op; l; r } :: Tac.Cond_jump { tst; yes; no } :: rest ->
      assert (d = tst);
      let yes = resolve_label ~proc ~label:yes in
      let no = resolve_label ~proc ~label:no in
      Builder.add_instrs
        builder
        ~label
        [ Mov { dst = Reg (Virt d); src = munch_operand l }
        ; Cmp { dst = Virt d; src = munch_operand r }
        ; (match op with
           | Eq -> Je yes
           | Neq -> Jne yes
           | Lt -> Jl yes
           | Leq -> Jle yes
           | Gt -> Jg yes
           | Geq -> Jge yes
           | _ -> failwith "impossible!")
        ; Jmp no
        ];
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Bin_op { d; op; l; r } :: rest ->
      let l = munch_operand l in
      let r = munch_operand r in
      Builder.add_instrs
        builder
        ~label
        (match op with
         | Plus -> [ Mov { dst = Reg (Virt d); src = l }; Add { dst = Virt d; src = r } ]
         | Minus -> [ Mov { dst = Reg (Virt d); src = l }; Sub { dst = Virt d; src = r } ]
         | Star -> [ Mov { dst = Reg (Virt d); src = l }; IMul { dst = Virt d; src = r } ]
         | Slash ->
           [ Mov { dst = Reg (Phys EAX); src = l }
           ; Cdq
           ; IDiv r
           ; Mov { dst = Reg (Virt d); src = Reg (Phys EAX) }
           ]
         (* these are all because I wanted to support [not]... *)
         | Eq ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Sete (Virt d)
           ]
         | Neq ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Setne (Virt d)
           ]
         | Lt ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Setl (Virt d)
           ]
         | Leq ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Setle (Virt d)
           ]
         | Gt ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Setg (Virt d)
           ]
         | Geq ->
           [ Mov { dst = Reg (Virt d); src = l }
           ; Cmp { dst = Virt d; src = r }
           ; Setge (Virt d)
           ]);
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Un_op { d; op; s } :: Tac.Cond_jump { tst; yes; no } :: rest ->
      assert (d = tst);
      let yes = resolve_label ~proc ~label:yes in
      let no = resolve_label ~proc ~label:no in
      let s = munch_operand s in
      Builder.add_instrs
        builder
        ~label
        (match op with
         | Not ->
           [ Mov { dst = Reg (Virt d); src = s }
           ; Cmp { dst = Virt d; src = Imm32 0l }
           ; Je yes
           ; Jmp no
           ]
         | Odd ->
           [ Mov { dst = Reg (Phys EAX); src = s }
           ; Cdq
           ; IDiv (Imm32 2l)
           ; Mov { dst = Reg (Virt d); src = Reg (Phys EDX) }
           ; Cmp { dst = Virt d; src = Imm32 1l }
           ; Je yes
           ; Jmp no
           ]
         | _ -> failwith "impossible");
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Un_op { d; op; s } :: rest ->
      let s = munch_operand s in
      Builder.add_instrs
        builder
        ~label
        (match op with
         | Not -> [ Mov { dst = Reg (Virt d); src = s }; Not (Virt d) ]
         | Odd ->
           [ Mov { dst = Reg (Phys EAX); src = s }
           ; Cdq
           ; IDiv (Imm32 2l)
           ; Mov { dst = Reg (Virt d); src = Reg (Phys EDX) }
           ; Cmp { dst = Virt d; src = Imm32 1l }
           ; Sete (Virt d)
           ]
         | Plus -> [ Mov { dst = Reg (Virt d); src = s } ]
         | Minus -> [ Mov { dst = Reg (Virt d); src = s }; Neg (Virt d) ]);
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Call p :: rest ->
      let process_label = resolve_label ~proc:(Some p) ~label:0 in
      push_access_link ~builder ~label ~caller:proc p;
      Builder.add_instrs
        builder
        ~label
        [ Call (Addr (Function process_label)); Add { dst = Phys RSP; src = Imm32 8l } ];
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Write e :: rest ->
      let e = munch_operand e in
      Builder.add_instrs
        builder
        ~label
        [ Mov { dst = Reg (Phys EDI); src = e }
        ; Push (Phys RBP)
        ; Call (Addr (Function "_write"))
        ; Add { dst = Phys RSP; src = Imm32 8l }
        ];
      munch_block ~builder ~proc ~framesize ~label rest
    | Tac.Read x :: rest ->
      let x = resolve_addr ~builder ~proc ~label x in
      Builder.add_instrs
        builder
        ~label
        [ Lea { dst = Phys RDI; src = Addr x }
        ; Push (Phys RBP)
        ; Call (Addr (Function "_read"))
        ; Add { dst = Phys RSP; src = Imm32 8l }
        ];
      munch_block ~builder ~proc ~framesize ~label rest
    | _ -> failwith "impossible"
  ;;

  let munch ~builder ~proc ~framesize (body : Tac.block list) =
    List.iter body ~f:(fun { label; instrs } ->
      let real_label = resolve_label ~proc ~label in
      if label = 0
      then
        Builder.add_instrs
          builder
          ~label:real_label
          [ Push (Phys RBP)
          ; Mov { dst = Reg (Phys RBP); src = Reg (Phys RSP) }
          ; Sub { dst = Phys RSP; src = Imm32 framesize }
          ];
      munch_block ~builder ~proc ~framesize ~label:real_label instrs)
  ;;
end

let compute_framesize locals =
  let nlocals = List.length locals in
  let nlocals = if nlocals % 2 = 0 then nlocals else nlocals + 1 in
  let nbytes = nlocals * 4 in
  (* call needs the stack frame to be 16-byte-aligned. The +8 comes from the access link. *)
  Int32.of_int_exn @@ if (nbytes + 8) % 16 = 0 then nbytes else nbytes + 8
;;

let isel ({ globals; procedures } : Tac.program) : Mir.program =
  (* assign stack slots *)
  List.iter procedures ~f:(fun { locals; _ } ->
    List.iteri locals ~f:(fun i sym -> Symbol.set_slot sym (Int32.of_int_exn (i + 1))));
  (* and then do codegen *)
  let builder = Builder.create globals in
  List.iter procedures ~f:(fun { name; body; locals } ->
    Mir.munch ~builder ~proc:name ~framesize:(compute_framesize locals) body);
  Builder.build builder
;;
