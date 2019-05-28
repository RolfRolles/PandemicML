let local_constant_fold_x86 varmap x86list = 
  let open X86 in
  let subregisters_with_immediate reg imm = 
    let i16 = Int32.logand imm 0xffffl in
    let i8h = Int32.logand (Int32.shift_right_logical imm 8) 0xffl in
    let i8l = Int32.logand imm 0xffl in
    match reg with
    | GeneralReg(Gd(r32)) ->
     (match r32 with
      | Eax -> [GeneralReg(Gw(Ax)),i16;GeneralReg(Gb(Ah)),i8h;GeneralReg(Gb(Al)),i8l]
      | Ecx -> [GeneralReg(Gw(Cx)),i16;GeneralReg(Gb(Ch)),i8h;GeneralReg(Gb(Cl)),i8l]
      | Edx -> [GeneralReg(Gw(Dx)),i16;GeneralReg(Gb(Dh)),i8h;GeneralReg(Gb(Dl)),i8l]
      | Ebx -> [GeneralReg(Gw(Bx)),i16;GeneralReg(Gb(Bh)),i8h;GeneralReg(Gb(Bl)),i8l]
      | Esp -> [GeneralReg(Gw(Sp)),i16]
      | Ebp -> [GeneralReg(Gw(Bp)),i16]
      | Esi -> [GeneralReg(Gw(Si)),i16]
      | Edi -> [GeneralReg(Gw(Di)),i16])
    | GeneralReg(Gw(r16)) ->
     (match r16 with
      | Ax -> [GeneralReg(Gb(Ah)),i8h;GeneralReg(Gb(Al)),i8l]
      | Cx -> [GeneralReg(Gb(Ch)),i8h;GeneralReg(Gb(Cl)),i8l]
      | Dx -> [GeneralReg(Gb(Dh)),i8h;GeneralReg(Gb(Dl)),i8l]
      | Bx -> [GeneralReg(Gb(Bh)),i8h;GeneralReg(Gb(Bl)),i8l]
      | Sp -> []
      | Bp -> []
      | Si -> []
      | Di -> [])
    | GeneralReg(Gb(_)) -> []
    | _ -> invalid_arg "subregisters"  
  in
  let subregisters r = List.map fst (subregisters_with_immediate r 0l) in
  let superregisters = function
  | GeneralReg(Gd(r32)) -> []
  | GeneralReg(Gw(r16)) ->
   (match r16 with
    | Ax -> [GeneralReg(Gd(Eax))]
    | Cx -> [GeneralReg(Gd(Ecx))]
    | Dx -> [GeneralReg(Gd(Edx))]
    | Bx -> [GeneralReg(Gd(Ebx))]
    | Sp -> [GeneralReg(Gd(Esp))]
    | Bp -> [GeneralReg(Gd(Ebp))]
    | Si -> [GeneralReg(Gd(Esi))]
    | Di -> [GeneralReg(Gd(Edi))])
  | GeneralReg(Gb(r8)) ->
   (match r8 with
    | Ah | Al -> [GeneralReg(Gd(Eax));GeneralReg(Gw(Ax))]
    | Ch | Cl -> [GeneralReg(Gd(Ecx));GeneralReg(Gw(Cx))]
    | Dh | Dl -> [GeneralReg(Gd(Edx));GeneralReg(Gw(Dx))]
    | Bh | Bl -> [GeneralReg(Gd(Ebx));GeneralReg(Gw(Bx))])
  | _ -> invalid_arg "superregisters"
  
  in
  let get_r16_of_r8 r8 =
    let sr = superregisters r8 in
    let sr = List.filter (function | GeneralReg(Gw(_)) -> true | _ -> false) sr in
    match sr with
    | [x] -> x
    | _ -> failwith "impossible"
  
  in
  let remove_register_subregister_and_superregister_mappings = function
  | (GeneralReg(_) as g) -> 
    let sr = subregisters g in
    List.iter (fun r -> Hashtbl.remove varmap r) sr;
    let sr = superregisters g in
    List.iter (fun r -> Hashtbl.remove varmap r) sr;
    Hashtbl.remove varmap g  
  | _ -> invalid_arg "remove_register_subregister_and_superregister_mappings"
  
  in
  let add_new_register_mappings = List.iter (fun (r,i) -> Hashtbl.replace varmap r i) in
  let is_constant r = try ignore(Hashtbl.find varmap r); true with Not_found -> false in
  let update_variable_mapping r i =
    remove_register_subregister_and_superregister_mappings r;
    match r,i with
    | GeneralReg(Gd(_)),Immediate(Id(imm))
    | GeneralReg(Gw(_)),Immediate(Iw(imm)) ->
      add_new_register_mappings (subregisters_with_immediate r imm);
      Hashtbl.replace varmap r imm
    | GeneralReg(Gb(_)),Immediate(Ib(i8)) ->
      Hashtbl.replace varmap r i8
    | _,_ -> invalid_arg "update_variable_mapping"
  in
  let defined_ir_registers_to_x86_registers defset =
    let de = IRUtil.VarSet.elements defset in
    let de = List.filter (fun v -> IRUtil.varno v < 8) de in
    let open IR in
    let map = function
    | Variable(0,TypeReg_32) -> GeneralReg(Gd(Eax))
    | Variable(1,TypeReg_32) -> GeneralReg(Gd(Ecx))
    | Variable(2,TypeReg_32) -> GeneralReg(Gd(Edx))
    | Variable(3,TypeReg_32) -> GeneralReg(Gd(Ebx))
    | Variable(4,TypeReg_32) -> GeneralReg(Gd(Esp))
    | Variable(5,TypeReg_32) -> GeneralReg(Gd(Ebp))
    | Variable(6,TypeReg_32) -> GeneralReg(Gd(Esi))
    | Variable(7,TypeReg_32) -> GeneralReg(Gd(Edi))
    | _ -> failwith "impossible: filtered out these registers by now"
    in
    List.map map de
  in
  let get_const = function
  | Immediate(Id(i32)) -> i32
  | Immediate(Iw(i16)) -> i16
  | Immediate(Ib(i8 )) -> i8
  | _ -> invalid_arg "get_const"
  in
  let fold_shift o reg i8 =
    let value = Hashtbl.find varmap reg in
    match reg with
    | GeneralReg(Gd(_)) -> Immediate(Id(X86LocalOpt.constant_fold_binop_32 o value i8))
    | GeneralReg(Gw(_)) -> Immediate(Iw(X86LocalOpt.constant_fold_binop_16 o value i8))
    | GeneralReg(Gb(_)) -> Immediate(Ib(X86LocalOpt.constant_fold_binop_8  o value i8))
    | _ -> invalid_arg "fold_shift"
  in
  let fold_binop_reg_imm o reg imm =
    let value = Hashtbl.find varmap reg in
    let imm = get_const imm in
    match reg with
    | GeneralReg(Gd(_)) -> Immediate(Id(X86LocalOpt.constant_fold_binop_32 o value imm))
    | GeneralReg(Gw(_)) -> Immediate(Iw(X86LocalOpt.constant_fold_binop_16 o value imm))
    | GeneralReg(Gb(_)) -> Immediate(Ib(X86LocalOpt.constant_fold_binop_8  o value imm))
    | _ -> invalid_arg "fold_binop_reg_imm"
  in
  let fold_binop_reg_reg o reg1 reg2 =
    let lhs = Hashtbl.find varmap reg1 in
    let rhs = Hashtbl.find varmap reg2 in
    match reg1 with
    | GeneralReg(Gd(_)) -> Immediate(Id(X86LocalOpt.constant_fold_binop_32 o lhs rhs))
    | GeneralReg(Gw(_)) -> Immediate(Iw(X86LocalOpt.constant_fold_binop_16 o lhs rhs))
    | GeneralReg(Gb(_)) -> Immediate(Ib(X86LocalOpt.constant_fold_binop_8  o lhs rhs))
    | _ -> invalid_arg "fold_binop_reg_reg"
  in
  let fold_unop o reg =
    let value = Hashtbl.find varmap reg in
    match reg with
    | GeneralReg(Gd(_)) -> Immediate(Id(X86LocalOpt.constant_fold_unop_32 o value))
    | GeneralReg(Gw(_)) -> Immediate(Iw(X86LocalOpt.constant_fold_unop_16 o value))
    | GeneralReg(Gb(_)) -> Immediate(Ib(X86LocalOpt.constant_fold_unop_8  o value))
    | _ -> invalid_arg "fold_unop"
  in
  let const_of_size reg imm =
    match reg with
    | GeneralReg(Gd(_)) -> Immediate(Id(imm))
    | GeneralReg(Gw(_)) -> Immediate(Iw(imm))
    | GeneralReg(Gb(_)) -> Immediate(Ib(imm))
    | _ -> invalid_arg "const_of_size"
  in
  let is_constant_reg r = match r with
  | GeneralReg(Gd(_))
  | GeneralReg(Gw(_))
  | GeneralReg(Gb(_)) -> is_constant r
  | _ -> false
  in
  let changed = ref false in
  let rec constant_propagate_and_fold insn = 
    let print_difference iold inew =
      changed := true;
    (*IDA.msg "Old insn: %s new insn: %s\n" (X86Disasm.string_of_x86instr iold) (X86Disasm.string_of_x86instr inew);*)
      inew
    in
    match insn with
    | { pref = _; instr = (Mov,[(GeneralReg(_) as reg);(Immediate(_) as imm)]) } ->
      update_variable_mapping reg imm;
      insn
    (* Need the right-sized immediate
    | { pref = _; instr = (Mov,[(GeneralReg(_) as reg1);(GeneralReg(_) as reg2)]) } 
      when is_constant reg2 ->
      update_variable_mapping reg imm;
      insn
    *)
    | { pref = _;  instr = (((Shl|Shr|Sar|Rol|Ror) as o),[(GeneralReg(_) as reg);Immediate(Ib(i8))]) }
      when is_constant reg ->
      let new_imm = fold_shift o reg i8 in
      update_variable_mapping reg new_imm;
      let new_insn = { pref = []; instr = (Mov,[reg;new_imm]) } in
      print_difference insn new_insn
    | { pref = _;  instr = (((Neg|Not|Inc|Dec|Bswap) as o),[(GeneralReg(_) as reg)]) }
      when is_constant reg ->
      let new_imm = fold_unop o reg in
      update_variable_mapping reg new_imm;
      let new_insn = { pref = []; instr = (Mov,[reg;new_imm]) } in
      print_difference insn new_insn
    | { pref = _;  instr = (((Add|Sub|Or|Xor|And) as o),[(GeneralReg(_) as reg);(Immediate(_) as imm)]) }
      when is_constant reg ->
      let new_imm = fold_binop_reg_imm o reg imm in
      update_variable_mapping reg new_imm;
      let new_insn = { pref = []; instr = (Mov,[reg;new_imm]) } in
      print_difference insn new_insn
    | { pref = _;  instr = (Mov,[(GeneralReg(_) as reg1);(GeneralReg(_) as reg2)]) }
      when is_constant reg2 ->
      remove_register_subregister_and_superregister_mappings reg1;
      let new_imm_int32 = Hashtbl.find varmap reg2 in
      let new_imm = const_of_size reg2 new_imm_int32 in
      update_variable_mapping reg1 new_imm;
      let new_insn = { pref = []; instr = (Mov,[reg1;new_imm]) } in
      print_difference insn new_insn
    | { pref = _;  instr = (((Add|Sub|Or|Xor|And) as o),[op1;(GeneralReg(_) as reg2)]) }
      when is_constant reg2 ->
      if is_constant_reg op1
      then
       (let new_imm = fold_binop_reg_reg o op1 reg2 in
        update_variable_mapping op1 new_imm;
        let new_insn = { pref = []; instr = (Mov,[op1;new_imm]) } in
        print_difference insn new_insn)
      else
       (let new_imm_int32 = Hashtbl.find varmap reg2 in
        let new_insn = { pref = []; instr = (o,[op1;const_of_size reg2 new_imm_int32]) } in
        changed := true;
        constant_propagate_and_fold new_insn)

    | { pref = _; instr = (Xchg,[(GeneralReg(Gb(_)) as lhs);(GeneralReg(Gb(_)) as rhs)]) }
      when superregisters lhs = superregisters rhs && lhs <> rhs && is_constant (get_r16_of_r8 lhs) ->
      let reg = get_r16_of_r8 lhs in
      let new_imm = 
        let old_imm = Hashtbl.find varmap reg in
        Immediate(Iw(LowLevel.xchg_word old_imm))
      in
      update_variable_mapping reg new_imm;
      let new_insn = { pref = []; instr = (Mov,[reg;new_imm]) } in
      print_difference insn new_insn

    | insn ->
      let defsir  = fst (X86AnalyzeDefUse.get_def_uses insn) in
      let defsx86 = defined_ir_registers_to_x86_registers defsir in
      List.iter remove_register_subregister_and_superregister_mappings defsx86;
      insn
  in
  let res = List.map constant_propagate_and_fold x86list in
  ((res,!changed),varmap)

let local_constant_fold_x86_state_in varmap x86list = 
  fst(local_constant_fold_x86 varmap x86list)
  
let local_constant_fold_x86_state_out x86list = 
  let varmap = Hashtbl.create 16 in
  local_constant_fold_x86 varmap x86list

let local_constant_fold_x86 x86list =
  fst(local_constant_fold_x86_state_out x86list)

