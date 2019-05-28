(*
let _ = 
  let open X86 in
  let faulting_sequence = 
    { pref = []; instr = (Add,[Memexpr(Md(Mem32(SS,Some(Ebp),None,Some(0x5bl))));GeneralReg(Gd(Ebx))]);}::
    { pref = []; instr = (Ret,[]) }::
    []
  in
  let ir = List.concat (List.map (X86ToIR.translate_instr 0l) faulting_sequence) in
  let ir = List.rev (List.tl (List.rev ir)) in
  determine_sequence_behaviors faulting_sequence ir 3

  let z3ctx = Z3SymbolicExecute.mk_context () in
  let ssa_tbl,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars ir in
  let _ = Z3SymbolicExecute.symbolic_execute z3ctx ir_ssa (verify_stack_differential ssa_tbl 4l) in
  let res = 
    match Z3.check z3ctx with
    | Z3.L_FALSE -> true
    | _ -> false
  in     
  IDA.msg "%b\n" res;

  let _ = Z3SymbolicExecute.symbolic_execute z3ctx [] (verify_set_const_32 ssa_tbl X86ToIRUtil.vEax 0l) in
  IDA.msg "%s\n" (Z3.context_to_string z3ctx);
  match Z3.check z3ctx with
  | Z3.L_FALSE -> true
  | _ -> false

*)

