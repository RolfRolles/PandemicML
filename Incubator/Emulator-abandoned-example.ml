(* Sets up the symbolic state for one particular example.  This is not *)
let example_initialize_memory_and_registers () = 
  let open Z3SymbolicExecute in
  let open IR in
  let assert_and_store_reg32 state r32 i32 =
    Hashtbl.replace state.regs r32 (Const(Int64.of_int32 i32,TypeReg_32));
    instr_to_z3 state.z3 (Assert(IRUtil.mk_eq (IRUtil.mk_evar r32) (Const(Int64.of_int32 i32,TypeReg_32))))
  in
  let state = mk_initial_state 0x004010E0l in
  let _ = Z3.trace_to_file state.z3 "c:\\temp\\trace.txt" in
  let mem = state.mem in
  
  let serial_memloc = 0x40000l in
  let user_memloc = 0x30000l in
  let stack_top = 0x20000l in
  
  (* Store pointers to name and serial, and a dummy return value *)
  let stack_bottom_displ = 4l in

(*let mem = assert_and_store_dword_at  mem ctx memvar (Int32.sub stack_top stack_bottom_displ) serial_memloc in
  let stack_bottom_displ = Int32.add stack_bottom_displ 4l in
  let mem = assert_and_store_dword_at  mem ctx memvar (Int32.sub stack_top stack_bottom_displ)   user_memloc in
  let stack_bottom_displ = Int32.add stack_bottom_displ 4l in*)

  (* Store 0xDEADBEEF in place of the return address *)
  let z3mem = Z3SymbolicExecute.mk_z3var state.z3 X86ToIRUtil.vMem in
  let mem = assert_and_store_dword_at mem state.z3 z3mem (Int32.sub stack_top stack_bottom_displ) 0xDEADBEEFl in
  let stack_bottom_displ = Int32.add stack_bottom_displ 4l in
  
  (* Assert and set the initial register states *)
  assert_and_store_reg32 state X86ToIRUtil.vEax serial_memloc;
  assert_and_store_reg32 state X86ToIRUtil.vEdi   user_memloc;

  (* Assert and store memory contents for name *)
  let mem = assert_and_store_string_at mem state.z3 z3mem user_memloc "Rolf Rolles" in
  
  (* Store memory contents, but don't assert them (they're symbolic) *)
  let mem = store_string_at mem serial_memloc "ABCD-0000000000" in

  (* Set all registers to zero; point ESP to the bottom of the stack *)
  assert_and_store_reg32 state X86ToIRUtil.vEsp (Int32.sub stack_top stack_bottom_displ);
  { state with mem = mem }

  (* assert that ESP = 0x1FFF4l *)
  (* assert that EIP = 0x401080l *)
  (* assert that everything else = 0 *)
  
