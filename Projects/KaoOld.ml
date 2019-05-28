(*
#use "c:\\paframework\\Projects\\Kao.ml";;
*)

(* A convenience function to create a default initial state.  Once again, this
   is for convenience, and you should override specific registers and memory
   locations. *)
let mk_initial_state eip =
{
  mem    = (fun a -> raise (MemoryContentsUnknown(a)));
  regs   = Hashtbl.create 50;
  eip    = eip;
  ssa    = Hashtbl.create 50;
  z3     = Z3.mk_context_x [|("MODEL", "true")|];
  instrs = [];
}

let kao_initialize_memory_and_registers () = 
  let open Z3SymbolicExecute in
  let open IR in
  let assert_and_store_reg32 state r32 i32 =
    Hashtbl.replace state.regs r32 (Const(Int64.of_int32 i32,TypeReg_32));
    instr_to_z3 state.z3 (Assert(IRUtil.mk_eq (IRUtil.mk_evar r32) (Const(Int64.of_int32 i32,TypeReg_32))))
  in
  
  (* 00401105 is the EIP of the start of the serial checking loop *)
  let state = mk_initial_state 0x00401105l in
  let z3mem = Z3SymbolicExecute.mk_z3var state.z3 X86ToIRUtil.vMem in

  (* We need to point esi to the raw hex version of the activation code, 
     which we will store at location 0x0 *)
  assert_and_store_reg32 state X86ToIRUtil.vEsi 0x00l;
  
  (* We need to point edi to the 32-byte output buffer,
     which we will store at location 0x20 *)
  assert_and_store_reg32 state X86ToIRUtil.vEdi 0x20l;

  (* We need to set ecx to 0x20 *)
  assert_and_store_reg32 state X86ToIRUtil.vEcx 0x20l;

  let memory_precondition_pairs = 
   [0x00l,0xF5l;
    0x01l,0xF3l;
    0x02l,0x76l;
    0x03l,0x86l;
    0x04l,0x53l;
    0x05l,0xB9l;
    0x06l,0xE4l;
    0x07l,0x78l;
    0x08l,0x2Bl;
    0x09l,0x17l;
    0x0Al,0x26l;
    0x0Bl,0xB2l;
    0x0Cl,0xE9l;
    0x0Dl,0x99l;
    0x0El,0x46l;
    0x0Fl,0x17l;
    0x10l,0x87l;
    0x11l,0xC1l;
    0x12l,0xC3l;
    0x13l,0x85l;
    0x14l,0xB5l;
    0x15l,0xBAl;
    0x16l,0x46l;
    0x17l,0x94l;
    0x18l,0xC9l;
    0x19l,0xF2l;
    0x1Al,0x82l;
    0x1Bl,0x55l;
    0x1Cl,0xB4l;
    0x1Dl,0xD3l;
    0x1El,0x14l;
    0x1Fl,0x57l;]
  in
  List.iter (fun (addr,value) -> assert_byte_at state.z3 z3mem addr value) memory_precondition_pairs;       
  (* We need to not explicitly set ebx and edx, since these are symbolic.
     Simply return the state as we have constructed it above. *)
  state
