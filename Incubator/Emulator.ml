(*
#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\Incubator\\emulator.ml";;
*)

(* Tried to read from a memory address whose contents were unset.  Used in the
   lambda memory. *)
exception MemoryContentsUnknown of int32

(* Function creates a hash table with default values for all registers.  This 
   is merely for convenience, as the user is expected to override some of 
   these values. *)
let mk_default_regctx () =
  let open IR in
  let regctx = Hashtbl.create 30 in
  let open X86ToIRUtil in
  Hashtbl.add regctx vEax (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEcx (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEdx (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEbx (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEsp (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEbp (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEsi (Const(0L,TypeReg_32));
  Hashtbl.add regctx vEdi (Const(0L,TypeReg_32));
  Hashtbl.add regctx vES  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vCS  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vSS  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vDS  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vFS  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vGS  (Const(3L,TypeReg_16));
  Hashtbl.add regctx vCF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vPF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vAF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vZF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vSF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vOF  (Const(0L,TypeReg_1));
  Hashtbl.add regctx vDF  (Const(0L,TypeReg_1));
  regctx

(* A state is a memory, register hash table, eip, ssa state (mapping the 
   vanilla register locations into their current SSA incarnations), and
   a z3 context. *)
type emu_state =
{
  mem: int32 -> int32;
  regs: (IR.var, IR.expr) Hashtbl.t;
  eip: int32;
  ssa: (IR.var, IR.var) Hashtbl.t;
  z3: Z3.context;
  instrs: IR.instr list;
}

(* A convenience function to create a default initial state.  Once again, this
   is for convenience, and you should override specific registers and memory
   locations. *)
let mk_initial_state eip =
{
  mem    = (fun a -> raise (MemoryContentsUnknown(a)));
  regs   = mk_default_regctx ();
  eip    = eip;
  ssa    = Hashtbl.create 50;
  z3     = Z3.mk_context_x [|("MODEL", "true")|];
  instrs = [];
}

(* Load a series of bytes in little-endian format from the memory valuation 
   and store it into an int64 *)
let load_quantity size mem a = 
  let n = (IRUtil.bits size)/8 in
  let rec aux c i =
    if i < n
    then aux (Int64.logor (Int64.shift_left (Int64.of_int32 (mem (Int32.add a (Int32.of_int i)))) (i*8)) c) (i+1)
    else c
  in
  aux 0L 0

(* Specialization of the above function for bytes/words/dwords/qwords *)
let load_byte  mem a = (load_quantity (IR.TypeReg_8)  mem a)
let load_word  mem a = (load_quantity (IR.TypeReg_16) mem a)
let load_dword mem a = (load_quantity (IR.TypeReg_32) mem a)
let load_qword = load_quantity (IR.TypeReg_64)
  
(* Recursively remove load expressions.  The load expression must evaluate to a
   constant address, and there must be constant bytes at that address according
   to the memory valuation function. *)
let rec remove_loads_expr mem e = let open IR in match e with
| Load(m,a,s)    -> 
  let a = 
    match a with 
    | Const(i,_) -> i 
    | _ -> failwith "remove_loads_expr:  address did not evaluate to a constant" 
  in
  Const(load_quantity s mem (Int64.to_int32 a),s)
| Binop(el,o,er) -> Binop(remove_loads_expr mem el, o, remove_loads_expr mem er)
| Unop(o,e)      -> Unop(o,remove_loads_expr mem e)
| Store(m,a,t,s) -> Store(remove_loads_expr mem m, remove_loads_expr mem a, remove_loads_expr mem t, s)
| Let(v,ea,eb)   -> Let(v,remove_loads_expr mem ea, remove_loads_expr mem eb)
| Cast(k,s,e)    -> Cast(k,s,remove_loads_expr mem e)
| Var(_)         -> e
| Const(_)       -> e

(* Same as above, but works on IR statements *)
let remove_loads_instr mem i = let open IR in match i with
| Assign(v,e) -> Assign(v,remove_loads_expr mem e)
| Jmp(e)      -> Jmp(remove_loads_expr mem e)
| CJmp(c,t,n) -> CJmp(remove_loads_expr mem c,remove_loads_expr mem t,remove_loads_expr mem n)
| Halt(e)     -> Halt(remove_loads_expr mem e)
| Assert(e)   -> Assert(remove_loads_expr mem e)
| Label(_)
| Special(_)
| Comment(_)  -> i

(* This is shitty.  This function determines whether a given IR statement 
   writes to memory, but only if the statement matches a syntactic pattern. *)
let is_store x = let open IR in match x with 
| Assign(Mem(_,e,es),Store(m,a,t,s)) -> true
| _ -> false

(*
Set initial state, memory contents, EIP

Step:
* IR translate EIP
* Substitute all loads for their values
* Constant fold with the state
* Determine next instruction
*)
let take_step state = 
  (* Print a banner, and then a list of instructions *)
  let print_trans decr trans = ()
  (*let _ = IDA.msg "%s\n" decr in 
    List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) trans *)
  in

  (* Get x86 instruction at EIP *)
  let x86instr,_,succ = X86Decode.decode state.eip in

  (* Translate x86 instruction to IR *)
  let trans = X86ToIR.translate_instr state.eip x86instr in
  print_trans "Original translation" trans;

  (* Convert to SSA *)
  let trans_ssa = IRSSA.bb_to_ssa_state_in state.ssa trans (fun l _ -> l) in

  (* Optimize SSA translation *)
  let trans_opt = IRLocalOpt.local_opt_state_in state.regs trans_ssa in

(*
  (* Remove loads by looking them up in in the int32 -> int32 memory map *)
  let trans_noloads = List.map (remove_loads_instr state.mem) trans_opt in

  (* Optimize trans_noloads *)
  let trans_noloads_opt = IRLocalOpt.local_opt_state_in state.regs trans_noloads in

  (* Remove temporaries from regs to keep it small *)
  print_trans "Optimized, load-removed translation" trans_noloads_opt;
*)
  let trans_noloads_opt = trans_opt in
    
  let was_jump = ref false in
  (* Function to extract the SSA'd CJmp-cond expression *)
  let following_address = let open IR in match succ with
  | ASMUtil.Jcc(ta,fa) ->
    (* Just changed this from trans_ssa *)
    let rlist = List.rev trans_noloads_opt in
    let last_ssa_instr = List.hd rlist in
    let beginning_ssa_instrs = List.rev (List.tl rlist) in
    let get_jcc_cond_expr () = 
      match last_ssa_instr with
      | CJmp(e,_,_) -> e
      | _ -> failwith "impossible: IR trans ended with CJmp, SSA trans of the same instructions did not?"
    in
    (* Get the destination address and the jump condition (SSA'd) *)
    let dest,cond = match (List.hd (List.rev trans_noloads_opt)) with
    | CJmp(Const(0L,TypeReg_1),_,_) -> fa,IRUtil.mk_not (get_jcc_cond_expr ())
    | CJmp(Const(1L,TypeReg_1),_,_) -> ta,get_jcc_cond_expr ()
    | i -> failwith ("Last SSA instruction should have been CJmp, was "^PpIR.ppInstr i^"instead")
    in 
    List.iter (Z3SymbolicExecute.instr_to_z3 state.z3) beginning_ssa_instrs;
    Z3SymbolicExecute.instr_to_z3 state.z3 (Assert(cond));
    was_jump := true;
    dest
  | ASMUtil.Flow(a)   -> a
  | ASMUtil.Jmp(a)    -> was_jump := true; a
  | ASMUtil.Call(a,_) -> a
  | ASMUtil.ICall(_)  -> failwith (Printf.sprintf "%lx: indirect call" state.eip)
  | ASMUtil.IJmp      -> failwith (Printf.sprintf "%lx: indirect jump" state.eip)
  | ASMUtil.Return    -> 
    let espval = Hashtbl.find state.regs X86ToIRUtil.vEsp in
    let espval = match espval with | Const(i,TypeReg_32) -> Int64.to_int32 i | _ -> failwith "non-constant in constant map" in
    Int64.to_int32 (load_dword state.mem espval)
  in 
  if not (!was_jump)
  then List.iter (Z3SymbolicExecute.instr_to_z3 state.z3) trans_opt;
  { state with eip = following_address; instrs = (List.rev trans)@(state.instrs) }

let dump_step state = 
  let get_var v = 
    let res = 
      try Hashtbl.find state.ssa v
      with Not_found -> v
    in res
  in
  let get_const_value_as_int32_string r = 
    let r = 
      try Some(Hashtbl.find state.regs (get_var r))
      with Not_found -> None 
    in
    match r with
    | Some(IR.Const(i,s)) when s = IR.TypeReg_32 -> Printf.sprintf "%08lx" (Int64.to_int32 i)
    | _ -> "????????"
  in
  let get_const_value_as_int1_string r =
    let r = 
      try Some(Hashtbl.find state.regs (get_var r))
      with Not_found -> None 
    in
    match r with
    | Some(IR.Const(i,s)) when s = IR.TypeReg_1 -> Printf.sprintf "%01lx" (Int64.to_int32 i)
    | _ -> "?"
  in
  IDA.msg "EIP: %08lx\n" state.eip;
  IDA.msg "EAX      ECX      EDX      EBX      ESP      EBP      ESI      EDI      CF PF AF ZF SF OF DF\n";
  let open X86ToIRUtil in
  IDA.msg "%s %s %s %s %s %s %s %s %s  %s  %s  %s  %s  %s  %s\n"
   (get_const_value_as_int32_string vEax)
   (get_const_value_as_int32_string vEcx)
   (get_const_value_as_int32_string vEdx)
   (get_const_value_as_int32_string vEbx)
   (get_const_value_as_int32_string vEsp)
   (get_const_value_as_int32_string vEbp)
   (get_const_value_as_int32_string vEsi)
   (get_const_value_as_int32_string vEdi)
   (get_const_value_as_int1_string  vCF )
   (get_const_value_as_int1_string  vPF )
   (get_const_value_as_int1_string  vAF )
   (get_const_value_as_int1_string  vZF )
   (get_const_value_as_int1_string  vSF )
   (get_const_value_as_int1_string  vOF )
   (get_const_value_as_int1_string  vDF )

let take_step state =
  dump_step state;
  let state = take_step state in
  dump_step state;
  state
  
let state = ref (*(example_initialize_memory_and_registers ());;*)(mk_initial_state 0l);;

let blah () = 
  let newstate = take_step !state in
  state := newstate;;

IDAHotKey.register "CTRL-F11" blah;;

let take_steps nopt aopt =
  let _ = match nopt,aopt with
  | None,None -> invalid_arg "take_steps:  no stop condition provided"
  | _,_ -> ()
  in
  let rec aux i =
    match nopt with
    | Some(n) when i >= n -> ()
    | _ -> 
     (match aopt with 
      | Some(a) when !state.eip = a -> ()
      | _ -> blah (); aux (i+1))
  in aux 0


let generate_jump_taken ea =
  let x86instr,_,succ = X86Decode.decode ea in
  let trans = X86ToIR.translate_instr ea x86instr in
  let trans_ssa = IRSSA.bb_to_ssa_state_in !state.ssa trans (fun l _ -> l) in
  let last_instr = List.hd (List.rev trans_ssa) in
  let jcc_cond = 
    match last_instr with
    | IR.CJmp(e,_,_) -> e
    | _ ->   
      IDA.msg "%s\n" (PpIR.ppInstr last_instr);
      failwith "impossible: IR trans ended with CJmp, SSA trans of the same instructions did not?"
  in
  IDA.msg "%s\n" (PpIR.ppExpr false jcc_cond);
  Z3SymbolicExecute.instr_to_z3 !state.z3 (IR.Assert(jcc_cond))

(*
#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\Incubator\\emulator.ml";;

Z3.trace_to_file !state.z3 "c:\\temp\\trace.txt";;
take_steps (Some(500)) (Some(IDA.get_screen_ea ()));;
generate_jump_taken (IDA.get_screen_ea ());;
Z3SymbolicExecute.symbolic_execute !state.z3 [] (IRUtil.mk_true);;
*)