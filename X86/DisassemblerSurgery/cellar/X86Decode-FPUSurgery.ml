(* TODO:  General cleanup at this point.  Introduce the oM operand functor for 
   the LEA instruction; clean up language regarding modrm_mem_reg_predicate vs.
   _discriminator 
   
   let oJz () = ...
   Need to behave differently when given a 16-bit address size:  and with 0xFFFFl.
   *)

open X86

(* Fetch bytes from an arbitrary source.  Do this more intelligently. *)
let get_byte = ref (fun _ -> failwith "X86Decode: get_byte uninitialized")
let init gb = get_byte := gb

(* Types and basic convenience functions:  prefixes, mod R/M *)
type opsizeprefix   = PF3_OpSize
type addrsizeprefix = PF4_AddrSize
type x86_address_size = AddrSize_16 | AddrSize_32 | AddrSize_64
type x86_operand_size = OpndSize_16 | OpndSize_32 | OpndSize_64
type x86_operating_mode = OM_16 | OM_32 | OM_64

type prefixcontext = { 
  group1pf: x86_group1_prefix list;  segmentpf: x86_segreg list; 
  opsizepf: opsizeprefix list;      addrsizepf: addrsizeprefix list }
let mk_default_pref  () = { group1pf = []; segmentpf = []; opsizepf = []; addrsizepf = [] }

type modRMSize = ModRM_32 | ModRM_16
type modRM = RegisterNumber of int 
           (* base register number, scale register number, scale factor, displacement *)
           | MemoryPhrase of int * int * int * int32 

type modrmcontext = { modrmsize: modRMSize; mreg: int; modrmbyte: int32; modrm: modRM }
let mk_default_modrm () = { modrmsize = ModRM_32; mreg = ~-1; modrmbyte = 0xFFFFFFFFl; modrm = RegisterNumber(~-1) }

(* Things that might go wrong *)
exception UnimplementedInstruction of string
exception InvalidLockPrefix of x86instrpref
exception TooLongInstruction
exception InvalidInstruction
exception DoMoreResearch of string
exception UnsupportedMode of x86_operating_mode
exception UnsupportedAddressSize of x86_address_size
exception UnsupportedOperandSize of x86_operand_size
let invalid () = raise InvalidInstruction

(* The "context" that we accrue while decoding an instruction *)
let operating_mode = ref OM_32
let prefs = ref (mk_default_pref ())
let modrm_context = ref (mk_default_modrm ())
let segov = ref None
let getseg () = match !segov with | None -> DS | Some(s) -> s

(* Convenience functions so we don't have to pass ea around everywhere and 
   constantly check that the instruction is too long *)
let max_instr_len = ref 15
let len_of_curr_instr = ref 0 
let current_ea = ref 0x0l
let decr_ea () = current_ea := Int32.pred !current_ea
let incr_ea () = current_ea := Int32.succ !current_ea
let get_cursor () = !current_ea
let reset_cursor ea = 
  len_of_curr_instr := 0; current_ea := ea

let consume_byte () = 
  if !len_of_curr_instr > !max_instr_len
  then raise TooLongInstruction;
  incr len_of_curr_instr; 
  let x = !get_byte !current_ea in incr_ea (); x

let consume_word () = 
  let lb = consume_byte () in
  let hb = consume_byte () in
  (Int32.logor (Int32.shift_left hb 8) lb)

let consume_dword () = 
  let lw = consume_word () in
  let hw = consume_word () in
  (Int32.logor (Int32.shift_left hw 16) lw)

(* int -> x86operand functions *)
let segreg_of_int = function
| 0 -> ES 
| 1 -> CS 
| 2 -> SS 
| 3 -> DS 
| 4 -> FS 
| 5 -> GS 
| 6
| 7 -> invalid ()
| x -> failwith ("segreg_of_int "^string_of_int x)

let reg32_of_int = function
| 0 -> Eax
| 1 -> Ecx
| 2 -> Edx
| 3 -> Ebx
| 4 -> Esp
| 5 -> Ebp
| 6 -> Esi
| 7 -> Edi
| x -> failwith ("reg32_of_int "^string_of_int x)

let reg16_of_int = function
| 0 -> Ax
| 1 -> Cx
| 2 -> Dx
| 3 -> Bx
| 4 -> Sp
| 5 -> Bp
| 6 -> Si
| 7 -> Di
| x -> failwith ("reg16_of_int "^string_of_int x)

let reg8_of_int = function
| 0 -> Al
| 1 -> Cl
| 2 -> Dl
| 3 -> Bl
| 4 -> Ah
| 5 -> Ch
| 6 -> Dh
| 7 -> Bh
| x -> failwith ("reg8_of_int: "^string_of_int x)

let mmxreg_of_int = function
| 0 -> MM0
| 1 -> MM1
| 2 -> MM2
| 3 -> MM3
| 4 -> MM4
| 5 -> MM5
| 6 -> MM6
| 7 -> MM7
| x -> failwith ("x86_mmxreg_of_int "^string_of_int x)

let xmmreg_of_int = function
| 0 -> XMM0
| 1 -> XMM1
| 2 -> XMM2
| 3 -> XMM3
| 4 -> XMM4
| 5 -> XMM5
| 6 -> XMM6
| 7 -> XMM7
| x -> failwith ("x86_xmmreg_of_int "^string_of_int x)

let debug_reg_of_int = function
| 0 -> DR0
| 1 -> DR1
| 2 -> DR2
| 3 -> DR3
| 4 -> DR4
| 5 -> DR5
| 6 -> DR6
| 7 -> DR7
| x -> failwith ("debug_reg_of_int "^string_of_int x)

let control_reg_of_int = function
| 0 -> CR0
| 1 -> CR1
| 2 -> CR2
| 3 -> CR3
| 4 -> CR4
| 5 -> CR5
| 6 -> CR6
| 7 -> CR7
| x -> failwith ("control_reg_of_int "^string_of_int x)

let opnd_reg32_of_int  i = GeneralReg(Gd(reg32_of_int i))
let opnd_reg16_of_int  i = GeneralReg(Gw(reg16_of_int i))
let opnd_reg8_of_int   i = GeneralReg(Gb(reg8_of_int  i))
let opnd_mmxreg_of_int i = MMXReg(mmxreg_of_int i)
let opnd_xmmreg_of_int i = XMMReg(xmmreg_of_int i)

let dseg32 = function | Esp | Ebp -> SS | _ -> DS
let dseg16 = function | Bp -> SS | _ -> DS
let sf _   = DS
let vseg f r = match !segov with Some(s) -> s | None -> f r
let bs16,bs32,ss = vseg dseg16,vseg dseg32,vseg sf
let extract_seg brn srn bs r =
  match brn <> ~-1, srn <> ~-1 with
  | true,_ -> bs (r brn)
  | _,_ -> ss ()

let make_memexpr_32 modrmcontext = 
  let r = reg32_of_int in
  match modrmcontext.modrm with
  (* Shouldn't happen; we should only pass MemoryPhrase`s to this function *)
  | RegisterNumber(_) -> invalid_arg "make_memexpr_32"
  | MemoryPhrase(brn,srn,sf,displ) ->
    let seg = extract_seg brn srn bs32 r in
    let br = if brn   <> ~-1  then Some(r brn)    else None in
    let sr = if srn   <> ~-1  then Some(r srn,sf) else None in
    let ds = if displ <> 0x0l then Some(displ)    else None in
    match br,sr,ds with
    | None,None,None -> Mem32(seg,br,sr,Some(0l))
    | _,_,_ -> Mem32(seg,br,sr,ds)

let make_memexpr_16 modrmcontext = 
  let r = reg16_of_int in
  match modrmcontext.modrm with
  (* Shouldn't happen; we should only pass MemoryPhrase`s to this function *)
  | RegisterNumber(_) -> invalid_arg "make_memexpr_16"
  | MemoryPhrase(brn,srn,_,displ) ->
    let seg = extract_seg brn srn bs16 r in
    let br = if brn   <> ~-1  then Some(r brn) else None in
    let sr = if srn   <> ~-1  then Some(r srn) else None in
    let ds = if displ <> 0x0l then Some(displ) else None in
    match br,sr,ds with
    | None,None,None -> Mem16(seg,br,sr,Some(0l))
    | _,_,_ -> Mem16(seg,br,sr,ds)

(* Turn a modrm context into a segmented and sized memory expression *)
let make_memexpr modrmcontext =
  match modrmcontext.modrmsize with
  | ModRM_32 -> make_memexpr_32 modrmcontext
  | ModRM_16 -> make_memexpr_16 modrmcontext

(* If we're in X-bit mode, and there is an [addr,op]size prefix, then we use Y-bit
   X,Y \in {16,32} /\ X != Y *)
let determine_addr_size () =
  match (!operating_mode,List.mem (PF4_AddrSize) (!prefs.addrsizepf)) with
  | OM_32,false | OM_16,true  -> AddrSize_32
  | OM_32,true  | OM_16,false -> AddrSize_16
  | x,_ -> raise (UnsupportedMode(x))

let determine_opnd_size () =
  match (!operating_mode,List.mem (PF3_OpSize) (!prefs.opsizepf)) with
  | OM_32,false | OM_16,true  -> OpndSize_32
  | OM_32,true  | OM_16,false -> OpndSize_16
  | x,_ -> raise (UnsupportedMode(x))

(* Functions that force a different suspension depending on address size, op
   size, both, modrm memory versus register encoding *)
let opsize_predicate f16 f32 = 
  match determine_opnd_size () with
  | OpndSize_32 -> Lazy.force f32
  | OpndSize_16 -> Lazy.force f16
  | x -> raise (UnsupportedOperandSize(x))

let addrsize_predicate f16 f32 = 
  match determine_addr_size () with
  | AddrSize_32 -> Lazy.force f32
  | AddrSize_16 -> Lazy.force f16
  | x -> raise (UnsupportedAddressSize(x))
  
let addropsize_predicate f1616 f1632 f3216 f3232 =
  match (determine_addr_size (),determine_opnd_size ()) with
  | AddrSize_16,OpndSize_16 -> Lazy.force f1616
  | AddrSize_16,OpndSize_32 -> Lazy.force f1632
  | AddrSize_32,OpndSize_16 -> Lazy.force f3216
  | AddrSize_32,OpndSize_32 -> Lazy.force f3232
  | x,_ -> raise (UnsupportedAddressSize(x))

let modrm_mem_reg_predicate fmem freg = 
  match !modrm_context.modrm with
  | RegisterNumber(r)     -> (Lazy.force freg) r
  | MemoryPhrase(_,_,_,_) -> Lazy.force fmem

let modrm_mem_reg_discriminator fmem freg = 
  match !modrm_context.modrm with
  | RegisterNumber(_)     -> Lazy.force freg
  | MemoryPhrase(_,_,_,_) -> Lazy.force fmem

let size_predicated_reg i = 
  opsize_predicate 
    (lazy (opnd_reg16_of_int i)) 
    (lazy (opnd_reg32_of_int i))

(* Miscellaneous (and duplicated, much to my chagrin) *)
let sign_extend_byte_word  b = if Int32.compare b 0x80l < 0 then b else Int32.logor 0xff00l     b
let sign_extend_byte_dword b = if Int32.compare b 0x80l < 0 then b else Int32.logor 0xffffff00l b

(* Operand producers.  These are generally not pure functions: they rely upon
   the state (e.g. which register is specified by the mod R/M, what the address
   size is, etc).  Some encodings are pure, e.g. when the operand is explicitly
   "AX", and not "some 16-bit register, depending on encoding".  
   
   Basically, in the main table below, an "instruction" is described by a 
   mnemonic and a list of operand-producing functions.  When it comes time to
   make an actual instruction, we call List.map (fun f -> f ()) on the list of
   operand producers. *)
let oGb   () = opnd_reg8_of_int  !modrm_context.mreg
let oGd   () = opnd_reg32_of_int !modrm_context.mreg
let oGw   () = opnd_reg16_of_int !modrm_context.mreg
let oGv   () = opsize_predicate (lazy (oGw ())) (lazy (oGd ()))
let oGz   () = oGv () (* Revisit on 64-bit *)
let oGd_q () = oGd () (* Revisit on 64-bit *)
let oNq   () = modrm_mem_reg_predicate (lazy (invalid ())) (lazy opnd_mmxreg_of_int)
let oPpi  () = opnd_mmxreg_of_int !modrm_context.mreg
let oPq   () = oPpi ()
let oPd   () = oPpi () (* correct? *)
let oVdq  () = opnd_xmmreg_of_int !modrm_context.mreg
let oVss  () = oVdq ()
let oVsd  () = oVdq ()
let oVps  () = oVdq ()
let oVpd  () = oVdq ()
let oVq   () = oVdq ()
let oCd   () = ControlReg(control_reg_of_int !modrm_context.mreg)
let oDd   () = DebugReg(debug_reg_of_int !modrm_context.mreg)

let oMb   () = modrm_mem_reg_predicate (lazy (Memexpr(Mb (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMw   () = modrm_mem_reg_predicate (lazy (Memexpr(Mw (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMd   () = modrm_mem_reg_predicate (lazy (Memexpr(Md (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMf   () = modrm_mem_reg_predicate (lazy (Memexpr(Mf (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMq   () = modrm_mem_reg_predicate (lazy (Memexpr(Mq (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMt   () = modrm_mem_reg_predicate (lazy (Memexpr(Mt (make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMdq  () = modrm_mem_reg_predicate (lazy (Memexpr(Mdq(make_memexpr !modrm_context)))) (lazy (invalid ()))
let oMs   () = oMf  () (* fword ptr *)
let oMpd  () = oMdq ()
let oMps  () = oMdq ()
let oMd_q () = oMd  () (* Revisit on 64-bit *)
(* Points to a pointer that depends on the operand size (16:16 or 16:32) *)
let oMp   () = modrm_mem_reg_predicate (lazy (opsize_predicate (lazy (oMd ())) (lazy (oMf ())))) (lazy (invalid ()))
(* Points to two consecutive addresses, which are determined by the operand size *)
let oMa   () = modrm_mem_reg_predicate (lazy (opsize_predicate (lazy (oMd ())) (lazy (oMq ())))) (lazy (invalid ()))

let oEb   () = modrm_mem_reg_predicate (lazy (oMb ())) (lazy (opnd_reg8_of_int))
let oEd   () = modrm_mem_reg_predicate (lazy (oMd ())) (lazy (opnd_reg32_of_int))
let oEw   () = modrm_mem_reg_predicate (lazy (oMw ())) (lazy (opnd_reg16_of_int))
let oEv   () = opsize_predicate (lazy (oEw ())) (lazy (oEd ()))
let oEd_q () = oEd () (* Revisit on 64-bit *)

let oQpi  () = modrm_mem_reg_predicate (lazy (oMq ())) (lazy opnd_mmxreg_of_int)
let oQd   () = oQpi ()
let oQq   () = oQpi ()

let oWdq  () = modrm_mem_reg_predicate (lazy (oMdq ())) (lazy opnd_xmmreg_of_int)
let oWps  () = oWdq ()
let oWpd  () = oWdq ()
let oWq   () = oWdq ()
let oWss  () = modrm_mem_reg_predicate (lazy (oMd ())) (lazy opnd_xmmreg_of_int)
let oWsd  () = modrm_mem_reg_predicate (lazy (oMq ())) (lazy opnd_xmmreg_of_int)

let oUps  () = modrm_mem_reg_predicate (lazy (invalid ())) (lazy opnd_xmmreg_of_int)
let oUpd  () = oUps ()
let oUq   () = oUps ()
let oUdq  () = oUps ()

let oUdqMd () = modrm_mem_reg_predicate (lazy (oMd ())) (lazy opnd_xmmreg_of_int)
let oUdqMq () = modrm_mem_reg_predicate (lazy (oMq ())) (lazy opnd_xmmreg_of_int)
let oUdqMw () = modrm_mem_reg_predicate (lazy (oMw ())) (lazy opnd_xmmreg_of_int)

let o1  () = Immediate(Ib(0x1l))
let oIb () = Immediate(Ib(consume_byte ()))
let oIbAsOpndSize () = 
  let i = consume_byte () in 
  opsize_predicate 
    (lazy (Immediate(Iw(sign_extend_byte_word i)))) 
    (lazy (Immediate(Id(sign_extend_byte_dword i))))

let oId () = Immediate(Id(consume_dword ()))
let oIw () = Immediate(Iw(consume_word ()))
let oIz () = opsize_predicate (lazy (oIw ())) (lazy (oId ()))
let oIv () = oIz () (* Revisit on 64-bit *)

let oAL     () = GeneralReg(Gb(Al))
let oCL     () = GeneralReg(Gb(Cl))
let oALR8L  () = GeneralReg(Gb(Al))
let oCLR9L  () = GeneralReg(Gb(Cl))
let oDLR10L () = GeneralReg(Gb(Dl))
let oBLR11L () = GeneralReg(Gb(Bl))
let oAHR12L () = GeneralReg(Gb(Ah))
let oCHR13L () = GeneralReg(Gb(Ch))
let oDHR14L () = GeneralReg(Gb(Dh))
let oBHR15L () = GeneralReg(Gb(Bh))

let oAX  () = GeneralReg(Gw(Ax))
let oDX  () = GeneralReg(Gw(Dx))
let orAX () = size_predicated_reg 0
let oeAX () = orAX ()
let oeCX () = size_predicated_reg 1
let oeDX () = size_predicated_reg 2
let oeBX () = size_predicated_reg 3
let oeSP () = size_predicated_reg 4
let oeBP () = size_predicated_reg 5
let oeSI () = size_predicated_reg 6
let oeDI () = size_predicated_reg 7

let orAXr8  () = oeAX ()
let orCXr9  () = oeCX ()
let orDXr10 () = oeDX ()
let orBXr11 () = oeBX ()
let orSPr12 () = oeSP ()
let orBPr13 () = oeBP ()
let orSIr14 () = oeSI ()
let orDIr15 () = oeDI ()

let oES () = SegReg(ES)
let oCS () = SegReg(CS)
let oDS () = SegReg(DS)
let oSS () = SegReg(SS)
let oFS () = SegReg(FS)
let oGS () = SegReg(GS)
let oSw () = SegReg(segreg_of_int !modrm_context.mreg)
let oRw () = modrm_mem_reg_predicate (lazy (raise (InvalidInstruction))) (lazy (opnd_reg16_of_int))
let oRd () = modrm_mem_reg_predicate (lazy (raise (InvalidInstruction))) (lazy (opnd_reg32_of_int))
let oRv () = modrm_mem_reg_predicate (lazy (raise (InvalidInstruction))) (lazy (opsize_predicate (lazy opnd_reg16_of_int) (lazy opnd_reg32_of_int)))
let oRdMb () = modrm_mem_reg_predicate (lazy (oMb ())) (lazy (opnd_reg32_of_int))
let oRdMw () = modrm_mem_reg_predicate (lazy (oMw ())) (lazy (opnd_reg16_of_int))

let oJb () = 
  let x = consume_byte () in 
  JccTarget(Int32.add (sign_extend_byte_dword x) !current_ea, !current_ea)

let oJz () = 
  let x = 
    addrsize_predicate 
     (lazy (consume_word ())) 
     (lazy (consume_dword ())) 
  in JccTarget(Int32.add x !current_ea, !current_ea)

let oXb () = 
  let s = getseg () in
  addrsize_predicate 
    (lazy (Memexpr(Mb(Mem16(s,Some(Si) ,None,None)))))
    (lazy (Memexpr(Mb(Mem32(s,Some(Esi),None,None)))))
let oXv () = 
  let s = getseg () in
  addropsize_predicate 
    (lazy (Memexpr(Mw(Mem16(s,Some(Si) ,None,None)))))
    (lazy (Memexpr(Md(Mem16(s,Some(Si) ,None,None)))))
    (lazy (Memexpr(Mw(Mem32(s,Some(Esi),None,None)))))
    (lazy (Memexpr(Md(Mem32(s,Some(Esi),None,None)))))
let oXz () = oXv ()

let oYb () = 
  addrsize_predicate 
    (lazy (Memexpr(Mb(Mem16(ES,Some(Di) ,None,None)))))
    (lazy (Memexpr(Mb(Mem32(ES,Some(Edi),None,None)))))

let oYv () = 
  addropsize_predicate 
    (lazy (Memexpr(Mw(Mem16(ES,Some(Di) ,None,None)))))
    (lazy (Memexpr(Md(Mem16(ES,Some(Di) ,None,None)))))
    (lazy (Memexpr(Mw(Mem32(ES,Some(Edi),None,None)))))
    (lazy (Memexpr(Md(Mem32(ES,Some(Edi),None,None)))))
let oYz () = oYv ()
                       
let oMbBx  () = Memexpr(Mb(Mem16(getseg (),Some(Bx) ,None,None)))
let oMbEbx () = Memexpr(Mb(Mem32(getseg (),Some(Ebx),None,None)))

let oAp () = 
  opsize_predicate 
  (lazy (let offs = consume_word ()  in let seg  = consume_word () in FarTarget(Ap16(seg, offs)))) 
  (lazy (let offs = consume_dword () in let seg  = consume_word () in FarTarget(Ap32(seg, offs))))

let oOb () = 
  let s = getseg () in
  addrsize_predicate
    (lazy (Memexpr(Mb(Mem16(s,None,None,Some(consume_word  ()))))))
    (lazy (Memexpr(Mb(Mem32(s,None,None,Some(consume_dword ()))))))
let oOv () = 
  let s = getseg () in
  addropsize_predicate 
    (lazy (Memexpr(Mw(Mem16(s,None,None,Some(consume_word  ()))))))
    (lazy (Memexpr(Md(Mem16(s,None,None,Some(consume_word  ()))))))
    (lazy (Memexpr(Mw(Mem32(s,None,None,Some(consume_dword ()))))))
    (lazy (Memexpr(Md(Mem32(s,None,None,Some(consume_dword ()))))))

let oSt0 () = FPUReg(ST0)

let oSt_of_int = function
| 0 -> oSt0
| 1 -> fun () -> FPUReg(ST1)
| 2 -> fun () -> FPUReg(ST2)
| 3 -> fun () -> FPUReg(ST3)
| 4 -> fun () -> FPUReg(ST4)
| 5 -> fun () -> FPUReg(ST5)
| 6 -> fun () -> FPUReg(ST6)
| 7 -> fun () -> FPUReg(ST7)
| x -> failwith ("oSt_of_int "^string_of_int x)

let oReal4    () = oMd ()
let oReal8    () = oMq ()
let oReal10   () = oMt ()
let oFPEnvLow () = oMd () (* Wrong, clearly *)
let oFPEnv    () = oMd () (* Wrong, clearly *)
let oSimdState() = oMd () (* Wrong, clearly, but IDA disassembles it this way *)

(* These data types describe the entries of the table below. *)
type x86_operand_producer = unit -> x86operand
type x86_specifier = x86mnem * (x86_operand_producer list)
type pred_type = PRED_OpSize      of x86_instruction_producer * x86_instruction_producer (* Size_16, Size_32 *)
               | PRED_AddrSize    of x86_instruction_producer * x86_instruction_producer (* Size_16, Size_32 *)
               | PRED_ModRMMemReg of x86_instruction_producer * x86_instruction_producer (* is mem, is reg *)
               | PRED_AddrOpSize  of x86_instruction_producer * x86_instruction_producer * 
                                     x86_instruction_producer * x86_instruction_producer (* A16O16, A16O32, A32O16, A32O32 *)
and x86_instruction_producer =
| Fatal   (* Tried to eat a prefix byte that should have been consumed already *)
| Invalid (* Invalid encoding *)
| Unimplemented of string (* Unimplemented -- none such exist *)
| Direct of x86mnem * (x86_operand_producer list) (* Direct instruction *)
| Group of x86_instruction_producer array (* Group indexed by modrm.mreg *)
| Predicated of pred_type (* Outcome depends on prefixes and encoding *)
| FPUSt0StN of x86mnem (* StN determined by modrm.rm *)
| FPUStNSt0 of x86mnem (* StN determined by modrm.rm *)
| FPUStN    of x86mnem (* StN determined by modrm.rm *)
| LowFun of (int -> x86_specifier) (* Function on modrm.rm *)
| LowGroup of x86_instruction_producer array (* Group indexed by modrm.rm *)
| Sse of x86_instruction_producer * x86_instruction_producer * x86_instruction_producer * x86_instruction_producer 
     (* SSE instruction: no prefix, F3 prefix, 66 prefix, F2 prefix *)
| SseNo of x86_instruction_producer (* SSE instruction: no prefix *)
| Sse66 of x86_instruction_producer (* SSE instruction: 66 prefix *)
| SseNo66 of x86_instruction_producer * x86_instruction_producer (* SSE instruction: no prefix, 66 prefix *)

(* This is more or less a literal translation of Intel #2B, appendix A.3 *)
let x86_instruction_production_table = [|
Direct(Add,[oEb;oGb]); (* 0x00 *)
Direct(Add,[oEv;oGv]);
Direct(Add,[oGb;oEb]);
Direct(Add,[oGv;oEv]);
Direct(Add,[oAL;oIb]);
Direct(Add,[orAX;oIz]);
Direct(Push,[oES]);
Direct(Pop,[oES]);
Direct(Or,[oEb;oGb]);
Direct(Or,[oEv;oGv]);
Direct(Or,[oGb;oEb]);
Direct(Or,[oGv;oEv]);
Direct(Or,[oAL;oIb]);
Direct(Or,[orAX;oIz]);
Direct(Push,[oCS]);
Fatal;
Direct(Adc,[oEb;oGb]); (* 0x10 *)
Direct(Adc,[oEv;oGv]);
Direct(Adc,[oGb;oEb]);
Direct(Adc,[oGv;oEv]);
Direct(Adc,[oAL;oIb]);
Direct(Adc,[orAX;oIz]);
Direct(Push,[oSS]);
Direct(Pop,[oSS]);
Direct(Sbb,[oEb;oGb]);
Direct(Sbb,[oEv;oGv]);
Direct(Sbb,[oGb;oEb]);
Direct(Sbb,[oGv;oEv]);
Direct(Sbb,[oAL;oIb]);
Direct(Sbb,[orAX;oIz]);
Direct(Push,[oDS]);
Direct(Pop,[oDS]);
Direct(And,[oEb;oGb]); (* 0x20 *)
Direct(And,[oEv;oGv]);
Direct(And,[oGb;oEb]);
Direct(And,[oGv;oEv]);
Direct(And,[oAL;oIb]);
Direct(And,[orAX;oIz]);
Fatal;
Direct(Daa,[]);
Direct(Sub,[oEb;oGb]);
Direct(Sub,[oEv;oGv]);
Direct(Sub,[oGb;oEb]);
Direct(Sub,[oGv;oEv]);
Direct(Sub,[oAL;oIb]);
Direct(Sub,[orAX;oIz]);
Fatal;
Direct(Das,[]);
Direct(Xor,[oEb;oGb]); (* 0x30 *)
Direct(Xor,[oEv;oGv]);
Direct(Xor,[oGb;oEb]);
Direct(Xor,[oGv;oEv]);
Direct(Xor,[oAL;oIb]);
Direct(Xor,[orAX;oIz]);
Fatal;
Direct(Aaa,[]);
Direct(Cmp,[oEb;oGb]);
Direct(Cmp,[oEv;oGv]);
Direct(Cmp,[oGb;oEb]);
Direct(Cmp,[oGv;oEv]);
Direct(Cmp,[oAL;oIb]);
Direct(Cmp,[orAX;oIz]);
Fatal;
Direct(Aas,[]);
Direct(Inc,[oeAX]);    (* 0x40 *)
Direct(Inc,[oeCX]);
Direct(Inc,[oeDX]);
Direct(Inc,[oeBX]);
Direct(Inc,[oeSP]);
Direct(Inc,[oeBP]);
Direct(Inc,[oeSI]);
Direct(Inc,[oeDI]);
Direct(Dec,[oeAX]);
Direct(Dec,[oeCX]);
Direct(Dec,[oeDX]);
Direct(Dec,[oeBX]);
Direct(Dec,[oeSP]);
Direct(Dec,[oeBP]);
Direct(Dec,[oeSI]);
Direct(Dec,[oeDI]);
Direct(Push,[orAXr8]);    (* 0x50 *)
Direct(Push,[orCXr9]);
Direct(Push,[orDXr10]);
Direct(Push,[orBXr11]);
Direct(Push,[orSPr12]);
Direct(Push,[orBPr13]);
Direct(Push,[orSIr14]);
Direct(Push,[orDIr15]);
Direct(Pop,[orAXr8]);
Direct(Pop,[orCXr9]);
Direct(Pop,[orDXr10]);
Direct(Pop,[orBXr11]);
Direct(Pop,[orSPr12]);
Direct(Pop,[orBPr13]);
Direct(Pop,[orSIr14]);
Direct(Pop,[orDIr15]);
Predicated(PRED_OpSize(Direct(Pushaw,[]),Direct(Pushad,[]))); (* 0x60 *)
Predicated(PRED_OpSize(Direct(Popaw,[]), Direct(Popad,[])));
Direct(Bound,[oGv;oMa]);
Direct(Arpl,[oEw;oGw]);
Fatal;
Fatal;
Fatal;
Fatal;
Direct(Push,[oIz]);
Direct(Imul,[oGv;oEv;oIz]);
Direct(Push,[oIbAsOpndSize]);
Direct(Imul,[oGv;oEv;oIb]);
Direct(Insb,[oYb;oDX]);
Predicated(PRED_OpSize(Direct(Insw,[oYz;oDX]),Direct(Insd,[oYz;oDX]))); (* Insw /Insd, [oYz;oDX] -- Mnem changes, given a mode *)
Direct(Outsb,[oDX;oXb]);
Predicated(PRED_OpSize(Direct(Outsw,[oDX;oXz]),Direct(Outsd,[oDX;oXz]))); (* Outsw /Outsd, [oDX;oXz] -- Mnem changes, given a mode *)
Direct(Jo ,[oJb]); (* 0x70 *)
Direct(Jno,[oJb]);
Direct(Jb ,[oJb]);
Direct(Jae,[oJb]);
Direct(Jz ,[oJb]);
Direct(Jnz,[oJb]);
Direct(Jbe,[oJb]);
Direct(Ja ,[oJb]);
Direct(Js ,[oJb]);
Direct(Jns,[oJb]);
Direct(Jp ,[oJb]);
Direct(Jnp,[oJb]);
Direct(Jl ,[oJb]);
Direct(Jge,[oJb]);
Direct(Jle,[oJb]);
Direct(Jg ,[oJb]);
Group([|Direct(Add,[oEb;oIb]);Direct(Or,[oEb;oIb]);Direct(Adc,[oEb;oIb]);Direct(Sbb,[oEb;oIb]);Direct(And,[oEb;oIb]);Direct(Sub,[oEb;oIb]);Direct(Xor,[oEb;oIb]);Direct(Cmp,[oEb;oIb])|]); (* 0x80 *)
Group([|Direct(Add,[oEv;oIz]);Direct(Or,[oEv;oIz]);Direct(Adc,[oEv;oIz]);Direct(Sbb,[oEv;oIz]);Direct(And,[oEv;oIz]);Direct(Sub,[oEv;oIz]);Direct(Xor,[oEv;oIz]);Direct(Cmp,[oEv;oIz])|]);
Group([|Direct(Add,[oEb;oIb]);Direct(Or,[oEb;oIb]);Direct(Adc,[oEb;oIb]);Direct(Sbb,[oEb;oIb]);Direct(And,[oEb;oIb]);Direct(Sub,[oEb;oIb]);Direct(Xor,[oEb;oIb]);Direct(Cmp,[oEb;oIb])|]);
Group([|Direct(Add,[oEv;oIbAsOpndSize]);Direct(Or,[oEv;oIbAsOpndSize]);Direct(Adc,[oEv;oIbAsOpndSize]);Direct(Sbb,[oEv;oIbAsOpndSize]);Direct(And,[oEv;oIbAsOpndSize]);Direct(Sub,[oEv;oIbAsOpndSize]);Direct(Xor,[oEv;oIbAsOpndSize]);Direct(Cmp,[oEv;oIbAsOpndSize])|]);
Direct(Test,[oEb;oGb]);
Direct(Test,[oEv;oGv]);
Direct(Xchg,[oEb;oGb]);
Direct(Xchg,[oEv;oGv]);
Direct(Mov,[oEb;oGb]);
Direct(Mov,[oEv;oGv]);
Direct(Mov,[oGb;oEb]);
Direct(Mov,[oGv;oEv]);
Direct(Mov,[oEv;oSw]);
Predicated(PRED_ModRMMemReg(Predicated(PRED_AddrOpSize(Direct(Lea,[oGw;oMw]),Direct(Lea,[oGd;oMw]),Direct(Lea,[oGw;oMd]),Direct(Lea,[oGd;oMd]))),Invalid));
Direct(Mov,[oSw;oEw]);
Group([|Direct(Pop,[oEv]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Sse(Direct(Nop,[]),Direct(Pause,[]),Direct(Nop,[]),Direct(Nop,[])); (* 0x90 *)
Direct(Xchg,[orAX;orCXr9]);
Direct(Xchg,[orAX;orDXr10]);
Direct(Xchg,[orAX;orBXr11]);
Direct(Xchg,[orAX;orSPr12]);
Direct(Xchg,[orAX;orBPr13]);
Direct(Xchg,[orAX;orSIr14]);
Direct(Xchg,[orAX;orDIr15]);
Predicated(PRED_OpSize(Direct(Cbw,[]),Direct(Cwde,[]))); (* CBW/CWDE/CDQE *)
Predicated(PRED_OpSize(Direct(Cwd,[]),Direct(Cdq,[])));  (* CWD/CDQ/CQO *)
Direct(CallF,[oAp]);
Direct(Wait,[]);
Predicated(PRED_OpSize(Direct(Pushfw,[]),Direct(Pushfd,[]))); (* PUSHF/D/Q *)
Predicated(PRED_OpSize(Direct(Popfw,[]), Direct(Popfd,[]))); (* POPF/D/Q *)
Direct(Sahf,[]);
Direct(Lahf,[]);
Direct(Mov,[oAL;oOb]);  (* 0xA0 *)
Direct(Mov,[orAX;oOv]);
Direct(Mov,[oOb;oAL]);
Direct(Mov,[oOv;orAX]);
Direct(Movsb,[oXb;oYb]);
Predicated(PRED_OpSize(Direct(Movsw,[oXv;oYv]),Direct(Movsd,[oXv;oYv]))); (* MOVS/W/D/Q *)
Direct(Cmpsb,[oXb;oYb]);
Predicated(PRED_OpSize(Direct(Cmpsw,[oXv;oYv]),Direct(Cmpsd,[oXv;oYv]))); (* CMPS/W/D/Q *)
Direct(Test,[oAL;oIb]);
Direct(Test,[orAX;oIz]);
Direct(Stosb,[oYb]);
Predicated(PRED_OpSize(Direct(Stosw,[oYv]),Direct(Stosd,[oYv]))); (* STOS/W/D/Q *)
Direct(Lodsb,[oXb]);
Predicated(PRED_OpSize(Direct(Lodsw,[oXv]),Direct(Lodsd,[oXv]))); (* LODS/W/D/Q *)
Direct(Scasb,[oYb]);
Predicated(PRED_OpSize(Direct(Scasw,[oYv]),Direct(Scasd,[oYv]))); (* SCAS/W/D/Q *)
Direct(Mov,[oALR8L;oIb]); (* 0xB0 *)
Direct(Mov,[oCLR9L;oIb]);
Direct(Mov,[oDLR10L;oIb]);
Direct(Mov,[oBLR11L;oIb]);
Direct(Mov,[oAHR12L;oIb]);
Direct(Mov,[oCHR13L;oIb]);
Direct(Mov,[oDHR14L;oIb]);
Direct(Mov,[oBHR15L;oIb]);
Direct(Mov,[orAXr8;oIv]);
Direct(Mov,[orCXr9;oIv]);
Direct(Mov,[orDXr10;oIv]);
Direct(Mov,[orBXr11;oIv]);
Direct(Mov,[orSPr12;oIv]);
Direct(Mov,[orBPr13;oIv]);
Direct(Mov,[orSIr14;oIv]);
Direct(Mov,[orDIr15;oIv]);
Group([|Direct(Rol,[oEb;oIb]);Direct(Ror,[oEb;oIb]);Direct(Rcl,[oEb;oIb]);Direct(Rcr,[oEb;oIb]);Direct(Shl,[oEb;oIb]);Direct(Shr,[oEb;oIb]);Direct(Sal,[oEb;oIb]);Direct(Sar,[oEb;oIb])|]); (* 0xC0 *)
Group([|Direct(Rol,[oEv;oIb]);Direct(Ror,[oEv;oIb]);Direct(Rcl,[oEv;oIb]);Direct(Rcr,[oEv;oIb]);Direct(Shl,[oEv;oIb]);Direct(Shr,[oEv;oIb]);Direct(Sal,[oEv;oIb]);Direct(Sar,[oEv;oIb])|]);
Direct(Ret,[oIw]);
Direct(Ret,[]);
Direct(Les,[oGz;oMp]);
Direct(Lds,[oGz;oMp]);
Group([|Direct(Mov,[oEb;oIb]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Group([|Direct(Mov,[oEv;oIz]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Direct(Enter,[oIw;oIb]);
Direct(Leave,[]);
Direct(Retf,[oIw]);
Direct(Retf,[]);
Direct(Int3,[]);
Direct(Int,[oIb]);
Direct(Into,[]);
Predicated(PRED_OpSize(Direct(Iretw,[]),Direct(Iretd,[]))); (* Revisit for 64-bit *)
Group([|Direct(Rol,[oEb;o1 ]);Direct(Ror,[oEb;o1 ]);Direct(Rcl,[oEb;o1 ]);Direct(Rcr,[oEb;o1 ]);Direct(Shl,[oEb;o1 ]);Direct(Shr,[oEb;o1 ]);Direct(Sal,[oEb;o1 ]);Direct(Sar,[oEb;o1 ])|]); (* 0xD0 *)
Group([|Direct(Rol,[oEv;o1 ]);Direct(Ror,[oEv;o1 ]);Direct(Rcl,[oEv;o1 ]);Direct(Rcr,[oEv;o1 ]);Direct(Shl,[oEv;o1 ]);Direct(Shr,[oEv;o1 ]);Direct(Sal,[oEv;o1 ]);Direct(Sar,[oEv;o1 ])|]);
Group([|Direct(Rol,[oEb;oCL]);Direct(Ror,[oEb;oCL]);Direct(Rcl,[oEb;oCL]);Direct(Rcr,[oEb;oCL]);Direct(Shl,[oEb;oCL]);Direct(Shr,[oEb;oCL]);Direct(Sal,[oEb;oCL]);Direct(Sar,[oEb;oCL])|]);
Group([|Direct(Rol,[oEv;oCL]);Direct(Ror,[oEv;oCL]);Direct(Rcl,[oEv;oCL]);Direct(Rcr,[oEv;oCL]);Direct(Shl,[oEv;oCL]);Direct(Shr,[oEv;oCL]);Direct(Sal,[oEv;oCL]);Direct(Sar,[oEv;oCL])|]);
Direct(Aam,[oIb]);
Direct(Aad,[oIb]);
Direct(Salc,[]);
Predicated(PRED_AddrSize(Direct(Xlat,[oMbBx]),Direct(Xlat,[oMbEbx])));

(*
   High-level viewpoint on FPU encoding:
   * All FPU encodings are broken down into two cases:  those where the Mod/RM 
     specifies a:
   * * Memory location
   * * Register
   * When there's a memory location, the register field of the mod/rm specifies
     an instruction in a sparsely-populated (but normally completely full) array,
     which takes at least one of its arguments as specified by the memory 
     expression dictated by the rest of the mod/rm encoding.
   * When there's a register, the Intel manuals divide the outcome up into eight 
     octants, specified by the register field of the mod/rm byte.  These octants 
     are either monolithic blocks of instructions that operate on the FPU 
     registers (where the eight ST(N) registers are selected by the low three bits 
     of the mod/rm byte), or are sparsely-populated ranges of instructions that
     take no operands.
*)
(* 0xD8 *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fadd, [oSt0;oReal4]);
    Direct(Fmul, [oSt0;oReal4]);
    Direct(Fcom, [oSt0;oReal4]);
    Direct(Fcomp,[oSt0;oReal4]);
    Direct(Fsub, [oSt0;oReal4]);
    Direct(Fsubr,[oSt0;oReal4]);
    Direct(Fdiv, [oSt0;oReal4]);
    Direct(Fdivr,[oSt0;oReal4])|]),
  Group([|
    FPUSt0StN(Fadd);
    FPUSt0StN(Fmul);
    FPUSt0StN(Fcom);
    FPUSt0StN(Fcomp);
    FPUSt0StN(Fsub);
    FPUSt0StN(Fsubr);
    FPUSt0StN(Fdiv);
    FPUSt0StN(Fdivr)|])));
    
(* 0xD9 *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fld,   [oReal4]);
    Invalid;
    Direct(Fst,   [oReal4]);
    Direct(Fstp,  [oReal4]);
    Direct(Fldenv,[oFPEnvLow]);
    Direct(Fldcw, [oMw]);
    Direct(Fstenv,[oFPEnvLow]);
    Direct(Fstcw, [oMw])|]),
  Group([|
    FPUSt0StN(Fld);
    FPUSt0StN(Fxch);
    LowGroup(Direct(Fnop,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid);
    Invalid;
    LowGroup(Direct(Fchs,[]);Direct(Fabs,[]);Invalid;Invalid;Direct(Ftst,[]);Direct(Fxam,[]);Invalid;Invalid);
    LowGroup([|
      Direct(Fld1,[]);
      Direct(Fldl2t,[]);
      Direct(Fldl2e,[]);
      Direct(Fldpi,[]);
      Direct(Fldlg2,[]);
      Direct(Fldln2,[]);
      Direct(Fldz,[]);
      Invalid;|]);
    LowGroup([|
      Direct(F2xm1,[]);
      Direct(Fyl2x,[]);
      Direct(Fptan,[]);
      Direct(Fpatan,[]);
      Direct(Fxtract,[]);
      Direct(Fprem1,[]);
      Direct(Fdecstp,[]);
      Direct(Fincstp,[]);|]);
    LowGroup([|
      Direct(Fprem,[]);
      Direct(Fyl2xp1,[]);
      Direct(Fsqrt,[]);
      Direct(Fsincos,[]);
      Direct(Frndint,[]);
      Direct(Fscale,[]);
      Direct(Fsin,[]);
      Direct(Fcos,[]);|]);|])));

(* 0xDA *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fiadd, [oMd]);
    Direct(Fimul, [oMd]);
    Direct(Ficom, [oMd]);
    Direct(Ficomp,[oMd]);
    Direct(Fisub, [oMd]);
    Direct(Fisubr,[oMd]);
    Direct(Fidiv, [oMd]);
    Direct(Fidivr,[oMd]);|]),
  Group([|
    FPUSt0StN(Fcmovb );
    FPUSt0StN(Fcmove );
    FPUSt0StN(Fcmovbe);
    FPUSt0StN(Fcmovu );
    Invalid;
    LowGroup(Invalid;Direct(Fucompp,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid);
    Invalid;
    Invalid;|])));

(* 0xDB *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fild,  [oMd]);
    Direct(Fisttp,[oMd]);
    Direct(Fist,  [oMd]);
    Direct(Fistp, [oMd]);
    Invalid;
    Direct(Fld,   [oReal10]);
    Invalid;
    Direct(Fstp,  [oReal10])|]),
  Group([|
    FPUSt0StN(Fcmovnb);
    FPUSt0StN(Fcmovne);
    FPUSt0StN(Fcmovnbe);
    FPUSt0StN(Fcmovnu);
    LowGroup(Invalid;Invalid;Direct(Fclex,[]);Direct(Finit,[]);Invalid;Invalid;Invalid;Invalid);
    FPUSt0StN(Fucomi);
    FPUSt0StN(Fcomi);
    Invalid;
    |])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fadd, [oSt0;oReal8]);
    Direct(Fmul, [oSt0;oReal8]);
    Direct(Fcom, [oSt0;oReal8]);
    Direct(Fcomp,[oSt0;oReal8]);
    Direct(Fsub, [oSt0;oReal8]);
    Direct(Fsubr,[oSt0;oReal8]);
    Direct(Fdiv, [oSt0;oReal8]);
    Direct(Fdivr,[oSt0;oReal8])|]),
  Group([|
    FPUStNSt0(Fadd);
    FPUStNSt0(Fmul);
    Invalid;
    Invalid;
    FPUStNSt0(Fsub);
    FPUStNSt0(Fsubr);
    FPUStNSt0(Fdiv);
    FPUStNSt0(Fdivr)|])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fld,   [oReal8]);
    Direct(Fisttp,[oMq]);
    Direct(Fst,   [oReal8]);
    Direct(Fstp,  [oReal8]);
    Direct(Frstor,[oFPEnv]);
    Invalid;
    Direct(Fsave ,[oFPEnv]);
    Direct(Fstsw, [oMw])|]),
  Group([|
    FPUStN(Ffree);
    Invalid;
    FPUStN(Fst);
    FPUStN(Fstp);
    FPUStN(Fucom);
    FPUStN(Fucomp);
    Invalid;
    Invalid;|])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fiadd, [oMw]);
    Direct(Fimul, [oMw]);
    Direct(Ficom, [oMw]);
    Direct(Ficomp,[oMw]);
    Direct(Fisub, [oMw]);
    Direct(Fisubr,[oMw]);
    Direct(Fidiv, [oMw]);
    Direct(Fidivr,[oMw])|]),
  Group([|
    FPUStNSt0(Faddp);
    FPUStNSt0(Fmulp);
    Invalid;
    LowGroup(Invalid;Direct(Fcompp,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid);
    FPUStNSt0(Fsubrp);
    FPUStNSt0(Fsubp);
    FPUStNSt0(Fdivrp);
    FPUStNSt0(Fdivp)|])));
    
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fild,  [oMw]);
    Direct(Fisttp,[oMw]);
    Direct(Fist,  [oMw]);
    Direct(Fistp, [oMw]);
    Direct(Fbld,  [oReal10]);
    Direct(Fild,  [oReal8]);
    Direct(Fbstp, [oReal10]);
    Direct(Fistp, [oReal8])|]),
  Group([|
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    LowGroup(Direct(Fstsw,[oAX]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid);
    FPUSt0StN(Fucomip);
    FPUSt0StN(Fcomip);
    Invalid|])));

Direct(Loopnz,[oJb]); (* 0xE0 *)
Direct(Loopz,[oJb]);
Direct(Loop,[oJb]);
Predicated(PRED_AddrSize(Direct(Jcxz,[oJb]),Direct(Jecxz,[oJb])));
Direct(In,[oAL;oIb]);
Direct(In,[oeAX;oIb]);
Direct(Out,[oIb;oAL]);
Direct(Out,[oIb;oeAX]);
Direct(Call,[oJz]);
Direct(Jmp,[oJz]);
Direct(JmpF,[oAp]);
Direct(Jmp,[oJb]);
Direct(In,[oAL;oDX]);
Direct(In,[oeAX;oDX]);
Direct(Out,[oDX;oAL]);
Direct(Out,[oDX;oeAX]);
Fatal; (* 0xF0 *)
Direct(Icebp,[]);
Fatal;
Fatal;
Direct(Hlt,[]);
Direct(Cmc,[]);
Group([|Direct(Test,[oEb;oIb]);Invalid;Direct(Not,[oEb]);Direct(Neg,[oEb]);Direct(Mul,[oEb]);Direct(Imul,[oEb]);Direct(Div,[oEb]);Direct(Idiv,[oEb])|]);
Group([|Direct(Test,[oEv;oIz]);Invalid;Direct(Not,[oEv]);Direct(Neg,[oEv]);Direct(Mul,[oEv]);Direct(Imul,[oEv]);Direct(Div,[oEv]);Direct(Idiv,[oEv])|]);
Direct(Clc,[]);
Direct(Stc,[]);
Direct(Cli,[]);
Direct(Sti,[]);
Direct(Cld,[]);
Direct(Std,[]);
Group([|Direct(Inc,[oEb]);Direct(Dec,[oEb]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;|]);
Group([|Direct(Inc,[oEv]);Direct(Dec,[oEv]);Direct(Call,[oEv]);Direct(CallF,[oEv]);Direct(Jmp,[oEv]);Direct(JmpF,[oEv]);Direct(Push,[oEv]);Invalid;|]);

Group([| (* 0x100 *)
  Predicated(PRED_ModRMMemReg(Direct(Sldt,[oMw]),Direct(Sldt,[oRv])));
  Predicated(PRED_ModRMMemReg(Direct(Str ,[oMw]),Direct(Str ,[oRv])));
  Direct(Lldt,[oEw]);
  Direct(Ltr ,[oEw]);
  Direct(Verr,[oEw]);
  Direct(Verw,[oEw]);
  Invalid;
  Invalid;|]);

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Sgdt,[oMs]);
    Direct(Sidt,[oMs]);
    Direct(Lgdt,[oMs]);
    Direct(Lidt,[oMs]);
    Direct(Smsw,[oMw]);
    Invalid;
    Direct(Lmsw,[oMw]);
    Direct(Invlpg,[oMb]);|]),
  Group([|
    LowGroup([|
      Invalid;
      Direct(Vmcall,[]);
      Direct(Vmlaunch,[]);
      Direct(Vmresume,[]);
      Direct(Vmxoff,[]);
      Invalid;
      Invalid;
      Invalid;|]);
    LowGroup(Direct(Monitor,[]);Direct(Mwait,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid);
    Invalid;
    Invalid;
    Direct(Smsw,[oRv]);
    Invalid;
    Direct(Lmsw,[oRw]);
    Invalid; (* Revisit for 64-bit (swapgs) *)
    |])));

Direct(Lar,[oGv;oEw]);
Direct(Lsl,[oGv;oEw]);
Invalid;
Direct(Syscall,[]);
Direct(Clts,[]);
Direct(Sysret,[]);
Direct(Invd,[]);
Direct(Wbinvd,[]);
Invalid;
Direct(Ud2,[]);
Invalid;
Direct(Nop,[oEv]);
Invalid;
Invalid;
Sse(Direct(Movups,[oVps;oWps]),Direct(Movss, [oVss;oWss]),Direct(Movupd,[oVpd;oWpd]),Direct(Movsd, [oVsd;oWsd])); (* 0x110 *)
Sse(Direct(Movups,[oWps;oVps]),Direct(Movss, [oWss;oVss]),Direct(Movupd,[oWpd;oVpd]),Direct(Movsd, [oWsd;oVsd]));
Sse(Predicated(PRED_ModRMMemReg(Direct(Movlps,[oVq;oMq]),Direct(Movhlps,[oVq;oUq]))),Direct(Movsldup,[oVq;oWq]),Direct(Movlpd,[oVq;oMq]),Direct(Movddup,[oVq;oWq]));
SseNo66(Direct(Movlps,[oMq;oVq]),Direct(Movlpd,[oMq;oVq]));
SseNo66(Direct(Unpcklpd,[oVpd;oWq]),Direct(Unpcklps,[oVps;oWq]));
SseNo66(Direct(Unpckhpd,[oVpd;oWq]),Direct(Unpckhps,[oVps;oWq]));
Sse(Predicated(PRED_ModRMMemReg(Direct(Movhps,[oVq;oMq]),Direct(Movlhps,[oVq;oUq]))),Direct(Movshdup,[oVq;oWq]),Direct(Movhpd,[oVq;oMq]),Invalid);
SseNo66(Direct(Movhps,[oMq;oVq]),Direct(Movhpd,[oMq;oVq]));
Predicated(PRED_ModRMMemReg(Group[|Direct(Prefetchnta,[oMb]);Direct(Prefetcht0,[oMb]);Direct(Prefetcht1,[oMb]);Direct(Prefetcht2,[oMb]);Invalid;Invalid;Invalid;Invalid|],Invalid));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Direct(Nop,[oEv]);
Direct(Mov,[oRd;oCd]); (* 0x120 *)
Direct(Mov,[oRd;oDd]);
Direct(Mov,[oCd;oRd]);
Direct(Mov,[oDd;oRd]);
Invalid;
Invalid;
Invalid;
Invalid;
SseNo66(Direct(Movaps,[oVpd;oWpd]),Direct(Movapd,[oVps;oWps]));
SseNo66(Direct(Movaps,[oWpd;oVpd]),Direct(Movapd,[oWps;oVps]));
Sse(Direct(Cvtpi2ps,[oVps;oQpi]),Direct(Cvtsi2ss,[oVss;oEd_q]),Direct(Cvtpi2pd,[oVpd;oQpi]),Direct(Cvtsi2sd,[oVsd;oEd_q]));
SseNo66(Direct(Movntps,[oMpd;oVpd]),Direct(Movntpd,[oMps;oVps]));
Sse(Direct(Cvttps2pi,[oPpi;oWps]),Direct(Cvttss2si,[oGd;oWss]),Direct(Cvttpd2pi,[oPpi;oWpd]),Direct(Cvttsd2si,[oGd;oWsd]));
Sse(Direct(Cvtps2pi,[oPpi;oWps]),Direct(Cvtss2si,[oGd_q;oWss]),Direct(Cvtpd2pi,[oPpi;oWpd]),Direct(Cvtsd2si,[oGd_q;oWsd]));
SseNo66(Direct(Ucomiss,[oVsd;oWsd]),Direct(Ucomisd,[oVss;oWss]));
SseNo66(Direct(Comiss, [oVsd;oWsd]),Direct(Comisd, [oVss;oWss]));
Direct(Wrmsr,[]); (* 0x130 *) 
Direct(Rdtsc,[]);
Direct(Rdmsr,[]);
Direct(Rdpmc,[]);
Direct(Sysenter,[]);
Direct(Sysexit,[]);
Invalid;
Direct(Getsec,[]);
Fatal; 
Invalid;
Fatal;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Direct(Cmovo ,[oGv;oEv]); (* 0x140 *)
Direct(Cmovno,[oGv;oEv]);
Direct(Cmovb ,[oGv;oEv]);
Direct(Cmovae,[oGv;oEv]);
Direct(Cmovz ,[oGv;oEv]);
Direct(Cmovnz,[oGv;oEv]);
Direct(Cmovbe,[oGv;oEv]);
Direct(Cmova ,[oGv;oEv]);
Direct(Cmovs ,[oGv;oEv]);
Direct(Cmovns,[oGv;oEv]);
Direct(Cmovp ,[oGv;oEv]);
Direct(Cmovnp,[oGv;oEv]);
Direct(Cmovl ,[oGv;oEv]);
Direct(Cmovge,[oGv;oEv]);
Direct(Cmovle,[oGv;oEv]);
Direct(Cmovg ,[oGv;oEv]);
SseNo66(Direct(Movmskps,[oGd;oUps]),Direct(Movmskpd,[oGd;oUpd])); (* 0x150 *)
Sse(Direct(Sqrtps,    [oVps;oWps]),Direct(Sqrtss,   [oVss;oWss]),Direct(Sqrtpd,    [oVpd;oWpd]),Direct(Sqrtsd,  [oVsd;oWsd]));
Sse(Direct(Rsqrtps,   [oVps;oWps]),Direct(Rsqrtss,  [oVss;oWss]),Invalid,                       Invalid);
Sse(Direct(Rcpps,     [oVps;oWps]),Direct(Rcpss,    [oVss;oWss]),Invalid,                       Invalid);
SseNo66(Direct(Andps, [oVps;oWps]),Direct(Andpd,    [oVpd;oWpd]));
SseNo66(Direct(Andnps,[oVps;oWps]),Direct(Andnpd,   [oVpd;oWpd]));
SseNo66(Direct(Orps,  [oVps;oWps]),Direct(Orpd,     [oVpd;oWpd]));
SseNo66(Direct(Xorps, [oVps;oWps]),Direct(Xorpd,    [oVpd;oWpd]));
Sse(Direct(Addps,     [oVps;oWps]),Direct(Addss,    [oVss;oWss]),Direct(Addpd,     [oVpd;oWpd]),Direct(Addsd,   [oVsd;oWsd]));
Sse(Direct(Mulps,     [oVps;oWps]),Direct(Mulss,    [oVss;oWss]),Direct(Mulpd,     [oVpd;oWpd]),Direct(Mulsd,   [oVsd;oWsd]));
Sse(Direct(Cvtps2pd,  [oVpd;oWps]),Direct(Cvtss2sd, [oVss;oWss]),Direct(Cvtpd2ps,  [oVps;oWpd]),Direct(Cvtsd2ss,[oVsd;oWsd]));
Sse(Direct(Cvtdq2ps,  [oVps;oWps]),Direct(Cvttps2dq,[oVdq;oWps]),Direct(Cvtps2dq,  [oVdq;oWps]),Invalid);
Sse(Direct(Subps,     [oVps;oWps]),Direct(Subss,    [oVss;oWss]),Direct(Subpd,     [oVpd;oWpd]),Direct(Subsd,   [oVsd;oWsd]));
Sse(Direct(Minps,     [oVps;oWps]),Direct(Minss,    [oVss;oWss]),Direct(Minpd,     [oVpd;oWpd]),Direct(Minsd,   [oVsd;oWsd]));
Sse(Direct(Divps,     [oVps;oWps]),Direct(Divss,    [oVss;oWss]),Direct(Divpd,     [oVpd;oWpd]),Direct(Divsd,   [oVsd;oWsd]));
Sse(Direct(Maxps,     [oVps;oWps]),Direct(Maxss,    [oVss;oWss]),Direct(Maxpd,     [oVpd;oWpd]),Direct(Maxsd,   [oVsd;oWsd]));
SseNo66(Direct(Punpcklbw,[oPq;oQd]),Direct(Punpcklbw,[oVdq;oWdq])); (* 0x160 *)
SseNo66(Direct(Punpcklwd,[oPq;oQd]),Direct(Punpcklwd,[oVdq;oWdq]));
SseNo66(Direct(Punpckldq,[oPq;oQd]),Direct(Punpckldq,[oVdq;oWdq]));
SseNo66(Direct(Packsswb, [oPq;oQd]),Direct(Packsswb, [oVdq;oWdq]));
SseNo66(Direct(Pcmpgtb,  [oPq;oQd]),Direct(Pcmpgtb,  [oVdq;oWdq]));
SseNo66(Direct(Pcmpgtw,  [oPq;oQd]),Direct(Pcmpgtw,  [oVdq;oWdq]));
SseNo66(Direct(Pcmpgtd,  [oPq;oQd]),Direct(Pcmpgtd,  [oVdq;oWdq]));
SseNo66(Direct(Packuswb, [oPq;oQd]),Direct(Packuswb, [oVdq;oWdq]));
SseNo66(Direct(Punpckhbw,[oPq;oQd]),Direct(Punpckhbw,[oVdq;oWdq]));
SseNo66(Direct(Punpckhwd,[oPq;oQd]),Direct(Punpckhwd,[oVdq;oWdq]));
SseNo66(Direct(Punpckhdq,[oPq;oQd]),Direct(Punpckhdq,[oVdq;oWdq]));
SseNo66(Direct(Packssdw, [oPq;oQd]),Direct(Packssdw, [oVdq;oWdq]));
Sse66(Direct(Punpcklqdq,[oVdq;oWdq]));
Sse66(Direct(Punpckhqdq,[oVdq;oWdq]));
SseNo66(Direct(Movd,[oPd;oEd_q]),Direct(Movd,[oVdq;oEd_q]));  (* Revisit on 64-bit *)
Sse(Direct(Movq,     [oPq ;oQq ]),Direct(Movdqu,   [oVdq;oWdq]),Direct(Movdqa,    [oVdq;oWdq]),Invalid);
Sse(Direct(Pshufw,   [oPq;oQq;oIb]),Direct(Pshufhw,[oVdq;oWdq;oIb]),Direct(Pshufd,[oVdq;oWdq;oIb]),Direct(Pshuflw,[oVdq;oWdq;oIb]));(* 0x170 *)

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrlw,[oNq;oIb]),Direct(Psrlw,[oUdq;oIb]));
    Invalid;
    SseNo66(Direct(Psraw,[oNq;oIb]),Direct(Psraw,[oUdq;oIb]));
    Invalid;
    SseNo66(Direct(Psllw,[oNq;oIb]),Direct(Psllw,[oUdq;oIb]));
    Invalid|])));

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrld,[oNq;oIb]),Direct(Psrld,[oUdq;oIb]));
    Invalid;
    SseNo66(Direct(Psrad,[oNq;oIb]),Direct(Psrad,[oUdq;oIb]));
    Invalid;
    SseNo66(Direct(Pslld,[oNq;oIb]),Direct(Pslld,[oUdq;oIb]));
    Invalid|])));

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrlq,[oNq;oIb]),Direct(Psrlq,[oUdq;oIb]));
    Sse66(Direct(Psrldq,[oUdq;oIb]));
    Invalid;
    Invalid;
    SseNo66(Direct(Psllq,[oNq;oIb]),Direct(Psllq,[oUdq;oIb]));
    Sse66(Direct(Pslldq,[oUdq;oIb]));|])));

SseNo66(Direct(Pcmpeqb,[oPq;oQq]),Direct(Pcmpeqb,[oVdq;oWdq]));
SseNo66(Direct(Pcmpeqw,[oPq;oQq]),Direct(Pcmpeqw,[oVdq;oWdq]));
SseNo66(Direct(Pcmpeqd,[oPq;oQq]),Direct(Pcmpeqd,[oVdq;oWdq]));
Direct(Emms,[]);
Direct(Vmread,[oEd;oGd]);  (* revisit for 64-bit *)
Direct(Vmwrite,[oEd;oGd]); (* revisit for 64-bit *)
Invalid;
Invalid;
Sse(Invalid,Invalid,Direct(Haddpd,[oVpd;oWpd]),Direct(Haddps,[oVps;oWps]));
Sse(Invalid,Invalid,Direct(Hsubpd,[oVpd;oWpd]),Direct(Hsubps,[oVps;oWps]));
Sse(Direct(Movd,[oEd_q;oPd]),Direct(Movq,[oVq;oWq]),Direct(Movd,[oEd_q;oVdq]),Invalid); (* Revisit on 64-bit *)
Sse(Direct(Movq,[oQq;oPq]),Direct(Movdqu,[oWdq;oVdq]),Direct(Movdqa,[oWdq;oVdq]),Invalid);
Direct(Jo ,[oJz]); (* 0x180 *)
Direct(Jno,[oJz]);
Direct(Jb ,[oJz]);
Direct(Jae,[oJz]);
Direct(Jz ,[oJz]);
Direct(Jnz,[oJz]);
Direct(Jbe,[oJz]);
Direct(Ja ,[oJz]);
Direct(Js ,[oJz]);
Direct(Jns,[oJz]);
Direct(Jp ,[oJz]);
Direct(Jnp,[oJz]);
Direct(Jl ,[oJz]);
Direct(Jge,[oJz]);
Direct(Jle,[oJz]);
Direct(Jg ,[oJz]);
Direct(Seto ,[oEb]); (* 0x190 *)
Direct(Setno,[oEb]);
Direct(Setb ,[oEb]);
Direct(Setae,[oEb]);
Direct(Setz ,[oEb]);
Direct(Setnz,[oEb]);
Direct(Setbe,[oEb]);
Direct(Seta ,[oEb]);
Direct(Sets ,[oEb]);
Direct(Setns,[oEb]);
Direct(Setp ,[oEb]);
Direct(Setnp,[oEb]);
Direct(Setl ,[oEb]);
Direct(Setge,[oEb]);
Direct(Setle,[oEb]);
Direct(Setg ,[oEb]);
Direct(Push,[oFS]); (* 0x1A0 *)
Direct(Pop,[oFS]);
Direct(Cpuid,[]);
Direct(Bt,[oEv;oGv]);
Direct(Shld,[oEv;oGv;oIb]);
Direct(Shld,[oEv;oGv;oCL]);
Invalid;
Invalid;
Direct(Push,[oGS]);
Direct(Pop,[oGS]);
Direct(Rsm,[]);
Direct(Bts,[oEv;oGv]);
Direct(Shrd,[oEv;oGv;oIb]);
Direct(Shrd,[oEv;oGv;oCL]);
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fxsave,[oSimdState]);
    Direct(Fxrstor,[oSimdState]);
    Direct(Ldmxcsr,[oMd]);
    Direct(Stmxcsr,[oMd]);
    Invalid;
    Invalid;
    Invalid;
    Direct(Clflush,[oMb])|]),
  Group([|
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Direct(Lfence,[]);
    Direct(Mfence,[]);
    Direct(Sfence,[]);|])
  ));
Direct(Imul,[oGv;oEv]);
Direct(Cmpxchg,[oEb;oGb]); (* 0x1B0 *)
Direct(Cmpxchg,[oEv;oGv]);
Direct(Lss,[oGv;oMp]);
Direct(Btr,[oEv;oGv]);
Direct(Lfs,[oGv;oMp]);
Direct(Lgs,[oGv;oMp]);
Direct(Movzx,[oGv;oEb]);
Direct(Movzx,[oGv;oEw]);
Direct(Popcnt,[oGv;oEv]);
Invalid; (* Group 10, all invalid *)
Group([|Invalid;Invalid;Invalid;Invalid;Direct(Bt ,[oEv;oIb]);Direct(Bts,[oEv;oIb]);Direct(Btr,[oEv;oIb]);Direct(Btc,[oEv;oIb])|]);
Direct(Btc,[oEv;oGv]);
Direct(Bsf,[oGv;oEv]);
Direct(Bsr,[oGv;oEv]);
Direct(Movsx,[oGv;oEb]);
Direct(Movsx,[oGv;oEw]);
Direct(Xadd,[oEb;oGb]);  (* 0x1C0 *)
Direct(Xadd,[oEv;oGv]);
Sse(Direct(Cmpps,[oVps;oWps;oIb]),Direct(Cmpss,[oVss;oWss;oIb]),Direct(Cmppd,[oVpd;oWpd;oIb]),Direct(Cmpsd,[oVsd;oWsd;oIb]));
SseNo(Direct(Movnti,[oMd_q;oGd_q]));
SseNo66(Direct(Pinsrw,[oPq;oEw;oIb]),  Direct(Pinsrw,[oVdq;oEw;oIb]));
SseNo66(Direct(Pextrw,[oGd;oNq;oIb]),  Direct(Pextrw,[oGd;oUdq;oIb]));
SseNo66(Direct(Shufps,[oVps;oWps;oIb]),Direct(Shufpd,[oVps;oWps;oIb]));
Predicated(PRED_ModRMMemReg(
  Group([|
    Invalid;
    Direct(Cmpxchg8b,[oMq]); (* Revisit on 64-bit *)
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Sse(Direct(Vmptrld,[oMq]),Direct(Vmxon,[oMq]),Direct(Vmclear,[oMq]),Invalid);
    SseNo(Direct(Vmptrst,[oMq]))|]),
  Invalid));
Direct(Bswap,[oeAX]);
Direct(Bswap,[oeCX]);
Direct(Bswap,[oeDX]);
Direct(Bswap,[oeBX]);
Direct(Bswap,[oeSP]);
Direct(Bswap,[oeBP]);
Direct(Bswap,[oeSI]);
Direct(Bswap,[oeDI]);
Sse(Invalid,Invalid,Direct(Addsubpd,[oVpd;oWpd]),Direct(Addsubps,[oVps;oWps])); (* 0x1D0 *)
SseNo66(Direct(Psrlw, [oPq;oQq]),Direct(Psrlw,  [oVdq;oWdq]));
SseNo66(Direct(Psrld, [oPq;oQq]),Direct(Psrld,  [oVdq;oWdq]));
SseNo66(Direct(Psrlq, [oPq;oQq]),Direct(Psrlq,  [oVdq;oWdq]));
SseNo66(Direct(Paddq, [oPq;oQq]),Direct(Paddq,  [oVdq;oWdq]));
SseNo66(Direct(Pmullw,[oPq;oQq]),Direct(Pmullw, [oVdq;oWdq]));
Sse(Invalid,Direct(Movq2dq,[oVdq;oNq]),Direct(Movq,   [oVdq;oWdq]),Direct(Movdq2q,[oPq;oUq]));
SseNo66(Direct(Pmovmskb,[oGd;oNq]),Direct(Pmovmskb,[oGd;oUdq]));
SseNo66(Direct(Psubusb,[oPq;oQq]), Direct(Psubusb,[oVdq;oWdq]));
SseNo66(Direct(Psubusw,[oPq;oQq]), Direct(Psubusw,[oVdq;oWdq]));
SseNo66(Direct(Pminub, [oPq;oQq]), Direct(Pminub, [oVdq;oWdq]));
SseNo66(Direct(Pand,   [oPq;oQq]), Direct(Pand,   [oVdq;oWdq]));
SseNo66(Direct(Paddusb,[oPq;oQq]), Direct(Paddusb,[oVdq;oWdq]));
SseNo66(Direct(Paddusw,[oPq;oQq]), Direct(Paddusw,[oVdq;oWdq]));
SseNo66(Direct(Pmaxub, [oPq;oQq]), Direct(Pmaxub, [oVdq;oWdq]));
SseNo66(Direct(Pandn,  [oPq;oQq]), Direct(Pandn,  [oVdq;oWdq]));
SseNo66(Direct(Pavgb,  [oPq;oQq]), Direct(Pavgb,  [oVdq;oWdq])); (* 0x1E0 *)
SseNo66(Direct(Psraw,  [oPq;oQq]), Direct(Psraw,  [oVdq;oWdq]));
SseNo66(Direct(Psrad,  [oPq;oQq]), Direct(Psrad,  [oVdq;oWdq]));
SseNo66(Direct(Pavgw,  [oPq;oQq]), Direct(Pavgw,  [oVdq;oWdq]));
SseNo66(Direct(Pmulhuw,[oPq;oQq]), Direct(Pmulhuw,[oVdq;oWdq]));
SseNo66(Direct(Pmulhw, [oPq;oQq]), Direct(Pmulhw, [oVdq;oWdq]));
Sse(Invalid,Direct(Cvtdq2pd,[oVpd;oWdq]),Direct(Cvttpd2dq,[oVdq;oWpd]),Direct(Cvtpd2dq,[oVdq;oWpd]));
SseNo66(Direct(Movntq,[oMq;oPq]),  Direct(Movntdq,[oMdq;oVdq]));
SseNo66(Direct(Psubsb, [oPq;oQq]), Direct(Psubsb, [oVdq;oWdq]));
SseNo66(Direct(Psubsw, [oPq;oQq]), Direct(Psubsw, [oVdq;oWdq]));
SseNo66(Direct(Pminsw, [oPq;oQq]), Direct(Pminsw, [oVdq;oWdq]));
SseNo66(Direct(Por,    [oPq;oQq]), Direct(Por,    [oVdq;oWdq]));
SseNo66(Direct(Paddsb, [oPq;oQq]), Direct(Paddsb, [oVdq;oWdq]));
SseNo66(Direct(Paddsw, [oPq;oQq]), Direct(Paddsw, [oVdq;oWdq]));
SseNo66(Direct(Pmaxsw, [oPq;oQq]), Direct(Pmaxsw, [oVdq;oWdq]));
SseNo66(Direct(Pxor,   [oPq;oQq]), Direct(Pxor,   [oVdq;oWdq]));
Sse(Invalid,Invalid,Invalid,Direct(Lddqu,[oVdq;oMdq])); (* 0x1F0 *)
SseNo66(Direct(Psllw,   [oPq;oQq]),Direct(Psllw,     [oVdq;oWdq]));
SseNo66(Direct(Pslld,   [oPq;oQq]),Direct(Pslld,     [oVdq;oWdq]));
SseNo66(Direct(Psllq,   [oPq;oQq]),Direct(Psllq,     [oVdq;oWdq]));
SseNo66(Direct(Pmuludq, [oPq;oQq]),Direct(Pmuludq,   [oVdq;oWdq]));
SseNo66(Direct(Pmaddwd, [oPq;oQq]),Direct(Pmaddwd,   [oVdq;oWdq]));
SseNo66(Direct(Psadbw,  [oPq;oQq]),Direct(Psadbw,    [oVdq;oWdq]));
SseNo66(Direct(Maskmovq,[oPq;oNq]),Direct(Maskmovdqu,[oVdq;oUdq]));
SseNo66(Direct(Psubb,   [oPq;oQq]),Direct(Psubb,     [oVdq;oWdq]));
SseNo66(Direct(Psubw,   [oPq;oQq]),Direct(Psubw,     [oVdq;oWdq]));
SseNo66(Direct(Psubd,   [oPq;oQq]),Direct(Psubd,     [oVdq;oWdq]));
SseNo66(Direct(Psubq,   [oPq;oQq]),Direct(Psubq,     [oVdq;oWdq]));
SseNo66(Direct(Paddb,   [oPq;oQq]),Direct(Paddb,     [oVdq;oWdq]));
SseNo66(Direct(Paddw,   [oPq;oQq]),Direct(Paddw,     [oVdq;oWdq]));
SseNo66(Direct(Paddd,   [oPq;oQq]),Direct(Paddd,     [oVdq;oWdq]));
Invalid;
SseNo66(Direct(Pshufb,   [oPq;oQq]),Direct(Pshufb,   [oVdq;oWdq])); (* 0x200 *)
SseNo66(Direct(Phaddw,   [oPq;oQq]),Direct(Phaddw,   [oVdq;oWdq]));
SseNo66(Direct(Phaddd,   [oPq;oQq]),Direct(Phaddd,   [oVdq;oWdq]));
SseNo66(Direct(Phaddsw,  [oPq;oQq]),Direct(Phaddsw,  [oVdq;oWdq]));
SseNo66(Direct(Pmaddubsw,[oPq;oQq]),Direct(Pmaddubsw,[oVdq;oWdq]));
SseNo66(Direct(Phsubw,   [oPq;oQq]),Direct(Phsubw,   [oVdq;oWdq]));
SseNo66(Direct(Phsubd,   [oPq;oQq]),Direct(Phsubd,   [oVdq;oWdq]));
SseNo66(Direct(Phsubsw,  [oPq;oQq]),Direct(Phsubsw,  [oVdq;oWdq]));
SseNo66(Direct(Psignb,   [oPq;oQq]),Direct(Psignb,   [oVdq;oWdq]));
SseNo66(Direct(Psignw,   [oPq;oQq]),Direct(Psignw,   [oVdq;oWdq]));
SseNo66(Direct(Psignd,   [oPq;oQq]),Direct(Psignd,   [oVdq;oWdq]));
SseNo66(Direct(Pmulhrsw, [oPq;oQq]),Direct(Pmulhrsw, [oVdq;oWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pblendvb,[oVdq;oWdq])); (* 0x210 *)
Invalid;
Invalid;
Invalid;
Sse66(Direct(Blendvps,[oVdq;oWdq]));
Sse66(Direct(Blendvpd,[oVdq;oWdq]));
Invalid;
Sse66(Direct(Ptest,[oVdq;oWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
SseNo66(Direct(Pabsb,[oPq;oQq]),Direct(Pabsb,[oVdq;oWdq]));
SseNo66(Direct(Pabsw,[oPq;oQq]),Direct(Pabsw,[oVdq;oWdq]));
SseNo66(Direct(Pabsd,[oPq;oQq]),Direct(Pabsd,[oVdq;oWdq]));
Invalid;
Sse66(Direct(Pmovsxbw,[oVdq;oUdqMq])); (* 0x220 *)
Sse66(Direct(Pmovsxbd,[oVdq;oUdqMd]));
Sse66(Direct(Pmovsxbq,[oVdq;oUdqMw]));
Sse66(Direct(Pmovsxwd,[oVdq;oUdqMq]));
Sse66(Direct(Pmovsxwq,[oVdq;oUdqMd]));
Sse66(Direct(Pmovsxdq,[oVdq;oUdqMq]));
Invalid;
Invalid;
Sse66(Direct(Pmuldq,  [oVdq;oWdq]));
Sse66(Direct(Pcmpeqq, [oVdq;oWdq]));
Sse66(Direct(Movntdqa,[oVdq;oMdq]));
Sse66(Direct(Packusdw,[oVdq;oWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pmovzxbw,[oVdq;oUdqMq])); (* 0x230 *)
Sse66(Direct(Pmovzxbd,[oVdq;oUdqMd]));
Sse66(Direct(Pmovzxbq,[oVdq;oUdqMw]));
Sse66(Direct(Pmovzxwd,[oVdq;oUdqMq]));
Sse66(Direct(Pmovzxwq,[oVdq;oUdqMd]));
Sse66(Direct(Pmovzxdq,[oVdq;oUdqMq]));
Invalid;
Sse66(Direct(Pcmpgtq,   [oVdq;oWdq]));
Sse66(Direct(Pminsb,    [oVdq;oWdq]));
Sse66(Direct(Pminsd,    [oVdq;oWdq]));
Sse66(Direct(Pminuw,    [oVdq;oWdq]));
Sse66(Direct(Pminud,    [oVdq;oWdq]));
Sse66(Direct(Pmaxsb,    [oVdq;oWdq]));
Sse66(Direct(Pmaxsd,    [oVdq;oWdq]));
Sse66(Direct(Pmaxuw,    [oVdq;oWdq]));
Sse66(Direct(Pmaxud,    [oVdq;oWdq]));
Sse66(Direct(Pmulld,    [oVdq;oWdq])); (* 0x240 *)
Sse66(Direct(Phminposuw,[oVdq;oWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x250 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x260 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x270 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x280 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x290 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x2A0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x2B0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x2C0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x2D0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x2E0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Direct(Crc32,[oGd;oEb]); (* 0x2F0 *)
Direct(Crc32,[oGd;oEv]);
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x300 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Roundps,[oVdq;oWdq;oIb]));
Sse66(Direct(Roundpd,[oVdq;oWdq;oIb]));
Sse66(Direct(Roundss,[oVss;oWss;oIb]));
Sse66(Direct(Roundsd,[oVsd;oWsd;oIb]));
Sse66(Direct(Blendps,[oVdq;oWdq;oIb]));
Sse66(Direct(Blendpd,[oVdq;oWdq;oIb]));
Sse66(Direct(Pblendw,[oVdq;oWdq;oIb]));
SseNo66(Direct(Palignr,[oPq;oQq;oIb]),Direct(Palignr,[oVdq;oWdq;oIb]));
Invalid; (* 0x310 *)
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pextrb,   [oRdMb;oVdq;oIb]));
Sse66(Direct(Pextrw,   [oRdMw;oVdq;oIb]));
Sse66(Direct(Pextrd,   [oEd;oVdq;oIb])); (* Revisit for 64-bit *)
Sse66(Direct(Extractps,[oEd;oVdq;oIb]));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pinsrb,  [oVdq;oEd;oIb])); (* 0x320 *)
Sse66(Direct(Insertps,[oVdq;oUdqMd;oIb]));
Sse66(Direct(Pinsrd,  [oVdq;oEd;oIb])); (* Revisit for 64-bit *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x330 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Dpps,   [oVdq;oWdq;oIb])); (* 0x340 *)
Sse66(Direct(Dppd,   [oVdq;oWdq;oIb]));
Sse66(Direct(Mpsadbw,[oVdq;oWdq;oIb]));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x350 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pcmpestrm,[oVdq;oWdq;oIb])); (* 0x360 *)
Sse66(Direct(Pcmpestri,[oVdq;oWdq;oIb]));
Sse66(Direct(Pcmpistrm,[oVdq;oWdq;oIb]));
Sse66(Direct(Pcmpistri,[oVdq;oWdq;oIb]));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x370 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x380 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x390 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3A0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3B0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3C0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3D0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3E0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid; (* 0x3F0 *)
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
|]

let hasModRM = 
  let o = false and x = true in
  [|x;x;x;x;o;o;o;o;x;x;x;x;o;o;o;o;  (* 0x00 *)
    x;x;x;x;o;o;o;o;x;x;x;x;o;o;o;o;  (* 0x10 *)
    x;x;x;x;o;o;o;o;x;x;x;x;o;o;o;o;  (* 0x20 *)
    x;x;x;x;o;o;o;o;x;x;x;x;o;o;o;o;  (* 0x30 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x40 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x50 *)
    o;o;x;x;o;o;o;o;o;x;o;x;o;o;o;o;  (* 0x60 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x70 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x80 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x90 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0xA0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0xB0 *)
    x;x;o;o;x;x;x;x;o;o;o;o;o;o;o;o;  (* 0xC0 *)
    x;x;x;x;o;o;o;o;x;x;x;x;x;x;x;x;  (* 0xD0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0xE0 *)
    o;o;o;o;o;o;x;x;o;o;o;o;o;o;x;x;  (* 0xF0 *)
    x;x;x;x;o;o;o;o;o;o;o;o;o;x;o;o;  (* 0x0F 0x00 *)
    x;x;x;x;x;x;x;x;x;o;o;o;o;o;o;x;  (* 0x0F 0x10 *)
    x;x;x;x;o;o;o;o;x;x;x;x;x;x;x;x;  (* 0x0F 0x20 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x30 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0x40 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0x50 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0x60 *)
    x;x;x;x;x;x;x;o;x;x;o;o;x;x;x;x;  (* 0x0F 0x70 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x80 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0x90 *)
    o;o;o;x;x;x;o;o;o;o;o;x;x;x;x;x;  (* 0x0F 0xA0 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0xB0 *)
    x;x;x;x;x;x;x;x;o;o;o;o;o;o;o;o;  (* 0x0F 0xC0 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0xD0 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;  (* 0x0F 0xE0 *)
    x;x;x;x;x;x;x;x;x;x;x;x;x;x;x;o;  (* 0x0F 0xF0 *)
    x;x;x;x;x;x;x;x;x;x;x;x;o;o;o;o;  (* 0x0F 0x38 0x00 *)
    x;o;o;o;x;x;o;x;o;o;o;o;x;x;x;o;  (* 0x0F 0x38 0x10 *)
    x;x;x;x;x;x;o;o;x;x;x;x;o;o;o;o;  (* 0x0F 0x38 0x20 *)
    x;x;x;x;x;x;o;x;x;x;x;x;x;x;x;x;  (* 0x0F 0x38 0x30 *)
    x;x;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x40 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x50 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x60 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x70 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x80 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0x90 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xA0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xB0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xC0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xD0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xE0 *)
    x;x;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x38 0xF0 *)
    o;o;o;o;o;o;o;o;x;x;x;x;x;x;x;x;  (* 0x0F 0x3A 0x00 *)
    o;o;o;o;x;x;x;x;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x10 *)
    x;x;x;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x20 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x30 *)
    x;x;x;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x40 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x50 *)
    x;x;x;x;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x60 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x70 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x80 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0x90 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0xA0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0xB0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0xC0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0xD0 *)
    o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;  (* 0x0F 0x3A 0xE0 *)
    x;x;o;o;o;o;o;o;o;o;o;o;o;o;o;o;|](* 0x0F 0x3A 0xF0 *)

(* This function produces the index used by the preceeding tables *)
let bytes_to_table_index () =
  let x =
    let b1 = consume_byte () in
    if b1 <> 0x0Fl then b1
    else
      let b2 = consume_byte () in
      match b2 with
      | 0x38l -> Int32.logor 0x200l (consume_byte ())
      | 0x3Al -> Int32.logor 0x300l (consume_byte ())
      | _ -> Int32.logor 0x100l b2
  in Int32.to_int x
  
(* Mod R/M decoding *)
let top2 x = Int32.to_int (Int32.logand (Int32.shift_right_logical (Int32.logand x 0xC0l) 6) 0x3l)
let mid3 x = Int32.to_int (Int32.logand (Int32.shift_right_logical (Int32.logand x 0x38l) 3) 0x7l)
let bot3 x = Int32.to_int (Int32.logand x 0x07l)
let dmrm s m mreg = { modrmsize = s; mreg = mreg; modrmbyte = m; modrm = RegisterNumber(~-1) }

let parse_modrm32 () =
  let m = consume_byte () in
  let m0d,mreg,rm = top2 m,mid3 m,bot3 m in
  let defctxt = dmrm (ModRM_32) m mreg in
  (* Is there a SIB byte? *)
  if not (m0d < 3 && rm == 4)
  then
   ((* SIB not present *)
    (* Is it a straight disp32? *)
    if (m0d == 0 && rm == 5) then { defctxt with modrm = MemoryPhrase(-1,-1,0,consume_dword ()) }
    else
     (if (m0d == 3)
      then { defctxt with modrm = RegisterNumber(rm) }
      else { defctxt with modrm = MemoryPhrase(rm,-1,0,
        match m0d with 
        | 0 -> 0x0l
        | 1 -> sign_extend_byte_dword (consume_byte ())
        | 2 -> consume_dword ()
        | _ -> failwith "Impossible")}))
  else
   ((* SIB present *)
    let sib   = consume_byte () in
    let ss,index,sreg = top2 sib,mid3 sib,bot3 sib in
    let srn,sf = if index <> 4 then index,ss else -1,0 in
    (* If m0d == 1 or m0d == 2, use ebp as the base; otherwise, use the specified
       register as the base *)    
    let brn = if (sreg <> 5 || m0d == 1 || m0d == 2) then sreg else ~-1 in
    { defctxt with modrm = MemoryPhrase(brn,srn,sf,
      match m0d with
      | 0 -> if sreg == 5 then consume_dword () else 0x0l
      | 1 -> sign_extend_byte_dword (consume_byte ())
      | 2 -> consume_dword ()
      | _ -> failwith "Impossible")})

let parse_modrm16 () =
  let m = consume_byte () in
  let m0d,mreg,rm = top2 m,mid3 m,bot3 m in
  let defctxt = dmrm (ModRM_16) m mreg in
  if (m0d == 3)
  then { defctxt with modrm = RegisterNumber(rm) }
  else if (m0d == 0 && rm == 6) then { defctxt with modrm = MemoryPhrase(-1,-1,0,consume_word ()) }
  else
   (let disp = 
      if (m0d == 2) then consume_word () else
      if (m0d == 1) then sign_extend_byte_word (consume_byte ()) else
      0x0l
    in
    let br,sr = match rm with
    | 0 -> (3,6)   | 1 -> (3,7)   | 2 -> (5,6)   | 3 -> (5,7)
    | 4 -> (6,~-1) | 5 -> (7,~-1) | 6 -> (5,~-1) | 7 -> (3,~-1)
    | _ -> failwith "Impossible"
    in { defctxt with modrm = MemoryPhrase(br,sr,0,disp) })

(* Parse Mod/RM depending on address size *)
let parse_modrm () = 
  match determine_addr_size () with
  | AddrSize_32 -> parse_modrm32 ()
  | AddrSize_16 -> parse_modrm16 ()
  | x           -> raise (UnsupportedAddressSize(x))

(* Eat prefix bytes and fill the lists in the prefix context *)
let eat_legacy_prefixes () = 
  let eat ctxt = 
    match (consume_byte ()) with
    | 0xF0l -> Some({ ctxt with group1pf   = (Lock )::(ctxt.group1pf) })
    | 0xF2l -> Some({ ctxt with group1pf   = (Repne)::(ctxt.group1pf) })
    | 0xF3l -> Some({ ctxt with group1pf   = (Rep  )::(ctxt.group1pf) })
    | 0x2El -> Some({ ctxt with segmentpf  = (CS)::(ctxt.segmentpf) })
    | 0x36l -> Some({ ctxt with segmentpf  = (SS)::(ctxt.segmentpf) })
    | 0x3El -> Some({ ctxt with segmentpf  = (DS)::(ctxt.segmentpf) })
    | 0x26l -> Some({ ctxt with segmentpf  = (ES)::(ctxt.segmentpf) })
    | 0x64l -> Some({ ctxt with segmentpf  = (FS)::(ctxt.segmentpf) })
    | 0x65l -> Some({ ctxt with segmentpf  = (GS)::(ctxt.segmentpf) })
    | 0x66l -> Some({ ctxt with opsizepf   = (PF3_OpSize)  ::(ctxt.opsizepf)   })
    | 0x67l -> Some({ ctxt with addrsizepf = (PF4_AddrSize)::(ctxt.addrsizepf) })
    | _ -> decr_ea (); None
  in
  let rec aux ctxt = 
    match (eat ctxt) with
    | Some(c) -> aux c
    | None    -> ctxt
  in aux (mk_default_pref ())
  
(* X86 decoder *)
let decode tabint =
  let low = Int32.to_int (Int32.logand !modrm_context.modrmbyte 0x7l) in
  let rec aux = function
  | Invalid -> raise InvalidInstruction
  | Fatal -> failwith "decode: tried to consume a prefix byte during instruction decoding, universe is broken"
  | Unimplemented(s) -> raise (UnimplementedInstruction(s))
  | Direct(m,l) -> (m,List.map (fun f -> f ()) l)
  | Group(a) -> aux a.(!modrm_context.mreg)
  | Predicated(t) -> (match t with
    | PRED_OpSize  (w16,w32)    ->        opsize_predicate     (lazy (aux w16)) (lazy (aux w32))
    | PRED_AddrSize(w16,w32)    ->      addrsize_predicate     (lazy (aux w16)) (lazy (aux w32))
    | PRED_ModRMMemReg(mem,reg) -> modrm_mem_reg_discriminator (lazy (aux mem)) (lazy (aux reg))
    | PRED_AddrOpSize(w1616,w1632,w3216,w3232) -> addropsize_predicate (lazy (aux w1616)) (lazy (aux w1632)) (lazy (aux w3216)) (lazy (aux w3232)))
  | LowFun(f) -> aux (let m,l = f low in Direct(m,l))
  | LowGroup(a) -> aux a.(low)
  | Sse(f1,f2,f3,f4) -> sse f1        f2        f3        f4
  | SseNo(no)        -> sse no        (Invalid) (Invalid) (Invalid)
  | Sse66(sx)        -> sse (Invalid) (Invalid) sx        (Invalid)
  | SseNo66(no,sx)   -> sse no        (Invalid) sx        (Invalid)
  and sse f1 f2 f3 f4 =
    match 
     (List.mem (Rep)        !prefs.group1pf,
      List.mem (PF3_OpSize) !prefs.opsizepf,
      List.mem (Repne)      !prefs.group1pf) 
    with
    | false, false, false -> aux f1
    | true,  false, false -> aux f2
    | false, true,  false -> aux f3
    | false, false, true  -> aux f4
    | _,_,_ -> raise (DoMoreResearch(Printf.sprintf "Multiple SSE prefixes: 0x%08x" tabint))
  in aux x86_instruction_production_table.(tabint)

let is_valid_lock_prefix = function
| Adc | Add | And | Btc | Btr | Bts | Cmpxchg | Cmpxchg8b | Dec | Inc | Neg 
| Not | Or | Sbb | Xadd | Xchg | Xor -> true
| _ -> false

let decode ea = 
  reset_cursor ea;
  prefs := eat_legacy_prefixes ();
  let table_index = bytes_to_table_index () in
  segov := 
   (match !prefs.segmentpf with
    | [] -> None
    | x::[] -> Some(x)
    (* Is the last segment prefix the valid one?  IDA seems to think so *)
    | x::y::_ -> raise (DoMoreResearch(Printf.sprintf "Multiple segment prefixes: 0x%08x" table_index)));
  modrm_context := (if hasModRM.(table_index) then parse_modrm () else mk_default_modrm ());
  let ((m,o) as i) = decode table_index in
  let instrbody = { pref = !prefs.group1pf; instr = i } in
  if (List.mem (Lock) !prefs.group1pf) && not (is_valid_lock_prefix m)
  then raise (InvalidLockPrefix(instrbody));
  let successors = match o,m with
  | JccTarget(t,f)::[],m ->
   (match m with
    | Call -> ASMUtil.Call(t,f)
    | Jmp  -> ASMUtil.Jmp(t)
    | Jo  | Jno | Jb  | Jae | Jz  | Jnz | Jbe | Ja  | Js  | Jns | Jp  | Jnp
    | Jl  | Jge | Jle | Jg  | Loopnz | Loopz | Loop | Jcxz | Jecxz -> ASMUtil.Jcc(t,f)
    | _ -> failwith "Instruction other than call/jmp/jcc had a JccTarget operand")
  | _,Call 
  | _,CallF -> ASMUtil.ICall(get_cursor ())
  | _,Jmp  
  | _,JmpF  -> ASMUtil.IJmp
  | _,Ret
  | _,Retf
  | _,Iretd
  | _,Iretw -> ASMUtil.Return
  | _,_     -> ASMUtil.Flow(get_cursor ())
  in
  (instrbody,Int32.to_int (Int32.sub (get_cursor ()) ea),successors)

let make_c_disassembler () =
  let make_hasmodrm () =
    let _ = Printf.printf "char hasModRM[1024] = \n{\n  " in
    let print_entry index b =
     (match b with
      | true  -> Printf.printf "1;"
      | false -> Printf.printf "0;");
      if (index land 0x0f) = 0x0f
      then 
        match index with
        | _ when index < 0x100 -> Printf.printf "  /* 0x%02X */\n  "           (index - 0x00F)
        | _ when index < 0x200 -> Printf.printf "  /* 0x0F 0x%02X */\n  "      (index - 0x10F)
        | _ when index < 0x300 -> Printf.printf "  /* 0x0F 0x38 0x%02X */\n  " (index - 0x20F)
        | _ when index < 0x400 -> Printf.printf "  /* 0x0F 0x3A 0x%02X */\n  " (index - 0x30F)
        | _ -> ()
    in
    Array.iteri print_entry hasModRM;
    Printf.printf "\n};\n\n"
  in
  let print index ip = 
    let print indent string = 
      let rec aux i =
        let _ = assert (indent >= 0) in
        if i > 0
        then let _ = Printf.printf "  " in aux (i-1)
        else ()
      in 
      let _ = aux indent in
      Printf.printf "%s\n" string
    in
    print 1 (Printf.sprintf "case 0x%04x:" index);
    print 1 "{";
    let rec print_entry il = function
    | Invalid           -> print il "// invalid"
    | Fatal             -> print il "// fatal"
    | Unimplemented(s)  -> print il ("// unimplemented "^s)
    | Direct(mnem,ops) -> print il ("// "^(X86Disasm.string_of_x86mnem mnem))
    | Group(arr) -> 
      print il "switch(modrmcontext.greg)";
      print il "{";
      let il2 = il + 1 in
      let rec aux il i entry = 
        print il (Printf.sprintf "case %d:" i);
        print il "{";
        print_entry (il+1) entry;
        print il "}"
      in 
      let _ = Array.iteri (aux (il2+1)) arr in
      print il "}"
    | Predicated(PRED_OpSize     (w16,w32)) ->
      print il "int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);";
      print il "switch(opsize)";
      print il "{";
      let il2 = il + 1 in
      print il2 "case 16:";
      print il2 "{";
      print_entry (il2+1) w16;
      print il2 "}";
      print il2 "case 32:";
      print il2 "{";
      print_entry (il2+1) w32;
      print il2 "}";
      print il "}"
    | Predicated(PRED_AddrSize   (w16,w32)) ->
      print il "int addrsize = determine_address_size(config, &prefixcontext);";
      print il "switch(addrsize)";
      print il "{";
      let il2 = il + 1 in
      print il2 "case 16:";
      print il2 "{";
      print_entry (il2+1) w16;
      print il2 "}";
      print il2 "case 32:";
      print il2 "{";
      print_entry (il2+1) w32;
      print il2 "}";
      print il "}"
    | Predicated(PRED_ModRMMemReg(mem,reg)) ->
      print il "switch(modrmcontext.byte >> 6)";
      print il "{";
      let il2 = il + 1 in
      print il2 "case 3:";
      print il2 "{";
      print_entry (il2+1) reg;
      print il2 "}";
      print il2 "default:";
      print il2 "{";
      print_entry (il2+1) mem;
      print il2 "}";
      print il "}"
    | Predicated(PRED_AddrOpSize (w1616,w1632,w3216,w3232)) -> print il "// AddrOpSize"
    | _                 -> print il "// unknown"
    in print_entry 2 ip;
    print 1 "}\n";
    print 0 ""
  in
  make_hasmodrm ();
  Array.iteri print x86_instruction_production_table
(*
  | LowFun(f) -> aux (let m,l = f low in Direct(m,l))
  | LowGroup(a) -> aux a.(low)
  | Sse(f1,f2,f3,f4) -> sse f1        f2        f3        f4
  | SseNo(no)        -> sse no        (Invalid) (Invalid) (Invalid)
  | Sse66(sx)        -> sse (Invalid) (Invalid) sx        (Invalid)
  | SseNo66(no,sx)   -> sse no        (Invalid) sx        (Invalid)
*)