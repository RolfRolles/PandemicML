(* TODO:  General cleanup at this point.  Clean up language regarding 
   modrm_mem_reg_predicate vs. _discriminator 
   
   let oJz () = ...
   Need to behave differently when given a 16-bit address size:  and with 0xFFFFl.

   OIbAsOpndSize
   OMbBx
   OMbEbx
*)

open X86
open X86InternalOperand

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

(* Points to either a 16- or 32-bit quantity *)
let oM    () = modrm_mem_reg_predicate (lazy (addrsize_predicate (lazy (oMw ())) (lazy (oMd ())))) (lazy (invalid ()))
(* Points to a pointer that depends on the operand size (16:16 or 16:32) *)
let oMp   () = modrm_mem_reg_predicate (lazy (opsize_predicate   (lazy (oMd ())) (lazy (oMf ())))) (lazy (invalid ()))
(* Points to two consecutive addresses, which are determined by the operand size *)
let oMa   () = modrm_mem_reg_predicate (lazy (opsize_predicate   (lazy (oMd ())) (lazy (oMq ())))) (lazy (invalid ()))

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
| 0 -> FPUReg(ST0)
| 1 -> FPUReg(ST1)
| 2 -> FPUReg(ST2)
| 3 -> FPUReg(ST3)
| 4 -> FPUReg(ST4)
| 5 -> FPUReg(ST5)
| 6 -> FPUReg(ST6)
| 7 -> FPUReg(ST7)
| x -> failwith ("oSt_of_int "^string_of_int x)

let oStN () = oSt_of_int (Int32.to_int (Int32.logand !modrm_context.modrmbyte 0x7l))

let oReal4    () = oMd ()
let oReal8    () = oMq ()
let oReal10   () = oMt ()
let oFPEnvLow () = oMd () (* Wrong, clearly *)
let oFPEnv    () = oMd () (* Wrong, clearly *)
let oSimdState() = oMd () (* Wrong, clearly, but IDA disassembles it this way *)

let function_of_x86_abstract_operand = function
| OAL        -> oAL
| OALR8L     -> oALR8L
| OCL        -> oCL
| OCLR9L     -> oCLR9L
| ODLR10L    -> oDLR10L
| OBLR11L    -> oBLR11L
| OAHR12L    -> oAHR12L
| OCHR13L    -> oCHR13L
| ODHR14L    -> oDHR14L
| OBHR15L    -> oBHR15L
| OAX        -> oAX
| ODX        -> oDX
| OeAX       -> oeAX
| OrAXr8     -> orAXr8
| OrAX       -> orAX
| OeCX       -> oeCX
| OrCXr9     -> orCXr9
| OeDX       -> oeDX
| OrDXr10    -> orDXr10
| OeBX       -> oeBX
| OrBXr11    -> orBXr11
| OeSP       -> oeSP
| OrSPr12    -> orSPr12
| OeBP       -> oeBP
| OrBPr13    -> orBPr13
| OeSI       -> oeSI
| OrSIr14    -> orSIr14
| OeDI       -> oeDI
| OrDIr15    -> orDIr15
| OCS        -> oCS
| ODS        -> oDS
| OES        -> oES
| OGS        -> oGS
| OFS        -> oFS
| OSS        -> oSS
| OSw        -> oSw
| OCd        -> oCd
| ODd        -> oDd
| OIb        -> oIb
| OIw        -> oIw
| OIv        -> oIv
| OIz        -> oIz
| O1         -> o1
| OGb        -> oGb
| OGw        -> oGw
| OGd        -> oGd
| OGv        -> oGv
| OGd_q      -> oGd_q
| OGz        -> oGz
| ORw        -> oRw
| ORd        -> oRd
| ORv        -> oRv
| OMb        -> oMb
| OMw        -> oMw
| OMd        -> oMd
| OMs        -> oMs
| OMq        -> oMq
| OMdq       -> oMdq
| OMd_q      -> oMd_q
| OMa        -> oMa
| OMp        -> oMp
| OMpd       -> oMpd
| OMps       -> oMps
| OM         -> oM
| OEb        -> oEb
| OEw        -> oEw
| OEd        -> oEd
| OEv        -> oEv
| OEd_q      -> oEd_q
| OOb        -> oOb
| OOv        -> oOv
| OAp        -> oAp
| OJb        -> oJb
| OJz        -> oJz
| OXb        -> oXb
| OXv        -> oXv
| OXz        -> oXz
| OYb        -> oYb
| OYv        -> oYv
| OYz        -> oYz
| OSt0       -> oSt0
| OStN       -> oStN
| OFPEnv     -> oFPEnv
| OFPEnvLow  -> oFPEnvLow
| OReal4     -> oReal4
| OReal8     -> oReal8
| OReal10    -> oReal10
| ONq        -> oNq
| OPd        -> oPd
| OPq        -> oPq
| OPpi       -> oPpi
| OVdq       -> oVdq
| OVpd       -> oVpd
| OVps       -> oVps
| OVsd       -> oVsd
| OVss       -> oVss
| OVq        -> oVq
| OUps       -> oUps
| OUpd       -> oUpd
| OUq        -> oUq
| OUdq       -> oUdq
| OQpi       -> oQpi
| OQd        -> oQd
| OQq        -> oQq
| OWdq       -> oWdq
| OWps       -> oWps
| OWpd       -> oWpd
| OWq        -> oWq
| OWss       -> oWss
| OWsd       -> oWsd
| OSimdState -> oSimdState
| OUdq_Md    -> oUdqMd
| OUdq_Mq    -> oUdqMq
| OUdq_Mw    -> oUdqMw
| ORd_Mb     -> oRdMb
| ORd_Mw     -> oRdMw
| OXw        -> failwith "OXw"
| OXd        -> failwith "OXd"
| OYw        -> failwith "OYw"
| OYd        -> failwith "OYd"


(* These data types describe the entries of the table below. *)
type x86_operand_producer = x86_abstract_operand
type x86_specifier = x86mnem * (x86_operand_producer list)
type pred_type = PRED_OpSize      of x86_instruction_producer * x86_instruction_producer (* Size_16, Size_32 *)
               | PRED_AddrSize    of x86_instruction_producer * x86_instruction_producer (* Size_16, Size_32 *)
               | PRED_ModRMMemReg of x86_instruction_producer * x86_instruction_producer (* is mem, is reg *)

and x86_instruction_producer =
| Fatal   (* Tried to eat a prefix byte that should have been consumed already *)
| Invalid (* Invalid encoding *)
| Unimplemented of string (* Unimplemented -- none such exist *)
| Direct of x86mnem * (x86_operand_producer list) (* Direct instruction *)
| Group of x86_instruction_producer array (* Group indexed by modrm.mreg *)
| Predicated of pred_type (* Outcome depends on prefixes and encoding *)
| LowGroup of x86_instruction_producer array (* Group indexed by modrm.rm *)
| Sse of x86_instruction_producer * x86_instruction_producer * x86_instruction_producer * x86_instruction_producer 
     (* SSE instruction: no prefix, F3 prefix, 66 prefix, F2 prefix *)
| SseNo of x86_instruction_producer (* SSE instruction: no prefix *)
| Sse66 of x86_instruction_producer (* SSE instruction: 66 prefix *)
| SseNo66 of x86_instruction_producer * x86_instruction_producer (* SSE instruction: no prefix, 66 prefix *)

(* This is more or less a literal translation of Intel #2B, appendix A.3 *)
let x86_instruction_production_table = [|
Direct(Add,[OEb;OGb]); (* 0x00 *)
Direct(Add,[OEv;OGv]);
Direct(Add,[OGb;OEb]);
Direct(Add,[OGv;OEv]);
Direct(Add,[OAL;OIb]);
Direct(Add,[OrAX;OIz]);
Direct(Push,[OES]);
Direct(Pop,[OES]);
Direct(Or,[OEb;OGb]);
Direct(Or,[OEv;OGv]);
Direct(Or,[OGb;OEb]);
Direct(Or,[OGv;OEv]);
Direct(Or,[OAL;OIb]);
Direct(Or,[OrAX;OIz]);
Direct(Push,[OCS]);
Fatal;
Direct(Adc,[OEb;OGb]); (* 0x10 *)
Direct(Adc,[OEv;OGv]);
Direct(Adc,[OGb;OEb]);
Direct(Adc,[OGv;OEv]);
Direct(Adc,[OAL;OIb]);
Direct(Adc,[OrAX;OIz]);
Direct(Push,[OSS]);
Direct(Pop,[OSS]);
Direct(Sbb,[OEb;OGb]);
Direct(Sbb,[OEv;OGv]);
Direct(Sbb,[OGb;OEb]);
Direct(Sbb,[OGv;OEv]);
Direct(Sbb,[OAL;OIb]);
Direct(Sbb,[OrAX;OIz]);
Direct(Push,[ODS]);
Direct(Pop,[ODS]);
Direct(And,[OEb;OGb]); (* 0x20 *)
Direct(And,[OEv;OGv]);
Direct(And,[OGb;OEb]);
Direct(And,[OGv;OEv]);
Direct(And,[OAL;OIb]);
Direct(And,[OrAX;OIz]);
Fatal;
Direct(Daa,[]);
Direct(Sub,[OEb;OGb]);
Direct(Sub,[OEv;OGv]);
Direct(Sub,[OGb;OEb]);
Direct(Sub,[OGv;OEv]);
Direct(Sub,[OAL;OIb]);
Direct(Sub,[OrAX;OIz]);
Fatal;
Direct(Das,[]);
Direct(Xor,[OEb;OGb]); (* 0x30 *)
Direct(Xor,[OEv;OGv]);
Direct(Xor,[OGb;OEb]);
Direct(Xor,[OGv;OEv]);
Direct(Xor,[OAL;OIb]);
Direct(Xor,[OrAX;OIz]);
Fatal;
Direct(Aaa,[]);
Direct(Cmp,[OEb;OGb]);
Direct(Cmp,[OEv;OGv]);
Direct(Cmp,[OGb;OEb]);
Direct(Cmp,[OGv;OEv]);
Direct(Cmp,[OAL;OIb]);
Direct(Cmp,[OrAX;OIz]);
Fatal;
Direct(Aas,[]);
Direct(Inc,[OeAX]);    (* 0x40 *)
Direct(Inc,[OeCX]);
Direct(Inc,[OeDX]);
Direct(Inc,[OeBX]);
Direct(Inc,[OeSP]);
Direct(Inc,[OeBP]);
Direct(Inc,[OeSI]);
Direct(Inc,[OeDI]);
Direct(Dec,[OeAX]);
Direct(Dec,[OeCX]);
Direct(Dec,[OeDX]);
Direct(Dec,[OeBX]);
Direct(Dec,[OeSP]);
Direct(Dec,[OeBP]);
Direct(Dec,[OeSI]);
Direct(Dec,[OeDI]);
Direct(Push,[OrAXr8]);    (* 0x50 *)
Direct(Push,[OrCXr9]);
Direct(Push,[OrDXr10]);
Direct(Push,[OrBXr11]);
Direct(Push,[OrSPr12]);
Direct(Push,[OrBPr13]);
Direct(Push,[OrSIr14]);
Direct(Push,[OrDIr15]);
Direct(Pop,[OrAXr8]);
Direct(Pop,[OrCXr9]);
Direct(Pop,[OrDXr10]);
Direct(Pop,[OrBXr11]);
Direct(Pop,[OrSPr12]);
Direct(Pop,[OrBPr13]);
Direct(Pop,[OrSIr14]);
Direct(Pop,[OrDIr15]);
Predicated(PRED_OpSize(Direct(Pushaw,[]),Direct(Pushad,[]))); (* 0x60 *)
Predicated(PRED_OpSize(Direct(Popaw,[]), Direct(Popad,[])));
Direct(Bound,[OGv;OMa]);
Direct(Arpl,[OEw;OGw]);
Fatal;
Fatal;
Fatal;
Fatal;
Direct(Push,[OIz]);
Direct(Imul,[OGv;OEv;OIz]);
Direct(Push,[(*OIbAsOpndSize*)]);
Direct(Imul,[OGv;OEv;OIb]);
Direct(Insb,[OYb;ODX]);
Predicated(PRED_OpSize(Direct(Insw,[OYz;ODX]),Direct(Insd,[OYz;ODX]))); (* Insw /Insd, [OYz;ODX] -- Mnem changes, given a mode *)
Direct(Outsb,[ODX;OXb]);
Predicated(PRED_OpSize(Direct(Outsw,[ODX;OXz]),Direct(Outsd,[ODX;OXz]))); (* Outsw /Outsd, [ODX;OXz] -- Mnem changes, given a mode *)
Direct(Jo ,[OJb]); (* 0x70 *)
Direct(Jno,[OJb]);
Direct(Jb ,[OJb]);
Direct(Jae,[OJb]);
Direct(Jz ,[OJb]);
Direct(Jnz,[OJb]);
Direct(Jbe,[OJb]);
Direct(Ja ,[OJb]);
Direct(Js ,[OJb]);
Direct(Jns,[OJb]);
Direct(Jp ,[OJb]);
Direct(Jnp,[OJb]);
Direct(Jl ,[OJb]);
Direct(Jge,[OJb]);
Direct(Jle,[OJb]);
Direct(Jg ,[OJb]);
Group([|Direct(Add,[OEb;OIb]);Direct(Or,[OEb;OIb]);Direct(Adc,[OEb;OIb]);Direct(Sbb,[OEb;OIb]);Direct(And,[OEb;OIb]);Direct(Sub,[OEb;OIb]);Direct(Xor,[OEb;OIb]);Direct(Cmp,[OEb;OIb])|]); (* 0x80 *)
Group([|Direct(Add,[OEv;OIz]);Direct(Or,[OEv;OIz]);Direct(Adc,[OEv;OIz]);Direct(Sbb,[OEv;OIz]);Direct(And,[OEv;OIz]);Direct(Sub,[OEv;OIz]);Direct(Xor,[OEv;OIz]);Direct(Cmp,[OEv;OIz])|]);
Group([|Direct(Add,[OEb;OIb]);Direct(Or,[OEb;OIb]);Direct(Adc,[OEb;OIb]);Direct(Sbb,[OEb;OIb]);Direct(And,[OEb;OIb]);Direct(Sub,[OEb;OIb]);Direct(Xor,[OEb;OIb]);Direct(Cmp,[OEb;OIb])|]);
Group([|Direct(Add,[OEv;(*OIbAsOpndSize*)]);Direct(Or,[OEv;(*OIbAsOpndSize*)]);Direct(Adc,[OEv;(*OIbAsOpndSize*)]);Direct(Sbb,[OEv;(*OIbAsOpndSize*)]);Direct(And,[OEv;(*OIbAsOpndSize*)]);Direct(Sub,[OEv;(*OIbAsOpndSize*)]);Direct(Xor,[OEv;(*OIbAsOpndSize*)]);Direct(Cmp,[OEv;(*OIbAsOpndSize*)])|]);
Direct(Test,[OEb;OGb]);
Direct(Test,[OEv;OGv]);
Direct(Xchg,[OEb;OGb]);
Direct(Xchg,[OEv;OGv]);
Direct(Mov,[OEb;OGb]);
Direct(Mov,[OEv;OGv]);
Direct(Mov,[OGb;OEb]);
Direct(Mov,[OGv;OEv]);
Direct(Mov,[OEv;OSw]);
Direct(Lea,[OGv;OM]);
Direct(Mov,[OSw;OEw]);
Group([|Direct(Pop,[OEv]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Sse(Direct(Nop,[]),Direct(Pause,[]),Direct(Nop,[]),Direct(Nop,[])); (* 0x90 *)
Direct(Xchg,[OrAX;OrCXr9]);
Direct(Xchg,[OrAX;OrDXr10]);
Direct(Xchg,[OrAX;OrBXr11]);
Direct(Xchg,[OrAX;OrSPr12]);
Direct(Xchg,[OrAX;OrBPr13]);
Direct(Xchg,[OrAX;OrSIr14]);
Direct(Xchg,[OrAX;OrDIr15]);
Predicated(PRED_OpSize(Direct(Cbw,[]),Direct(Cwde,[]))); (* CBW/CWDE/CDQE *)
Predicated(PRED_OpSize(Direct(Cwd,[]),Direct(Cdq,[])));  (* CWD/CDQ/CQO *)
Direct(CallF,[OAp]);
Direct(Wait,[]);
Predicated(PRED_OpSize(Direct(Pushfw,[]),Direct(Pushfd,[]))); (* PUSHF/D/Q *)
Predicated(PRED_OpSize(Direct(Popfw,[]), Direct(Popfd,[]))); (* POPF/D/Q *)
Direct(Sahf,[]);
Direct(Lahf,[]);
Direct(Mov,[OAL;OOb]);  (* 0xA0 *)
Direct(Mov,[OrAX;OOv]);
Direct(Mov,[OOb;OAL]);
Direct(Mov,[OOv;OrAX]);
Direct(Movsb,[OXb;OYb]);
Predicated(PRED_OpSize(Direct(Movsw,[OXv;OYv]),Direct(Movsd,[OXv;OYv]))); (* MOVS/W/D/Q *)
Direct(Cmpsb,[OXb;OYb]);
Predicated(PRED_OpSize(Direct(Cmpsw,[OXv;OYv]),Direct(Cmpsd,[OXv;OYv]))); (* CMPS/W/D/Q *)
Direct(Test,[OAL;OIb]);
Direct(Test,[OrAX;OIz]);
Direct(Stosb,[OYb]);
Predicated(PRED_OpSize(Direct(Stosw,[OYv]),Direct(Stosd,[OYv]))); (* STOS/W/D/Q *)
Direct(Lodsb,[OXb]);
Predicated(PRED_OpSize(Direct(Lodsw,[OXv]),Direct(Lodsd,[OXv]))); (* LODS/W/D/Q *)
Direct(Scasb,[OYb]);
Predicated(PRED_OpSize(Direct(Scasw,[OYv]),Direct(Scasd,[OYv]))); (* SCAS/W/D/Q *)
Direct(Mov,[OALR8L;OIb]); (* 0xB0 *)
Direct(Mov,[OCLR9L;OIb]);
Direct(Mov,[ODLR10L;OIb]);
Direct(Mov,[OBLR11L;OIb]);
Direct(Mov,[OAHR12L;OIb]);
Direct(Mov,[OCHR13L;OIb]);
Direct(Mov,[ODHR14L;OIb]);
Direct(Mov,[OBHR15L;OIb]);
Direct(Mov,[OrAXr8;OIv]);
Direct(Mov,[OrCXr9;OIv]);
Direct(Mov,[OrDXr10;OIv]);
Direct(Mov,[OrBXr11;OIv]);
Direct(Mov,[OrSPr12;OIv]);
Direct(Mov,[OrBPr13;OIv]);
Direct(Mov,[OrSIr14;OIv]);
Direct(Mov,[OrDIr15;OIv]);
Group([|Direct(Rol,[OEb;OIb]);Direct(Ror,[OEb;OIb]);Direct(Rcl,[OEb;OIb]);Direct(Rcr,[OEb;OIb]);Direct(Shl,[OEb;OIb]);Direct(Shr,[OEb;OIb]);Direct(Sal,[OEb;OIb]);Direct(Sar,[OEb;OIb])|]); (* 0xC0 *)
Group([|Direct(Rol,[OEv;OIb]);Direct(Ror,[OEv;OIb]);Direct(Rcl,[OEv;OIb]);Direct(Rcr,[OEv;OIb]);Direct(Shl,[OEv;OIb]);Direct(Shr,[OEv;OIb]);Direct(Sal,[OEv;OIb]);Direct(Sar,[OEv;OIb])|]);
Direct(Ret,[OIw]);
Direct(Ret,[]);
Direct(Les,[OGz;OMp]);
Direct(Lds,[OGz;OMp]);
Group([|Direct(Mov,[OEb;OIb]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Group([|Direct(Mov,[OEv;OIz]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
Direct(Enter,[OIw;OIb]);
Direct(Leave,[]);
Direct(Retf,[OIw]);
Direct(Retf,[]);
Direct(Int3,[]);
Direct(Int,[OIb]);
Direct(Into,[]);
Predicated(PRED_OpSize(Direct(Iretw,[]),Direct(Iretd,[]))); (* Revisit for 64-bit *)
Group([|Direct(Rol,[OEb;O1 ]);Direct(Ror,[OEb;O1 ]);Direct(Rcl,[OEb;O1 ]);Direct(Rcr,[OEb;O1 ]);Direct(Shl,[OEb;O1 ]);Direct(Shr,[OEb;O1 ]);Direct(Sal,[OEb;O1 ]);Direct(Sar,[OEb;O1 ])|]); (* 0xD0 *)
Group([|Direct(Rol,[OEv;O1 ]);Direct(Ror,[OEv;O1 ]);Direct(Rcl,[OEv;O1 ]);Direct(Rcr,[OEv;O1 ]);Direct(Shl,[OEv;O1 ]);Direct(Shr,[OEv;O1 ]);Direct(Sal,[OEv;O1 ]);Direct(Sar,[OEv;O1 ])|]);
Group([|Direct(Rol,[OEb;OCL]);Direct(Ror,[OEb;OCL]);Direct(Rcl,[OEb;OCL]);Direct(Rcr,[OEb;OCL]);Direct(Shl,[OEb;OCL]);Direct(Shr,[OEb;OCL]);Direct(Sal,[OEb;OCL]);Direct(Sar,[OEb;OCL])|]);
Group([|Direct(Rol,[OEv;OCL]);Direct(Ror,[OEv;OCL]);Direct(Rcl,[OEv;OCL]);Direct(Rcr,[OEv;OCL]);Direct(Shl,[OEv;OCL]);Direct(Shr,[OEv;OCL]);Direct(Sal,[OEv;OCL]);Direct(Sar,[OEv;OCL])|]);
Direct(Aam,[OIb]);
Direct(Aad,[OIb]);
Direct(Salc,[]);
Predicated(PRED_AddrSize(Direct(Xlat,[(*OMbBx*)]),Direct(Xlat,[(*OMbEbx*)])));

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
    Direct(Fadd, [OSt0;OReal4]);
    Direct(Fmul, [OSt0;OReal4]);
    Direct(Fcom, [OSt0;OReal4]);
    Direct(Fcomp,[OSt0;OReal4]);
    Direct(Fsub, [OSt0;OReal4]);
    Direct(Fsubr,[OSt0;OReal4]);
    Direct(Fdiv, [OSt0;OReal4]);
    Direct(Fdivr,[OSt0;OReal4])|]),
  Group([|
    Direct(Fadd, [OSt0;OStN]);
    Direct(Fmul, [OSt0;OStN]);
    Direct(Fcom, [OSt0;OStN]);
    Direct(Fcomp,[OSt0;OStN]);
    Direct(Fsub, [OSt0;OStN]);
    Direct(Fsubr,[OSt0;OStN]);
    Direct(Fdiv, [OSt0;OStN]);
    Direct(Fdivr,[OSt0;OStN])|])));
    
(* 0xD9 *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fld,   [OReal4]);
    Invalid;
    Direct(Fst,   [OReal4]);
    Direct(Fstp,  [OReal4]);
    Direct(Fldenv,[OFPEnvLow]);
    Direct(Fldcw, [OMw]);
    Direct(Fstenv,[OFPEnvLow]);
    Direct(Fstcw, [OMw])|]),
  Group([|
    Direct(Fld, [OSt0;OStN]);
    Direct(Fxch,[OSt0;OStN]);
    LowGroup([|Direct(Fnop,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
    Invalid;
    LowGroup([|Direct(Fchs,[]);Direct(Fabs,[]);Invalid;Invalid;Direct(Ftst,[]);Direct(Fxam,[]);Invalid;Invalid|]);
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
    Direct(Fiadd, [OMd]);
    Direct(Fimul, [OMd]);
    Direct(Ficom, [OMd]);
    Direct(Ficomp,[OMd]);
    Direct(Fisub, [OMd]);
    Direct(Fisubr,[OMd]);
    Direct(Fidiv, [OMd]);
    Direct(Fidivr,[OMd]);|]),
  Group([|
    Direct(Fcmovb ,[OSt0;OStN]);
    Direct(Fcmove ,[OSt0;OStN]);
    Direct(Fcmovbe,[OSt0;OStN]);
    Direct(Fcmovu ,[OSt0;OStN]);
    Invalid;
    LowGroup([|Invalid;Direct(Fucompp,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
    Invalid;
    Invalid;|])));

(* 0xDB *)
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fild,  [OMd]);
    Direct(Fisttp,[OMd]);
    Direct(Fist,  [OMd]);
    Direct(Fistp, [OMd]);
    Invalid;
    Direct(Fld,   [OReal10]);
    Invalid;
    Direct(Fstp,  [OReal10])|]),
  Group([|
    Direct(Fcmovnb, [OSt0;OStN]);
    Direct(Fcmovne, [OSt0;OStN]);
    Direct(Fcmovnbe,[OSt0;OStN]);
    Direct(Fcmovnu, [OSt0;OStN]);
    LowGroup([|Invalid;Invalid;Direct(Fclex,[]);Direct(Finit,[]);Invalid;Invalid;Invalid;Invalid|]);
    Direct(Fucomi,  [OSt0;OStN]);
    Direct(Fcomi,   [OSt0;OStN]);
    Invalid;
    |])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fadd, [OSt0;OReal8]);
    Direct(Fmul, [OSt0;OReal8]);
    Direct(Fcom, [OSt0;OReal8]);
    Direct(Fcomp,[OSt0;OReal8]);
    Direct(Fsub, [OSt0;OReal8]);
    Direct(Fsubr,[OSt0;OReal8]);
    Direct(Fdiv, [OSt0;OReal8]);
    Direct(Fdivr,[OSt0;OReal8])|]),
  Group([|
    Direct(Fadd, [OStN;OSt0]);
    Direct(Fmul, [OStN;OSt0]);
    Invalid;
    Invalid;
    Direct(Fsub, [OStN;OSt0]);
    Direct(Fsubr,[OStN;OSt0]);
    Direct(Fdiv, [OStN;OSt0]);
    Direct(Fdivr,[OStN;OSt0])|])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fld,   [OReal8]);
    Direct(Fisttp,[OMq]);
    Direct(Fst,   [OReal8]);
    Direct(Fstp,  [OReal8]);
    Direct(Frstor,[OFPEnv]);
    Invalid;
    Direct(Fsave ,[OFPEnv]);
    Direct(Fstsw, [OMw])|]),
  Group([|
    Direct(Ffree, [OStN]);
    Invalid;
    Direct(Fst,   [OStN]);
    Direct(Fstp,  [OStN]);
    Direct(Fucom, [OStN]);
    Direct(Fucomp,[OStN]);
    Invalid;
    Invalid;|])));

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fiadd, [OMw]);
    Direct(Fimul, [OMw]);
    Direct(Ficom, [OMw]);
    Direct(Ficomp,[OMw]);
    Direct(Fisub, [OMw]);
    Direct(Fisubr,[OMw]);
    Direct(Fidiv, [OMw]);
    Direct(Fidivr,[OMw])|]),
  Group([|
    Direct(Faddp, [OStN;OSt0]);
    Direct(Fmulp, [OStN;OSt0]);
    Invalid;
    LowGroup([|Invalid;Direct(Fcompp,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
    Direct(Fsubrp,[OStN;OSt0]);
    Direct(Fsubp, [OStN;OSt0]);
    Direct(Fdivrp,[OStN;OSt0]);
    Direct(Fdivp, [OStN;OSt0])|])));
    
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fild,  [OMw]);
    Direct(Fisttp,[OMw]);
    Direct(Fist,  [OMw]);
    Direct(Fistp, [OMw]);
    Direct(Fbld,  [OReal10]);
    Direct(Fild,  [OReal8]);
    Direct(Fbstp, [OReal10]);
    Direct(Fistp, [OReal8])|]),
  Group([|
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    LowGroup([|Direct(Fstsw,[OAX]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
    Direct(Fucomip,[OSt0;OStN]);
    Direct(Fcomip, [OSt0;OStN]);
    Invalid|])));

Direct(Loopnz,[OJb]); (* 0xE0 *)
Direct(Loopz,[OJb]);
Direct(Loop,[OJb]);
Predicated(PRED_AddrSize(Direct(Jcxz,[OJb]),Direct(Jecxz,[OJb])));
Direct(In,[OAL;OIb]);
Direct(In,[OeAX;OIb]);
Direct(Out,[OIb;OAL]);
Direct(Out,[OIb;OeAX]);
Direct(Call,[OJz]);
Direct(Jmp,[OJz]);
Direct(JmpF,[OAp]);
Direct(Jmp,[OJb]);
Direct(In,[OAL;ODX]);
Direct(In,[OeAX;ODX]);
Direct(Out,[ODX;OAL]);
Direct(Out,[ODX;OeAX]);
Fatal; (* 0xF0 *)
Direct(Icebp,[]);
Fatal;
Fatal;
Direct(Hlt,[]);
Direct(Cmc,[]);
Group([|Direct(Test,[OEb;OIb]);Invalid;Direct(Not,[OEb]);Direct(Neg,[OEb]);Direct(Mul,[OEb]);Direct(Imul,[OEb]);Direct(Div,[OEb]);Direct(Idiv,[OEb])|]);
Group([|Direct(Test,[OEv;OIz]);Invalid;Direct(Not,[OEv]);Direct(Neg,[OEv]);Direct(Mul,[OEv]);Direct(Imul,[OEv]);Direct(Div,[OEv]);Direct(Idiv,[OEv])|]);
Direct(Clc,[]);
Direct(Stc,[]);
Direct(Cli,[]);
Direct(Sti,[]);
Direct(Cld,[]);
Direct(Std,[]);
Group([|Direct(Inc,[OEb]);Direct(Dec,[OEb]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid;|]);
Group([|Direct(Inc,[OEv]);Direct(Dec,[OEv]);Direct(Call,[OEv]);Direct(CallF,[OEv]);Direct(Jmp,[OEv]);Direct(JmpF,[OEv]);Direct(Push,[OEv]);Invalid;|]);

Group([| (* 0x100 *)
  Predicated(PRED_ModRMMemReg(Direct(Sldt,[OMw]),Direct(Sldt,[ORv])));
  Predicated(PRED_ModRMMemReg(Direct(Str ,[OMw]),Direct(Str ,[ORv])));
  Direct(Lldt,[OEw]);
  Direct(Ltr ,[OEw]);
  Direct(Verr,[OEw]);
  Direct(Verw,[OEw]);
  Invalid;
  Invalid;|]);

Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Sgdt,[OMs]);
    Direct(Sidt,[OMs]);
    Direct(Lgdt,[OMs]);
    Direct(Lidt,[OMs]);
    Direct(Smsw,[OMw]);
    Invalid;
    Direct(Lmsw,[OMw]);
    Direct(Invlpg,[OMb]);|]),
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
    LowGroup([|Direct(Monitor,[]);Direct(Mwait,[]);Invalid;Invalid;Invalid;Invalid;Invalid;Invalid|]);
    Invalid;
    Invalid;
    Direct(Smsw,[ORv]);
    Invalid;
    Direct(Lmsw,[ORw]);
    Invalid; (* Revisit for 64-bit (swapgs) *)
    |])));

Direct(Lar,[OGv;OEw]);
Direct(Lsl,[OGv;OEw]);
Invalid;
Direct(Syscall,[]);
Direct(Clts,[]);
Direct(Sysret,[]);
Direct(Invd,[]);
Direct(Wbinvd,[]);
Invalid;
Direct(Ud2,[]);
Invalid;
Direct(Nop,[OEv]);
Invalid;
Invalid;
Sse(Direct(Movups,[OVps;OWps]),Direct(Movss, [OVss;OWss]),Direct(Movupd,[OVpd;OWpd]),Direct(Movsd, [OVsd;OWsd])); (* 0x110 *)
Sse(Direct(Movups,[OWps;OVps]),Direct(Movss, [OWss;OVss]),Direct(Movupd,[OWpd;OVpd]),Direct(Movsd, [OWsd;OVsd]));
Sse(Predicated(PRED_ModRMMemReg(Direct(Movlps,[OVq;OMq]),Direct(Movhlps,[OVq;OUq]))),Direct(Movsldup,[OVq;OWq]),Direct(Movlpd,[OVq;OMq]),Direct(Movddup,[OVq;OWq]));
SseNo66(Direct(Movlps,[OMq;OVq]),Direct(Movlpd,[OMq;OVq]));
SseNo66(Direct(Unpcklpd,[OVpd;OWq]),Direct(Unpcklps,[OVps;OWq]));
SseNo66(Direct(Unpckhpd,[OVpd;OWq]),Direct(Unpckhps,[OVps;OWq]));
Sse(Predicated(PRED_ModRMMemReg(Direct(Movhps,[OVq;OMq]),Direct(Movlhps,[OVq;OUq]))),Direct(Movshdup,[OVq;OWq]),Direct(Movhpd,[OVq;OMq]),Invalid);
SseNo66(Direct(Movhps,[OMq;OVq]),Direct(Movhpd,[OMq;OVq]));
Predicated(PRED_ModRMMemReg(Group[|Direct(Prefetchnta,[OMb]);Direct(Prefetcht0,[OMb]);Direct(Prefetcht1,[OMb]);Direct(Prefetcht2,[OMb]);Invalid;Invalid;Invalid;Invalid|],Invalid));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Direct(Nop,[OEv]);
Direct(Mov,[ORd;OCd]); (* 0x120 *)
Direct(Mov,[ORd;ODd]);
Direct(Mov,[OCd;ORd]);
Direct(Mov,[ODd;ORd]);
Invalid;
Invalid;
Invalid;
Invalid;
SseNo66(Direct(Movaps,[OVpd;OWpd]),Direct(Movapd,[OVps;OWps]));
SseNo66(Direct(Movaps,[OWpd;OVpd]),Direct(Movapd,[OWps;OVps]));
Sse(Direct(Cvtpi2ps,[OVps;OQpi]),Direct(Cvtsi2ss,[OVss;OEd_q]),Direct(Cvtpi2pd,[OVpd;OQpi]),Direct(Cvtsi2sd,[OVsd;OEd_q]));
SseNo66(Direct(Movntps,[OMpd;OVpd]),Direct(Movntpd,[OMps;OVps]));
Sse(Direct(Cvttps2pi,[OPpi;OWps]),Direct(Cvttss2si,[OGd;OWss]),Direct(Cvttpd2pi,[OPpi;OWpd]),Direct(Cvttsd2si,[OGd;OWsd]));
Sse(Direct(Cvtps2pi,[OPpi;OWps]),Direct(Cvtss2si,[OGd_q;OWss]),Direct(Cvtpd2pi,[OPpi;OWpd]),Direct(Cvtsd2si,[OGd_q;OWsd]));
SseNo66(Direct(Ucomiss,[OVsd;OWsd]),Direct(Ucomisd,[OVss;OWss]));
SseNo66(Direct(Comiss, [OVsd;OWsd]),Direct(Comisd, [OVss;OWss]));
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
Direct(Cmovo ,[OGv;OEv]); (* 0x140 *)
Direct(Cmovno,[OGv;OEv]);
Direct(Cmovb ,[OGv;OEv]);
Direct(Cmovae,[OGv;OEv]);
Direct(Cmovz ,[OGv;OEv]);
Direct(Cmovnz,[OGv;OEv]);
Direct(Cmovbe,[OGv;OEv]);
Direct(Cmova ,[OGv;OEv]);
Direct(Cmovs ,[OGv;OEv]);
Direct(Cmovns,[OGv;OEv]);
Direct(Cmovp ,[OGv;OEv]);
Direct(Cmovnp,[OGv;OEv]);
Direct(Cmovl ,[OGv;OEv]);
Direct(Cmovge,[OGv;OEv]);
Direct(Cmovle,[OGv;OEv]);
Direct(Cmovg ,[OGv;OEv]);
SseNo66(Direct(Movmskps,[OGd;OUps]),Direct(Movmskpd,[OGd;OUpd])); (* 0x150 *)
Sse(Direct(Sqrtps,    [OVps;OWps]),Direct(Sqrtss,   [OVss;OWss]),Direct(Sqrtpd,    [OVpd;OWpd]),Direct(Sqrtsd,  [OVsd;OWsd]));
Sse(Direct(Rsqrtps,   [OVps;OWps]),Direct(Rsqrtss,  [OVss;OWss]),Invalid,                       Invalid);
Sse(Direct(Rcpps,     [OVps;OWps]),Direct(Rcpss,    [OVss;OWss]),Invalid,                       Invalid);
SseNo66(Direct(Andps, [OVps;OWps]),Direct(Andpd,    [OVpd;OWpd]));
SseNo66(Direct(Andnps,[OVps;OWps]),Direct(Andnpd,   [OVpd;OWpd]));
SseNo66(Direct(Orps,  [OVps;OWps]),Direct(Orpd,     [OVpd;OWpd]));
SseNo66(Direct(Xorps, [OVps;OWps]),Direct(Xorpd,    [OVpd;OWpd]));
Sse(Direct(Addps,     [OVps;OWps]),Direct(Addss,    [OVss;OWss]),Direct(Addpd,     [OVpd;OWpd]),Direct(Addsd,   [OVsd;OWsd]));
Sse(Direct(Mulps,     [OVps;OWps]),Direct(Mulss,    [OVss;OWss]),Direct(Mulpd,     [OVpd;OWpd]),Direct(Mulsd,   [OVsd;OWsd]));
Sse(Direct(Cvtps2pd,  [OVpd;OWps]),Direct(Cvtss2sd, [OVss;OWss]),Direct(Cvtpd2ps,  [OVps;OWpd]),Direct(Cvtsd2ss,[OVsd;OWsd]));
Sse(Direct(Cvtdq2ps,  [OVps;OWps]),Direct(Cvttps2dq,[OVdq;OWps]),Direct(Cvtps2dq,  [OVdq;OWps]),Invalid);
Sse(Direct(Subps,     [OVps;OWps]),Direct(Subss,    [OVss;OWss]),Direct(Subpd,     [OVpd;OWpd]),Direct(Subsd,   [OVsd;OWsd]));
Sse(Direct(Minps,     [OVps;OWps]),Direct(Minss,    [OVss;OWss]),Direct(Minpd,     [OVpd;OWpd]),Direct(Minsd,   [OVsd;OWsd]));
Sse(Direct(Divps,     [OVps;OWps]),Direct(Divss,    [OVss;OWss]),Direct(Divpd,     [OVpd;OWpd]),Direct(Divsd,   [OVsd;OWsd]));
Sse(Direct(Maxps,     [OVps;OWps]),Direct(Maxss,    [OVss;OWss]),Direct(Maxpd,     [OVpd;OWpd]),Direct(Maxsd,   [OVsd;OWsd]));
SseNo66(Direct(Punpcklbw,[OPq;OQd]),Direct(Punpcklbw,[OVdq;OWdq])); (* 0x160 *)
SseNo66(Direct(Punpcklwd,[OPq;OQd]),Direct(Punpcklwd,[OVdq;OWdq]));
SseNo66(Direct(Punpckldq,[OPq;OQd]),Direct(Punpckldq,[OVdq;OWdq]));
SseNo66(Direct(Packsswb, [OPq;OQd]),Direct(Packsswb, [OVdq;OWdq]));
SseNo66(Direct(Pcmpgtb,  [OPq;OQd]),Direct(Pcmpgtb,  [OVdq;OWdq]));
SseNo66(Direct(Pcmpgtw,  [OPq;OQd]),Direct(Pcmpgtw,  [OVdq;OWdq]));
SseNo66(Direct(Pcmpgtd,  [OPq;OQd]),Direct(Pcmpgtd,  [OVdq;OWdq]));
SseNo66(Direct(Packuswb, [OPq;OQd]),Direct(Packuswb, [OVdq;OWdq]));
SseNo66(Direct(Punpckhbw,[OPq;OQd]),Direct(Punpckhbw,[OVdq;OWdq]));
SseNo66(Direct(Punpckhwd,[OPq;OQd]),Direct(Punpckhwd,[OVdq;OWdq]));
SseNo66(Direct(Punpckhdq,[OPq;OQd]),Direct(Punpckhdq,[OVdq;OWdq]));
SseNo66(Direct(Packssdw, [OPq;OQd]),Direct(Packssdw, [OVdq;OWdq]));
Sse66(Direct(Punpcklqdq,[OVdq;OWdq]));
Sse66(Direct(Punpckhqdq,[OVdq;OWdq]));
SseNo66(Direct(Movd,[OPd;OEd_q]),Direct(Movd,[OVdq;OEd_q]));  (* Revisit on 64-bit *)
Sse(Direct(Movq,     [OPq ;OQq ]),Direct(Movdqu,   [OVdq;OWdq]),Direct(Movdqa,    [OVdq;OWdq]),Invalid);
Sse(Direct(Pshufw,   [OPq;OQq;OIb]),Direct(Pshufhw,[OVdq;OWdq;OIb]),Direct(Pshufd,[OVdq;OWdq;OIb]),Direct(Pshuflw,[OVdq;OWdq;OIb]));(* 0x170 *)

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrlw,[ONq;OIb]),Direct(Psrlw,[OUdq;OIb]));
    Invalid;
    SseNo66(Direct(Psraw,[ONq;OIb]),Direct(Psraw,[OUdq;OIb]));
    Invalid;
    SseNo66(Direct(Psllw,[ONq;OIb]),Direct(Psllw,[OUdq;OIb]));
    Invalid|])));

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrld,[ONq;OIb]),Direct(Psrld,[OUdq;OIb]));
    Invalid;
    SseNo66(Direct(Psrad,[ONq;OIb]),Direct(Psrad,[OUdq;OIb]));
    Invalid;
    SseNo66(Direct(Pslld,[ONq;OIb]),Direct(Pslld,[OUdq;OIb]));
    Invalid|])));

Predicated(PRED_ModRMMemReg(
  Invalid,
  Group([|
    Invalid;
    Invalid;
    SseNo66(Direct(Psrlq,[ONq;OIb]),Direct(Psrlq,[OUdq;OIb]));
    Sse66(Direct(Psrldq,[OUdq;OIb]));
    Invalid;
    Invalid;
    SseNo66(Direct(Psllq,[ONq;OIb]),Direct(Psllq,[OUdq;OIb]));
    Sse66(Direct(Pslldq,[OUdq;OIb]));|])));

SseNo66(Direct(Pcmpeqb,[OPq;OQq]),Direct(Pcmpeqb,[OVdq;OWdq]));
SseNo66(Direct(Pcmpeqw,[OPq;OQq]),Direct(Pcmpeqw,[OVdq;OWdq]));
SseNo66(Direct(Pcmpeqd,[OPq;OQq]),Direct(Pcmpeqd,[OVdq;OWdq]));
Direct(Emms,[]);
Direct(Vmread,[OEd;OGd]);  (* revisit for 64-bit *)
Direct(Vmwrite,[OEd;OGd]); (* revisit for 64-bit *)
Invalid;
Invalid;
Sse(Invalid,Invalid,Direct(Haddpd,[OVpd;OWpd]),Direct(Haddps,[OVps;OWps]));
Sse(Invalid,Invalid,Direct(Hsubpd,[OVpd;OWpd]),Direct(Hsubps,[OVps;OWps]));
Sse(Direct(Movd,[OEd_q;OPd]),Direct(Movq,[OVq;OWq]),Direct(Movd,[OEd_q;OVdq]),Invalid); (* Revisit on 64-bit *)
Sse(Direct(Movq,[OQq;OPq]),Direct(Movdqu,[OWdq;OVdq]),Direct(Movdqa,[OWdq;OVdq]),Invalid);
Direct(Jo ,[OJz]); (* 0x180 *)
Direct(Jno,[OJz]);
Direct(Jb ,[OJz]);
Direct(Jae,[OJz]);
Direct(Jz ,[OJz]);
Direct(Jnz,[OJz]);
Direct(Jbe,[OJz]);
Direct(Ja ,[OJz]);
Direct(Js ,[OJz]);
Direct(Jns,[OJz]);
Direct(Jp ,[OJz]);
Direct(Jnp,[OJz]);
Direct(Jl ,[OJz]);
Direct(Jge,[OJz]);
Direct(Jle,[OJz]);
Direct(Jg ,[OJz]);
Direct(Seto ,[OEb]); (* 0x190 *)
Direct(Setno,[OEb]);
Direct(Setb ,[OEb]);
Direct(Setae,[OEb]);
Direct(Setz ,[OEb]);
Direct(Setnz,[OEb]);
Direct(Setbe,[OEb]);
Direct(Seta ,[OEb]);
Direct(Sets ,[OEb]);
Direct(Setns,[OEb]);
Direct(Setp ,[OEb]);
Direct(Setnp,[OEb]);
Direct(Setl ,[OEb]);
Direct(Setge,[OEb]);
Direct(Setle,[OEb]);
Direct(Setg ,[OEb]);
Direct(Push,[OFS]); (* 0x1A0 *)
Direct(Pop,[OFS]);
Direct(Cpuid,[]);
Direct(Bt,[OEv;OGv]);
Direct(Shld,[OEv;OGv;OIb]);
Direct(Shld,[OEv;OGv;OCL]);
Invalid;
Invalid;
Direct(Push,[OGS]);
Direct(Pop,[OGS]);
Direct(Rsm,[]);
Direct(Bts,[OEv;OGv]);
Direct(Shrd,[OEv;OGv;OIb]);
Direct(Shrd,[OEv;OGv;OCL]);
Predicated(PRED_ModRMMemReg(
  Group([|
    Direct(Fxsave,[OSimdState]);
    Direct(Fxrstor,[OSimdState]);
    Direct(Ldmxcsr,[OMd]);
    Direct(Stmxcsr,[OMd]);
    Invalid;
    Invalid;
    Invalid;
    Direct(Clflush,[OMb])|]),
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
Direct(Imul,[OGv;OEv]);
Direct(Cmpxchg,[OEb;OGb]); (* 0x1B0 *)
Direct(Cmpxchg,[OEv;OGv]);
Direct(Lss,[OGv;OMp]);
Direct(Btr,[OEv;OGv]);
Direct(Lfs,[OGv;OMp]);
Direct(Lgs,[OGv;OMp]);
Direct(Movzx,[OGv;OEb]);
Direct(Movzx,[OGv;OEw]);
Direct(Popcnt,[OGv;OEv]);
Invalid; (* Group 10, all invalid *)
Group([|Invalid;Invalid;Invalid;Invalid;Direct(Bt ,[OEv;OIb]);Direct(Bts,[OEv;OIb]);Direct(Btr,[OEv;OIb]);Direct(Btc,[OEv;OIb])|]);
Direct(Btc,[OEv;OGv]);
Direct(Bsf,[OGv;OEv]);
Direct(Bsr,[OGv;OEv]);
Direct(Movsx,[OGv;OEb]);
Direct(Movsx,[OGv;OEw]);
Direct(Xadd,[OEb;OGb]);  (* 0x1C0 *)
Direct(Xadd,[OEv;OGv]);
Sse(Direct(Cmpps,[OVps;OWps;OIb]),Direct(Cmpss,[OVss;OWss;OIb]),Direct(Cmppd,[OVpd;OWpd;OIb]),Direct(Cmpsd,[OVsd;OWsd;OIb]));
SseNo(Direct(Movnti,[OMd_q;OGd_q]));
SseNo66(Direct(Pinsrw,[OPq;OEw;OIb]),  Direct(Pinsrw,[OVdq;OEw;OIb]));
SseNo66(Direct(Pextrw,[OGd;ONq;OIb]),  Direct(Pextrw,[OGd;OUdq;OIb]));
SseNo66(Direct(Shufps,[OVps;OWps;OIb]),Direct(Shufpd,[OVps;OWps;OIb]));
Predicated(PRED_ModRMMemReg(
  Group([|
    Invalid;
    Direct(Cmpxchg8b,[OMq]); (* Revisit on 64-bit *)
    Invalid;
    Invalid;
    Invalid;
    Invalid;
    Sse(Direct(Vmptrld,[OMq]),Direct(Vmxon,[OMq]),Direct(Vmclear,[OMq]),Invalid);
    SseNo(Direct(Vmptrst,[OMq]))|]),
  Invalid));
Direct(Bswap,[OeAX]);
Direct(Bswap,[OeCX]);
Direct(Bswap,[OeDX]);
Direct(Bswap,[OeBX]);
Direct(Bswap,[OeSP]);
Direct(Bswap,[OeBP]);
Direct(Bswap,[OeSI]);
Direct(Bswap,[OeDI]);
Sse(Invalid,Invalid,Direct(Addsubpd,[OVpd;OWpd]),Direct(Addsubps,[OVps;OWps])); (* 0x1D0 *)
SseNo66(Direct(Psrlw, [OPq;OQq]),Direct(Psrlw,  [OVdq;OWdq]));
SseNo66(Direct(Psrld, [OPq;OQq]),Direct(Psrld,  [OVdq;OWdq]));
SseNo66(Direct(Psrlq, [OPq;OQq]),Direct(Psrlq,  [OVdq;OWdq]));
SseNo66(Direct(Paddq, [OPq;OQq]),Direct(Paddq,  [OVdq;OWdq]));
SseNo66(Direct(Pmullw,[OPq;OQq]),Direct(Pmullw, [OVdq;OWdq]));
Sse(Invalid,Direct(Movq2dq,[OVdq;ONq]),Direct(Movq,   [OVdq;OWdq]),Direct(Movdq2q,[OPq;OUq]));
SseNo66(Direct(Pmovmskb,[OGd;ONq]),Direct(Pmovmskb,[OGd;OUdq]));
SseNo66(Direct(Psubusb,[OPq;OQq]), Direct(Psubusb,[OVdq;OWdq]));
SseNo66(Direct(Psubusw,[OPq;OQq]), Direct(Psubusw,[OVdq;OWdq]));
SseNo66(Direct(Pminub, [OPq;OQq]), Direct(Pminub, [OVdq;OWdq]));
SseNo66(Direct(Pand,   [OPq;OQq]), Direct(Pand,   [OVdq;OWdq]));
SseNo66(Direct(Paddusb,[OPq;OQq]), Direct(Paddusb,[OVdq;OWdq]));
SseNo66(Direct(Paddusw,[OPq;OQq]), Direct(Paddusw,[OVdq;OWdq]));
SseNo66(Direct(Pmaxub, [OPq;OQq]), Direct(Pmaxub, [OVdq;OWdq]));
SseNo66(Direct(Pandn,  [OPq;OQq]), Direct(Pandn,  [OVdq;OWdq]));
SseNo66(Direct(Pavgb,  [OPq;OQq]), Direct(Pavgb,  [OVdq;OWdq])); (* 0x1E0 *)
SseNo66(Direct(Psraw,  [OPq;OQq]), Direct(Psraw,  [OVdq;OWdq]));
SseNo66(Direct(Psrad,  [OPq;OQq]), Direct(Psrad,  [OVdq;OWdq]));
SseNo66(Direct(Pavgw,  [OPq;OQq]), Direct(Pavgw,  [OVdq;OWdq]));
SseNo66(Direct(Pmulhuw,[OPq;OQq]), Direct(Pmulhuw,[OVdq;OWdq]));
SseNo66(Direct(Pmulhw, [OPq;OQq]), Direct(Pmulhw, [OVdq;OWdq]));
Sse(Invalid,Direct(Cvtdq2pd,[OVpd;OWdq]),Direct(Cvttpd2dq,[OVdq;OWpd]),Direct(Cvtpd2dq,[OVdq;OWpd]));
SseNo66(Direct(Movntq,[OMq;OPq]),  Direct(Movntdq,[OMdq;OVdq]));
SseNo66(Direct(Psubsb, [OPq;OQq]), Direct(Psubsb, [OVdq;OWdq]));
SseNo66(Direct(Psubsw, [OPq;OQq]), Direct(Psubsw, [OVdq;OWdq]));
SseNo66(Direct(Pminsw, [OPq;OQq]), Direct(Pminsw, [OVdq;OWdq]));
SseNo66(Direct(Por,    [OPq;OQq]), Direct(Por,    [OVdq;OWdq]));
SseNo66(Direct(Paddsb, [OPq;OQq]), Direct(Paddsb, [OVdq;OWdq]));
SseNo66(Direct(Paddsw, [OPq;OQq]), Direct(Paddsw, [OVdq;OWdq]));
SseNo66(Direct(Pmaxsw, [OPq;OQq]), Direct(Pmaxsw, [OVdq;OWdq]));
SseNo66(Direct(Pxor,   [OPq;OQq]), Direct(Pxor,   [OVdq;OWdq]));
Sse(Invalid,Invalid,Invalid,Direct(Lddqu,[OVdq;OMdq])); (* 0x1F0 *)
SseNo66(Direct(Psllw,   [OPq;OQq]),Direct(Psllw,     [OVdq;OWdq]));
SseNo66(Direct(Pslld,   [OPq;OQq]),Direct(Pslld,     [OVdq;OWdq]));
SseNo66(Direct(Psllq,   [OPq;OQq]),Direct(Psllq,     [OVdq;OWdq]));
SseNo66(Direct(Pmuludq, [OPq;OQq]),Direct(Pmuludq,   [OVdq;OWdq]));
SseNo66(Direct(Pmaddwd, [OPq;OQq]),Direct(Pmaddwd,   [OVdq;OWdq]));
SseNo66(Direct(Psadbw,  [OPq;OQq]),Direct(Psadbw,    [OVdq;OWdq]));
SseNo66(Direct(Maskmovq,[OPq;ONq]),Direct(Maskmovdqu,[OVdq;OUdq]));
SseNo66(Direct(Psubb,   [OPq;OQq]),Direct(Psubb,     [OVdq;OWdq]));
SseNo66(Direct(Psubw,   [OPq;OQq]),Direct(Psubw,     [OVdq;OWdq]));
SseNo66(Direct(Psubd,   [OPq;OQq]),Direct(Psubd,     [OVdq;OWdq]));
SseNo66(Direct(Psubq,   [OPq;OQq]),Direct(Psubq,     [OVdq;OWdq]));
SseNo66(Direct(Paddb,   [OPq;OQq]),Direct(Paddb,     [OVdq;OWdq]));
SseNo66(Direct(Paddw,   [OPq;OQq]),Direct(Paddw,     [OVdq;OWdq]));
SseNo66(Direct(Paddd,   [OPq;OQq]),Direct(Paddd,     [OVdq;OWdq]));
Invalid;
SseNo66(Direct(Pshufb,   [OPq;OQq]),Direct(Pshufb,   [OVdq;OWdq])); (* 0x200 *)
SseNo66(Direct(Phaddw,   [OPq;OQq]),Direct(Phaddw,   [OVdq;OWdq]));
SseNo66(Direct(Phaddd,   [OPq;OQq]),Direct(Phaddd,   [OVdq;OWdq]));
SseNo66(Direct(Phaddsw,  [OPq;OQq]),Direct(Phaddsw,  [OVdq;OWdq]));
SseNo66(Direct(Pmaddubsw,[OPq;OQq]),Direct(Pmaddubsw,[OVdq;OWdq]));
SseNo66(Direct(Phsubw,   [OPq;OQq]),Direct(Phsubw,   [OVdq;OWdq]));
SseNo66(Direct(Phsubd,   [OPq;OQq]),Direct(Phsubd,   [OVdq;OWdq]));
SseNo66(Direct(Phsubsw,  [OPq;OQq]),Direct(Phsubsw,  [OVdq;OWdq]));
SseNo66(Direct(Psignb,   [OPq;OQq]),Direct(Psignb,   [OVdq;OWdq]));
SseNo66(Direct(Psignw,   [OPq;OQq]),Direct(Psignw,   [OVdq;OWdq]));
SseNo66(Direct(Psignd,   [OPq;OQq]),Direct(Psignd,   [OVdq;OWdq]));
SseNo66(Direct(Pmulhrsw, [OPq;OQq]),Direct(Pmulhrsw, [OVdq;OWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pblendvb,[OVdq;OWdq])); (* 0x210 *)
Invalid;
Invalid;
Invalid;
Sse66(Direct(Blendvps,[OVdq;OWdq]));
Sse66(Direct(Blendvpd,[OVdq;OWdq]));
Invalid;
Sse66(Direct(Ptest,[OVdq;OWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
SseNo66(Direct(Pabsb,[OPq;OQq]),Direct(Pabsb,[OVdq;OWdq]));
SseNo66(Direct(Pabsw,[OPq;OQq]),Direct(Pabsw,[OVdq;OWdq]));
SseNo66(Direct(Pabsd,[OPq;OQq]),Direct(Pabsd,[OVdq;OWdq]));
Invalid;
Sse66(Direct(Pmovsxbw,[OVdq;OUdq_Mq])); (* 0x220 *)
Sse66(Direct(Pmovsxbd,[OVdq;OUdq_Md]));
Sse66(Direct(Pmovsxbq,[OVdq;OUdq_Mw]));
Sse66(Direct(Pmovsxwd,[OVdq;OUdq_Mq]));
Sse66(Direct(Pmovsxwq,[OVdq;OUdq_Md]));
Sse66(Direct(Pmovsxdq,[OVdq;OUdq_Mq]));
Invalid;
Invalid;
Sse66(Direct(Pmuldq,  [OVdq;OWdq]));
Sse66(Direct(Pcmpeqq, [OVdq;OWdq]));
Sse66(Direct(Movntdqa,[OVdq;OMdq]));
Sse66(Direct(Packusdw,[OVdq;OWdq]));
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pmovzxbw,[OVdq;OUdq_Mq])); (* 0x230 *)
Sse66(Direct(Pmovzxbd,[OVdq;OUdq_Md]));
Sse66(Direct(Pmovzxbq,[OVdq;OUdq_Mw]));
Sse66(Direct(Pmovzxwd,[OVdq;OUdq_Mq]));
Sse66(Direct(Pmovzxwq,[OVdq;OUdq_Md]));
Sse66(Direct(Pmovzxdq,[OVdq;OUdq_Mq]));
Invalid;
Sse66(Direct(Pcmpgtq,   [OVdq;OWdq]));
Sse66(Direct(Pminsb,    [OVdq;OWdq]));
Sse66(Direct(Pminsd,    [OVdq;OWdq]));
Sse66(Direct(Pminuw,    [OVdq;OWdq]));
Sse66(Direct(Pminud,    [OVdq;OWdq]));
Sse66(Direct(Pmaxsb,    [OVdq;OWdq]));
Sse66(Direct(Pmaxsd,    [OVdq;OWdq]));
Sse66(Direct(Pmaxuw,    [OVdq;OWdq]));
Sse66(Direct(Pmaxud,    [OVdq;OWdq]));
Sse66(Direct(Pmulld,    [OVdq;OWdq])); (* 0x240 *)
Sse66(Direct(Phminposuw,[OVdq;OWdq]));
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
Direct(Crc32,[OGd;OEb]); (* 0x2F0 *)
Direct(Crc32,[OGd;OEv]);
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
Sse66(Direct(Roundps,[OVdq;OWdq;OIb]));
Sse66(Direct(Roundpd,[OVdq;OWdq;OIb]));
Sse66(Direct(Roundss,[OVss;OWss;OIb]));
Sse66(Direct(Roundsd,[OVsd;OWsd;OIb]));
Sse66(Direct(Blendps,[OVdq;OWdq;OIb]));
Sse66(Direct(Blendpd,[OVdq;OWdq;OIb]));
Sse66(Direct(Pblendw,[OVdq;OWdq;OIb]));
SseNo66(Direct(Palignr,[OPq;OQq;OIb]),Direct(Palignr,[OVdq;OWdq;OIb]));
Invalid; (* 0x310 *)
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pextrb,   [ORd_Mb;OVdq;OIb]));
Sse66(Direct(Pextrw,   [ORd_Mw;OVdq;OIb]));
Sse66(Direct(Pextrd,   [OEd;OVdq;OIb])); (* Revisit for 64-bit *)
Sse66(Direct(Extractps,[OEd;OVdq;OIb]));
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Invalid;
Sse66(Direct(Pinsrb,  [OVdq;OEd;OIb])); (* 0x320 *)
Sse66(Direct(Insertps,[OVdq;OUdq_Md;OIb]));
Sse66(Direct(Pinsrd,  [OVdq;OEd;OIb])); (* Revisit for 64-bit *)
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
Sse66(Direct(Dpps,   [OVdq;OWdq;OIb])); (* 0x340 *)
Sse66(Direct(Dppd,   [OVdq;OWdq;OIb]));
Sse66(Direct(Mpsadbw,[OVdq;OWdq;OIb]));
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
Sse66(Direct(Pcmpestrm,[OVdq;OWdq;OIb])); (* 0x360 *)
Sse66(Direct(Pcmpestri,[OVdq;OWdq;OIb]));
Sse66(Direct(Pcmpistrm,[OVdq;OWdq;OIb]));
Sse66(Direct(Pcmpistri,[OVdq;OWdq;OIb]));
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
  | Direct(m,l) -> (m,List.map (fun f -> (function_of_x86_abstract_operand f) ()) l,l)
  | Group(a) -> aux a.(!modrm_context.mreg)
  | Predicated(t) -> (match t with
    | PRED_OpSize  (w16,w32)    ->        opsize_predicate     (lazy (aux w16)) (lazy (aux w32))
    | PRED_AddrSize(w16,w32)    ->      addrsize_predicate     (lazy (aux w16)) (lazy (aux w32))
    | PRED_ModRMMemReg(mem,reg) -> modrm_mem_reg_discriminator (lazy (aux mem)) (lazy (aux reg)))
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
  let (m,o,aol) = decode table_index in
  let i = (m,o) in
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
  (instrbody,Int32.to_int (Int32.sub (get_cursor ()) ea),successors,aol)

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
        print il "}";
        print il "break;";
        print il ""
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
      print il2 "break;";
      print il2 "";
      print il2 "case 32:";
      print il2 "{";
      print_entry (il2+1) w32;
      print il2 "}";
      print il2 "break;";
      print il2 "";
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
      print il2 "break;";
      print il2 "";
      print il2 "case 32:";
      print il2 "{";
      print_entry (il2+1) w32;
      print il2 "}";
      print il2 "break;";
      print il2 "";
      print il "}"
    | Predicated(PRED_ModRMMemReg(mem,reg)) ->
      print il "switch(modrmcontext.byte >> 6)";
      print il "{";
      let il2 = il + 1 in
      print il2 "case 3:";
      print il2 "{";
      print_entry (il2+1) reg;
      print il2 "}";
      print il2 "break;";
      print il2 "";
      print il2 "default:";
      print il2 "{";
      print_entry (il2+1) mem;
      print il2 "}";
      print il2 "break;";
      print il2 "";
      print il "}"
    | LowGroup(arr) -> 
      print il "switch(modrmcontext.greg)";
      print il "{";
      let il2 = il + 1 in
      let rec aux il i entry = 
        print il (Printf.sprintf "case %d:" i);
        print il "{";
        print_entry (il+1) entry;
        print il "}";
        print il "break;";
        print il ""
      in 
      let _ = Array.iteri (aux (il2+1)) arr in
      print il "}"
    | Sse(ipno,ipF3,ip66,ipF2) -> print il "// SSE/all"
    | SseNo(ipno) -> print il "// SSE/no"
    | Sse66(ip66) -> print il "// SSE/66"
    | SseNo66(ipno,ip66) -> print il "// SSE/no,66"
    in print_entry 2 ip;
    print 1 "}";
    print 1 "break;";
    print 0 ""
  in
  make_hasmodrm ();
  Array.iteri print x86_instruction_production_table
