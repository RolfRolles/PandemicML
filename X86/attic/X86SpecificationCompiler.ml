(*
What would I do if I was designing, from scratch, something that I wanted to 
call an "automated disassembler/assembler generator"?

First I would need information about all of the encodings used by the processor.

This includes:
* Stem, including any mandatory prefixes and escape sequences
* Mnemonic
* Description of operands

The operands themselves correspond to bundles of semantic information.

The encoding for an Md operand specifies that the ModRM MUST specify a register.
It is an error if this is not true.

In the decoder, the Md operand type checks the MOD term and fails if necessary.
In the encoder, the Md operand simply generates the instruction without caring.

Let's say I'm only doing this for x86 for now, but I still want to use this 
idea of using a language to do it.

What will I need?

The ability to specify prefixes.
The Mod/RM concept will need special attention.

The logic for decoding is:
* Eat byte, decompose into 3 sub-byte quantities
* Special case: no SIB (m0d == 3 || rm != 4)
* * Special case: plain DISP32 (m0d == 0 && rm == 5)
* * Special case: register (m0d == 3): return register number
* * Special case: memory location (m0d != 3):
* * * Special case: does the location have a displacement? (mod == 1 || mod == 2)?
* Special case: SIB
* * Eat byte, decompose into 3 sub-byte quantities
* * Obtain scale register number (index <> 4)
* * Obtain scale factor (index <> 4)
* * Obtain base register number (sreg <> 5 || m0d == 1 || m0d == 2)
* * Match over: MOD
* * * 0: displacement if sreg==5
* * * 1: displacement
* * * 2: displacement
* * * _: error

This logic should be triggered when:
* The ModR/M table indicates the presence of the byte
* Operating in 32-bit mode
* There is different logic for operating in 16-bit mode.

* Consume designated prefixes
* Consult ModR/M table
* Decode ModR/M
* Eat byte
* Fetch indexed entry from the table
* Through recursive application, ultimately obtain the instruction description
* Record the mnemonic
* Get the list of operands
* Trigger operand decoding to make structified operands
* Done

We'll need some reserved words in our language: eat8,eat16,eat32,put8,etc.

How about a new data type:  a union that describes an encoding?

Plans so far:

* Write a lexer and a parser for this functional language we have here.
* Types including int, int32, bool, bytes, option, * for tuple, -> for function.
* Variants with and without parameters
* First-class functions

* Special support for:
* * enc_type:  An ordinary variant, no parameters, with a specified encoding
* * populated_type:  A variant that is empty, but is filled with anything
    observed to inhabit a designated populated_type variable.
*)

enc_type prefixes =
| 0x66 PF4_OpSize

enc_type x86_general_reg32 =
| 0 Eax
| 1 Ecx
| 2 Edx
| 3 Ebx
| 4 Esp
| 5 Ebp
| 6 Esi
| 7 Edi

enc_type x86_general_reg16 =
| 0 Ax
| 1 Cx
| 2 Dx
| 3 Bx
| 4 Sp
| 5 Bp
| 6 Si
| 7 Di

enc_type x86_general_reg8 =
| 0 Al
| 1 Cl
| 2 Dl
| 3 Bl
| 4 Ah
| 5 Ch
| 6 Dh
| 7 Bh

enc_type x86_segment_reg =
| 0 ES 
| 1 CS 
| 2 SS 
| 3 DS 
| 4 FS 
| 5 GS 

enc_type x86_mmx_reg =
| 0 MM0
| 1 MM1
| 2 MM2
| 3 MM3
| 4 MM4
| 5 MM5
| 6 MM6
| 7 MM7

enc_type x86_xmm_reg =
| 0 XMM0
| 1 XMM1
| 2 XMM2
| 3 XMM3
| 4 XMM4
| 5 XMM5
| 6 XMM6
| 7 XMM7

enc_type x86_debug_reg =
| 0 DR0
| 1 DR1
| 2 DR2
| 3 DR3
| 4 DR4
| 5 DR5
| 6 DR6
| 7 DR7

enc_type x86_control_reg = 
| 0 CR0
| 1 CR1
| 2 CR2
| 3 CR3
| 4 CR4
| 5 CR5
| 6 CR6
| 7 CR7

enc_type x86_fpu_reg =
| 0 ST0
| 1 ST1
| 2 ST2
| 3 ST3
| 4 ST4
| 5 ST5
| 6 ST6
| 7 ST7

type x86_general_reg =
| Gb of x86_general_reg8
| Gd of x86_general_reg32
| Gw of x86_general_reg16

type x86_immediate = 
| Ib of int32
| Id of int32
| Iw of int32

type x86_addr_expr =
| Mem16 of x86_segreg * x86_reg16 option * x86_reg16 option * int32 option
| Mem32 of x86_segreg * x86_reg32 option * (x86_reg32 * int) option * (int32 option)

type x86_far_target =
| Ap32 of int32 * int32 (** 16-bit seg, 32-bit offset *)
| Ap16 of int32 * int32 (** 16-bit seg, 16-bit offset *)

type x86_mem_size =
| Mb
| Mw
| Md
| Mf
| Mq
| Mt
| Mdq

type x86operand =
| GeneralReg of x86_general_reg
| ControlReg of x86_control_reg
| DebugReg of x86_debug_reg
| SegReg of x86_segreg
| FPUReg of x86_fpureg
| MMXReg of x86_mmxreg
| XMMReg of x86_xmmreg
| Immediate of x86_immediate
| Memexpr of x86_mem_size * x86_mem_expr
| JccTarget of int32 * int32
| FarTarget of x86_far_target

type x86instr = x86mnem * x86operand option * x86operand option * x86operand option
type x86_group1_prefix = Rep | Repne | Lock
type x86instrpref = { pref: x86_group1_prefix list; instr: x86instr; }

type x86_flags = X86F_C | X86F_P | X86F_A | X86F_S | X86F_Z | X86F_O | X86F_D

populated_type operands
populated_type mnemonics

type operand =
{
  enum: operands;
  decode: dec_context -> x86operand;
  encode: enc_context -> x86operand -> _bytes;
  match:  x86operand  -> _bool;
}

(* Hmm, interesting conundrum here.  I want to automatically associate the 
   variant OGb with the oGb structure. 
   
let populate operands OGb associate opnd2struct OGb oGb =
   *)
let oGb =
{
  enum = OGb;
  decode = (fun d -> GeneralReg(Gb(_decode x86_general_reg8 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (function | GeneralReg(Gb(_)) -> true | _ -> false);
}

let oGw =
{
  enum = OGw;
  decode = (fun d -> GeneralReg(Gw(_decode x86_general_reg16 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (function | GeneralReg(Gw(_)) -> true | _ -> false);
}

let oGd =
{
  enum = OGd;
  decode = (fun d -> GeneralReg(Gd(_decode x86_general_reg32 d.modrm.greg)));
  encode = (fun e o -> match o with | GeneralReg(Gd(d)) -> { e with modrm.greg = _encode x86_general_reg32 );
  match  = (function | GeneralReg(Gd(_)) -> true | _ -> false);
}

let oGv =
{
  enum = OGd;
  decode = 
   (fun d -> 
      if d.opsize = OPND_32 
      then GeneralReg(Gd(_decode x86_general_reg32 d.modrm.greg))
      else GeneralReg(Gw(_decode x86_general_reg16 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (fun x -> oGd.match x || oGw.match x);
}

let oGz =
{
  enum = OGz;
  decode = oGv.decode;
  encode = oGv.encode;
  match  = oGv.match;
}

let oGd_q =
{
  enum = OGd_q;
  decode = oGd.decode;
  encode = oGd.encode;
  match  = oGd.match;
}

let oNq =
{
  enum = ONq;
  decode = (fun d -> if d.modrm.is_mem then _invalid else MMXReg(_decode x86_mmx_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (function | MMXReg(_) -> true | _ -> false);
}

let oPpi =
{
  enum = OPpi;
  decode = (fun d -> MMXReg(_decode x86_mmx_reg d.modrm.gpart));
  encode = (fun e o -> );
  match  = oNq.match;
}

let oPq = { enum = OPq; decode = oPpi.decode; encode = oPpi.encode; match  = oPpi.match; }
let oPq = { enum = OPd; decode = oPpi.decode; encode = oPpi.encode; match  = oPpi.match; }

let oVdq =
{
  enum = OVdq;
  decode = (fun d -> _decode x86_xmm_reg d.modrm.gpart);
  encode = (fun e o -> );
  match  = (function | XMMReg(_) -> true | _ -> false);
}

let oVss = { enum = OVss; decode = oVdq.decode; encode = oVdq.encode; match  = oVdq.match; }
let oVsd = { enum = OVsd; decode = oVdq.decode; encode = oVdq.encode; match  = oVdq.match; }
let oVps = { enum = OVps; decode = oVdq.decode; encode = oVdq.encode; match  = oVdq.match; }
let oVpd = { enum = OVpd; decode = oVdq.decode; encode = oVdq.encode; match  = oVdq.match; }
let oVq  = { enum = OVq;  decode = oVdq.decode; encode = oVdq.encode; match  = oVdq.match; }

let oCd =
{
  enum = OCd;
  decode = (fun d -> ControlReg(_decode x86_control_reg d.modrm.gpart));
  encode = (fun e o -> );
  match  = (function | ControlReg(_) -> true | _ -> false);
}

let oDd =
{
  enum = ODd;
  decode = (fun d -> DebugReg(_decode x86_debug_reg d.modrm.gpart));
  encode = (fun e o -> );
  match  = (function | DebugReg(_) -> true | _ -> false);
}

let oDd =
{
  enum = ODd;
  decode = (fun d -> DebugReg(_decode x86_debug_reg d.modrm.gpart));
  encode = (fun e o -> );
  match  = (function | DebugReg(_) -> true | _ -> false);
}

let oMb =
{
  enum = OMb;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mb,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mb,_) -> true | _ -> false);
}

let oMw =
{
  enum = OMw;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mw,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mw,_) -> true | _ -> false);
}

let oMd =
{
  enum = OMd;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Md,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Md,_) -> true | _ -> false);
}

let oMf =
{
  enum = OMf;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mf,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mf,_) -> true | _ -> false);
}

let oMq =
{
  enum = OMq;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mq,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mq,_) -> true | _ -> false);
}

let oMt =
{
  enum = OMt;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mt,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mt,_) -> true | _ -> false);
}

let oMdq =
{
  enum = OMdq;
  decode = (fun d -> if d.modrm.is_mem then Memexpr(Mdq,decode_mem_expr d) else _invalid);
  encode = (fun e o -> );
  match  = (function | Memexpr(Mdq,_) -> true | _ -> false);
}

let oMs   = { enum = OMs;   decode =  oMf.decode; encode =  oMf.encode; match  =  oMf.match; }
let oMpd  = { enum = OMpd;  decode = oMdq.decode; encode = oMdq.encode; match  = oMdq.match; }
let oMps  = { enum = OMps;  decode = oMdq.decode; encode = oMdq.encode; match  = oMdq.match; }
let oMd_q = { enum = OMd_q; decode =  oMd.decode; encode =  oMd.encode; match  =  oMd.match; }

(* Points to a pointer that depends on the operand size (16:16 or 16:32) *)
let oMp   () = modrm_mem_reg_predicate (lazy (opsize_predicate (lazy (oMd ())) (lazy (oMf ())))) (lazy (invalid ()))
(* Points to two consecutive addresses, which are determined by the operand size *)
let oMa   () = modrm_mem_reg_predicate (lazy (opsize_predicate (lazy (oMd ())) (lazy (oMq ())))) (lazy (invalid ()))

let oEb =
{
  enum = OEb;
  decode = (fun d -> if d.modrm.is_mem then oMb.decode d else GeneralReg(Gb(_decode x86_general_reg8 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (fun x -> oMb.match x || oGb.match x);
}

let oEw =
{
  enum = OEw;
  decode = (fun d -> if d.modrm.is_mem then oMb.decode d else GeneralReg(Gw(_decode x86_general_reg16 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (fun x -> oMw.match x || oGw.match x);
}

let oEd =
{
  enum = OEd;
  decode = (fun d -> if d.modrm.is_mem then oMb.decode d else GeneralReg(Gd(_decode x86_general_reg32 d.modrm.greg)));
  encode = (fun e o -> );
  match  = (fun x -> oMd.match x || oGd.match x);
}

let oEv =
{
  enum = OEd;
  decode = (fun d -> if d.opsize = OPND_32 then oEd.decode d else oEw.decode d);
  encode = (fun e o -> );
  match  = (fun x -> oEd.match x || oEw.match x);
}

let oEd_q = { enum = OEd_q; decode =  oEv.decode; encode =  oEv.encode; match  =  oEv.match; }

let oQpi =
{
  enum = OQpi;
  decode = (fun d -> if d.modrm.is_mem then oMq.decode d else MMXReg(_decode x86_mmx_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMq.match x || oPpi.match x);
}

let oQd   = { enum = OQd; decode = oQpi.decode; encode = oQpi.encode; match = oQpi.match; }
let oQq   = { enum = OQq; decode = oQpi.decode; encode = oQpi.encode; match = oQpi.match; }

let oWdq =
{
  enum = OWdq;
  decode = (fun d -> if d.modrm.is_mem then oMdq.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMdq.match x || oVdq.match x);
}

let oWps   = { enum = OWps; decode = oWdq.decode; encode = oWdq.encode; match = oWdq.match; }
let oWpd   = { enum = OWpd; decode = oWdq.decode; encode = oWdq.encode; match = oWdq.match; }
let oWq    = { enum = OWq;  decode = oWdq.decode; encode = oWdq.encode; match = oWdq.match; }

let oWss =
{
  enum = OWss;
  decode = (fun d -> if d.modrm.is_mem then oMd.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMd.match x || oVdq.match x);
}

let oWsd =
{
  enum = OWsd;
  decode = (fun d -> if d.modrm.is_mem then oMq.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMq.match x || oVdq.match x);
}

let oUps =
{
  enum = OUps;
  decode = (fun d -> if d.modrm.is_mem then _invalid else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = oVdq.match;
}

let oUpd   = { enum = OUpd; decode = oUps.decode; encode = oUps.encode; match = oUps.match; }
let oUdq   = { enum = OUdq; decode = oUps.decode; encode = oUps.encode; match = oUps.match; }
let oUq    = { enum = OUq;  decode = oUps.decode; encode = oUps.encode; match = oUps.match; }

let oUdqMd =
{
  enum = OUdqMd;
  decode = (fun d -> if d.modrm.is_mem then oMd.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMd.match x || oVdq.match x);
}

let oUdqMq =
{
  enum = OUdqMq;
  decode = (fun d -> if d.modrm.is_mem then oMq.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMq.match x || oVdq.match x);
}

let oUdqMw =
{
  enum = OUdqMw;
  decode = (fun d -> if d.modrm.is_mem then oMw.decode d else XMMReg(_decode x86_xmm_reg d.modrm.epart));
  encode = (fun e o -> );
  match  = (fun x -> oMw.match x || oVdq.match x);
}

let o1 =
{
  enum = O1;
  decode = (fun d -> Immediate(Ib(0x1l)));
  encode = (fun e o -> e);
  match  = (function | Immediate(Ib(0x1l)) -> true | _ -> false);
}

let oIb =
{
  enum = OIb;
  decode = (fun d -> Immediate(Ib(eat8 ())));
  encode = (fun e o -> );
  match  = (function | Immediate(Ib(_)) -> true | _ -> false);
}

let oIw =
{
  enum = OIw;
  decode = (fun d -> Immediate(Iw(eat16 ())));
  encode = (fun e o -> );
  match  = (function | Immediate(Iw(_)) -> true | _ -> false);
}

let oId =
{
  enum = OId;
  decode = (fun d -> Immediate(Id(eat32 ())));
  encode = (fun e o -> );
  match  = (function | Immediate(Id(_)) -> true | _ -> false);
}

let oIbv =
{
  enum = OIbv;
  decode = 
   (fun d -> 
      if d.opsize = OPND_32 
      then Immediate(Id(sign_extend 8 32 (eat8 ())) 
      else Immediate(Iw(sign_extend 8 16 (eat8 ())));
  encode = (fun e o -> );
  match  = (fun x -> oIw.match x || oId.match x);
}

let oIz =
{
  enum = OIz;
  decode = (fun d -> if d.opsize = OPND_32 then oId.decode d else oIw.decode d);
  encode = (fun e o -> );
  match  = (fun x -> oIw.match x || oId.match x);
}

let oIv = { enum = OIv;  decode = oIz.decode; encode = oIz.encode; match = oIz.match; }

let oAL =
{
  enum = OAL;
  decode = (fun d -> GeneralReg(Gb(Al)));
  encode = (fun e o -> e);
  match  = (function | GeneralReg(Gb(Al)) -> true | _ -> false);
}

let oCL =
{
  enum = OCL;
  decode = (fun d -> GeneralReg(Gb(Cl)));
  encode = (fun e o -> e);
  match  = (function | GeneralReg(Gb(Cl)) -> true | _ -> false);
}

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
let oRw () = modrm_mem_reg_predicate (lazy (invalid ())) (lazy (opnd_reg16_of_int))
let oRd () = modrm_mem_reg_predicate (lazy (invalid ())) (lazy (opnd_reg32_of_int))
let oRv () = modrm_mem_reg_predicate (lazy (invalid ())) (lazy (opsize_predicate (lazy opnd_reg16_of_int) (lazy opnd_reg32_of_int)))
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

(*
I can simplify things a bit.  Let's get rid of populated_type.
Keep enc_type and introduce association, associate, and associative_image.

association decode_of_operand operand (decode_context -> decode_context)
association encode_of_operand operand (encode_context -> x86operand -> encode_context)
association  match_of_operand operand (x86operand -> bool)

let decode_oGv d = ...
let encode_oGv e o = ...
let match_oGv  x = ...

associate decode_of_operand OGv decode_oGv
associate encode_of_operand OGv encode_oGv
associate  match_of_operand OGv  match_oGv

associative_image decode_of_operand OGv
*)

(*
Some insights from the GSDL paper:

They specify instructions like this:

val main[00 /r] = binop ADD r/m8 /r

What I am envisioning right now is something similar to this, albeit using the
"raw" operand types.  I.e.

val main[00 /r] = binop Add OEb OGb

For decoding:

val binop d mnem op1t op2t =
  let op1 = associative_image decode_of_operand op1t d in
  let op2 = associative_image decode_of_operand op2t d in
  { d with mnem = mnem; op1 = op1; op2 = op2; }
  
For encoding:

val binop e mnem 

The encoding function has to go the other way.
We have to associate A

"binop" then looks up the OEb and OGb decoders, applies them in sequence,
and then returns (Add,[oEb reified;oGb reified]).  Basically, exactly what I
am already doing in the code I wrote manually.

So along these lines, I don't think what I have so far is bad.  The insights
from that paper are 
A) the val main[...] syntax for tables
B) the "sub-decoders" like /r, /0, etc.
C) the fact that I need to read the paper again with regards to prefixes.

I think I should basically continue what I am doing.

type decoder_context =
{
  prefixes
  stem eaten so far
  modrm option
  
}

type encoder_context


It's also going to need to return some prefixes if necessary.


*)


(*
Then we can generate encoders and decoders automatically.

Union type describing the prefixes.  Prefixes should be a reserved word.
Some way to specify the prefix's encoding byte.
Maybe this could all make up an array; [ NoPrefix, ..., PF4_Whatever, ... ]
Then I could just consult the array and have some sort of indicator noting 
the presence.

Next, there should be a fixed array specifying the ModR/M table.  The compiler
should generate this automatically somehow (might need to think about how).

Next, we'll need some way of specifying the logic for ModR/M decoding.



The logic for encoding is:
* Is group specified? if so, then we have our RM
* Encode G-part, get gpart
* Encode E-part, get components of epart





Stuff that I am going to need:

* A language for describing the disassembler logic.
* It's going to need:

*)
