open X86
open X86InternalOperand
open X86TypeCheck
open X86Constraints

(* Funny probability trick here, do the math and you'll see why this algorithm
   chooses an element from the list uniformly at random, without knowing a 
   priori the size of the list:  on iteration i, the current element is chosen
   with 1/i probability.  It is mathematically correct (assuming the PRNG is 
   uniform, blah blah).  I learned this trick at a bar in Berkeley from a
   machine learning specialist. *)
let pick_random_element list =
  let rec aux i res = function
  | x::xs -> aux (i+1) (if Random.int i = 0 then Some(x) else res) xs 
  | [] -> res
  in aux 1 (None) list
let get_some = function | Some(s) -> s | _ -> failwith "empty random element list"
let pick_random_element x = get_some (pick_random_element x)

let oAx   = GeneralReg(Gw(Ax))
let oCx   = GeneralReg(Gw(Cx))
let oDx   = GeneralReg(Gw(Dx))
let oBx   = GeneralReg(Gw(Bx))
let oSp   = GeneralReg(Gw(Sp))
let oBp   = GeneralReg(Gw(Bp))
let oSi   = GeneralReg(Gw(Si))
let oDi   = GeneralReg(Gw(Di))
let oGw   = [oAx;oCx;oDx;oBx;oSp;oBp;oSi;oDi]
let oAl   = GeneralReg(Gb(Al))
let oCl   = GeneralReg(Gb(Cl))
let oDl   = GeneralReg(Gb(Dl))
let oBl   = GeneralReg(Gb(Bl))
let oAh   = GeneralReg(Gb(Ah))
let oCh   = GeneralReg(Gb(Ch))
let oDh   = GeneralReg(Gb(Dh))
let oBh   = GeneralReg(Gb(Bh))
let oGb   = [oAl;oCl;oDl;oBl;oAh;oCh;oDh;oBh]
let oEax  = GeneralReg(Gd(Eax))
let oEcx  = GeneralReg(Gd(Ecx))
let oEdx  = GeneralReg(Gd(Edx))
let oEbx  = GeneralReg(Gd(Ebx))
let oEsp  = GeneralReg(Gd(Esp))
let oEbp  = GeneralReg(Gd(Ebp))
let oEsi  = GeneralReg(Gd(Esi))
let oEdi  = GeneralReg(Gd(Edi))
let oGd   = [oEax;oEcx;oEdx;oEbx;oEsp;oEbp;oEsi;oEdi]
let oCS   = SegReg(CS)
let oDS   = SegReg(DS)
let oES   = SegReg(ES)
let oGS   = SegReg(GS)
let oFS   = SegReg(FS)
let oSS   = SegReg(SS)
let oSw   = [oCS;oDS;oES;oGS;oFS;oSS]
let oCR0  = ControlReg(CR0)
let oCR1  = ControlReg(CR1)
let oCR2  = ControlReg(CR2)
let oCR3  = ControlReg(CR3)
let oCR4  = ControlReg(CR4)
let oCR5  = ControlReg(CR5)
let oCR6  = ControlReg(CR6)
let oCR7  = ControlReg(CR7)
let oCRx  = [oCR0;oCR1;oCR2;oCR3;oCR4;oCR5;oCR6;oCR7]
let oDR0  = DebugReg(DR0)
let oDR1  = DebugReg(DR1)
let oDR2  = DebugReg(DR2)
let oDR3  = DebugReg(DR3)
let oDR4  = DebugReg(DR4)
let oDR5  = DebugReg(DR5)
let oDR6  = DebugReg(DR6)
let oDR7  = DebugReg(DR7)
let oDRx  = [oDR0;oDR1;oDR2;oDR3;oDR4;oDR5;oDR6;oDR7]
let oSt0  = FPUReg(ST0)
let oSt1  = FPUReg(ST1)
let oSt2  = FPUReg(ST2)
let oSt3  = FPUReg(ST3)
let oSt4  = FPUReg(ST4)
let oSt5  = FPUReg(ST5)
let oSt6  = FPUReg(ST6)
let oSt7  = FPUReg(ST7)
let oStN  = [oSt0;oSt1;oSt2;oSt3;oSt4;oSt5;oSt6;oSt7]
let oMM0  = MMXReg(MM0)
let oMM1  = MMXReg(MM1)
let oMM2  = MMXReg(MM2)
let oMM3  = MMXReg(MM3)
let oMM4  = MMXReg(MM4)
let oMM5  = MMXReg(MM5)
let oMM6  = MMXReg(MM6)
let oMM7  = MMXReg(MM7)
let oMMX  = [oMM0;oMM1;oMM2;oMM3;oMM4;oMM5;oMM6;oMM7]
let oXMM0 = XMMReg(XMM0)
let oXMM1 = XMMReg(XMM1)
let oXMM2 = XMMReg(XMM2)
let oXMM3 = XMMReg(XMM3)
let oXMM4 = XMMReg(XMM4)
let oXMM5 = XMMReg(XMM5)
let oXMM6 = XMMReg(XMM6)
let oXMM7 = XMMReg(XMM7)
let oXMM  = [oXMM0;oXMM1;oXMM2;oXMM3;oXMM4;oXMM5;oXMM6;oXMM7]

let rnd_word    () = Immediate(Iw(Random.int32 0x10000l))     
let rnd_dword   () = Immediate(Id(Random.int32 Int32.max_int))
let rnd_oreg8   () = pick_random_element oGb
let rnd_oreg16  () = pick_random_element oGw
let rnd_oreg32  () = pick_random_element oGd
let rnd_segreg  () = pick_random_element oSw
let rnd_ctrlreg () = pick_random_element oCRx
let rnd_dbgreg  () = pick_random_element oDRx
let rnd_fpureg  () = pick_random_element oStN
let rnd_mmxreg  () = pick_random_element oMMX
let rnd_xmmreg  () = pick_random_element oXMM

let rnd_seg     () = pick_random_element [CS;DS;ES;GS;FS;SS]
let rnd_reg8    () = pick_random_element [Al;Cl;Dl;Bl;Ah;Ch;Dh;Bh]
let rnd_reg16   () = pick_random_element [Ax;Cx;Dx;Bx;Sp;Bp;Si;Di]
let rnd_reg32   () = pick_random_element [Eax;Ecx;Edx;Ebx;Esp;Ebp;Esi;Edi]
                                          
let size_16_32_f f16 f32 = function       
| Some(OPSZ_16) -> f16 ()                 
| Some(OPSZ_32) -> f32 ()                 
| None -> if Random.bool () then f16 () else f32 ()
| Some(_) -> invalid_arg "size_16_32_f"

let size_16_32 v16 v32 so = size_16_32_f (fun () -> v16) (fun () -> v32) so

let size_32_64_f f32 f64 = function
| Some(OPSZ_32) -> f32 ()
| Some(OPSZ_64) -> f64 ()
| None -> if Random.bool () then f32 () else f64 ()
| Some(_) -> invalid_arg "size_32_64_f"

let size_32_48_f f32 f48 = function
| Some(OPSZ_32) -> f32 ()
| Some(OPSZ_48) -> f48 ()
| None -> if Random.bool () then f32 () else f48 ()
| Some(_) -> invalid_arg "size_32_48_f"

let generate_random_mem16 () =
  let rnd_displ_bp choose_none = 
    let i = if choose_none then Random.int 3 else 1 + (Random.int 2) in
    match i with
    | 0 -> None
    | 1 -> Some(Int32.logand 0xFFFFl (Int32.sub (Random.int32 256l) 128l))
    | 2 -> Some(Random.int32 0x10000l)
    | _ -> failwith "impossible"
  in
  let rnd_displ = fun _ -> rnd_displ_bp true in
  let b,s,d = match Random.int 8 with
  | 0 -> Some(Bx),Some(Si),rnd_displ()
  | 1 -> Some(Bx),Some(Di),rnd_displ()
  | 2 -> Some(Bp),Some(Si),rnd_displ()
  | 3 -> Some(Bp),Some(Di),rnd_displ()
  | 4 -> Some(Si),None,rnd_displ()
  | 5 -> Some(Di),None,rnd_displ()
  | 6 -> 
    if Random.bool ()
    then None,None,Some(Random.int32 0x10000l)
    else Some(Bp),None,rnd_displ_bp false
  | 7 -> Some(Bx),None,rnd_displ()
  | _ -> failwith "Impossible"
  in Mem16(rnd_seg(),b,s,d)
  
let generate_random_mem32 () =
  let rnd_sf () = Random.int 4 in
  let rnd_scale () =
    match Random.int 7 with
    | 0 -> Some(Eax,rnd_sf ())
    | 1 -> Some(Ecx,rnd_sf ())
    | 2 -> Some(Edx,rnd_sf ())
    | 3 -> Some(Ebx,rnd_sf ())
    | 4 -> Some(Ebp,rnd_sf ())
    | 5 -> Some(Esi,rnd_sf ())
    | 6 -> Some(Edi,rnd_sf ())
    | _ -> failwith "impossible"
  in
  let rnd_displ () = 
    match Random.int 3 with
    | 0 -> None
    | 1 -> Some(Int32.sub (Random.int32 256l) 128l)
    | 2 -> Some(Random.int32 Int32.max_int)
    | _ -> failwith "impossible"
  in
  let b,s,d = match Random.int 7 with
  | 0 -> Some(rnd_reg32 ()),None,None
  | 1 -> Some(rnd_reg32 ()),None,rnd_displ ()
  | 2 -> Some(rnd_reg32 ()),rnd_scale(),None
  | 3 -> Some(rnd_reg32 ()),rnd_scale(),rnd_displ ()
  | 4 -> None,rnd_scale (),None
  | 5 -> None,rnd_scale (),rnd_displ ()
  | 6 -> None,None,Some(Random.int32 Int32.max_int)
  | _ -> failwith "impossible"
  in Mem32(rnd_seg(),b,s,d)
  
let mem_pretty = function
| Mem16(s,Some(b),sr,Some(i))       when i = 0l -> Mem16(s,Some(b),sr,None)
| Mem32(s,Some(b),sr,Some(i))       when i = 0l -> Mem32(s,Some(b),sr,None)
| Mem32(s,None,Some(sr,sf),Some(i)) when i = 0l -> Mem32(s,None,Some(sr,sf),None)
| m -> m

let rnd_memexpr () = mem_pretty (if Random.bool () then generate_random_mem16 () else generate_random_mem32 ())
let rnd_mb  () = Memexpr(Mb (rnd_memexpr ())) 
let rnd_mw  () = Memexpr(Mw (rnd_memexpr ())) 
let rnd_md  () = Memexpr(Md (rnd_memexpr ())) 
let rnd_mf  () = Memexpr(Mf (rnd_memexpr ())) 
let rnd_mq  () = Memexpr(Mq (rnd_memexpr ())) 
let rnd_mt  () = Memexpr(Mt (rnd_memexpr ())) 
let rnd_mdq () = Memexpr(Mdq(rnd_memexpr ()))

let rnd_mem_reg bmem fmem freg = if bmem && Random.bool () then fmem () else freg ()

let rnd_ew bmem = rnd_mem_reg bmem rnd_mw rnd_oreg16
let rnd_ed bmem = rnd_mem_reg bmem rnd_md rnd_oreg32

let rnd_o16 () = Mem16(rnd_seg(),None,None,Some(Random.int32 0x10000l))
let rnd_o32 () = Mem32(rnd_seg(),None,None,Some(Random.int32 Int32.max_int))

let rec rnd_xx = function
| Some(OPSZ_16) -> Mem16(rnd_seg (),Some(Si) ,None,None)
| Some(OPSZ_32) -> Mem32(rnd_seg (),Some(Esi),None,None)
| Some(_) -> failwith "rnd_xx:  unknown address size"
| None -> if Random.bool () then rnd_xx (Some(OPSZ_16)) else rnd_xx (Some(OPSZ_32))

let rec rnd_yx = function
| Some(OPSZ_16) -> Mem16(ES,Some(Di) ,None,None)
| Some(OPSZ_32) -> Mem32(ES,Some(Edi),None,None)
| Some(_) -> failwith "rnd_yx:  unknown address size"
| None -> if Random.bool () then rnd_yx (Some(OPSZ_16)) else rnd_yx (Some(OPSZ_32))

let generate_operand bmem so ao = function
| OAL
| OALR8L  -> oAl
| OCL
| OCLR9L  -> oCl
| ODLR10L -> oDl
| OBLR11L -> oBl
| OAHR12L -> oAh
| OCHR13L -> oCh
| ODHR14L -> oDh
| OBHR15L -> oBh
(* Expect to change this again soon!!1 *)
| OAx     -> oAx
| ODx     -> oDx
| OeAX
| OrAXr8
| OrAX    -> size_16_32 oAx oEax so
| OeCX
| OrCXr9  -> size_16_32 oCx oEcx so
| OeDX
| OrDXr10 -> size_16_32 oDx oEdx so
| OeBX
| OrBXr11 -> size_16_32 oBx oEbx so
| OeSP
| OrSPr12 -> size_16_32 oSp oEsp so
| OeBP
| OrBPr13 -> size_16_32 oBp oEbp so
| OeSI
| OrSIr14 -> size_16_32 oSi oEsi so
| OeDI
| OrDIr15 -> size_16_32 oDi oEdi so
| OCS     -> oCS
| ODS     -> oDS
| OES     -> oES
| OGS     -> oGS
| OFS     -> oFS
| OSS     -> oSS
| OSw     -> rnd_segreg ()
| OCd     -> rnd_ctrlreg ()
| ODd     -> rnd_dbgreg ()
| OIb     -> Immediate(Ib(Random.int32 0x100l))
| OIw     -> rnd_word ()
| OIv
| OIz     -> size_16_32_f rnd_word rnd_dword so
| O1      -> Immediate(Ib(0x1l))
| OGb     -> rnd_oreg8 ()
| ORw
| OGw     -> rnd_oreg16 ()
| ORd
| OGd_q
| OGd     -> rnd_oreg32 ()
| ORv
| OGv
| OGz     -> size_16_32_f rnd_oreg16 rnd_oreg32 so

| OMb     -> rnd_mb ()
| OMw     -> rnd_mw ()
| OReal4
| OMd
| OMd_q   -> rnd_md ()
| OMs     -> rnd_mf ()
| OReal8
| OMq     -> rnd_mq ()
| OReal10 -> rnd_mt ()
| OMpd
| OMps
| OMdq    -> rnd_mdq ()

| OMa     -> size_32_64_f rnd_md rnd_mq so
| OMp     -> size_32_48_f rnd_md rnd_mf so
| OM      -> size_16_32_f rnd_mw rnd_md so
| OFPEnv
| OFPEnvLow
| OSimdState -> rnd_md ()

| OEb     -> rnd_mem_reg bmem rnd_mb rnd_oreg8
| OEw     -> rnd_ew bmem
| OEd
| OEd_q   -> rnd_ed bmem
| OEv     -> size_16_32_f (fun () -> (rnd_ew bmem)) (fun () -> (rnd_ed bmem)) so

| OSt0    -> oSt0
| OStN    -> rnd_fpureg ()

| OOb     -> Memexpr(Mb(if Random.bool () then rnd_o16 () else rnd_o32 ()))
| OOv     -> size_16_32_f 
               (fun () -> Memexpr(Mw(if Random.bool () then rnd_o16 () else rnd_o32 ())))
               (fun () -> Memexpr(Md(if Random.bool () then rnd_o16 () else rnd_o32 ())))
               so
(* BUG HERE *)
| OAp      -> 
  if Random.bool ()
  then FarTarget(Ap16(Random.int32 0x10000l,     Random.int32 Int32.max_int))
  else FarTarget(Ap32(Random.int32 Int32.max_int,Random.int32 Int32.max_int))

| OJb
| OJz      -> failwith "random jump not supported just yet"

| OXb -> Memexpr(Mb(rnd_xx ao))
| OXw -> Memexpr(Mw(rnd_xx ao))
| OXd -> Memexpr(Md(rnd_xx ao))
| OXv 
| OXz -> size_16_32_f (fun () -> Memexpr(Mw(rnd_xx ao))) (fun () -> Memexpr(Md(rnd_xx ao))) so

| OYb -> Memexpr(Mb(rnd_yx ao))
| OYw -> Memexpr(Mw(rnd_yx ao))
| OYd -> Memexpr(Md(rnd_yx ao))
| OYv
| OYz -> size_16_32_f (fun () -> Memexpr(Mw(rnd_yx ao))) (fun () -> Memexpr(Md(rnd_yx ao))) so

| ONq
| OPd
| OPq
| OPpi -> rnd_mmxreg ()
| OUps
| OUpd
| OUq
| OUdq
| OVdq
| OVpd
| OVps
| OVsd
| OVss
| OVq  -> rnd_xmmreg ()
| OQpi
| OQd
| OQq  -> rnd_mem_reg bmem rnd_mq rnd_mmxreg
| OWdq
| OWps
| OWpd
| OWq  -> rnd_mem_reg bmem rnd_mdq rnd_xmmreg
| OWss -> rnd_mem_reg bmem rnd_md rnd_xmmreg
| OWsd -> rnd_mem_reg bmem rnd_mq rnd_xmmreg
| OUdq_Md -> rnd_mem_reg bmem rnd_md rnd_xmmreg
| OUdq_Mq -> rnd_mem_reg bmem rnd_mq rnd_xmmreg
| OUdq_Mw -> rnd_mem_reg bmem rnd_mw rnd_xmmreg
| ORd_Mb  -> rnd_mem_reg bmem rnd_mb rnd_oreg32
| ORd_Mw  -> rnd_mem_reg bmem rnd_mw rnd_oreg32



let x86_mnem_arr = [|
  Aaa;
  Aad;
  Aam;
  Aas;
  Adc;
  Add;
  Addpd;
  Addps;
  Addsd;
  Addss;
  Addsubpd;
  Addsubps;
  And;
  Andnpd;
  Andnps;
  Andpd;
  Andps;
  Arpl;
  Blendpd;
  Blendps;
  Blendvpd;
  Blendvps;
  Bound;
  Bsf;
  Bsr;
  Bswap;
  Bt;
  Btc;
  Btr;
  Bts;
  Call;
  CallF;
  Cbw;
  Cdq;
  Clc;
  Cld;
  Clflush;
  Cli;
  Clts;
  Cmc;
  Cmova;
  Cmovae;
  Cmovb;
  Cmovbe;
  Cmovg;
  Cmovge;
  Cmovl;
  Cmovle;
  Cmovno;
  Cmovnp;
  Cmovns;
  Cmovnz;
  Cmovo;
  Cmovp;
  Cmovs;
  Cmovz;
  Cmp;
  Cmppd;
  Cmpps;
  Cmpsb;
  Cmpsd;
  Cmpss;
  Cmpsw;
  Cmpxchg;
  Cmpxchg8b;
  Comisd;
  Comiss;
  Cpuid;
  Crc32;
  Cvtdq2pd;
  Cvtdq2ps;
  Cvtpd2dq;
  Cvtpd2pi;
  Cvtpd2ps;
  Cvtpi2pd;
  Cvtpi2ps;
  Cvtps2dq;
  Cvtps2pd;
  Cvtps2pi;
  Cvtsd2si;
  Cvtsd2ss;
  Cvtsi2sd;
  Cvtsi2ss;
  Cvtss2sd;
  Cvtss2si;
  Cvttpd2dq;
  Cvttpd2pi;
  Cvttps2dq;
  Cvttps2pi;
  Cvttsd2si;
  Cvttss2si;
  Cwd;
  Cwde;
  Daa;
  Das;
  Dec;
  Div;
  Divpd;
  Divps;
  Divsd;
  Divss;
  Dppd;
  Dpps;
  Emms;
  Enter;
  Extractps;
  F2xm1;
  Fabs;
  Fadd;
  Faddp;
  Fbld;
  Fbstp;
  Fchs;
  Fclex;
  Fcmovb;
  Fcmovbe;
  Fcmove;
  Fcmovnb;
  Fcmovnbe;
  Fcmovne;
  Fcmovnu;
  Fcmovu;
  Fcom;
  Fcomi;
  Fcomip;
  Fcomp;
  Fcompp;
  Fcos;
  Fdecstp;
  Fdiv;
  Fdivp;
  Fdivr;
  Fdivrp;
  Ffree;
  Fiadd;
  Ficom;
  Ficomp;
  Fidiv;
  Fidivr;
  Fild;
  Fimul;
  Fincstp;
  Finit;
  Fist;
  Fistp;
  Fisttp;
  Fisub;
  Fisubr;
  Fld;
  Fld1;
  Fldcw;
  Fldenv;
  Fldl2e;
  Fldl2t;
  Fldlg2;
  Fldln2;
  Fldpi;
  Fldz;
  Fmul;
  Fmulp;
  Fnop;
  Fpatan;
  Fprem;
  Fprem1;
  Fptan;
  Frndint;
  Frstor;
  Fsave;
  Fscale;
  Fsin;
  Fsincos;
  Fsqrt;
  Fst;
  Fstcw;
  Fstenv;
  Fstp;
  Fstsw;
  Fsub;
  Fsubp;
  Fsubr;
  Fsubrp;
  Ftst;
  Fucom;
  Fucomi;
  Fucomip;
  Fucomp;
  Fucompp;
  Fxam;
  Fxch;
  Fxrstor;
  Fxsave;
  Fxtract;
  Fyl2x;
  Fyl2xp1;
  Getsec;
  Haddpd;
  Haddps;
  Hlt;
  Hsubpd;
  Hsubps;
  Icebp;
  Idiv;
  Imul;
  In;
  Inc;
  Insb;
  Insd;
  Insertps;
  Insw;
  Int;
  Int3;
  Into;
  Invd;
  Invlpg;
  Iretd;
  Iretw;
  Ja;
  Jae;
  Jb;
  Jbe;
  Jcxz;
  Jecxz;
  Jg;
  Jge;
  Jl;
  Jle;
  Jmp;
  JmpF;
  Jno;
  Jnp;
  Jns;
  Jnz;
  Jo;
  Jp;
  Js;
  Jz;
  Lahf;
  Lar;
  Lddqu;
  Ldmxcsr;
  Lds;
  Lea;
  Leave;
  Les;
  Lfence;
  Lfs;
  Lgdt;
  Lgs;
  Lidt;
  Lldt;
  Lmsw;
  Lodsb;
  Lodsd;
  Lodsw;
  Loop;
  Loopnz;
  Loopz;
  Lsl;
  Lss;
  Ltr;
  Maskmovdqu;
  Maskmovq;
  Maxpd;
  Maxps;
  Maxsd;
  Maxss;
  Mfence;
  Minpd;
  Minps;
  Minsd;
  Minss;
  Monitor;
  Mov;
  Movapd;
  Movaps;
  Movd;
  Movddup;
  Movdq2q;
  Movdqa;
  Movdqu;
  Movhlps;
  Movhpd;
  Movhps;
  Movlhps;
  Movlpd;
  Movlps;
  Movmskpd;
  Movmskps;
  Movntdq;
  Movntdqa;
  Movnti;
  Movntpd;
  Movntps;
  Movntq;
  Movq;
  Movq2dq;
  Movsb;
  Movsd;
  Movshdup;
  Movsldup;
  Movss;
  Movsw;
  Movsx;
  Movupd;
  Movups;
  Movzx;
  Mpsadbw;
  Mul;
  Mulpd;
  Mulps;
  Mulsd;
  Mulss;
  Mwait;
  Neg;
  Nop;
  Not;
  Or;
  Orpd;
  Orps;
  Out;
  Outsb;
  Outsd;
  Outsw;
  Pabsb;
  Pabsd;
  Pabsw;
  Packssdw;
  Packsswb;
  Packusdw;
  Packuswb;
  Paddb;
  Paddd;
  Paddq;
  Paddsb;
  Paddsw;
  Paddusb;
  Paddusw;
  Paddw;
  Palignr;
  Pand;
  Pandn;
  Pause;
  Pavgb;
  Pavgw;
  Pblendvb;
  Pblendw;
  Pcmpeqb;
  Pcmpeqd;
  Pcmpeqq;
  Pcmpeqw;
  Pcmpestri;
  Pcmpestrm;
  Pcmpgtb;
  Pcmpgtd;
  Pcmpgtq;
  Pcmpgtw;
  Pcmpistri;
  Pcmpistrm;
  Pextrb;
  Pextrd;
  Pextrw;
  Phaddd;
  Phaddsw;
  Phaddw;
  Phminposuw;
  Phsubd;
  Phsubsw;
  Phsubw;
  Pinsrb;
  Pinsrd;
  Pinsrw;
  Pmaddubsw;
  Pmaddwd;
  Pmaxsb;
  Pmaxsd;
  Pmaxsw;
  Pmaxub;
  Pmaxud;
  Pmaxuw;
  Pminsb;
  Pminsd;
  Pminsw;
  Pminub;
  Pminud;
  Pminuw;
  Pmovmskb;
  Pmovsxbd;
  Pmovsxbq;
  Pmovsxbw;
  Pmovsxdq;
  Pmovsxwd;
  Pmovsxwq;
  Pmovzxbd;
  Pmovzxbq;
  Pmovzxbw;
  Pmovzxdq;
  Pmovzxwd;
  Pmovzxwq;
  Pmuldq;
  Pmulhrsw;
  Pmulhuw;
  Pmulhw;
  Pmulld;
  Pmullw;
  Pmuludq;
  Pop;
  Popad;
  Popaw;
  Popcnt;
  Popfd;
  Popfw;
  Por;
  Prefetchnta;
  Prefetcht0;
  Prefetcht1;
  Prefetcht2;
  Psadbw;
  Pshufb;
  Pshufd;
  Pshufhw;
  Pshuflw;
  Pshufw;
  Psignb;
  Psignd;
  Psignw;
  Pslld;
  Pslldq;
  Psllq;
  Psllw;
  Psrad;
  Psraw;
  Psrld;
  Psrldq;
  Psrlq;
  Psrlw;
  Psubb;
  Psubd;
  Psubq;
  Psubsb;
  Psubsw;
  Psubusb;
  Psubusw;
  Psubw;
  Ptest;
  Punpckhbw;
  Punpckhdq;
  Punpckhqdq;
  Punpckhwd;
  Punpcklbw;
  Punpckldq;
  Punpcklqdq;
  Punpcklwd;
  Push;
  Pushad;
  Pushaw;
  Pushfd;
  Pushfw;
  Pxor;
  Rcl;
  Rcpps;
  Rcpss;
  Rcr;
  Rdmsr;
  Rdpmc;
  Rdtsc;
  Ret;
  Retf;
  Rol;
  Ror;
  Roundpd;
  Roundps;
  Roundsd;
  Roundss;
  Rsm;
  Rsqrtps;
  Rsqrtss;
  Sahf;
  Sal;
  Salc;
  Sar;
  Sbb;
  Scasb;
  Scasd;
  Scasw;
  Seta;
  Setae;
  Setb;
  Setbe;
  Setg;
  Setge;
  Setl;
  Setle;
  Setno;
  Setnp;
  Setns;
  Setnz;
  Seto;
  Setp;
  Sets;
  Setz;
  Sfence;
  Sgdt;
  Shl;
  Shld;
  Shr;
  Shrd;
  Shufpd;
  Shufps;
  Sidt;
  Sldt;
  Smsw;
  Sqrtpd;
  Sqrtps;
  Sqrtsd;
  Sqrtss;
  Stc;
  Std;
  Sti;
  Stmxcsr;
  Stosb;
  Stosd;
  Stosw;
  Str;
  Sub;
  Subpd;
  Subps;
  Subsd;
  Subss;
  Syscall;
  Sysenter;
  Sysexit;
  Sysret;
  Test;
  Ucomisd;
  Ucomiss;
  Ud2;
  Unpckhpd;
  Unpckhps;
  Unpcklpd;
  Unpcklps;
  Verr;
  Verw;
  Vmcall;
  Vmclear;
  Vmlaunch;
  Vmptrld;
  Vmptrst;
  Vmread;
  Vmresume;
  Vmwrite;
  Vmxoff;
  Vmxon;
  Wait;
  Wbinvd;
  Wrmsr;
  Xadd;
  Xlat;
  Xchg;
  Xor;
  Xorpd;
  Xorps;|]
  
let x86_mnem_arr_len = Array.length x86_mnem_arr
  
let get_random_mnem () = x86_mnem_arr.(Random.int x86_mnem_arr_len)

let list_of_tuple  (a,b)   = [a;b]
let list_of_triple (a,b,c) = [a;b;c]

let generate_random_instruction excl =
  let rec choose_mnem () =
    let mnem = get_random_mnem () in
    if excl mnem
    then choose_mnem ()
    else mnem
  in
(*print_endline "About to pick mnem";*)
  let mnem = choose_mnem () in
(*print_endline ("Chose mnem "^X86Disasm.string_of_x86mnem mnem);*)
  let aopndl,(opcnstr,adcnstr) = pick_random_element (X86EncodeTable.mnem_to_encodings mnem) in
  let aux scl acl = 
    let _,opndl = List.fold_left 
     (fun ((scl,acl),optriplelist) aop ->
        match scl,acl with
        | [],[]        -> (([],[]),(aop,None,None)::optriplelist)
        | s::ss, a::az -> ((ss,az),(aop,Some(s),Some(a))::optriplelist)
        | s::ss, []    -> ((ss,[]),(aop,Some(s),None)::optriplelist)
        | [],    a::az -> (([],az),(aop,None,Some(a))::optriplelist))
      ((scl,acl),[])
      aopndl
    in List.rev opndl
  in
  let op_size_constraint_list =
    match opcnstr with
    | OpNone      -> []
    | Op12SizeEq
    | Op12GvMa
    | Op12GvMp
    | Op12GzMp    -> list_of_tuple  (pick_random_element (satisfying_size_configurations2 opcnstr))
    | Op123SizeEq -> list_of_triple (pick_random_element (satisfying_size_configurations3 opcnstr))
  in
  let addr_size_constraint_list =
    match adcnstr with
    | AddrNone -> []
    | Addr12SizeEq -> list_of_tuple (pick_random_element (satisfying_addr_configurations2 adcnstr))
  in
  let opnd_dec_list = aux op_size_constraint_list addr_size_constraint_list in
  (* Now we have a list like (Abstract Operand, Size of Operand option) *)
  (mnem,List.map (fun (a,so,ao) -> generate_operand true so ao a) opnd_dec_list)