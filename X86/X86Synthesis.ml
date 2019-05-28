(* 
#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\X86\\X86Synthesis.ml";; 

If one of the operands specifies that it is memory only or register only, put in a predicate

*)

(*
  Xlat;
  Loop;
  Loopnz;
  Loopz;
  Call;
  CallF;
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
*)

let x86_mnem_arr = 
  let open X86 in
[|
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
  Xchg;
  Xor;
  Xorpd;
  Xorps;|]

let l_encodings = 
  Array.fold_left (fun l mnem -> 
    let l_aol_pair_sc_ac_enc = X86Encode.mnem_to_encodings_full mnem in
    let l_massaged_list = 
      List.map 
       (fun ((l_ao,(sc,ac)),enc) -> mnem,l_ao,sc,sc,enc) 
        l_aol_pair_sc_ac_enc
    in
    l_massaged_list@l)
    []
  (*X86Random.*)x86_mnem_arr

(* Need to groom the encoder data set to recognize mandatory prefixes *)
type mandatory_prefix = 
| NoPrefix 
| Pfx66
| PfxF2
| PfxF3

let eat_mandatory_prefix = function
| 0x66l::bs -> Pfx66,bs
| 0xF2l::bs -> PfxF2,bs
| 0xF3l::bs -> PfxF3,bs
| bs        -> NoPrefix,bs

let mk_stem = function
| 0x0Fl::0x3Al::x::tl   -> 0x300 + (Int32.to_int x),tl
| 0x0Fl::0x38l::x::tl   -> 0x200 + (Int32.to_int x),tl
| 0x0Fl::x::tl          -> 0x100 + (Int32.to_int x),tl
| x::tl when x < 0x100l -> Int32.to_int x,tl
| _ -> invalid_arg "mk_stem"

let get_pfx_stem l =
  let pfx,l_i32 = eat_mandatory_prefix l in
  let stem,tl = mk_stem l_i32 in
  (pfx,stem,tl)

(* This dictates whether the encoding is only valid if the MOD specifies 
   either a register or a memory location (but does not allow both). 
   How to use this information:
   
   Check every instruction to see whether it requires a specific mod.
   If it does, we need to generate a predicated term.
   Considerations surrounding inserting that into the table:
   
   If we are inserting a predicated term, then either the existing term
   must be unspecified (in which case we simply replace it), or it must
   be a compatible predicated term where the corresponding arm is empty.
   
   That's easy enough, and I just wrote the code for it.  But now there's
   a new consideration.  I need to incorporate this stuff nicely into the
   table.  In particular, these terms only appear in ModRM groups or 
   adjacent to other ModRM instructions.  So I guess I only need to check
   them for those two encoding types.  Let's go read that code ...
   
   *)
let requires_specific_mod x = let open X86InternalOperand in match x with 
| OMb
| OMw
| OMd
| OMs
| OMq
| OMdq
| OMd_q
| OMa
| OMp
| OMpd
| OMps
| OM 

| OFPEnv
| OFPEnvLow
| OReal4
| OReal8
| OReal10 
| OSimdState -> Some(true)

| ONq
| ORw
| ORd
| ORv 
| OStN

| OUps
| OUpd
| OUq
| OUdq -> Some(false)

| _ -> None

let requires_specific_mod =
  let rec aux existing = function
  | [] -> existing
  | x::xs ->
    let o = requires_specific_mod x in
   (match o,existing with
    | o,None            -> aux o xs
    | o,e    when o = e -> aux o xs
    | None,e            -> aux e xs
    (* This is the case where neither one is none, i.e. they are both some(x), 
       and they did not equal one another, i.e. it was specified both that the
       encoding required a register, and that it required memory. *)
    | _,_ -> invalid_arg "requires_specific_mod:  MOD overconstrained")
  in aux (None)
  
type pred_type = PRED_OpSize      of table_contents * table_contents (* Size_16, Size_32 *)
               | PRED_AddrSize    of table_contents * table_contents (* Size_16, Size_32 *)
               | PRED_ModRMMemReg of table_contents * table_contents (* is mem, is reg *)
               | PRED_AddrOpSize  of table_contents * table_contents * 
                                     table_contents * table_contents (* A16O16, A16O32, A32O16, A32O32 *)

and table_contents = 
| Unspecified
| Fatal
| Direct of X86.x86mnem * X86InternalOperand.x86_abstract_operand list
| Group of table_contents array
| Predicated of pred_type
| LowGroup of table_contents array
(** No prefix, 66h, F2h, F3h *)
| MandatoryPrefix of table_contents * table_contents * table_contents * table_contents

let rec tbl_to_str = function
| Unspecified -> "Unspecified"
| Fatal -> "Fatal"
| Direct(mnem,ops) -> 
  let l = List.map X86InternalOperand.string_of_x86_abstract_operand ops in
  let b = Buffer.create 3 in
  List.iter (fun op -> Buffer.add_string b op; Buffer.add_char b ';') l;
  "Direct("^(X86Disasm.string_of_x86mnem mnem)^",["^Buffer.contents b^"])"
| Group(arr) -> 
  let b = Buffer.create 8 in
  Array.iter (fun tbl -> let s = tbl_to_str tbl in Buffer.add_string b s; Buffer.add_char b ';') arr;
  "Group("^Buffer.contents b^")"
| Predicated(PRED_OpSize(w16,w32))   -> "Predicated(PRED_OpSize("  ^tbl_to_str w16^","^tbl_to_str w32^"))"
| Predicated(PRED_AddrSize(w16,w32)) -> "Predicated(PRED_AddrSize("^tbl_to_str w16^","^tbl_to_str w32^"))"
| Predicated(PRED_ModRMMemReg(m,r))  -> "Predicated(PRED_ModRMMemReg("^tbl_to_str m^","^tbl_to_str r^"))"
| Predicated(PRED_AddrOpSize(a1616,a1632,a3216,a3232)) -> 
  "Predicated(PRED_AddrOpSize("^tbl_to_str a1616^","^tbl_to_str a1632^","^tbl_to_str a3216^","^tbl_to_str a3232^"))"
| LowGroup(arr) -> 
  let b = Buffer.create 8 in
  Array.iter (fun tbl -> let s = tbl_to_str tbl in Buffer.add_string b s; Buffer.add_char b ';') arr;
  "LowGroup("^Buffer.contents b^")"
| MandatoryPrefix(a,b,c,d) ->
  "MandatoryPrefix("^
  tbl_to_str a^","^
  tbl_to_str b^","^
  tbl_to_str c^","^
  tbl_to_str d^"))"

let process_one_encoding decarr has_modrm (mnem,l_ao,sc,ac,enc)  =
  let _ = IDA.msg "%s " (X86Disasm.string_of_x86mnem mnem) in
  let l = List.map X86InternalOperand.string_of_x86_abstract_operand l_ao in
  List.iter (IDA.msg "%s, ") l;
  IDA.msg "\n%!";

  let element = Direct(mnem,l_ao) in

  let rec aux pfx = function
  | Unspecified -> 
   (match pfx with
    | NoPrefix -> element
    | Pfx66 -> MandatoryPrefix(Unspecified,element,Unspecified,Unspecified)
    | PfxF2 -> MandatoryPrefix(Unspecified,Unspecified,element,Unspecified)
    | PfxF3 -> MandatoryPrefix(Unspecified,Unspecified,Unspecified,element))
  | Direct(m,l) as dir when pfx <> NoPrefix -> 
      aux
      pfx 
     (MandatoryPrefix(dir,Unspecified,Unspecified,Unspecified))
  | MandatoryPrefix(a,b,c,d) -> 
   (match pfx with
    | NoPrefix when a = Unspecified -> MandatoryPrefix(element,b,c,d)
    | Pfx66    when b = Unspecified -> MandatoryPrefix(a,element,c,d)
    | PfxF2    when c = Unspecified -> MandatoryPrefix(a,b,element,d)
    | PfxF3    when d = Unspecified -> MandatoryPrefix(a,b,c,element)
    | _ -> invalid_arg "update_mandatory_prefix 1")
  | _ -> invalid_arg "update_mandatory_prefix 2"
  in
  let update_mandatory_prefix b = 
    let pfx,stem,tl = get_pfx_stem b in 
    if tl <> []
    then IDA.msg "update_mandatory_prefix: tail bytes\n" 
    else
      let mp = aux pfx decarr.(stem) in
      decarr.(stem) <- mp
  in
  let update_size_predicated stem b32 = 
    match decarr.(stem) with 
    | Unspecified ->
     (match b32 with
      | true  -> Predicated(PRED_OpSize(Unspecified,element))
      | false -> Predicated(PRED_OpSize(element,Unspecified)))
    | Predicated(PRED_OpSize(t16,t32)) ->
     (match b32 with
      | true  when t32 = Unspecified -> Predicated(PRED_OpSize(t16,element))
      | false when t16 = Unspecified -> Predicated(PRED_OpSize(element,t32))
      | _ -> invalid_arg "update_size_predicated 1")
    | _ -> invalid_arg "update_size_predicated 2"
  in
  let update_size_predicated b b32 = 
    let pfx,stem,tl = get_pfx_stem b in 
    if tl <> []
    then IDA.msg "update_size_predicated: tail bytes\n" 
    else
      let mp = update_size_predicated stem b32 in
      decarr.(stem) <- mp
  in
  
  (* Get rid of reference to decarr.(stem), element
     Pass element in as an argument *)
  let update_mod_predicated stem bm = 
    match decarr.(stem) with 
    | Unspecified ->
     (match bm with
      | false -> Predicated(PRED_ModRMMemReg(Unspecified,element))
      | true -> Predicated(PRED_ModRMMemReg(element,Unspecified)))
    | Predicated(PRED_ModRMMemReg(mem,reg)) ->
     (match bm with
      | true  when mem = Unspecified -> Predicated(PRED_ModRMMemReg(element,reg))
      | false when reg = Unspecified -> Predicated(PRED_ModRMMemReg(mem,element))
      | _ -> invalid_arg "update_mod_predicated 1")
    | _ -> invalid_arg "update_mod_predicated 2"
  in
  let update_mod_predicated b bm = 
    let pfx,stem,tl = get_pfx_stem b in 
    if tl <> []
    then IDA.msg "update_mod_predicated: tail bytes\n" 
    else
      let mp = update_mod_predicated stem bm in
      decarr.(stem) <- mp
  in
  let signify_modrm b = 
    let _,stem,tl = get_pfx_stem b in 
    if tl <> []
    then IDA.msg "signify_modrm: tail bytes\n" 
    else
      has_modrm.(stem) <- true
  in
  let update_group b32 i o_bm = 
    let pfx,stem,tl = get_pfx_stem b32 in 
    if tl <> []
    then IDA.msg "update_group: tail bytes\n" 
    else
      let x = decarr.(stem) in
      match o_bm,x with 
      | None,Unspecified ->
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        decarr.(stem) <- Group(empty_group)
      
      | Some(true),Unspecified ->
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(empty_group),Unspecified))

      | Some(false),Unspecified ->
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Unspecified,Group(empty_group)))

      | None,Group(arr) -> 
        arr.(i) <- element
        (*IDA.msg "update_group:  adding non-mod-predicated instruction to mod predicate?\n"*)

      | Some(true),Group(arr) -> 
        let arr2 = Array.copy arr in
        arr2.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(arr2),Group(arr)))

      | Some(false),Group(arr) -> 
        let arr2 = Array.copy arr in
        arr2.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(arr),Group(arr2)))

      | None,Predicated(PRED_ModRMMemReg(Unspecified,Unspecified)) -> 
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        let eg2 = Array.copy empty_group in
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(empty_group),Group(eg2)))
      | None,Predicated(PRED_ModRMMemReg(Group(amem),Unspecified)) -> 
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
       (if amem.(i) = Unspecified
        then amem.(i) <- element
        else IDA.msg "update_group: replacing specified group element?\n");
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(amem),Group(empty_group)))
      | None,Predicated(PRED_ModRMMemReg(Unspecified,Group(areg))) -> 
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
       (if areg.(i) = Unspecified
        then areg.(i) <- element
        else IDA.msg "update_group: replacing specified group element?\n");
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(empty_group),Group(areg)))
      | None,Predicated(PRED_ModRMMemReg(Group(amem),Group(areg))) -> 
       (if amem.(i) = Unspecified
        then amem.(i) <- element
        else IDA.msg "update_group: replacing specified group element?\n");
       (if areg.(i) = Unspecified
        then areg.(i) <- element
        else IDA.msg "update_group: replacing specified group element?\n")

      | None,Predicated(PRED_ModRMMemReg(_,_)) -> 
        IDA.msg "update_group:  adding non-mod-predicated instruction to mod predicate?\n"
        
      | Some(true),Predicated(PRED_ModRMMemReg(Unspecified,r)) ->
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(Group(empty_group),r))

      | Some(true),Predicated(PRED_ModRMMemReg(Group(arr),r)) ->
        arr.(i) <- element

      | Some(true),Predicated(PRED_ModRMMemReg(x,r)) ->
        IDA.msg "update_group:  adding mem-mod-predicated instruction to %s?\n" (tbl_to_str x); ()

      | Some(false),Predicated(PRED_ModRMMemReg(m,Unspecified)) ->
        let empty_group = Array.make 8 (Unspecified) in
        empty_group.(i) <- element;
        decarr.(stem) <- Predicated(PRED_ModRMMemReg(m,Group(empty_group)))

      | Some(false),Predicated(PRED_ModRMMemReg(m,Group(arr))) ->
        arr.(i) <- element

      | Some(false),Predicated(PRED_ModRMMemReg(n,x)) ->
        IDA.msg "update_group:  adding reg-mod-predicated instruction to %s?\n" (tbl_to_str x)
        
      | Some(x),y -> 
        IDA.msg "update_group:  default condition %b, %s?\n" x (tbl_to_str y)

      | None,y -> 
        IDA.msg "update_group:  default condition %s?\n" (tbl_to_str y);
        

  in
    
  let open X86Encode in
  match enc with
  | Literal(b)      -> update_mandatory_prefix b
  | Native32(b)     -> update_size_predicated b true 
  | Native16(b)     -> update_size_predicated b false
  | ModRM(b)        -> signify_modrm b; update_mandatory_prefix b
  | ModRMGroup(i,b) -> signify_modrm b; update_group b i (requires_specific_mod l_ao)

let build_decoding_table =
  let decarr = Array.make 1024 (Unspecified) in
  let has_modrm = Array.make 1024 false in
  let f = process_one_encoding decarr has_modrm in
  List.iter (fun enc -> try f enc with Invalid_argument(s) -> IDA.msg "ERR: %s\n%!" s; ()) l_encodings;
  let rec aux i =
    if i = 1024
    then ()
    else
      let mrm,dec = has_modrm.(i),decarr.(i) in
      IDA.msg "%b: %s\n" mrm (tbl_to_str dec);
      aux (i+1)
  in
  aux 0
      

