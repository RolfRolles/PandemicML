(** Generic description of x86/32 assembly language.  Current as of the XYZ 
    revision of the Intel manuals.
    
    Each instruction is represented by:  a list of its {!x86_group1_prefix}es,
    a mnemonic (its {!x86mnem}), and a list of its operands ({!x86operand}s).
    
    The enumeration for mnemonics is just a giant collection of nullary sum type 
    constructors: {!x86mnem}.
    
    Each operand is one of the following:
    
    {ul 
      {- A general register from the {!x86_general_reg} enumeration, whose type is further broken down into
        {ul {li [Gb(r)], where [r] is [Al], [Cl], ... from the {!x86_reg8} enumeration. }
            {li [Gw(r)], where [r] is [Ax], [Cx], ... from the {!x86_reg16} enumeration. }
            {li [Gd(r}], where [r] is [Eax], [Ecx], ... from the {!x86_reg32} enumeration.}}}
      {- A control register, e.g. [CR0], from the {!x86_control_reg} enumeration.}
      {- A debug register, e.g. [DR0], from the {!x86_debug_reg} enumeration.}
      {- A segment register, e.g. [DS], from the {!x86_segreg} enumeration.}
      {- An FPU register, e.g. [ST0], from the {!x86_fpureg} enumeration.}
      {- An MMX register, e.g. [MM0], from the {!x86_mmxreg} enumeration.}
      {- An XMM register, e.g. [XMM0], from the {!x86_xmmreg} enumeration.}
      {- A immediate value, whose type is further broken down into
        {ul {li [Ib(i)], where [i] is an 8-bit value in an int32}
            {li [Iw(i)], where [i] is a 16-bit value in an int32}
            {li [Id(i}], where [i] is a 32-bit value in an int32}}}
      {- A memory expression from the {!x86_mem_expr} enumeration, whose type is further broken down into
        {ul {li [Mb (m)]}
            {li [Mw (m)]}
            {li [Md (m}]}
            {li [Mf (m)]}
            {li [Mq (m)]}
            {li [Mt (m}]}
            {li [Mdq(m}]}}
         where [m] is from the {!x86_addr_expr} enumeration:
        {ul {- [Mem16(seg,basereg option,scalereg option,displacement option)]}
            {- [Mem32(seg,basereg option,(scalereg * scalefactor) option,displacement option)]}}}
      {- A jump target, which is a pair of int32s}
      {- A 32:16 ([Ap16]) or 32:32 ([Ap32]) far address, from the {!x86_far_target} enumeration}
    }
*)

type x86mnem = 
| Aaa
| Aad
| Aam
| Aas
| Adc
| Add 
| Addpd
| Addps
| Addsd
| Addss
| Addsubpd
| Addsubps
| And
| Andnpd
| Andnps
| Andpd
| Andps
| Arpl
| Blendpd
| Blendps
| Blendvpd
| Blendvps
| Bound
| Bsf
| Bsr
| Bswap
| Bt
| Btc
| Btr
| Bts
| Call
| CallF
| Cbw
| Cdq
| Clc
| Cld
| Clflush
| Cli
| Clts
| Cmc
| Cmova 
| Cmovae
| Cmovb 
| Cmovbe
| Cmovg 
| Cmovge
| Cmovl 
| Cmovle
| Cmovno
| Cmovnp
| Cmovns
| Cmovnz
| Cmovo 
| Cmovp 
| Cmovs 
| Cmovz 
| Cmp
| Cmppd
| Cmpps
| Cmpsb
| Cmpsd
| Cmpss
| Cmpsw
| Cmpxchg
| Cmpxchg8b
| Comisd
| Comiss
| Cpuid
| Crc32
| Cvtdq2pd
| Cvtdq2ps
| Cvtpd2dq
| Cvtpd2pi
| Cvtpd2ps
| Cvtpi2pd
| Cvtpi2ps
| Cvtps2dq
| Cvtps2pd
| Cvtps2pi
| Cvtsd2si
| Cvtsd2ss
| Cvtsi2sd
| Cvtsi2ss
| Cvtss2sd
| Cvtss2si
| Cvttpd2dq
| Cvttpd2pi
| Cvttps2dq
| Cvttps2pi
| Cvttsd2si
| Cvttss2si
| Cwd
| Cwde
| Daa
| Das
| Dec
| Div
| Divpd
| Divps
| Divsd
| Divss
| Dppd
| Dpps
| Emms
| Enter
| Extractps
| F2xm1
| Fabs
| Fadd
| Faddp
| Fbld
| Fbstp
| Fchs
| Fclex
| Fcmovb
| Fcmovbe
| Fcmove
| Fcmovnb
| Fcmovnbe
| Fcmovne
| Fcmovnu
| Fcmovu
| Fcom
| Fcomi
| Fcomip
| Fcomp
| Fcompp
| Fcos
| Fdecstp
| Fdiv
| Fdivp
| Fdivr
| Fdivrp
| Ffree
| Fiadd
| Ficom
| Ficomp
| Fidiv
| Fidivr
| Fild
| Fimul
| Fincstp
| Finit
| Fist
| Fistp
| Fisttp
| Fisub
| Fisubr
| Fld
| Fld1
| Fldcw
| Fldenv
| Fldl2e
| Fldl2t
| Fldlg2
| Fldln2
| Fldpi
| Fldz
| Fmul
| Fmulp
| Fnop
| Fpatan
| Fprem
| Fprem1
| Fptan
| Frndint
| Frstor
| Fsave
| Fscale
| Fsin
| Fsincos
| Fsqrt
| Fst
| Fstcw
| Fstenv
| Fstp
| Fstsw
| Fsub
| Fsubp
| Fsubr
| Fsubrp
| Ftst
| Fucom
| Fucomi
| Fucomip
| Fucomp
| Fucompp
| Fxam
| Fxch
| Fxrstor
| Fxsave
| Fxtract
| Fyl2x
| Fyl2xp1
| Getsec
| Haddpd
| Haddps
| Hlt
| Hsubpd
| Hsubps
| Icebp
| Idiv
| Imul
| In
| Inc
| Insb
| Insd
| Insertps
| Insw
| Int
| Int3
| Into
| Invd
| Invlpg
| Iretd
| Iretw
| Ja 
| Jae
| Jb 
| Jbe
| Jcxz
| Jecxz
| Jg 
| Jge
| Jl 
| Jle
| Jmp
| JmpF
| Jno
| Jnp
| Jns
| Jnz
| Jo 
| Jp 
| Js 
| Jz 
| Lahf
| Lar
| Lddqu
| Ldmxcsr
| Lds
| Lea
| Leave
| Les
| Lfence
| Lfs
| Lgdt
| Lgs
| Lidt
| Lldt
| Lmsw
| Lodsb
| Lodsd
| Lodsw
| Loop
| Loopnz
| Loopz
| Lsl
| Lss
| Ltr 
| Maskmovdqu
| Maskmovq
| Maxpd
| Maxps
| Maxsd
| Maxss
| Mfence
| Minpd
| Minps
| Minsd
| Minss
| Monitor
| Mov
| Movapd
| Movaps
| Movd
| Movddup
| Movdq2q
| Movdqa
| Movdqu
| Movhlps
| Movhpd
| Movhps
| Movlhps
| Movlpd
| Movlps
| Movmskpd
| Movmskps
| Movntdq
| Movntdqa
| Movnti
| Movntpd
| Movntps
| Movntq
| Movq
| Movq2dq
| Movsb
| Movsd
| Movshdup
| Movsldup
| Movss 
| Movsw
| Movsx
| Movupd
| Movups
| Movzx
| Mpsadbw
| Mul
| Mulpd
| Mulps
| Mulsd
| Mulss
| Mwait
| Neg
| Nop
| Not
| Or
| Orpd
| Orps
| Out
| Outsb
| Outsd
| Outsw
| Pabsb
| Pabsd
| Pabsw
| Packssdw
| Packsswb 
| Packusdw
| Packuswb 
| Paddb
| Paddd
| Paddq
| Paddsb
| Paddsw
| Paddusb
| Paddusw
| Paddw
| Palignr
| Pand
| Pandn
| Pause
| Pavgb
| Pavgw
| Pblendvb
| Pblendw
| Pcmpeqb
| Pcmpeqd
| Pcmpeqq
| Pcmpeqw
| Pcmpestri
| Pcmpestrm
| Pcmpgtb  
| Pcmpgtd  
| Pcmpgtq
| Pcmpgtw  
| Pcmpistri
| Pcmpistrm
| Pextrb
| Pextrd
| Pextrw
| Phaddd
| Phaddsw
| Phaddw
| Phminposuw
| Phsubd
| Phsubsw
| Phsubw
| Pinsrb
| Pinsrd
| Pinsrw
| Pmaddubsw
| Pmaddwd
| Pmaxsb
| Pmaxsd
| Pmaxsw
| Pmaxub
| Pmaxud
| Pmaxuw
| Pminsb
| Pminsd
| Pminsw
| Pminub
| Pminud
| Pminuw
| Pmovmskb
| Pmovsxbd
| Pmovsxbq
| Pmovsxbw
| Pmovsxdq
| Pmovsxwd
| Pmovsxwq
| Pmovzxbd
| Pmovzxbq
| Pmovzxbw
| Pmovzxdq
| Pmovzxwd
| Pmovzxwq
| Pmuldq
| Pmulhrsw
| Pmulhuw
| Pmulhw
| Pmulld
| Pmullw
| Pmuludq
| Pop
| Popad
| Popaw
| Popcnt
| Popfd
| Popfw
| Por
| Prefetchnta
| Prefetcht0
| Prefetcht1
| Prefetcht2
| Psadbw
| Pshufb
| Pshufd
| Pshufhw
| Pshuflw
| Pshufw
| Psignb
| Psignd
| Psignw
| Pslld
| Pslldq
| Psllq
| Psllw
| Psrad
| Psraw
| Psrld
| Psrldq
| Psrlq
| Psrlw
| Psubb
| Psubd
| Psubq
| Psubsb
| Psubsw
| Psubusb
| Psubusw
| Psubw
| Ptest
| Punpckhbw
| Punpckhdq
| Punpckhqdq
| Punpckhwd
| Punpcklbw
| Punpckldq
| Punpcklqdq
| Punpcklwd
| Push
| Pushad
| Pushaw
| Pushfd
| Pushfw
| Pxor
| Rcl
| Rcpps
| Rcpss
| Rcr
| Rdmsr
| Rdpmc
| Rdtsc
| Ret
| Retf
| Rol
| Ror
| Roundpd
| Roundps
| Roundsd
| Roundss
| Rsm
| Rsqrtps
| Rsqrtss
| Sahf
| Sal
| Salc
| Sar
| Sbb
| Scasb
| Scasd
| Scasw
| Seta 
| Setae
| Setb 
| Setbe
| Setg 
| Setge
| Setl 
| Setle
| Setno
| Setnp
| Setns
| Setnz
| Seto 
| Setp 
| Sets 
| Setz 
| Sfence
| Sgdt
| Shl
| Shld
| Shr
| Shrd
| Shufpd
| Shufps
| Sidt
| Sldt
| Smsw
| Sqrtpd
| Sqrtps
| Sqrtsd
| Sqrtss
| Stc
| Std
| Sti
| Stmxcsr       
| Stosb
| Stosd
| Stosw
| Str 
| Sub
| Subpd
| Subps
| Subsd
| Subss
| Syscall
| Sysenter
| Sysexit
| Sysret
| Test
| Ucomisd
| Ucomiss
| Ud2
| Unpckhpd
| Unpckhps
| Unpcklpd
| Unpcklps
| Verr
| Verw
| Vmcall
| Vmclear
| Vmlaunch
| Vmptrld
| Vmptrst
| Vmread
| Vmresume
| Vmwrite
| Vmxoff
| Vmxon  
| Wait
| Wbinvd
| Wrmsr
| Xadd
| Xlat
| Xchg
| Xor
| Xorpd
| Xorps

type x86_reg8 =
| Al
| Cl
| Dl
| Bl
| Ah
| Ch
| Dh
| Bh

type x86_reg16 =
| Ax
| Cx
| Dx
| Bx
| Sp
| Bp
| Si
| Di

type x86_reg32 =
| Eax
| Ecx
| Edx
| Ebx
| Esp
| Ebp
| Esi
| Edi

type x86_general_reg =
| Gb of x86_reg8
| Gd of x86_reg32
| Gw of x86_reg16

type x86_control_reg =
| CR0
| CR1
| CR2
| CR3
| CR4
| CR5
| CR6
| CR7

type x86_debug_reg =
| DR0
| DR1
| DR2
| DR3
| DR4
| DR5
| DR6
| DR7

type x86_segreg =
| ES
| CS
| SS
| DS
| FS
| GS

type x86_fpureg = 
| ST0
| ST1
| ST2
| ST3
| ST4
| ST5
| ST6
| ST7

type x86_mmxreg =
| MM0
| MM1
| MM2
| MM3
| MM4
| MM5
| MM6
| MM7

type x86_xmmreg =
| XMM0
| XMM1
| XMM2
| XMM3
| XMM4
| XMM5
| XMM6
| XMM7

type x86_immediate = 
| Ib of int32
| Id of int32
| Iw of int32

type x86_addr_expr =
| Mem16 of x86_segreg * x86_reg16 option * x86_reg16 option * int32 option
| Mem32 of x86_segreg * x86_reg32 option * (x86_reg32 * int) option * (int32 option)

type x86_mem_expr =
| Mb  of x86_addr_expr
| Mw  of x86_addr_expr
| Md  of x86_addr_expr
| Mf  of x86_addr_expr
| Mq  of x86_addr_expr
| Mt  of x86_addr_expr
| Mdq of x86_addr_expr

type x86_far_target =
| Ap32 of int32 * int32 (** 16-bit seg, 32-bit offset *)
| Ap16 of int32 * int32 (** 16-bit seg, 16-bit offset *)

type x86operand =
| GeneralReg of x86_general_reg
| ControlReg of x86_control_reg
| DebugReg of x86_debug_reg
| SegReg of x86_segreg
| FPUReg of x86_fpureg
| MMXReg of x86_mmxreg
| XMMReg of x86_xmmreg
| Immediate of x86_immediate
| Memexpr of x86_mem_expr
| JccTarget of int32 * int32
| FarTarget of x86_far_target

type x86instr = x86mnem * (x86operand list)
type x86_group1_prefix = Rep | Repne | Lock
type x86instrpref = { pref: x86_group1_prefix list; instr: x86instr; }

type x86_flags = X86F_C | X86F_P | X86F_A | X86F_S | X86F_Z | X86F_O | X86F_D
