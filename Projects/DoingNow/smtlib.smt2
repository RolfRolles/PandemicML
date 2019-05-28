This stuff can all be replaced with bitvectors:

Option for a BV type:  bv1+size
COND: bv4
REG32/16/8, FLAGS, SEG, CONTROL, DEBUG: bv3
SIZE, SHIFT: bv2
MSIZE: bv1
X86Mnem: bv8
X86ControlMnem: bv5
X86Operand:  tag bv3, + however large an X86Mem is
X86Instr:  X86Mnem+X86Operand*3
X86Regs:  8*bv32
X86Flags: 7*bv1
X86Cntrl: 8*bv32
X86Debug: 8*bv32
X86Segs:  6*bv16
Imm:  bv2 + bv32
Reg:  bv2 + bv3
X86MemSize32:  bv4+bv6+bv33
X86MemSize16:  bv4+bv4+bv17
MEMSIZED:  tag bv1 + max(X86MemSize32,X86MemSize16)
X86Mem:  bv2+bv3+(memsized)

Pair:  two BV types, no problem
Pair:  what happens with an array?

X86Mems:  three arrays...
X86State:  most of this can be converted into a BV, except the three arrays.
X86StatePair:  


(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)) )) )
(declare-datatypes () ((X86Mems  (mk-x86-mems  (def mem8to32) (fs mem8to32) (gs mem8to32)) )) )
(declare-datatypes () ((X86State (mk-x86-state (regs X86Regs) (flags X86Flags) (cntrl X86Cntrl) (debug X86Debug) (segs X86Segs) (mems X86Mems) (path bv1)) )) )
(declare-datatypes (T1) ((X86StatePair (mk-pair (state X86State) (second T1)) )) )

; Enumerations describing the registers and flags, and also sizes
(declare-datatypes () 
  (
    (REG32 EAX ECX EDX EBX ESP EBP ESI EDI)
    (REG16 AX CX DX BX SP BP SI DI)
    (REG8  AL CL DL BL AH CH DH BH)
    (FLAGS ZF SF PF CF OF AF DF)
    (SEG   CS DS ES FS GS SS)
    (CONTROL CR0 CR1 CR2 CR3 CR4 CR5 CR6 CR7)
    (DEBUG   DR0 DR1 DR2 DR3 DR4 DR5 DR6 DR7)
    (SIZE  S32 S16 S8)
    (MSIZE M32 M16)
    (SHIFT S0 S1 S2 S3)
    (COND S NS O NO Z NZ P NP A AE B BE L LE G GE)
    (X86Mnem Add Adc And Cmp Or Sub Sbb Xor Movzx Movsx Inc Dec Xadd Not Clc Stc Cmc Salc Cbw Cwde Cwd Cdq Bswap
       Sahf Lahf Neg Shl Sal Shr Sar Rol Ror Rcl Rcr Shld Shrd Popcnt Push Pop Pushfd Pushfw Popfd Popfw
       Pushad Pushaw Popad Popaw Bsf Bsr Bt Btc Btr Bts Aaa Aas Aad Aam Daa Das Mul Imul Cld Std Mov Lea Xchg 
       Nop Wait Pause Cmpxchg Cmpxchg8b Lodsb Lodsw Lodsd Stosb Stosw Stosd Movsb Movsw Movsd Cmpsb Cmpsw Cmpsd
       Seto Setno Setb Setae Setz Setnz Setbe Seta Sets Setns Setp Setnp Setl Setge Setle Setg Test
       Cmovo Cmovno Cmovb Cmovae Cmovz Cmovnz Cmovbe Cmova Cmovs Cmovns Cmovp Cmovnp Cmovl Cmovge Cmovle Cmovg)
    (X86ControlMnem Jo Jno Jb Jae Jz Jnz Jbe Ja Js Jns Jp Jnp Jl Jge Jle Jg Loop Call Ret Retn)
  )
)


(declare-datatypes () 
 (
  (X86Operand
    (Void)
    (GeneralReg (r Reg))
    (Immediate  (i Imm))
    (Memexpr    (m X86Mem))
    (ControlReg (c CONTROL))
    (DebugReg   (d DEBUG))
    (SegReg     (s SEG))
  )
 )
)

(declare-datatypes () ((X86Instr (mk-x86-instr (mnem X86Mnem) (op1 X86Operand) (op2 X86Operand) (op3 X86Operand)) )) )
(declare-datatypes () ((X86Regs  (mk-x86-regs  (eax bv32) (ecx bv32) (edx bv32) (ebx bv32) (esp bv32) (ebp bv32) (esi bv32) (edi bv32)) )) )
(declare-datatypes () ((X86Flags (mk-x86-flags (zf bv1) (sf bv1) (pf bv1) (cf bv1) (of bv1) (af bv1) (df bv1)))) )
(declare-datatypes () ((X86Cntrl (mk-x86-cntrl (cr0 bv32) (cr1 bv32) (cr2 bv32) (cr3 bv32) (cr4 bv32) (cr5 bv32) (cr6 bv32) (cr7 bv32)) )) )
(declare-datatypes () ((X86Debug (mk-x86-debug (dr0 bv32) (dr1 bv32) (dr2 bv32) (dr3 bv32) (dr4 bv32) (dr5 bv32) (dr6 bv32) (dr7 bv32)) )) )
(declare-datatypes () ((X86Segs  (mk-x86-segs  (cs  bv16) (ds  bv16) (es  bv16) (fs bv16) (gs bv16) (ss bv16)) )) )
(declare-datatypes () ((Imm (Id (id bv32 )) (Iw (iw bv16))  (Ib (ib bv8 )))) )
(declare-datatypes () ((Reg (Gd (gd REG32)) (Gw (gw REG16)) (Gb (gb REG8)))) )
(declare-datatypes () ((X86MemSize32 (mk-mem32 (br (Option REG32)) (srf (Option (Pair REG32 SHIFT))) (d (Option bv32))) )) )
(declare-datatypes () ((X86MemSize16 (mk-mem16 (br (Option REG16)) (sr (Option REG16)) (d (Option bv16))) )) )
(declare-datatypes (T1) ((Option None (Some (v T1)) )) )

(declare-datatypes () ((MEMSIZED (Mem32 (m32 X86MemSize32)) (Mem16 (m16 X86MemSize16)) )) )
(declare-datatypes () ((X86Mem (mk-mem (sz SIZE) (seg SEG) (me MEMSIZED)) )) )

