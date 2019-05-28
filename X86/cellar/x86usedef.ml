(* Going to need to change this to reflect the changes I made to the rest of the
   framework with respect to Reg8 => Gb etc. *)

type x86_flags = X86F_C | X86F_P | X86F_A | X86F_S | X86F_Z | X86F_O | X86F_D
type x86_defuse = x86_operand list * x86_flags list
let def_none = ([],[])
let use_none = ([],[])
let defuse_none = (def_none,use_none)

let extract_lhs_register m o = match o with
| Reg8(_)::_ | Reg16(_)::_ | Reg32(_)::_ -> List.hd o
| _ -> raise (X86InvalidEncoding m o)

(* Cmovcc Gv, Ev *)
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
| Cmovz   -> 

(* Setcc Eb *)
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

(* Binary operations *)
| Adc
| Add 
| And
| Cmp
| Lea
| Or
| Rcl
| Rcr
| Rol
| Ror
| Sal
| Sar
| Sbb
| Shl
| Shr
| Sub
| Test
| Xchg
| Xor

(* BCD operations *)
| Aaa
| Aad
| Aam
| Aas
| Daa
| Das

(* Unary operations *)
| Bswap
| Dec
| Inc
| Neg
| Not

(* Nop -- might be single-byte or double-byte (with mod/rm).  Never uses or defines anything. *)
| Nop

(* Convert X to Y; use EAX and EDX (or AX and DX) *)
| Cbw
| Cdq
| Cwd
| Cwde

(* String instructions; use EDI and ESI (or SI and DI), and ECX if REP prefix is set *)
| Cmpsb
| Cmpsd
| Cmpsw
| Lodsb
| Lodsd
| Lodsw
| Movsb
| Movsd
| Movsw
| Scasb
| Scasd
| Scasw
| Stosb
| Stosd
| Stosw

(* Mass data in/out *)
| Insb
| Insd
| Insw
| Outsb
| Outsd
| Outsw

(* Long control transfers; always fixed *)
| CallF
| JmpF

(* Jmp/call, might be Jz or mod/rm *)
| Call
| Jmp

(* Jcc, aways Jz *)
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
| Jno
| Jnp
| Jns
| Jnz
| Jo 
| Jp 
| Js 
| Jz 

(* Loop stuff, uses/defines ECX *)
| Loop
| Loopnz
| Loopz

(* Bit scan; uses RHS, sets LHS (potentially) *)
| Bsf (* Bit scan forward; find LSB of LHS, put bit position in RHS and clear ZF, else set ZF and set LHS to zero *)
| Bsr (* Bit scan reverse; find MSB of LHS, put bit position in RHS and clear ZF, else set ZF and set LHS to zero *)
(* Bit test; uses RHS, sets CF *)
| Bt  (* Bit test; select the RHS (mod opsize) position in the LHS quantity, and set the CF accordingly *)
(* Bit test and XZY; uses LHS and RHS, sets LHS and CF *)
| Btc (* Bit test and complement; like bt, but complements the bit afterwards *)
| Btr (* Bit test and reset; like bt, but clears the bit afterwards *)
| Bts (* Bit test and set; like bt, but sets the bit afterwards *)

(* Interrupt-related stuff *)
| Icebp
| Int3
| Int
| Into
| Ud2

(* Defines segment registers as well as LHS; uses RHS (mem) *)
| Lds
| Les
| Lfs
| Lgs
| Lss

(* Flags-related stuff *)
| Clc   -> (([],[X86F_C]),x86_use_none)
| Cld   -> (([],[X86F_D]),x86_use_none)
| Cli   ->  x86_defuse_none_pair
| Cmc   -> (([],[X86F_C]),([],[X86F_C]))
| Lahf  -> (([Reg8(Ah)],[X86F_C]),([],[X86F_C;X86F_P;X86F_A;X86F_S;X86F_Z;X86F_O]))
| Sahf  -> (([],[X86F_C;X86F_P;X86F_A;X86F_S;X86F_Z;X86F_O]),([Reg8(Ah)],[X86F_C]))
| Salc  -> (([],[Reg8(Al)]),([],[X86F_C]))
| Stc   -> (([],[X86F_C]),x86_use_none)
| Std   -> (([],[X86F_D]),x86_use_none)
| Sti   -> x86_defuse_none_pair

(* Port I/O *)
| In
| Out

(* Function pro-/epi-logue stuff *)
| Enter -> (([Reg32(Esp);Reg32(Ebp)],[]),([Reg32(Esp);Reg32(Ebp)],[]))
| Ret   -> (([Reg32(Esp)],[]),([Reg32(Esp)],[]))
| Retf  -> raise (X86DefUse ("Retf"))
| Leave -> (([Reg32(Esp);Reg32(Ebp)],[]),([Reg32(Esp);Reg32(Ebp)],[]))

(* Data movement *)
(* Defines LHS (register or memory location), uses everything on the RHS and all constituents of the LHS *)
| Mov
(* Defines LHS (register), uses everything on the RHS *)
| Movsx -> 
| Movzx

(* Mul/div *)
| Div
| Idiv
| Imul
| Mul

(* Big shifts (look up) *)
| Shld
| Shrd

(* Stack-related *)
| Pop
| Popad
| Popaw
| Popfd
| Popfw
| Push
| Pushad
| Pushaw
| Pushfd
| Pushfw

| Arpl
| Bound
| Clts
| Cmpxchg
| Cpuid
| Crc32
| Emms
| Getsec
| Hlt
| Invd
| Lar
| Lsl
| Popcnt
| Prefetchnta
| Prefetcht0
| Prefetcht1
| Prefetcht2
| Rdmsr
| Rdpmc
| Rdtsc
| Rsm
| Syscall
| Sysenter
| Sysexit
| Sysret
| Vmread
| Vmwrite
| Wait
| Wbinvd
| Wrmsr
| Xadd

(*
let used_by_modrm_expression modrm =
  let to_reg = match modrm.modrmsize with
  | ModRM_16 -> reg16_of_int 
  | ModRM_32 -> reg32_of_int
  in
  match modrm.modrm with
  | RegisterNumber(r) -> [to_reg r]
  | MemoryPhrase(b,s,_,_) -> 
    let used_regs_list = if b <> ~-1 then [to_reg b] else [] in
    if s <> ~-1 then ((to_reg s)::used_regs_list) else used_regs_list in
*)
(* This needs re-working for sub-register quantities, e.g. eax vs. ax *)
(*
let uses_register (a,m,l) r =
  match m with
  | Inc | Dec | Bswap | Neg | Not | Call -> 
   (match l with 
    | [(Reg32(_) as r')] when r' = r -> true
    | [Memexpr(Eb(m))]
    | [Memexpr(Ew(m))]
    | [Memexpr(Ed(m))] -> List.mem r (used_by_modrm_expression n)
    | _ -> false)
  | 
*)

