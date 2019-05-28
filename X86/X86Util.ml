open X86

let x86_is_cjmp = function
| Ja  | Jae | Jb | Jbe | Jcxz | Jecxz | Jg | Jge | Jl | Jle | Jno | Jnp
| Jns | Jnz | Jo | Jp  | Js   | Jz -> true
| _ -> false

let x86_is_call  = function | Call  -> true | _ -> false
let x86_is_jmp   = function | Jmp   -> true | _ -> false
let x86_is_ret   = function | Ret   -> true | _ -> false
let x86_is_retf  = function | Retf  -> true | _ -> false
let x86_is_callf = function | CallF -> true | _ -> false
let x86_is_jmpf  = function | JmpF  -> true | _ -> false

(* Jcctargets might be a Gv or an Ev too *)
let extract_jcctargets_opt = function | JccTarget(ta,fa)::[] -> Some(ta,fa) | _ -> None
let extract_jcctargets     = function | JccTarget(ta,fa)::[] -> (ta,fa)     | _ -> failwith "extract_jcctargets: typechecking prevents this"

let default_seg = function
| Mem16(_,Some(Bp),_,_)
| Mem32(_,Some(Esp),_,_)
| Mem32(_,Some(Ebp),_,_) -> SS
| _ -> DS

let get_seg = function | Mem16(s,_,_,_) | Mem32(s,_,_,_) -> s

let reg16_covers_reg8 r16 r8 = 
  let open X86 in
  match (r16,r8) with
  | Ax,Al | Ax,Ah
  | Cx,Cl | Cx,Ch
  | Dx,Dl | Dx,Dh
  | Bx,Bl | Bx,Bh -> true
  | _ -> false

let reg32_covers_reg8 r32 r8 = 
  let open X86 in
  match (r32,r8) with
  | Eax,Al | Eax,Ah
  | Ecx,Cl | Ecx,Ch
  | Edx,Dl | Edx,Dh
  | Ebx,Bl | Ebx,Bh -> true
  | _ -> false

let reg32_covers_reg16 r32 r16 = 
  let open X86 in
  match (r32,r16) with
  | Eax,Ax
  | Ecx,Cx
  | Edx,Dx
  | Ebx,Bx
  | Esp,Sp
  | Ebp,Bp
  | Esi,Si
  | Edi,Di -> true
  | _ -> false

let reg32_low8 r32 =
  let open X86 in
  match r32 with
  | Eax -> Al
  | Ecx -> Cl
  | Edx -> Dl
  | Ebx -> Bl
  | _ -> invalid_arg "reg32_lowpart"

let reg32_high8 r32 =
  let open X86 in
  match r32 with
  | Eax -> Ah
  | Ecx -> Ch
  | Edx -> Dh
  | Ebx -> Bh
  | _ -> invalid_arg "reg32_lowpart"

let reg32_low16 r32 =
  let open X86 in
  match r32 with
  | Eax -> Ax
  | Ecx -> Cx
  | Edx -> Dx
  | Ebx -> Bx
  | Esp -> Sp
  | Ebp -> Bp
  | Esi -> Si
  | Edi -> Di

let reg16_low8 r16 =
  let open X86 in
  match r16 with
  | Ax -> Al
  | Cx -> Cl
  | Dx -> Dl
  | Bx -> Bl
  | _ -> invalid_arg "reg16_lowpart"

let reg16_high8 r16 =
  let open X86 in
  match r16 with
  | Ax -> Ah
  | Cx -> Ch
  | Dx -> Dh
  | Bx -> Bh
  | _ -> invalid_arg "reg16_lowpart"

let reg32_super_of_reg16 r16 = 
  let open X86 in 
  match r16 with
  | Ax -> Eax
  | Cx -> Ecx
  | Dx -> Edx
  | Bx -> Ebx
  | Sp -> Esp
  | Bp -> Ebp
  | Si -> Esi
  | Di -> Edi

let reg32_super_of_reg8 r8 = 
  let open X86 in 
  match r8 with
  | Al -> Eax
  | Ah -> Eax
  | Cl -> Ecx
  | Ch -> Ecx
  | Dl -> Edx
  | Dh -> Edx
  | Bl -> Ebx
  | Bh -> Ebx