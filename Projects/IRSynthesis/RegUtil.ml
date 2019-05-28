type qtype = | Reg of X86.x86_general_reg | Flag of X86.x86_flags
let string_of_qtype = function
| Reg(r)  -> X86Disasm.string_of_x86_general_reg r 
| Flag(f) -> X86Disasm.string_of_x86_flags f

let qtype_getter_setter r = 
  let t16 = Int32.logand 0xFFFFl in let s16 x y = Int32.(logor (logand 0xFFFF0000l x) (t16 y)) in
  let t8l = Int32.logand 0xFFl   in let s8l x y = Int32.(logor (logand 0xFFFFFF00l x) (t8l y)) in 
  let t8h x = t8l (Int32.shift_right_logical x 8) in
  let s8h x y = Int32.(logor (logand 0xFFFF00FFl x) (shift_left (t8h y) 8)) in
  let open X86 in let open JITRegion in match r with | Reg(r) -> (match r with
  | Gd(Eax) -> (fun s -> s.eax),    (fun x s -> { s with eax = x })
  | Gd(Ecx) -> (fun s -> s.ecx),    (fun x s -> { s with ecx = x })
  | Gd(Edx) -> (fun s -> s.edx),    (fun x s -> { s with edx = x })
  | Gd(Ebx) -> (fun s -> s.ebx),    (fun x s -> { s with ebx = x })
  | Gd(Esp) -> (fun s -> s.esp),    (fun x s -> { s with esp = x })
  | Gd(Ebp) -> (fun s -> s.ebp),    (fun x s -> { s with ebp = x })
  | Gd(Esi) -> (fun s -> s.esi),    (fun x s -> { s with esi = x })
  | Gd(Edi) -> (fun s -> s.edi),    (fun x s -> { s with edi = x })
  | Gw(Ax)  -> (fun s -> t16 s.eax),(fun x s -> { s with eax = s16 s.eax x })
  | Gw(Cx)  -> (fun s -> t16 s.ecx),(fun x s -> { s with ecx = s16 s.ecx x })
  | Gw(Dx)  -> (fun s -> t16 s.edx),(fun x s -> { s with edx = s16 s.edx x })
  | Gw(Bx)  -> (fun s -> t16 s.ebx),(fun x s -> { s with ebx = s16 s.ebx x })
  | Gw(Sp)  -> (fun s -> t16 s.esp),(fun x s -> { s with esp = s16 s.esp x })
  | Gw(Bp)  -> (fun s -> t16 s.ebp),(fun x s -> { s with ebp = s16 s.ebp x })
  | Gw(Si)  -> (fun s -> t16 s.esi),(fun x s -> { s with esi = s16 s.esi x })
  | Gw(Di)  -> (fun s -> t16 s.edi),(fun x s -> { s with edi = s16 s.edi x })
  | Gb(Al)  -> (fun s -> t8l s.eax),(fun x s -> { s with eax = s8l s.eax x })
  | Gb(Cl)  -> (fun s -> t8l s.ecx),(fun x s -> { s with ecx = s8l s.ecx x })
  | Gb(Dl)  -> (fun s -> t8l s.edx),(fun x s -> { s with edx = s8l s.edx x })
  | Gb(Bl)  -> (fun s -> t8l s.ebx),(fun x s -> { s with ebx = s8l s.ebx x })
  | Gb(Ah)  -> (fun s -> t8h s.eax),(fun x s -> { s with eax = s8h s.eax x })
  | Gb(Ch)  -> (fun s -> t8h s.ecx),(fun x s -> { s with ecx = s8h s.ecx x })
  | Gb(Dh)  -> (fun s -> t8h s.edx),(fun x s -> { s with edx = s8h s.edx x })
  | Gb(Bh)  -> (fun s -> t8h s.ebx),(fun x s -> { s with ebx = s8h s.ebx x }))
  | Flag(f) -> let m32 = X86Misc.mask32_of_x86_flags f in 
               (fun s -> if Int32.logand s.eflags m32 <> 0l then 1l else 0l),
               (fun x s -> if Int32.logand 1l x = 0l 
                  then { s with eflags = Int32.(logand (lognot m32) s.eflags) }
                  else { s with eflags = Int32.logor s.eflags m32 })

let l_regflags = 
  let d32,d16,d8,d1 = IRUtil.(mk_dword_of_int32,mk_word_of_int32,mk_byte_of_int32,mk_bit_of_int32) in
  X86ToIRUtil.(X86.([
    "eax",Reg(Gd(Eax)),d32,vEax,vEaxAfter;"ecx",Reg(Gd(Ecx)),d32,vEcx,vEcxAfter;
    "ebx",Reg(Gd(Ebx)),d32,vEbx,vEbxAfter;"edx",Reg(Gd(Edx)),d32,vEdx,vEdxAfter;
    "esp",Reg(Gd(Esp)),d32,vEsp,vEspAfter;"ebp",Reg(Gd(Ebp)),d32,vEbp,vEbpAfter;
    "esi",Reg(Gd(Esi)),d32,vEsi,vEsiAfter;"edi",Reg(Gd(Edi)),d32,vEdi,vEdiAfter;

    "ax", Reg(Gw(Ax)), d16,vAx, vAxAfter; "cx", Reg(Gw(Cx)), d16,vCx, vCxAfter;
    "bx", Reg(Gw(Bx)), d16,vBx, vBxAfter; "dx", Reg(Gw(Dx)), d16,vDx, vDxAfter;
    "sp", Reg(Gw(Sp)), d16,vSp, vSpAfter; "bp", Reg(Gw(Bp)), d16,vBp, vBpAfter;
    "si", Reg(Gw(Si)), d16,vSi, vSiAfter; "di", Reg(Gw(Di)), d16,vDi, vDiAfter;

    "al", Reg(Gb(Al)), d8, vAl, vAlAfter; "cl", Reg(Gb(Cl)), d8, vCl, vClAfter;
    "bl", Reg(Gb(Bl)), d8, vBl, vBlAfter; "dl", Reg(Gb(Dl)), d8, vDl, vDlAfter;
    "ah", Reg(Gb(Ah)), d8, vAh, vAhAfter; "ch", Reg(Gb(Ch)), d8, vCh, vChAfter;
    "bh", Reg(Gb(Bh)), d8, vBh, vBhAfter; "dh", Reg(Gb(Dh)), d8, vDh, vDhAfter;

    "OF", Flag(X86F_O),d1, vOF, vOFAfter; "DF", Flag(X86F_D),d1, vDF, vDFAfter;
    "SF", Flag(X86F_S),d1, vSF, vSFAfter; "ZF", Flag(X86F_Z),d1, vZF, vZFAfter;
    "AF", Flag(X86F_A),d1, vAF, vAFAfter; "PF", Flag(X86F_P),d1, vPF, vPFAfter;
    "CF", Flag(X86F_C),d1, vCF, vCFAfter;]))
  
let ht_str2setter,ht_reg2aftervar,ht_reg2var = Hashtbl.create 64,Hashtbl.create 64,Hashtbl.create 64
let _ = List.iter (fun (s,r,_,_,_) -> Hashtbl.replace ht_str2setter s (snd (qtype_getter_setter  r))) l_regflags
let _ = List.iter (fun (_,r,_,_,v) -> Hashtbl.replace ht_reg2aftervar r v) l_regflags
let _ = List.iter (fun (_,r,_,v,_) -> Hashtbl.replace ht_reg2var r v) l_regflags

