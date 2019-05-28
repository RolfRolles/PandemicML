open X86

(* Random utility functions useful in manipulating encoded x86 *)

(* int -> x86operand functions *)
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

let segreg_of_int invalid = function
| 0 -> ES 
| 1 -> CS 
| 2 -> SS 
| 3 -> DS 
| 4 -> FS 
| 5 -> GS 
| 6
| 7 -> invalid ()
| x -> failwith ("segreg_of_int "^string_of_int x)

let opnd_reg32_of_int  i = GeneralReg(Gd(reg32_of_int i))
let opnd_reg16_of_int  i = GeneralReg(Gw(reg16_of_int i))
let opnd_reg8_of_int   i = GeneralReg(Gb(reg8_of_int  i))
let opnd_mmxreg_of_int i = MMXReg(mmxreg_of_int i)
let opnd_xmmreg_of_int i = XMMReg(xmmreg_of_int i)

