open X86

let int_of_reg8 = function
| Al -> 0
| Cl -> 1
| Dl -> 2
| Bl -> 3
| Ah -> 4
| Ch -> 5
| Dh -> 6
| Bh -> 7

let int_of_reg16 = function
| Ax -> 0
| Cx -> 1
| Dx -> 2
| Bx -> 3
| Sp -> 4
| Bp -> 5
| Si -> 6
| Di -> 7

let int_of_reg32 = function
| Eax -> 0
| Ecx -> 1
| Edx -> 2
| Ebx -> 3
| Esp -> 4
| Ebp -> 5
| Esi -> 6
| Edi -> 7

let int_of_x86_control_reg = function
| CR0 -> 0
| CR1 -> 1
| CR2 -> 2
| CR3 -> 3
| CR4 -> 4
| CR5 -> 5
| CR6 -> 6
| CR7 -> 7

let int_of_x86_debug_reg = function
| DR0 -> 0
| DR1 -> 1
| DR2 -> 2
| DR3 -> 3
| DR4 -> 4
| DR5 -> 5
| DR6 -> 6
| DR7 -> 7

let int_of_x86_general_reg = function
| Gb(g) -> int_of_reg8  g
| Gw(g) -> int_of_reg16 g
| Gd(g) -> int_of_reg32 g

let int_of_x86_seg_reg = function
| ES -> 0
| CS -> 1
| SS -> 2
| DS -> 3
| FS -> 4
| GS -> 5

let int_of_x86_mmx_reg = function
| MM0 -> 0
| MM1 -> 1
| MM2 -> 2
| MM3 -> 3
| MM4 -> 4
| MM5 -> 5
| MM6 -> 6
| MM7 -> 7

let int_of_x86_xmm_reg = function
| XMM0 -> 0
| XMM1 -> 1
| XMM2 -> 2
| XMM3 -> 3
| XMM4 -> 4
| XMM5 -> 5
| XMM6 -> 6
| XMM7 -> 7

let int_of_x86_fpu_reg = function
| ST0 -> 0
| ST1 -> 1
| ST2 -> 2
| ST3 -> 3
| ST4 -> 4
| ST5 -> 5
| ST6 -> 6
| ST7 -> 7

let int_of_x86operand = function
| GeneralReg(g) -> int_of_x86_general_reg g
| ControlReg(c) -> int_of_x86_control_reg c
| DebugReg(d)   -> int_of_x86_debug_reg d
| SegReg(s)     -> int_of_x86_seg_reg s
| FPUReg(f)     -> int_of_x86_fpu_reg f
| MMXReg(m)     -> int_of_x86_mmx_reg m
| XMMReg(x)     -> int_of_x86_xmm_reg x
| Immediate(_)
| Memexpr(_)
| JccTarget(_,_)
| FarTarget(_) -> invalid_arg "int_of_x86operand"

let list_of_immediate i = 
  let b i = Int32.logand i 0xFFl in
  let s = Int32.shift_right_logical in
  match i with
  | Ib(i) -> [b i]
  | Iw(i) -> [b i; b (s i 8)]
  | Id(i) -> [b i; b (s i 8); b (s i 16); b (s i 24)]

let make_encoded_byte t m b = 
  if not (0 <= m && m <= 7) then invalid_arg "make_encoded_byte: invalid g-part";
  if not (0 <= t && t <= 3) then invalid_arg "make_encoded_byte: invalid t-part";
  if not (0 <= b && b <= 7) then invalid_arg "make_encoded_byte: invalid b-part";
  Int32.of_int ((t lsl 6) lor (m lsl 3) lor b)

let byte_of_seg = function
| CS -> 0x2El
| SS -> 0x36l
| DS -> 0x3El
| ES -> 0x26l
| FS -> 0x64l
| GS -> 0x65l

let is_quantity_signed_byte x = x >= 0xffffff80l && x <= 0x7Fl

