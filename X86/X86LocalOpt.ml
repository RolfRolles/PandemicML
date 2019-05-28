open X86

let constant_fold_binop_32 op i1 i2 =
  let open LowLevel in
  match op with 
  | Add -> Int32.add     i1 i2
  | Sub -> Int32.sub     i1 i2
  | And -> Int32.logand  i1 i2
  | Or  -> Int32.logor   i1 i2
  | Xor -> Int32.logxor  i1 i2
  | Shr -> Int32.shift_right_logical i1 (Int32.to_int i2)
  (* This is wrong, necessitates a new interface
     Can't treat sar r16/r8, imm8 as 32-bit quantity when doing Sar *)
  | Sar -> Int32.shift_right         i1 (Int32.to_int i2)
  | Shl -> Int32.shift_left          i1 (Int32.to_int i2)
  | Ror -> ror_dword i1 i2
  | Rol -> rol_dword i1 i2
  | Mov -> i2
  | _ -> invalid_arg ("constant_fold_binop_32: "^X86Disasm.string_of_x86mnem op)

let constant_fold_binop_16 op i1 i2 = 
  let open LowLevel in
  match op with 
  | Add -> add_word i1 i2
  | Sub -> sub_word i1 i2
  | And -> and_word i1 i2
  | Or  -> or_word  i1 i2
  | Xor -> xor_word i1 i2
  | Shr -> shr_word i1 i2
  | Sar -> sar_word i1 i2
  | Shl -> shl_word i1 i2
  | Ror -> ror_word i1 i2
  | Rol -> rol_word i1 i2
  | Mov -> i2
  | _ -> invalid_arg ("constant_fold_binop_16: "^X86Disasm.string_of_x86mnem op)

let constant_fold_binop_8 op i1 i2 = 
  let open LowLevel in
  match op with 
  | Add -> add_byte i1 i2
  | Sub -> sub_byte i1 i2
  | And -> and_byte i1 i2
  | Or  -> or_byte  i1 i2
  | Xor -> xor_byte i1 i2
  | Shr -> shr_word i1 i2
  | Sar -> sar_byte i1 i2
  | Shl -> shl_byte i1 i2
  | Ror -> ror_byte i1 i2
  | Rol -> rol_byte i1 i2
  | Mov -> i2
  | _ -> invalid_arg ("constant_fold_binop_8: "^X86Disasm.string_of_x86mnem op)

let constant_fold_unop op i1 =
  match op with
  | Neg -> Int32.neg i1
  | Not -> Int32.lognot i1
  | Inc -> Int32.succ i1
  | Dec -> Int32.pred i1
  | _ -> invalid_arg ("constant_fold_unop: "^X86Disasm.string_of_x86mnem op)

let constant_fold_unop_32 op i1 =
  let open LowLevel in
  match op with
  | Bswap -> bswap_dword i1
  | _ -> constant_fold_unop op i1

let constant_fold_unop_16 op i1 =
  let open LowLevel in
  match op with
  | Bswap -> 0l
  | _ -> Int32.logand (constant_fold_unop op i1) 0xffffl

let constant_fold_unop_8  op i1 = Int32.logand (constant_fold_unop op i1) 0xffl
