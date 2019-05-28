exception InvalidInstruction

let mips32_gpr_of_int = function
| 00 -> Zero
| 01 -> At
| 02 -> V0
| 03 -> V1
| 04 -> A0
| 05 -> A1
| 06 -> A2
| 07 -> A3
| 08 -> T0
| 09 -> T1
| 10 -> T2
| 11 -> T3
| 12 -> T4
| 13 -> T5
| 14 -> T6
| 15 -> T7
| 16 -> S0
| 17 -> S1
| 18 -> S2
| 19 -> S3
| 20 -> S4
| 21 -> S5
| 22 -> S6
| 23 -> S7
| 24 -> T8
| 25 -> T9
| 26 -> K0
| 27 -> K1
| 28 -> GP
| 29 -> SP
| 30 -> S8
| 31 -> RA
| _  -> invalid_arg "mips32_gpr_of_int"

let mips32_fpr_of_int = function
| 0  -> F0
| 1  -> F1
| 2  -> F2
| 3  -> F3
| 4  -> F4
| 5  -> F5
| 6  -> F6
| 7  -> F7
| 8  -> F8
| 9  -> F9
| 10 -> F10
| 11 -> F11
| 12 -> F12
| 13 -> F13
| 14 -> F14
| 15 -> F15
| 16 -> F16
| 17 -> F17
| 18 -> F18
| 19 -> F19
| 20 -> F20
| 21 -> F21
| 22 -> F22
| 23 -> F23
| 24 -> F24
| 25 -> F25
| 26 -> F26
| 27 -> F27
| 28 -> F28
| 29 -> F29
| 30 -> F30
| 31 -> F31
| _ -> invalid_arg "mips32_fpr_of_int"

let mips32_format_of_int = function
| 16 -> S
| 17 -> D
| 20 -> W
| 21 -> L
| 22 -> PS
| _ -> invalid_arg "mips32_format_of_int"

let decode_itype dw = 
  let int = (Int32.to_int (Int32.logand dw (Int32.lognot 0xFC000000l))) in
  let rs = (int lsr 21) land 0x1f in
  let rt = (int lsr 16) land 0x1f in
  let im = int land 0xffff in
  (rs,rt,im)

let decode_jtype dw = 
  Int32.to_int (Int32.logand dw (Int32.lognot 0xFC000000l))

let decode_rtype dw = 
  let int = (Int32.to_int (Int32.logand dw (Int32.lognot 0xFC000000l))) in
  let rs = (int lsr 21) land 0x1f in
  let rt = (int lsr 16) land 0x1f in
  let rd = (int lsr 11) land 0x1f in
  let sa = (int lsr 6)  land 0x1f in
  let fn = int land 0x3f in
  (rs,rt,rd,sa,fn)

let decode ea = 
  let gpr i = GPR(mips32_gpr_of_int i) in
  let fpr i = FPR(mips32_gpr_of_int i) in
  let dw = consume_dword () in
  let opcode = Int32.to_int (Int32.shift_right_logical dw 26) in
  match opcode with
  (* SPECIAL *)
  | 0  -> 
    let rs,rt,rd,sa,fn = decode_rtype opcode in
   (match fn with
    | 32 -> (Add,[gpr rd;gpr rs; gpr rd])
    
  (* COP1 *)
  | 17 -> 
    let fmt,???,fs,fd,opcode = decode_rtype opcode in
   (match opcode with
    (* ABS.xx; fmt selects between the .S, .D, and .PS forms *)
    | 5 -> (Abs(mips32_format_of_int fmt),[fpr fd;fpr fs])
    | _ -> raise InvalidInstruction)
  | 
  
      
    