let int_of_x86_flags = function
| X86.X86F_O -> 11
| X86.X86F_D -> 10
| X86.X86F_S ->  7
| X86.X86F_Z ->  6
| X86.X86F_A ->  4
| X86.X86F_P ->  2
| X86.X86F_C ->  0

let   mask_of_x86_flags f = 1 lsl (int_of_x86_flags f)
let mask32_of_x86_flags f = Int32.of_int (mask_of_x86_flags f)

let fl2eflags sf zf af pf cf ofl df =
  let ofl = (ofl land 1) lsl 11 in
  let df  = (df  land 1) lsl 10 in
  let sf  = (sf  land 1) lsl  7 in
  let zf  = (zf  land 1) lsl  6 in
  let af  = (af  land 1) lsl  4 in
  let pf  = (pf  land 1) lsl  2 in
  let cf  = (cf  land 1)       in
  sf lor zf lor af lor pf lor cf lor ofl lor df
  
let eflags2fl flags = 
  let ofl = (flags lsr 11) land 1 in
  let df  = (flags lsr 10) land 1 in
  let sf  = (flags lsr  7) land 1 in
  let zf  = (flags lsr  6) land 1 in
  let af  = (flags lsr  4) land 1 in
  let pf  = (flags lsr  2) land 1 in
  let cf  =  flags         land 1 in
  (sf,zf,af,pf,cf,ofl,df)

let flags_str f32 = 
  let (sf,zf,af,pf,cf,ofl,df) = eflags2fl (Int32.to_int f32) in
  let str = "XXXXXXX" in
  str.[0] <- if cf  = 1 then 'C' else 'c';
  str.[1] <- if pf  = 1 then 'P' else 'p';
  str.[2] <- if af  = 1 then 'A' else 'a';
  str.[3] <- if sf  = 1 then 'S' else 's';
  str.[4] <- if zf  = 1 then 'Z' else 'z';
  str.[5] <- if ofl = 1 then 'O' else 'o';
  str.[6] <- if df  = 1 then 'D' else 'd';
  str