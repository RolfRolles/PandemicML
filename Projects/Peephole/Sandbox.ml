let replace_pushes_pops inlist = 
  let rec aux outlist = function
  | { pref = _; instr = (Push,[x]) }::xs ->
    match X86TypeCheck.size_of_operand x with 
    | X86TypeCheck.OPSZ_32 -> 
    | X86TypeCheck.OPSZ_16 -> 
  | { pref = _; instr = (Pop,[x]) }::xs ->
    match X86TypeCheck.size_of_operand x with 
    | X86TypeCheck.OPSZ_32 -> 
    | X86TypeCheck.OPSZ_16 -> 
  
    
(*
  mov r2, r7
  and $0xff, r7
  add $membase, r7
*)
let sandbox_memory_reads membase inlist = 
