type x86_complete_state = 
{
  eax: int32;
  ecx: int32;
  edx: int32;
  ebx: int32;
  esp: int32;
  ebp: int32;
  esi: int32;
  edi: int32;
  
  eflags: int;
  
  memory: int32 array;
}

val fl2eflags         : int -> int -> int -> int -> int -> int -> int
val eflags2fl         : int -> int * int * int * int * int * int
val do_flags_assign   : int -> int -> int -> int -> int -> int -> IR.instr list
val do_vars3_assign   : int32 -> int32 -> int32 -> IR.instr list
val do_vars_assign    : x86_complete_state -> IR.instr list
val do_complete_vars_assign : x86_complete_state -> IR.instr list
val mk_initial_state3 : int -> int -> int -> int -> int -> int -> int32 -> int32 -> int32 -> (IR.var, IR.expr) Hashtbl.t
val mk_initial_state  : x86_complete_state -> (IR.var, IR.expr) Hashtbl.t