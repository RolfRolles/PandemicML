#use "c:\\paframework\\useframework.ml";;

(* Notepad.exe entrypoint; patch these bytes
   010031F8: 28 DB 0F B6 DB 81 C3 BB BB BB BB 01 D8 0B 09 00 *)
(* Addresses where those four instructions live *)
let iaddrs = [0x010031F8l;0x010031FAl;0x010031FDl;0x01003203l] in
(* Create a list of IR translations for each instruction *)
let instrs = List.concat (List.map X86ToIR.translate iaddrs) in
let tbl,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars instrs in
(* Solve for the input state such that, after execution, eax = 0x12345678 *)
let res = Z3SymbolicExecute.symbolic_execute (Z3SymbolicExecute.mk_context ()) instrs (IRUtil.mk_eq (IRUtil.mk_evar (Hashtbl.find tbl X86ToIRUtil.vEax)) (IRUtil.mk_dword 0x12345678L)) in
IDA.msg "%s\n" res

(* (part of) The result:  EAX -> bv1450744509[32] *)
