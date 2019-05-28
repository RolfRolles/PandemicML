#use "c:\\paframework\\useframework.ml";;

let blah () =
  let instrs = X86ToIR.translate (IDA.get_screen_ea ()) in
  List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) instrs;;

let blah () = Z3SymbolicExecute.symbolic_execute (X86ToIR.translate (IDA.get_screen_ea ())) (IRUtil.mk_eq X86ToIRUtil.eEax (IRUtil.mk_dword 0x12345678L))

IDAHotKey.register "CTRL-F11" blah;;