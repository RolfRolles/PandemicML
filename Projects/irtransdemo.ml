let translation = X86ToIR.translate (IDA.get_screen_ea ()) in
List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) translation