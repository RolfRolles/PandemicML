#use "c:\\paframework\\framework.ml";;
open IDATrace;;

let x86_local_dse instrs = IRLocalOpt.local_dse instrs X86ToIRUtil.reserved_vars

let process lr ea = 
  IDA.msg "%lx\n" ea;
  let b,l = 
    try
      (false,(List.rev (X86ToIR.translate ea))@(!lr))
    with _ -> (IDA.msg "%lx: exception in IR trans" ea; (true,!lr))
  in
  lr := l;
  b || ea <> 0x151042l;;

let tracer = 
{ obj = ref []; 
  process = process;
  initialize = (fun _ _ -> ());
} ;;
begin_trace tracer;;

let l = tracer.obj in
let l = !l in
List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) l;;
let opt = x86_local_dse l;;