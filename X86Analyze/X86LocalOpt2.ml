exception Stop

let x86_local_dse instrs = IRLocalOpt.local_dse instrs X86ToIRUtil.reserved_vars

let split_along_statement_boundaries irlist = 
  let rec aux current_instruction instructions_list = function
  | [] -> List.rev ((List.rev current_instruction)::instructions_list)
  | ((IR.Label(_)) as l)::((IR.Comment(_)) as c)::xs ->
    let h = [c;l] in
    if current_instruction = []
    then aux h instructions_list xs
    else aux h ((List.rev current_instruction)::instructions_list) xs
  | x::xs -> aux (x::current_instruction) instructions_list xs
  in aux [] [] irlist
  
let testing ea x86l =
  let ir = 
    try
      List.concat (List.map (X86ToIR.translate_instr 0l) x86l)
    with
      Invalid_argument _ -> 
       (IDA.msg "%08lx: IR translation failed, can't optimize\n" ea; raise Stop)
  in
  let ir = x86_local_dse (IRLocalOpt.local_opt ir) in
  List.iter2
   (fun x86 irbundle ->
      IDA.msg "\nX86: %s\n" (X86Disasm.string_of_x86instr x86);
      List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) irbundle)
    x86l
   (split_along_statement_boundaries ir)

let testing ea x86l =
  try
    testing ea x86l
  with 
    Stop -> ()

(*
let testing cfg = C.G.iter_vertex (fun v -> testing (C.G.V.label v) (C.get_ir cfg v)) cfg;;
*)