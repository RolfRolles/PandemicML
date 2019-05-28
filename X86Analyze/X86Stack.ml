let get_stack_delta_common ir =
  let v2cmap = Hashtbl.create 16 in
  Hashtbl.replace v2cmap X86ToIRUtil.vEsp (IR.Const(0x12345678L,IR.TypeReg_32));
  let _ = IRLocalOpt.local_opt_state_in v2cmap ir in
  let espres = 
    try Some(Hashtbl.find v2cmap X86ToIRUtil.vEsp)
    with Not_found -> None
  in
  match espres with
  | Some(IR.Const(i,_)) -> Some(Int64.to_int32 (Int64.sub i 0x12345678L))
  | Some(_) -> None
  | None -> None

let get_stack_delta_instr instr = 
  let ir = X86ToIR.translate_instr 0l instr in
  get_stack_delta_common ir

let get_stack_delta_addr addr =
  let ir = X86ToIR.translate addr in
  get_stack_delta_common ir
  
(*
let get_maximal_stack_displacement displist = 
  let rec aux max list = match max,list with
  | z,[] -> z
  | Some(None),Some(y)::xs -> aux (Some(Some(y))) xs
  | Some(Some(x)),Some(y)::xs -> aux (Some(if x < y then x else y)) xs
  | None,Some(y)::xs -> aux (None) xs
  | _,None::xs -> aux (Some(None)) xs
  in aux (Some(None)) displist
  
match (get_maximal_stack_displacement displist) with
| None -> IDA.msg "Empty list"
| Some(None) -> IDA.msg "Lost track of ESP"
| Some(Some(y)) -> IDA.msg "%d"
*)