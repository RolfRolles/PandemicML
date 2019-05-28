open IR

let bb_to_ssa tbl instrs f_after =
  let rec aux revlist = function
  | [] -> List.rev (f_after revlist tbl)
  | x::xs -> 
    let tx = IRLocalOpt.replace_instr_var_with_var false tbl x in
    let ti = match tx with
    | Assign(a,e) -> 
      if (IRUtil.varno a) < IRUtil.var_base
      then (let v = IRUtil.dup_var a in Hashtbl.replace tbl a v; Assign(v,e))
      else tx
    | CJmp(_,_,_)
    | Jmp(_)
    | Halt(_)
    | Assert(_)
    | Label(_)
    | Special(_)
    | Comment(_) -> tx
    in aux (ti::revlist) xs
  in aux [] instrs

let bb_to_ssa_state_in = bb_to_ssa

let bb_to_ssa_state_out instrs f_instr =
  let tbl = Hashtbl.create 100 in
  let instrs = bb_to_ssa tbl instrs f_instr in
  (tbl,instrs)

let bb_to_ssa instrs f_instr =
  let (_,instrs) = bb_to_ssa_state_out instrs f_instr in
  instrs
  