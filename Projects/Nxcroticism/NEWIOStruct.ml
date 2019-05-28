type memloc = NEWGadget.memloc
type l_memacc = (IR.var * memloc * IR.typereg) list
type io =
{
  l_io:      ((IR.var,IR.expr) Hashtbl.t * l_memacc * l_memacc) list;
  l_reads:   (memloc * IR.typereg) list;
  h_reads:   ((memloc * IR.typereg),IR.var) Hashtbl.t;
  l_writes:  (memloc * IR.typereg) list;
  h_writes:  ((memloc * IR.typereg),IR.var) Hashtbl.t;
  l_common:  (memloc * IR.typereg) list;
}

let ht_of_io io = match io.l_io with | (h,_,_)::_ -> h  | _ -> failwith "ht_of_io"

let make_io_struct ir_ssa rng32 = 
  let ht = NEWX86IRRandomizedEvaluator.initialize_registers rng32 in
  let reads,writes = NEWIRRandomizedEvaluator.eval_stmts ir_ssa ht in
  (ht,reads,writes)
  
let make_io_structs x86l rng32 n =
  match List.rev (List.concat (List.map (X86ToIR.translate_instr 0l) x86l)) with
  | IR.Jmp(e_jt)::is -> 
    let ht_ssa,ir_ssa = IRSSA.bb_to_ssa_state_out (List.rev is) (fun x _ -> x) in
    let ir_ssa = ir_ssa@(X86ToIRUtil.mk_after ht_ssa) in
    let rec aux out i = if i = n then out else aux ((make_io_struct ir_ssa rng32)::out) (i+1) in
    let ir_ssa = 
      let vmem = 
        try Hashtbl.find ht_ssa X86ToIRUtil.vMem 
        with Not_found -> X86ToIRUtil.vMem
      in ir_ssa@[IRUtil.mk_assign X86ToIRUtil.vMemAfter (IRUtil.mk_evar vmem)]
    in
    Some(ir_ssa,e_jt,aux [] 0)
  | _ -> None

let is_true x = x = 1l
let is_eval_true ht e = is_true (NEWIREval.eval_to_int32 ht e)
let all_ios_true io e = List.exists (fun (h,_,_) -> is_eval_true h e) io.l_io

