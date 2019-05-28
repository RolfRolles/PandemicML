open FrameworkUtil
open DataStructures
open Encodable

(* No encoding required *)
let cache_write_binops gc enc s_clobbered size s_vbvi = 
  VarBinopVarInt32Set.iter 
   (fun (vr,b,vm,d32) -> GadgetCache.accept_write_binop gc size vr b vm d32 enc s_clobbered)
    s_vbvi

let cache_write_binops gc enc s_clobbered s_vbvi_t =
  triplicate_iter (cache_write_binops gc enc s_clobbered) s_vbvi_t

(* No encoding required *)
let cache_write_regs gc enc s_clobbered size s_vvi =
  VarVarInt32Set.iter
   (fun (vr,vm,d32) -> GadgetCache.accept_write_reg gc size vr vm d32 enc s_clobbered)
    s_vvi

let cache_write_regs gc enc s_clobbered s_vvi_t =
  triplicate_iter (cache_write_regs gc enc s_clobbered) s_vvi_t

(* No encoding required *)
let cache_read_binops gc enc s_clobbered size s_vbvi = 
  VarBinopVarInt32Set.iter 
   (fun (vr,b,vm,d32) -> GadgetCache.accept_read_binop gc size vr b vm d32 enc s_clobbered)
    s_vbvi

let cache_read_binops gc enc s_clobbered s_vbvi_t =
  triplicate_iter (cache_read_binops gc enc s_clobbered) s_vbvi_t

(* No encoding required *)
let cache_read_regs gc enc s_clobbered size s_vvi =
  VarVarInt32Set.iter
   (fun (vr,vm,d32) -> GadgetCache.accept_read_reg gc size vr vm d32 enc s_clobbered)
    s_vvi

let cache_read_regs gc enc s_clobbered s_vvi_t =
  triplicate_iter (cache_read_regs gc enc s_clobbered) s_vvi_t

(* No encoding required *)
let cache_reg_binops gc enc s_clobbered size =
  VarVarBinopVarSet.iter (fun (vd,vl,b,vr) -> GadgetCache.accept_reg_binop gc size vd vl b vr enc s_clobbered)

let cache_reg_binops gc enc s_clobbered s_vbv_t =
  triplicate_iter (cache_reg_binops gc s_clobbered enc) s_vbv_t

(* No encoding required *)
let cache_reg_copies gc enc s_clobbered size =
  (* Is this correct? Wrong ordering? *)
  VarVarSet.iter (fun (vdst,vsrc) -> GadgetCache.accept_reg_copy gc size vdst vsrc enc s_clobbered)

let cache_reg_copies gc enc s_clobbered s_vv_t =
  triplicate_iter (cache_reg_copies gc enc s_clobbered) s_vv_t

let cache_stack_reads gc enc s_clobbered size =
  VarInt32Set.iter 
   (fun (v,d32) -> 
      let enc = { enc with constant_position = Some(Int32.to_int d32,size); } in
      GadgetCache.accept_set_reg_value gc size v enc s_clobbered)

let cache_stack_reads gc enc s_clobbered =
  triplicate_iter (cache_stack_reads gc enc s_clobbered)    

let deopt f = function | None -> () | Some(s) -> f s

let cache_write_behaviors gc enc ver s_clobbered = 
  let open VerifyCandidates in
  let _ = f_printf "cache_write_behaviors\n%!" in
  deopt (cache_write_binops gc enc s_clobbered) ver.write_binops;
  deopt (cache_write_regs   gc enc s_clobbered) ver.mem_write_reg;
  ()
  
let cache_read_behaviors gc enc ver s_clobbered = 
  let open VerifyCandidates in
  let _ = f_printf "cache_read_behaviors\n%!" in
  deopt (cache_read_binops gc enc s_clobbered) ver.read_binops;
  deopt (cache_read_regs   gc enc s_clobbered) ver.mem_read_const;
  ()

let cache_reg_and_stack_behaviors gc enc ver s_clobbered = 
  let open VerifyCandidates in
  let _ = f_printf "cache_reg_and_stack_behaviors\n%!" in
  deopt (cache_reg_binops gc enc s_clobbered) ver.reg_binops;
  deopt (cache_reg_copies gc enc s_clobbered) ver.copied_regs;
  deopt (cache_stack_reads gc enc s_clobbered) ver.esp_read_const;
  ()

(* Basically, we want to reject if:
   - write to the stack, OR
   - write to the stack binop, OR
   - read from the stack binop (could maybe allow this later), OR
   - write to multiple locations (could just immediately reject if the number of writes is greater than one), OR
   - if it reads and writes, the latter not is a subset of the former (done), OR  
*)
let cache_verified_results gc ver x86l = 
  (* Some syntax sugar to make the checks below shorter. *)
  let vie_t   t = let e = VarInt32Set.is_empty         in e t.val32 && e t.val16 && e t.val8 in
  let vvie_t  t = let e = VarVarInt32Set.is_empty      in e t.val32 && e t.val16 && e t.val8 in
  let vbvie_t t = let e = VarBinopVarInt32Set.is_empty in e t.val32 && e t.val16 && e t.val8 in
  let vbie_t  t = let e = VarBinopInt32Set.is_empty    in e t.val32 && e t.val16 && e t.val8 in
  let wrap f = function | Some(x) -> f x | None -> true in

  (* Determine the presence of memory read / write operations *)
  let open VerifyCandidates in
  let b_stack_write_binops = not (wrap vbie_t  ver.write_esp_binops) in
  let b_stack_writes       = not (wrap  vie_t  ver.esp_write_reg)    in
  let b_mem_write_binops   = not (wrap vbvie_t ver.write_binops)     in
  let b_mem_writes         = not (wrap vvie_t  ver.mem_write_reg)    in

  let b_stack_read_binops  = not (wrap vbie_t  ver.read_esp_binops)  in
  let b_stack_reads        = not (wrap vie_t   ver.esp_read_const)   in  
  let b_mem_read_binops    = not (wrap vbvie_t ver.read_binops)      in
  let b_mem_reads          = not (wrap vvie_t  ver.mem_read_const)   in
  
  (* Compute some statistics about the gadget necessary for encoding and 
     further synthesis *)
  let framesize = Int32.to_int ver.stack_displacement in
  let retpos    = Int32.to_int ver.return_address_displacement in
  let enc = mk_encodable x86l ver.addresses framesize retpos in
  let s_clobbered = X86ToIRUtil.s_general_registers_noesp_nosp in
  
  let s_clobbered = 
    fold_triple 
     (fun x -> x) 
     (fun x y -> IRUtil.VarSet.diff y x) 
      s_clobbered 
      ver.preserved_regs 
  in
    
  (* Exclude writes to the stack and binops that read from the stack.  Could
     potentially use stack binops at a later time. *)
  if (b_stack_write_binops || b_stack_writes || b_stack_read_binops)
  then (f_printf "Rejected due to stack writes / stack read binops\n%!"; ())
  else 
  begin
    (* If gadget has write behaviors, use those and nothing else.  This is so
       we don't use something that writes to memory as a more innocuous gadget
       that will not cause a memory fault if used spuriously. *)
    if (b_mem_write_binops || b_mem_writes)
    then cache_write_behaviors gc enc ver s_clobbered
    else 
    begin
      (* If gadget has read behaviors, use those for similar reasons.  
         Supposedly, by a check that I put in in the verification module, 
         write behaviors will be preferred over read ones but this is OK
         since the reads are a subset of the writes. *)
      if (b_mem_read_binops || b_mem_reads)
      then cache_read_behaviors gc enc ver s_clobbered
      
      (* If it does not have memory behaviors, then deal with its ordinary 
         register-based behaviors. *)
      else cache_reg_and_stack_behaviors gc enc ver s_clobbered
    end
  end
