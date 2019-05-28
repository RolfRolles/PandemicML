open FrameworkUtil
open DataStructures
open Candidate

type verified_results =
{
  (* Addresses of gadget *)
  addresses: (int32,unit) Hashtbl.t;
  
  (* X86 instructions *)
  x86: X86.x86instrpref list;

  (* Consistent stack displacement *)
  stack_displacement: int32;
  
  (* Depth of return address within stack *)
  return_address_displacement: int32;
  
  (* Those registers which are preserved *)
  preserved_regs:     IRUtil.VarSet.t triplicate;
  
  (* Those registers which are copied *)
  copied_regs:        VarVarSet.t triplicate option;
  
  (* Those registers which are set to constants *)
  set_const:          VarInt32Set.t triplicate option;
  
  (* Reads from memory, non-ESP *)
  mem_read_const:     VarVarInt32Set.t triplicate option;
  
  (* Reads from memory, ESP *)
  esp_read_const:     VarInt32Set.t triplicate option;

  (* Writes to memory, non-ESP *)
  mem_write_reg:      VarVarInt32Set.t triplicate option;
  
  (* Writes to memory, ESP *)
  esp_write_reg:      VarInt32Set.t triplicate option;

  (* Writes to memory as binop, non-ESP *)
  write_binops:       VarBinopVarInt32Set.t triplicate option;
  
  (* Writes to memory as binop, ESP *)
  write_esp_binops:   VarBinopInt32Set.t triplicate option;

  (* Reads from memory as binop, non-ESP *)
  read_binops:        VarBinopVarInt32Set.t triplicate option;
  
  (* Reads from memory as binop, ESP *)
  read_esp_binops:    VarBinopInt32Set.t triplicate option;

  (* Register-based binops *)
  reg_binops:         VarVarBinopVarSet.t triplicate option;
}

let print_verified_results vr = 
  let _ = f_printf "\nESP displacement: 0x%lx\n" vr.stack_displacement in
  
  f_printf "Preserved registers: {";
  print_triplicate dump_varset vr.preserved_regs;
  f_printf "}\n";
  
 (match vr.copied_regs      with | None -> () | Some(s) -> dump_reg_copies s);
 (match vr.set_const        with | None -> () | Some(s) -> dump_set_const s);
 (match vr.mem_read_const   with | None -> () | Some(s) -> dump_load_const s);
 (match vr.esp_read_const   with | None -> () | Some(s) -> dump_read_esp s);
 (match vr.mem_write_reg    with | None -> () | Some(s) -> dump_write_const s);
 (match vr.esp_write_reg    with | None -> () | Some(s) -> dump_write_esp s);
 (match vr.reg_binops       with | None -> () | Some(s) -> dump_register_binop_behaviors s);
 (match vr.read_binops      with | None -> () | Some(s) -> dump_read_binop_behaviors s);
 (match vr.read_esp_binops  with | None -> () | Some(s) -> dump_read_esp_binops s);
 (match vr.write_binops     with | None -> () | Some(s) -> dump_write_binop_behaviors s);
 (match vr.write_esp_binops with | None -> () | Some(s) -> dump_write_esp_binops s);
  f_printf "\n"

(* Makes the following condition, which should be UNSAT to be true: 
  (For all w in l_w_byte_addr32. (idx != w)) && mempre[idx] != mempost[idx] *)
let mk_write_safety_predicate l_byte_expr vmempost =
  let vidx     = IRUtil.new_var IR.TypeReg_32 in
  let eidx     = IRUtil.mk_evar vidx in
  let emempost = IRUtil.mk_evar vmempost in
  let addrne be = IRUtil.mk_ne eidx be in
  let snd   = IRUtil.mk_ne 
    (IRUtil.mk_load X86ToIRUtil.eMem eidx IR.TypeReg_8)
    (IRUtil.mk_load emempost         eidx IR.TypeReg_8)
  in 
  let addrexpr = match l_byte_expr with
  | []     -> IRUtil.mk_true
  | hd::tl -> List.fold_left (fun p a32 -> IRUtil.mk_and p (addrne a32)) (addrne hd) tl
  in 
  IRUtil.mk_and snd addrexpr

let generate_memloc_set access_t access_binop_t = 
  (* Generate list of verified write locations *)
  let memlocs_t = { val8 = []; val16 = []; val32 = []; } in
  let memlocs_t = 
    triple_fold_triple 
      VarVarInt32Set.fold 
     (fun (_,vr,d32)   acc -> (vr,d32)::acc) 
      memlocs_t 
      access_t 
  in
  let memlocs_t = 
    triple_fold_triple 
      VarBinopVarInt32Set.fold 
     (fun (_,_,vr,d32) acc -> (vr,d32)::acc) 
      memlocs_t 
      access_binop_t 
  in
  
  let expr_set size list = 
    let n = Int32.of_int (IRUtil.bits size / 8) in
    let rec aux i acc = 
      if i = n
      then acc
      else 
        let set = 
          List.fold_left 
           (fun set (vr,d32) -> VarInt32Set.add (vr,Int32.add d32 i) set) 
            acc 
            list 
        in
        aux (Int32.succ i) set
    in
    aux 0l VarInt32Set.empty
  in
  let memlocs_t = triplicate_map expr_set memlocs_t in
  VarInt32Set.union (VarInt32Set.union memlocs_t.val8 memlocs_t.val16) memlocs_t.val32
  
let generate_memloc_list s_memaddrs s_write_reg_t s_write_binop_t = 
  VarInt32Set.elements s_memaddrs

let mk_addr_expr v32 d32 = IRUtil.mk_add (IRUtil.mk_evar v32) (IRUtil.mk_dword_of_int32 d32)

let generate_memexpr_list l_memaddrs =
  List.map (fun (v32,d32) -> mk_addr_expr v32 d32) l_memaddrs

let mk_distinction_assertion s_memlocs1 s_memlocs2 =
  VarInt32Set.fold (fun (vout,dout) p -> 
    VarInt32Set.fold (fun (vin,din) p ->
      if vin <> vout
      then IRUtil.mk_and (IRUtil.mk_ne (mk_addr_expr vout dout) (mk_addr_expr vin din)) p
      else p)
      s_memlocs1
      p)
    s_memlocs2
    IRUtil.mk_true
    
let mk_write_distinction_assertion s_memlocs = 
  mk_distinction_assertion s_memlocs s_memlocs
  
(* Duplicated; also in GenerateIOPairs *)
let nonzero_rand_i32 () = Int32.succ (Random.int32 (Int32.pred Int32.max_int))

let has_unconstrained_memaccesses s_memregs ir =
  let memregasg = List.map (fun v -> (v,nonzero_rand_i32 ())) (IRUtil.VarSet.elements s_memregs) in
  let ht = new_ht IR.TypeReg_32 memregasg in
  let oir = IRLocalOpt.local_opt_state_in ht ir in
  
  let open IR in  
  let rec extract_memaddrs = function
  | Cast(_,_,e)
  | Unop(_,e)        -> extract_memaddrs e
  | Load(e1,a,_)     -> a::(extract_memaddrs e1)
  | Let(_,e1,e2)
  | Binop(e1,_,e2)   -> (extract_memaddrs e1)@(extract_memaddrs e2)                  
  | Store(e1,a,e3,_) -> a::((extract_memaddrs e1)@(extract_memaddrs e3))
  | Var(_)
  | Const(_)         -> []
  in

  let extract_memaddrs = function
  | Assign(_,e)     
  | Jmp(e)          
  | Halt(e)         
  | Assert(e)       -> extract_memaddrs e
  | CJmp(e1,e2,e3)  -> (extract_memaddrs e1)@(extract_memaddrs e2)@(extract_memaddrs e3)
  | Label(_)
  | Special(_)
  | Comment(_)      -> []
  in

  let memaddrs = List.fold_left (fun acc i -> (extract_memaddrs i)@acc) [] oir in
  List.exists (function | Const(_,_) -> false | _ -> true) memaddrs

(* So in which order should I verify these properties?
   DEPENDENCIES:
   
   Set const:  Technically, this could be affected by reads and writes, i.e., false negatives.  But 
   it is very useful to do this first, so as to reduce the number of queries regarding binops.  At 
   worst, we could issue this query again after verifying the reads and writes.

   Write reg binops: affects reads.
   Memwrite  binops: affects reads.
   
   Read reg:     this is affected by prior writes.
   Read binops:  affected by prior writes.

   Copy:       I guess technically this could go by way of a write to memory, followed by a read
   Reg binops: I guess technically this could go by way of a write to memory, followed by a read
   
   So, verify writes first.  Then verify write safety.
   Verify reads next.  Then verify read safety.
   
   Then verify everything else.
   *)


(* let _ = IDA.msg "2\n" in*)

(* Remove binops such as add [eax], al and add al, [eax] where the value register
   depends upon the address register. *)
let remove_dependent_mem_binops s_mem_binop_t =
  let had_dep = ref false in
  let s_w32 = 
    VarBinopVarInt32Set.filter 
     (fun (vv,_,vr,_)  -> if vv <> vr then true else (had_dep := true; false))
      s_mem_binop_t.val32 
  in
  let s_w16 = 
    VarBinopVarInt32Set.filter 
     (fun (v16,_,vr,_) -> 
        if v16 <> (X86ToIRUtil.v16_of_v32_exn vr) then true else (had_dep := true; false))
      s_mem_binop_t.val16 
  in
  let s_w8 = 
    VarBinopVarInt32Set.filter 
     (fun (v8,_,vr,_) -> 
        match X86ToIRUtil.v8p_opt_of_v32_exn vr with
        | Some(vh8,vl8) -> 
          if v8 <> vh8 && v8 <> vl8 then true else (had_dep := true; false)
        | None -> true) 
      s_mem_binop_t.val8
  in
  (!had_dep,{ val8 = s_w8; val16 = s_w16; val32 = s_w32; })

let verify_proper_return e_jt disp32 =
  let open VerifyUtil in
  IRUtil.mk_ne 
    e_jt 
   (mk_load_disp32_pre X86ToIRUtil.vEsp disp32 IR.TypeReg_32)

let verify_proper_return p verify e_jt disp32 =
  verify (IRUtil.mk_and p (verify_proper_return e_jt disp32))

type ver_err =
| BadAssembly
| BadIR
| NoReturnBehavior
| BadReturnBehavior
| BadStackDelta
| WriteSafetyFailed
| ReadSafetyFailed
| ReadWriteCorrelationFailed
| TooManyWrites
| ReadOutsideStackFrame
| NoInterestingBehaviors
| VerifiedBehaviors of verified_results

(* This hash table is not actually used for anything.  It will be replaced in
   the caller.  So we can make it once instead of many times. *)
let dummy_ht = Hashtbl.create 1

let verify_candidate_sequence_behaviors ir candidates e_jt retloc32 disp32 verify ssa_tbl (*z3debug*) =
  (* First, generate the set of memory locations that are potentially written to,
     and generate a (way too large) predicate asserting that these locations do
     not overlap.  This is necessary to allow the write and subsequent read 
     queries to verify properly. *)
  let s_c_write_memlocs = generate_memloc_set candidates.c_mem_write_reg candidates.c_write_binops in
  let pwrite = mk_write_distinction_assertion s_c_write_memlocs in
  
(*f_printf "Write indistinction predicate: %s\n" (PpIR.ppExpr false pwrite);*)

  (* Get rid of write binops where the address register is also used as the 
     right-hand-side.  E.g., add [eax], al.  These are useless. *)
  let b_w_dep,c_write_binops = remove_dependent_mem_binops candidates.c_write_binops in
  
  (* Verify the write operations with respect to the distinction assertion that
     was generated above. *)
  let s_write_reg_t   = IOWrite.verify_write_behaviors pwrite verify ssa_tbl candidates.c_mem_write_reg in
  let s_write_binop_t = IOBinop.verify_memwrite_binop_behaviors pwrite verify ssa_tbl c_write_binops in  
  
  (* Regenerate the written locations, only containing those that were actually
     verified. *)
  let s_write_memlocs = generate_memloc_set s_write_reg_t s_write_binop_t in

  (* Determine whether the sequence is write-safe.  I.e., the only locations 
     that are written are the ones that were previously verified. *)
  let bwritesafe = 
    
    (* Look up the final memory variable. *)
    match ht_find_opt ssa_tbl X86ToIRUtil.vMem with
    (* If this does not exist in the SSA hash table, that means that the 
       sequence is syntactically guaranteed not to write to memory, and so the
       sequence is write-safe. *)
    | None -> true

    (* If it did syntactically write to memory, check the write safety predicate. *)
    | Some(memvar) ->
      let memlocs = generate_memexpr_list (VarInt32Set.elements s_write_memlocs) in
      let res = verify (mk_write_safety_predicate memlocs memvar) in
    (*f_printf "\nWrite safety predicate: %b\n" res;*)
      res
  in
  
  (* If we weren't write-safe, there's no point in verifying anything else. *)
  if not bwritesafe
  then WriteSafetyFailed
  else   

    (* Verify the constant-setting behaviors of the sequence.  It is useful to
       do this before subsequent queries.  In particular, sequences that set
       some register to zero generate a lot of false positives for binary 
       operations.  So verify these behaviors first, and then prune the binop
       and read binop sets to remove those false positives.  This prevents 
       having to issue a lot of calls to the theorem prover. *)
    let s_set_const_t = IOConstant.verify_set_const_behaviors verify ssa_tbl candidates.c_set_const in

    let filter_zero_const size =
      let s_v_const = select_member size s_set_const_t in
      VarBinopVarInt32Set.filter (fun (vr,_,_,_) -> not (VarInt32Set.mem (vr,0l) s_v_const))
    in
    let c_read_binops = triplicate_map filter_zero_const candidates.c_read_binops in
    
    (* Remove dependent read binops such as add al, [eax], as these are useless. *)
    let b_r_dep,c_read_binops  = remove_dependent_mem_binops c_read_binops in

    (* Generate an assertion that says that the written locations do not overlap
       with the read locations, thereby disentangling these behaviors and 
       allowing the reads to be verified. *)
    let esp = X86ToIRUtil.vEsp in
    let c_mem_read_const_with_ret = 
    { 
      candidates.c_mem_read_const with 
      val32 = 
        VarVarInt32Set.add 
         (esp,esp,retloc32) 
         (candidates.c_mem_read_const.val32);
    }
    in

    let s_c_read_memlocs = generate_memloc_set c_mem_read_const_with_ret c_read_binops in
    let pread = mk_distinction_assertion s_c_read_memlocs s_write_memlocs in
    
    if not (verify_proper_return pread verify e_jt retloc32)
    then 
     ((*f_printf "Failed return check\n";*)
      NoReturnBehavior)
    else

      (* Verify the read behaviors with respect to the read/write distinction assertion. *)
      let s_read_binop_t = IOBinop.verify_memread_binop_behaviors pread verify ssa_tbl c_read_binops in
      let s_read_reg_t   = IORead.verify_read_behaviors pread verify ssa_tbl candidates.c_mem_read_const in    
      
      (* Generate the set of read locations, post-verification. *)
      let s_read_memlocs = generate_memloc_set s_read_reg_t s_read_binop_t in
  
      (* Function to collect the registers that are used as bases for reads/writes. *)
      let collect_memregs = VarInt32Set.fold (fun (v,_) set -> IRUtil.VarSet.add v set) in
      
      (* Collect the read/write registers.  With respect to putting ESP in the set,
         I thought that this would be naturally entailed by the sequence, but 
         apparently, by changing this from "singleton" to "empty", the subsequent
         query is false. *)
      let memregs = collect_memregs s_write_memlocs (IRUtil.VarSet.singleton X86ToIRUtil.vEsp) in
      let memregs = collect_memregs s_read_memlocs memregs in
      
      (* This check seems good for what it does, although more work might need
         to be done in this area.  It excludes gadgets that write to non-stack
         addresses that also read from different addresses. 
         
         I think I want to change the subset ordering:  that the reads are a 
         subset of the writes, and not vice versa.  Effectively, it 
         accomplishes the same check.
         
         How about, is_empty writelocs || is_empty readlocs || 
         subset writelocs readlocs || subset readlocs writelocs <- maybe equal?
         *)
      let s_write_memlocs_noesp = VarInt32Set.filter (fun (x,_) -> x <> esp) s_write_memlocs in
      let s_read_memlocs_noesp  = VarInt32Set.filter (fun (x,_) -> x <> esp) s_read_memlocs  in
      let bhas_uncorrelated_reads = 
        not
         ((VarInt32Set.is_empty s_read_memlocs_noesp) || 
          (VarInt32Set.subset s_write_memlocs_noesp s_read_memlocs_noesp))

      in
      
      if bhas_uncorrelated_reads
      then ReadWriteCorrelationFailed
      else

      (* Assign random values to each of the registers used as memory locations,
         then constant-fold the sequence, and see if all memory addresses are 
         now constant.  The idea being that, if this is true, then we have 
         accounted for all reads and writes exhibited by the sequence. *)
      let bhas_unconstrained_memaccesses = has_unconstrained_memaccesses memregs ir in
      
    (*f_printf "Has unconstrained memaccesses: %b\n" bhas_unconstrained_memaccesses;*)
      
      (* If it has unpredictable memory accesses, then we reject the sequence. *)
      if bhas_unconstrained_memaccesses
      then ReadSafetyFailed
      else
        
        (* Filter out register binops where the destination is proved to be zero. *)
        let filter_zero_const size =
          let s_v_const = select_member size s_set_const_t in
          VarVarBinopVarSet.filter (fun (vr,_,_,_) -> not (VarInt32Set.mem (vr,0l) s_v_const))
        in
        let c_reg_binops = triplicate_map filter_zero_const candidates.c_reg_binops in
        
        (* Verify the copy behaviors and the register binops behaviors. *)
        let s_copy_pairs_t = IOCopy.verify_copy_behaviors verify ssa_tbl candidates.c_copied_regs in
        let s_reg_binop_t  = IOBinop.verify_register_binop_behaviors verify ssa_tbl c_reg_binops in
        
        (* Some syntax sugar to make the checks below shorter. *)
        let vve_t   t = let e = VarVarSet.is_empty           in e t.val32 && e t.val16 && e t.val8 in
        let vie_t   t = let e = VarInt32Set.is_empty         in e t.val32 && e t.val16 && e t.val8 in
        let vvie_t  t = let e = VarVarInt32Set.is_empty      in e t.val32 && e t.val16 && e t.val8 in
        let vvbve_t t = let e = VarVarBinopVarSet.is_empty   in e t.val32 && e t.val16 && e t.val8 in
        let vbvie_t t = let e = VarBinopVarInt32Set.is_empty in e t.val32 && e t.val16 && e t.val8 in
        let vbie_t  t = let e = VarBinopInt32Set.is_empty    in e t.val32 && e t.val16 && e t.val8 in
        
        (* Don't compute the preserved set unless it has some other interesting behaviors *)
        let b,s_preserved = 
  
          (* Did it have any interesting verifiable behaviors at all? *)
          if not 
           ((vve_t s_copy_pairs_t)   &&
            (vie_t s_set_const_t)    &&
            (vvie_t s_read_reg_t)    &&
            (vvie_t s_write_reg_t)   &&
            (vbvie_t s_write_binop_t) &&
            (vbvie_t s_read_binop_t)  &&
            (vvbve_t s_reg_binop_t))
  
          (* Yes, something was verified, so compute the preserved behaviors. *)
          then true,IOPreserve.verify_preserved_behaviors verify ssa_tbl candidates.c_preserved_regs
  
          (* No, return a junk value that won't be used. *)
          else 
            false,
            {  val8 = IRUtil.VarSet.empty; 
              val16 = IRUtil.VarSet.empty; 
              val32 = IRUtil.VarSet.empty; }
        in

        (* Were there verifiable behaviors? *)
        if not b
        
        (* No, bail. *)
        then NoInterestingBehaviors
        
        (* Yes, there were behaviors.  Return the verified set. *)
        else 
          let split_vvi_set size s_t = 
            VarVarInt32Set.partition 
             (fun (_,v,_) -> v <> X86ToIRUtil.vEsp) 
             (select_member size s_t) 
          in
          let f_map_vvi_var_away size s_t =
            VarVarInt32Set.fold 
             (fun (v,_,i) set -> VarInt32Set.add (v,i) set) 
             (select_member size s_t)
              VarInt32Set.empty
          in
  
          let s_read_reg_t,s_read_reg_esp_t = 
            triplicate_depair (triplicate split_vvi_set s_read_reg_t) 
          in
          let s_read_esp_t = triplicate f_map_vvi_var_away s_read_reg_esp_t in
          
          let s_write_reg_t,s_write_reg_esp_t = 
            triplicate_depair (triplicate split_vvi_set s_write_reg_t) 
          in
          let s_write_esp_t = triplicate f_map_vvi_var_away s_write_reg_esp_t in
  
          let split_vbvi_set size s_t = 
            VarBinopVarInt32Set.partition 
             (fun (_,_,v,_) -> v <> X86ToIRUtil.vEsp) 
             (select_member size s_t)
          in
          let f_map_vbvi_var_away size s_t =
            VarBinopVarInt32Set.fold 
             (fun (v,b,_,i) set -> VarBinopInt32Set.add (v,b,i) set) 
             (select_member size s_t)
              VarBinopInt32Set.empty
          in
          
          let s_read_binop_t,s_read_binop_esp_t = 
            triplicate_depair (triplicate split_vbvi_set s_read_binop_t) 
          in
          let s_read_binop_esp_t = triplicate f_map_vbvi_var_away s_read_binop_esp_t in
  
          let s_write_binop_t,s_write_binop_esp_t = 
            triplicate_depair (triplicate split_vbvi_set s_write_binop_t) 
          in
          let s_write_binop_esp_t = triplicate f_map_vbvi_var_away s_write_binop_esp_t in
  
          let s_read_esp_t = 
            if (vie_t s_read_esp_t)
            then s_read_esp_t
            else 
              let filter size = 
                let i = Int32.of_int ((IRUtil.bits size / 8) - 1) in
                VarInt32Set.filter 
                 (fun (_,begin32) -> 
                    (Int32.add begin32 i) < retloc32 || begin32 > (Int32.add retloc32 3l))
              in
              triplicate_map filter s_read_esp_t
          in
          
          (* This check apparently does not work.  It rejects everything. *)
          let has_reads_outside_frame =
            let foldf i32 = fun (_,begin32) b -> b && not (Int32.add begin32 i32 >= disp32) in
            let b = VarInt32Set.fold (foldf 0l) s_read_esp_t.val8  true in
            let b = VarInt32Set.fold (foldf 1l) s_read_esp_t.val16 b in
            let b = VarInt32Set.fold (foldf 3l) s_read_esp_t.val32 b in
            b
          
          in
          
          (*if has_reads_outside_frame
          then ReadOutsideStackFrame
          else*)
            let mk_opt e v = if e v then None else Some(v) in
            
            VerifiedBehaviors({
              addresses = dummy_ht;
              x86 = [];
              stack_displacement = disp32;
              return_address_displacement = retloc32;
              preserved_regs   = s_preserved;
              copied_regs      = mk_opt vve_t   s_copy_pairs_t;
              set_const        = mk_opt vie_t   s_set_const_t;
              mem_read_const   = mk_opt vvie_t  s_read_reg_t;
              esp_read_const   = mk_opt vie_t   s_read_esp_t;
              mem_write_reg    = mk_opt vvie_t  s_write_reg_t;
              esp_write_reg    = mk_opt vie_t   s_write_esp_t;
              read_binops      = mk_opt vbvie_t s_read_binop_t;
              read_esp_binops  = mk_opt vbie_t  s_read_binop_esp_t;
              write_binops     = mk_opt vbvie_t s_write_binop_t;
              write_esp_binops = mk_opt vbie_t  s_write_binop_esp_t;
              reg_binops       = mk_opt vvbve_t s_reg_binop_t;
            })
