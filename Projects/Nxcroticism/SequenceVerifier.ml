open FrameworkUtil
open DataStructures

module Z3 = Z3.V3

let f_tptime = ref 0.0

(* Reject a sequence immediately if:
* x86 syntactically uses:
* * FS/GS prefix
* * Hard-coded memory accesses
* * 16-bit memory locations when instruction is not LEA
* * Memory locations that are not 8/16/32 bits
* * Control/debug/segment/FPU/XMM/MMX registers
* * Far pointers
* * Jump targets
*)
let x86_syntactic_reject { X86.pref = pref; X86.instr = (m,o); } = 
  let open X86 in
  let is_lea = match m with | Lea -> true | _ -> false in

  let reject_operand = function
  (* Don't reject general registers or immediates *)
  | GeneralReg(_) -> false
  | Immediate(_) -> false
  
  (* Reject anything with an FS prefix *)
  | Memexpr(Mb(Mem32(FS,_,_,_))) -> true
  | Memexpr(Mw(Mem32(FS,_,_,_))) -> true
  | Memexpr(Md(Mem32(FS,_,_,_))) -> true

  (* Reject anything with a GS prefix *)
  | Memexpr(Mb(Mem32(GS,_,_,_))) -> true
  | Memexpr(Mw(Mem32(GS,_,_,_))) -> true
  | Memexpr(Md(Mem32(GS,_,_,_))) -> true  

  (* Reject anything with a hard-coded memory address *)
  | Memexpr(Mb(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Mw(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Md(Mem32(_,None,None,Some(_)))) -> true

  (* Reject 16-bit memory addresses when the instruction is not LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) when not is_lea -> true
  | Memexpr(Mw(Mem16(_,_,_,_))) when not is_lea -> true
  | Memexpr(Md(Mem16(_,_,_,_))) when not is_lea -> true
  
  (* Accept 16-bit memory access if instruction is LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) -> false
  | Memexpr(Mw(Mem16(_,_,_,_))) -> false
  | Memexpr(Md(Mem16(_,_,_,_))) -> false
  
  (* Accept 8/16/32-bit memory access if not rejected above *)
  | Memexpr(Mb(_)) -> false
  | Memexpr(Mw(_)) -> false
  | Memexpr(Md(_)) -> false
  
  (* Reject "large" memory accesses *)
  | Memexpr(Mf(_)) -> true
  | Memexpr(Mq(_)) -> true
  | Memexpr(Mt(_)) -> true
  | Memexpr(Mdq(_)) -> true

  (* Reject anything with a control/debug/segment/FPU/MMX/XMM register *)
  | ControlReg(_) -> true
  | DebugReg(_) -> true
  | SegReg(_) -> true
  | FPUReg(_) -> true
  | MMXReg(_) -> true
  | XMMReg(_) -> true
  
  (* Reject anything with a far pointer *)
  | FarTarget(_) -> true
  
  (* Reject jumps (should have been rejected already anyway) *)
  | JccTarget(_) -> true
  
  in
  (* Apply the test above to all operands *)
  let rec aux = function
  | [] -> false
  | o::os -> if reject_operand o then true else aux os
  in 
  
  (* Reject direction/interrupt flag manipulations *)
  if m = X86.Cld || m = X86.Std || m = X86.Sti || m = X86.Cli
  then true
  else aux o

let determine_sequence_behaviors addr_ht x86l ir n =
  if (List.exists x86_syntactic_reject x86l) || ir = []
  then VerifyCandidates.BadAssembly
  else
  begin
    let e_opt,ir = match List.rev ir with
    (* This is what we *should* have *)
    | IR.Jmp(e)::is -> Some(e),List.rev is
    (* Forget anything else. *)
    | _ -> None,ir
    in
    
    match e_opt with
    | None -> VerifyCandidates.BadIR
    (* The destination for a return is a variable that is read off of the bottom 
       of the stack.  Given that the IR translator generates a new variable for
       this purpose and does not re-use variables, the variable will be preserved
       under SSA translation. *)
    | Some(e_jt) ->
      let ssa_tbl,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars ir in
      let ioc = GenerateIOPairs.make_io_structs ir n e_jt in
      let _ = IOPair.print_io_container ioc in
      
      let verify pc = 
        let before = Sys.time () in
        let z3ctx = Z3.mk_context_x [|("MODEL", "true");("SOFT_TIMEOUT", "5000")|] in
        let _ = Z3.set_logic z3ctx "QF_ABV" in
        let str = Z3SymbolicExecute.symbolic_execute z3ctx ir_ssa pc in
        (* blaaah, what a piece of shit *)
        let res = (str = "unsat\n") in
        
       (if res = false && Z3.get_search_failure z3ctx = Z3.TIMEOUT 
        then
          let ctxt_str = Z3.context_to_string z3ctx in
          f_printf "Timed-out query: %s%!\n" ctxt_str);
        
      
        Z3.del_context z3ctx;
        let after = Sys.time () in
        let delta = (after -. before) in
      (*f_printf "Verified in %f: %s was %b\n" delta (PpIR.ppExpr false pc) res;*)
      (*f_printf "Query verified in %f\n" delta;*)
        f_tptime := !f_tptime +. delta;
        
        res
      in
      
      let has_consistent_return_behavior,retloc32 =
        let open IOPair in
        match ioc.retaddr_displacement_opt with
        (* Negative return location -> ESP moved downwards *)
        | Some(d) when d >= 0l -> true,d
        | _ -> false,0l
      in
    (*let _ = List.iter (fun x -> f_printf "%s\n" (X86Disasm.string_of_x86instr x)) x86l in*)
      
      let open VerifyCandidates in
      match has_consistent_return_behavior,IOEsp.determine_and_verify_esp_delta verify ssa_tbl ioc with
      | true,Some(disp32) -> 
       (let candidates = Candidate.generate_sequence_behaviors_candidates ioc in
        let res = 
          VerifyCandidates.verify_candidate_sequence_behaviors 
            ir 
            candidates 
            e_jt
            retloc32
            disp32 
            verify 
            ssa_tbl 
        in match res with
        | VerifyCandidates.VerifiedBehaviors(v) -> 
          VerifyCandidates.VerifiedBehaviors({ v with addresses = addr_ht; x86 = x86l; })
        | _ -> res)
      | false,Some(disp32) -> VerifyCandidates.BadReturnBehavior
      | _,None -> VerifyCandidates.BadStackDelta
  end

