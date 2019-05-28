(* GREP FIXME *)

(* DUPLICATED FROM NEWSmallerROPC.ml; put this into NEWIREval directly (expr_memless?) *)
let eval ht e = NEWIREval.expr e ht (fun _ _ -> failwith "Memory read in eval") (fun _ _ _ -> failwith "Memory write in eval")
let eval_to_int32 ht e = match (eval ht e) with | IR.Const(i,_) -> Int64.to_int32 i | _ -> failwith "eval_to_int32"
let is_eval_false ht e = eval_to_int32 ht e = 0l

(* COPIED FROM IRLocalOpt BECAUSE IT WAS NOT EXPOSED, FIX THIS *)
let extract_uses_and_defs i = let open IR in let open IRUtil in let open IRLocalOpt in match i with
| Assign(v,e)    -> (Some v, var_setify  (extract_uses e))
| CJmp(e1,e2,e3) -> (None  , var_setify ((extract_uses e1)@(extract_uses e2)@(extract_uses e3)))
| Jmp(e)
| Halt(e)
| Assert(e)  -> (None, var_setify (extract_uses e))
| Label(_)
| Special(_)
| Comment(_) -> (None, VarSet.empty)

(* Very handy debugging function.  Given a trace in SSA form, and a hash table
   containing the results of evaluating the trace, print out the IR annotated
   with comments saying which registers had which values. *)
let diagnose ir ht =
  let print_value v = Printf.printf "%s->%s, " (PpIR.ppVar v) (try (PpIR.ppExpr false (Hashtbl.find ht v)) with Not_found -> "???") in
  let print_set s = IRUtil.VarSet.iter print_value s in
  List.iter (fun i -> let _ = Printf.printf "\n%s ;" (PpIR.ppInstr i) in
    match extract_uses_and_defs i with
    | Some(v),s -> print_value v; print_set s
    | None,s    -> print_set s)
    ir

(* DUPLICATED FROM X86ToIR *)
let mk_ite eb ethen eelse =
  let open IRTypeCheck in let open IRUtil in
  (* Sizes must be equal, but we assume that we're given well-typed 
     expressions; if this is not the case, typechecking will detect
     this *)
  let sz e = type_of_integer_type (typecheck_expr e) in
  let ethen = mk_and (mk_signed_cast (sz ethen) eb) ethen in
  let eelse = mk_and (mk_signed_cast (sz eelse) (mk_not eb)) eelse in
  mk_or ethen eelse

(* Get a list of prefixes of `list`, length l to length n, in order of increasing length.
   This is used to harvest sequences from obfuscated disassembly, which might correspond
   to something generalizable. *)
let take_prefixes l n list = 
  let _ = if not (l < n && n > 0 && l > 0) then invalid_arg "take_prefixes" in
  let rec take i last out = function
  | []           -> out
  | _ when i = n -> out
  | x::xs -> take (i+1) (x::last) ((x::last)::out) xs
  in 
  let rec discard j = function
  | [] -> []
  | x::xs when j < l -> discard (j+1) xs
  | l -> l
  in
  discard 1 (List.rev (take 0 [] [] list))

(* Functions for random number generation in memory reads.  These allow us to
   quickly discard erroneous memory behaviors. *)
let zerorng _  = 0l
let ffrng   _  = 0xFFl
let prndgen _  = Int32.succ (Random.int32 0xFFl)
let a_rnd      = Array.init 4096 prndgen
let arrrnd a32 = a_rnd.(Int32.to_int a32 land 0xfff)
let l_rng      = [zerorng;ffrng;arrrnd]

(* Register states in which to evaluate the sequences.  Counterpart to the 
   memory states above. *)
let l_cand_ht = let i = NEWX86IRRandomizedEvaluator.initialize_registers in 
 [i zerorng;i (fun _ -> 0xFFFFFFFFl);i (fun _ -> 1l);i (fun _ -> Int32.succ (Random.int32 Int32.max_int))]

(* Given a "keep" function that ignores certain memory addresses, see if the 
   contents of the other memory locations are the same. *)
let compare_mem keep ht1 ht2 = 
  try (Hashtbl.iter 
   (fun a v -> if (try Hashtbl.find ht2 a <> v with Not_found -> keep a) then raise Not_found) ht1; true)
  with Not_found -> false
let compare_mem2 keep ht1 ht2 = compare_mem keep ht1 ht2 && compare_mem keep ht2 ht1

(* Generalization components; out-of-place, but can't be moved right now *)
let vGen,vIdx   = IR.Variable(500,IR.TypeReg_32),IRUtil.new_var IR.TypeReg_8
let vGen1,vGen2 = IRUtil.new_var IR.TypeReg_32, IRUtil.new_var IR.TypeReg_32
let eGen        = IRUtil.mk_evar vGen
let eIdx        = IRUtil.mk_evar vIdx
let eGen1,eGen2 = IRUtil.mk_evar vGen1,IRUtil.mk_evar vGen2
let genp        = IRUtil.mk_ne eGen1 eGen2

(* Hash table utilities for evaluating predicates over two runs *)
let get_after_values   ht = List.rev_map (fun (_,va) -> (va,eval ht (IRUtil.mk_evar va)))
let get_after2_values  ht = get_after_values ht X86ToIRUtil.l_p_var_varafter2
let get_after_values   ht = get_after_values ht X86ToIRUtil.l_p_var_varafter

let f_candidate h m = (h,m) let f_obfuscated h m = (h,m,get_after2_values h)

(* Evaluate a sequence in a given register context with a given rng *)
let eval_with_rng ir_ssa ht f rng =
  let ht,memctx = Hashtbl.copy ht,Hashtbl.create 30 in 
  let fr,_,fw,_,_ = NEWIRRandomizedEvaluator.mk_mem_funs_ext memctx rng in
  let _ = NEWIREval.stmts ir_ssa ht fr fw in
  f ht memctx
  
(* Generate a list of evaluation results, given a list of input hash tables *)
let evaluate_in_all_hts ir_ssa f l_ht = 
  List.fold_left (fun acc ht -> (List.rev_map (eval_with_rng ir_ssa ht f) l_rng)@acc) [] l_ht
    
(* Register/flags inequality predicates. These state that some register or flag
   was different in the "after" states. *)
let regs_ineq,flags_ineq = 
  let flags = IRUtil.var_setify X86ToIRUtil.([vZF;vSF;vOF;vAF;vPF;vCF;vDF]) in
  let mk_ne acc v1 v2 = IRUtil.(mk_or acc (mk_ne (mk_evar v1) (mk_evar v2))) in
  List.fold_left2 
   (fun (pr,pf) (vb,va1) (_,va2) -> 
    if IRUtil.VarSet.mem vb flags 
    then (pr,mk_ne pf va1 va2)
    else (mk_ne pr va1 va2,pf))
   (IRUtil.mk_false,IRUtil.mk_false)
    X86ToIRUtil.l_p_var_varafter
    X86ToIRUtil.l_p_var_varafter2

(* Memory inequality predicate.  This says that all memory locations must 
   contain the same values.  This is obviously wrong, since we need to ignore
   certain locations. *)
let mem_ineq = 
  let v = IRUtil.new_var IR.TypeReg_32 in let e = IRUtil.mk_evar v in
  let em1 = IRUtil.mk_evar X86ToIRUtil.vMemAfter  in
  let em2 = IRUtil.mk_evar X86ToIRUtil.vMemAfter2 in
  IRUtil.(mk_ne (mk_load em1 e IR.TypeReg_8) (mk_load em2 e IR.TypeReg_8))

(* Given one candidate and an obfuscated sequence, *)
let eval_obfuscated (cir,l_cand) (oir,l_obfs) bver =
  let rec aux f lo lc = match lc,lo with
  (* Evaluate a pair of states. *)
  | (cht,cmemctx)::cs,(oht,omemctx,l_val)::os ->
    
    (* Add the ... ???? ... values to the cht *)
    List.iter (fun (v,e) -> Hashtbl.replace cht v e) l_val;

  (*Printf.printf "Diagnosing CIR:\n"; diagnose cir cht;
    Printf.printf "Diagnosing OIR:\n"; diagnose oir oht;*)
    
    (* Get the value of ESP from one of the hash tables. *)
    let esp = eval_to_int32 cht X86ToIRUtil.eEsp in

    (* Form the memory discard predicate based on [esp-4,esp).  This will 
       probably need re-evaluation. *)
    let keep a = not(Int32.sub esp 4l <= a && a < esp) in

    (* Evaluate our register/memory/flags predicates. *)
    let r,m,g = is_eval_false cht regs_ineq,compare_mem2 keep cmemctx omemctx,is_eval_false cht flags_ineq in
    
    (* Remove the ... ???? ... values from cht *)
    List.iter (fun (v,_) -> Hashtbl.remove  cht v)   l_val; 
    
    let _ = Printf.printf "Registers matched: %b, Memory: %b, Flags: %b\n" r m g in    
    
    (* If the registers and memory predicates matched, keep iterating *)
    if (r&&m) then aux g os cs else None

  (* Once we have evaluated all states ... *)
  | [],[] -> 
    (* v:  verify predicate with SMT solver *)
    let v p = not (bver && NEWVerify.verify (cir@oir) p) in 

    (* Return a tuple of whether the properties were verifiable *)
    Some(v regs_ineq,v mem_ineq,if f then v flags_ineq else false)

  | _,_ -> failwith "eval_obfuscated:  list lengths differed"
  in
  aux true l_obfs l_cand

(* I can't quite figure out what the purpose of this is.  Presumably it relates
   to the vEaxAfter/vEaxAfter2 variables.  But I don't understand the mechanism
   by which this is happening. *)
let ssa_after f vmem ir tbl = 
 ([IR.Assign(vmem,(try IRUtil.mk_evar (Hashtbl.find tbl X86ToIRUtil.vMem) with Not_found -> X86ToIRUtil.eMem))])@
 (List.rev (f tbl))@
  ir

let ssa_after2 = ssa_after X86ToIRUtil.mk_after2 X86ToIRUtil.vMemAfter2 
let ssa_after  = ssa_after X86ToIRUtil.mk_after  X86ToIRUtil.vMemAfter

(* Get all registers and immediates used in an x86 sequence... why? *)
let extract_x86parts x86l = let open X86 in let open X86ToIRUtil in let open IRUtil in
  let rht,iht,mht = Hashtbl.create 10,Hashtbl.create 10,Hashtbl.create 10 in
  List.iter (fun { pref = _; instr = (m,l) } ->
    Hashtbl.replace mht m ();
    List.iter (function 
      | GeneralReg(Gd(r)) -> Hashtbl.replace rht (var_of_reg32  r)     ()
      | GeneralReg(Gw(r)) -> Hashtbl.replace rht (vr32_of_reg16 r)     () (* Should probably note this? *)
      | GeneralReg(Gb(r)) -> Hashtbl.replace rht (vr32_of_reg8  r)     () (* Should probably note this? *)
      | Immediate(Id(i))  -> Hashtbl.replace iht (mk_byte_of_int32  i) ()
      | Immediate(Iw(i))  -> Hashtbl.replace iht (mk_word_of_int32  i) ()
      | Immediate(Ib(i))  -> Hashtbl.replace iht (mk_dword_of_int32 i) ()
      | _ -> ()) l)
    x86l;
 (rht,iht,mht)

(* Get the common values from two hash tables. *)
let ht_intersect ht1 ht2 = 
  let h = Hashtbl.create 10 in
  Hashtbl.iter (fun k _ -> if Hashtbl.mem ht2 k then Hashtbl.replace h k ()) ht1;
  h

(* IR translation function. *)
let irtrans x86l = List.concat (List.map (X86ToIR.translate_instr 0l) x86l)
  
(* My shoddy attempts at writing a generalization function. *)
let generalize2 x86obf x86rep = 
  (* Translate the obfuscated and replacement x86 sequences. *)
  let oir,rir = irtrans x86obf, irtrans x86rep in
  
  (* Get the x86 parts from both sequences, and those common to both. *)
  let orht,oiht,omht = extract_x86parts x86obf in
  let rrht,riht,rmht = extract_x86parts x86rep in  
  let crht,ciht,cmht = ht_intersect orht rrht,ht_intersect oiht riht,ht_intersect omht rmht in
  
  (* ???? *)
  let test_gen vorig vnew l = 
    (* Replace variable "vorig" with "vnew" in the IR sequence. *)
    let gen_ir ir = 
      let h = Hashtbl.create 10 in Hashtbl.replace h vorig vnew;
      List.map (IRLocalOpt.replace_instr_var_with_var true h) ir
    in
    
    (* Make SSA from both sequences w/variable replacements *)
    let rir = IRSSA.bb_to_ssa (gen_ir rir) ssa_after  in
    let oir = IRSSA.bb_to_ssa (gen_ir oir) ssa_after2 in
    
    (* Print both sequences *)
    List.iter (fun i -> Printf.printf "%s\n" (PpIR.ppInstr i)) rir;
    List.iter (fun i -> Printf.printf "%s\n" (PpIR.ppInstr i)) oir;
    
    (* Get the hash tables resulting from evaluating these sequences *)
    let l_cht   = evaluate_in_all_hts rir f_candidate  l_cand_ht in
    let l_oht   = evaluate_in_all_hts oir f_obfuscated l_cand_ht in
    
    (* See if their evaluations match.  If so, accrue vnew onto our list. *)
    match eval_obfuscated (rir,l_cht) (oir,l_oht) false with
    | Some(r,m,f) -> if r && m then vnew::l else l
    | None        -> l
  in
  (* For all variables referenced by the obfuscated sequence, see if we can
     replace the variable by the other one and have the generalization match. *)
  Hashtbl.fold (fun v _ l -> (v,List.fold_left (fun l v2 -> if v=v2 then v::l else test_gen v v2 l) [] lreg)::l)
    orht 
    []

let generalize x86obf x86rep = 
  let dw0         = IRUtil.mk_dword_of_int32 0l in

  (* Translate the obfuscated and replacement x86 sequences. *)
  let oir,rir = irtrans x86obf, irtrans x86rep in
  
  (* Get the x86 parts from both sequences, and those common to both. *)
  let orht,oiht,omht = extract_x86parts x86obf in
  let rrht,riht,rmht = extract_x86parts x86rep in
  let crht,ciht,cmht = ht_intersect orht rrht,ht_intersect oiht riht,ht_intersect omht rmht in

  let lreg = X86ToIRUtil.([vEax;vEcx;vEdx;vEbx;vEsp;vEbp;vEsi;vEdi]) in

  (* Make low:  cast to low 3-bits *)
  let mkl e = IRUtil.(mk_and e (mk_byte_of_int32 7l)) in

  (* For each register variable v and index number i, append (f v i) to the list *)
  let make_ir f   = snd(List.fold_left (fun (i,l) v -> (Int32.succ i,((f v i)::l))) (0l,[]) lreg) in

  (* At the top:  assign vGen based upon eIdx *)
  let head_ir v i = IRUtil.(mk_assign vGen (mk_ite (mk_eq (mk_byte_of_int32 i) (mkl eIdx)) (mk_evar v) eGen)) in
  (* At the bottom:  assign v based upon eIdx *)
  let foot_ir v i = IRUtil.(mk_assign v    (mk_ite (mk_eq (mk_byte_of_int32 i) (mkl eIdx)) eGen (mk_evar v))) in
  let head_ir = (IRUtil.mk_assign vGen dw0)::(make_ir head_ir) in
  let foot_ir = make_ir foot_ir in

  let get_gen_candidates vorig = 
    (* Replace variable "vorig" with "vGen" in the IR sequence. *)
    let gen_ir vrepl = 
      let h = Hashtbl.create 10 in Hashtbl.replace h vorig vGen; Hashtbl.replace h vrepl vGen;
      List.map (IRLocalOpt.replace_instr_var_with_var true h)
    in
    (* Wrap the transformed IR in the header/footer sequences *)
    let newir v ir = head_ir@(gen_ir v ir)@foot_ir in
    
    (*  *)
    let _,cands = 
      List.fold_left (fun (i,l) v ->  
        (* Make the replacement/obfuscated IR sequences *)
        let rir = IRSSA.bb_to_ssa (newir v rir) ssa_after  in
        let oir = IRSSA.bb_to_ssa (newir v oir) ssa_after2 in
        
        (* Assign vIdx and vGen in the hash tables (this should be unnecessary) *)
        let func f ir_ssa =
          List.fold_left (fun acc ht -> 
            let _,_ = Hashtbl.replace ht vIdx (IRUtil.mk_byte_of_int32 i),Hashtbl.replace ht vGen dw0 in
            let ret = (List.rev_map (eval_with_rng ir_ssa ht f) l_rng)@acc in
            Hashtbl.remove ht vIdx; Hashtbl.remove ht vGen;
            ret)
            []
            l_cand_ht
        in
        
        (* Verify that the generalized sequences matched and add them to the list *)
        let l = match eval_obfuscated (rir,func f_candidate rir) (oir,func f_obfuscated oir) false with
        | Some(r,m,f) -> if r && m then v::l else l
        | None        -> l
        in
       (Int32.succ i,l)) 
       (0l,[])
        lreg
    in
   (vorig,cands)
  in
  Hashtbl.fold (fun v _ l -> (get_gen_candidates v)::l) crht []

let cand = let open X86 in [{ pref = []; instr = (Push,[GeneralReg(Gd(Eax))]) }]
let obfs = let open X86 in
 [{ pref = []; instr = (Sub,[GeneralReg(Gd(Esp));Immediate(Id(4l))]) };
  { pref = []; instr = (Mov,[Memexpr(Md(Mem32(SS,Some(Esp),None,None)));GeneralReg(Gd(Eax))]) };]

let cand1 = let open X86 in [{ pref = []; instr = (Add,[GeneralReg(Gd(Ebx));Immediate(Id(1234l))]) }]
let obfs1 = let open X86 in
 [{ pref = []; instr = (Push,[GeneralReg(Gd(Ecx))]) };
  { pref = []; instr = (Mov,[GeneralReg(Gd(Ecx));Immediate(Id(1234l))]) };
  { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Ecx))]) };
  { pref = []; instr = (Pop,[GeneralReg(Gd(Ecx))]) };]

let verify_and_generalize c o =
  let cht,cir = IRSSA.bb_to_ssa_state_out (irtrans c) ssa_after  in
  let oht,oir = IRSSA.bb_to_ssa_state_out (irtrans o) ssa_after2 in
  let l_cht   = evaluate_in_all_hts cir f_candidate  l_cand_ht in
  let l_oht   = evaluate_in_all_hts oir f_obfuscated l_cand_ht in
  match eval_obfuscated (cir,l_cht) (oir,l_oht) true with
  | Some(r,m,b) -> 
    Printf.printf "Registers matched: %b, Memory: %b, Flags: %b\n" r m b;
    let l = generalize o c in
    List.iter (fun (v,l) -> Printf.printf "%s: " (PpIR.ppVar v); List.iter (fun v -> Printf.printf "%s," (PpIR.ppVar v)) l; Printf.printf "\n") l
  | None        -> Printf.printf "Did not match!\n"

let _ = verify_and_generalize cand1 obfs1

(* I'll need to make a tweak to the memory verification.  Consider: push eax vs. push ebx.
   If we were to issue a constraint like !(x in [esp-3,esp]) => vMemAfter[x] != vMemAfter2[x]
   then these would be considered equivalent.
   So really, the condition is a bit more subtle than I had originally conceived.
   Can not ignore values below ESP if both sequences wrote to them.
   Have to process the write lists or something.
   
   About memory, here's what I'm thinking.
   * Apply location prediction.
   * The lists of reads and writes must be identical, except perhaps for
     the area immediately below ESP.
   * However, if both write to the same area below ESP, then it must be
     considered as part of write verification.
     
   So, maybe have a function that does this:
   Given two hash tables, iterate over one of them.
   Return:
   * List:  reads  that were present in this one, but not the other
   * List:  writes that were present in this one, but not the other
   * Another two lists for those involving ESP
   
   Location prediction can also be put to use in synthesizing potential rewrite
   rules.  If we have established that a sequence reads from a particular address,
   then use that memory location in synthesizing candidates.
   
   I think we can use the ROP compiler machinery directly in some of this.
   *)

(* Gibberish:
   Collect all registers seen in either sequence
   Replace all of them with "gen" variables
   Iterate over all possible combinations of the "idx" variables
   This demonstrates when it is possible to generalize
   
   ? Issue I'm not sure what to do about just yet.  This is a variation on the
     issue that just bit me.

   + What happens when the same variable is selected twice?
   + I.e., both ebx and ecx are assigned to ecx.
   + So at the end of the sequence, what should be written
     to ecx?
     It seems like the only answer to this question could be
     "whatever was last assigned to ecx".
     But that includes ALL modifications.
     
     An example will help make this concrete.
     push ecx      
     mov ecx, 1234h  
     add ebx, ecx    add ebx, 1234h
     pop ecx
     
     Let's say we replace both ebx and ecx with ecx.
     
     push ecx
     mov ecx, 1234h
     add ecx, ecx    add ecx, 1234h
     pop ecx
     
     What should ecx contain?  vEcxGen?
     What should ebx contain?  vEcxGen?
     
     vEsp = vEsp - 4
     vMem = Store(vMem,vEsp,vEcx)
     vEcx = 0x1234
     vEbx = vEbx + vEcx            vEbx = vEbx + 0x1234
     vEcx = Load(vMem,vEsp)
     vEsp = vEsp + 4

     vEaxGen = vEaxIdx == 0 ? vEax : vEaxIdx == 1 ? vEcx : vEaxIdx == 2 ? vEdx ...
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ...

     // Yeah, this looks tough.  The crux of it is that it needs to be reflected
     // that vEbxGen and vEcxGen are referring to the same location.  So given that
     // vEcxGen = 0x1234
     // vEbxGen = vEbxGen + vEcxGen
     //           ^ this variable needs to contain 0x1234 or it isn't going to work.
     // So perhaps this is the wrong strategy.
     // Here's a semblance of another thought...
     // What if I replaced the uses of each variable instead of the definitions?
     // That sounds like a nightmare too.

     vEspGen = vEspGen - 4
     vMem    = Store(vMem,vEspGen,vEcxGen)
     vEcxGen = 0x1234
     vEbxGen = vEbxGen + vEcxGen   vEbxGen = vEbxGen + 0x1234
     vEcxGen = Load(vMem,vEspGen)
     vEspGen = vEspGen + 4
     
     // No, this is wrong.  We don't want to update vEbx in this case, since it is
     // not modified by the generalized sequence.
     vEbx = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : ...
     vEcx = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : ...

     // It needs to be something more like this, but this is wrong too:
     vEbx = vEaxIdx == 3 ? vEaxGen : vEcxIdx == 3 ? vEcxGen : ...
     
     // Corner cases:
     // No index references ecx.  In which case, take vEcx.
     // vEcxGen references ecx.  I think this wouldn't matter; because something
     // that was overwritten later could have been replaced by vEcx.
     // We do want to overwrite ecx with the last value written that was modified
     // to point to vEcx.  Is there any way to get that automatically?
     // I think that differs for every sequence.
     
     // Do we need any of this?  Shouldn't we just be able to compare vEaxGen from
     // the end of one sequence to vEaxGen from the end of the other?
     
     // Would that even work?
     // Would I end up with the same problem as the one that started this rant?
     // Let's say we replace vEbxGen with vEcx
     
     // it to whatever vEcx had been replaced with.
     // More than one index references vEcx.  In which case...
     
     The problem I had originally was that I had different variables that 
     referred to the same physical quanitity.  They became desynchronized.
     Specifically:

     vEcx    = 0x1234
     vEbxGen = vEbxGen + vEcx
               ^ this variable needs to contain 0x1234 or it isn't going to work.
     
     This is problematic, because vEcx is updated, but vEbxGen was merely
     *initialized to the same value* as vEcx.  I.e. vEbxGen does not contain
     0x1234, it contains the initial value of vEcx.
     
     To fix this, I also replaced vEcx with vEbxGen.  It works nicely, but I 
     don't know if it will generalize to multiple variables.
     
     What about a different strategy?
     Can I intersperse blocks of IR between each instruction to re-synchronize
     them?
     
     { ebx <- ecx }
     vEaxGen = vEaxIdx == 0 ? vEax : vEaxIdx == 1 ? vEcx : vEaxIdx == 2 ? vEdx ...
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ...

     vEspGen = vEspGen - 4
     vMem    = Store(vMem,vEspGen,vEcxGen)
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEcx    = 0x1234
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEbxGen = vEbxGen + vEcx
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEcx    = Load(vMem,vEspGen)
     vEspGen = vEspGen + 4
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     
     That could work.  How do we generalize it to multiple variables?

     vEspGen = vEspGen - 4
     vMem    = Store(vMem,vEspGen,vEcxGen)
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ... // INTERSPERSED

     vEcxGen = 0x1234
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ... // INTERSPERSED

     vEbxGen = vEbxGen + vEcx
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ... // INTERSPERSED

     vEcx    = Load(vMem,vEspGen)
     vEspGen = vEspGen + 4
     vEcxGen = vEcxIdx == 0 ? vEax : vEcxIdx == 1 ? vEcx : vEcxIdx == 2 ? vEdx ... // INTERSPERSED
     vEbxGen = vEbxIdx == 0 ? vEax : vEbxIdx == 1 ? vEcx : vEbxIdx == 2 ? vEdx ... // INTERSPERSED
     
*)

(* let ht_ssa,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars (List.rev is) in
   let ir_ssa = ir_ssa@(X86ToIRUtil.mk_after ht_ssa) in
   let rec aux out i = if i = n then out else aux ((make_io_struct ir_ssa rng32)::out) (i+1) in
   let ir_ssa = 
     let vmem = 
       try Hashtbl.find ht_ssa X86ToIRUtil.vMem 
       with Not_found -> X86ToIRUtil.vMem
     in ir_ssa@[IRUtil.mk_assign X86ToIRUtil.vMemAfter (IRUtil.mk_evar vmem)]

   From here, what do I need to do to "see it work"?
   Gonna need an obfuscated sequence and an unobfuscated one
   Gonna need to evaluate the candidate, w/-After  SSA variables
   Gonna need to evaluate the obfuscate, w/-After2 SSA variables
   Then pass it to eval_obfuscated
   The memory test is going to fail no matter what; fix that
   From here, write an in-memory applicator and watch it make the transformation
   
   After that, there are more questions to address:
   1) What do I do about sequences with constants in them?
   2) FS/GS:  probably not solvable w/o rewrite of IR trans
   
   Later, play with generalizations, and ob/deob generation
*)
  
(* Automated Reverse Engineering of Peephole Obfuscators via Superoptimization

Project executive summary:  This tool is aimed at generically deobfuscating any protection that obfuscates via template-based local expansion of instructions.  We employ SMT-based superoptimization to infer the template expansion and contraction rules, which are used to deobfuscate the code.  Additionally, we produce two programs automatically:  a deobfuscator for any code protected by that protection, as well an obfuscator to produce code mimicking the obfuscation.  These generated programs do not require SMT solving and are hence very fast, showcasing a little-mentioned benefit of semantics-based methods:  that the expensive work done in theorem-proving can be leveraged to construct very efficient tools that do not rely upon SMT solvers.  We conclude that template-based instruction expansion by itself cannot be considered a "strong" protection against reverse engineering.

Components:  
1)  Trace gatherer.
2)  Simplification prover.

Executive summary:  Once traces are collected, use the ROP compiler mechanics to compare the randomized executions of two different sequences of code.  Use the theorem prover to establish that the rewrite is correct with respect to *something*.

Outline:
A)  Randomly evaluate candidate sequences (CS). (outline continued below)
B)  Randomly evaluate obfuscated sequences (OS) in the CS contexts.
C)  Evaluate predicates over the combined hash tables to see if they match.
D)  If they do match, then use theorem-prover based techniques to prove very simple theorems about IR(CS)@IR(OC):
* * * (vRegAfter != vRegAfter2) || ... must be unsatisfiable
* * * Could do the same for flags
* * * NOT(x in spoiled ESP interval) => MemAfter[x] = MemAfter2[x]
E)  These are the rewrite rules.

3)  Generalizer for rewrite rules.

Tossing around some ideas here:
* What does it mean to "generalize" a rewrite rule?
* * Some fact regarding transformation correctness that we proved, involving
    specific registers or constants, is actually true when we replace those 
    registers or constants with other values.
* * It might not be the case that EVERY replacement of that quantity will
    result in a correct transformation.  For example, a lot of the obfuscation
    involves the stack, say, pushing a register and performing some operation
    involving that register, then popping the register.  It is unlikely that
    replacing that register with ESP will yield a correct or sensible 
    transformation.
* * For constants that appear more than once, it might not be the case that
    replacing each occurrence will be correct.
    
* Generalizable quantities:  extract the operands to a hash table.  Keep only
  the ones that occur in both the source and the target sequences.  NO, THAT
  WILL NOT WORK ENTIRELY.
  
* Generalization process:  for each generalizable quantity, choose a 
  replacement that does not occur in either sequence.  This has to have some 
  intelligence behind it.  For example, 
  push eax / add byte ptr [esp], bl / pop eax => add al, bl.
  
  Note that the quantity al is contingent upon eax.  So these quantities
  have some correlation and cannot be replaced arbitrarily.
  
  The generalization that happens here is that if we replace eax with another
  general-purpose register (ecx, edx, ebx), we end up with add low8(reg), bl.
  Another generalization is that if we replace bl by any other 8-bit register,
  we get add al, 8bitreg.
  
  So we need to unify these generalizations.


4)  Facility to use the deobfuscator within the code.

5)  Emitter for deobfuscator and obfuscator.
*)
(* Think about this.  What are the merits of using a random array versus using
   different random values every time? Well, why do I want those values at all?
   
   So I can compare the memory hash tables.  Anything not immediately below ESP
   factors into the comparison.
   
   Will there be ill-effects from re-using the same random values for every
   sequence?
   
   I don't know right now.  Fortunately, it's easy enough to swap the function
   pointer.
   
   Well, this could have broader ramifications upon the design, though.  Right
   now I'm struggling with an issue of design optimization.  If the random
   numbers are generated differently for each candidate, that means I have to
   re-evaluate the obfuscated examples for each sample that I want to test
   against.  So that kind of sucks.  But if I use the swappable random number 
   generator scheme, that's not an issue.
*)

(* So what am I actually going to need? 
1) The set of randomly-generated input states
2) I/O hash tables for all of those, plus read lists
3) Initial memory contexts (hash tables) gathered from the read lists





* Some of the patterns are going to need to make use of unspecified constants.

* Might be wise to expand the location prediction.

[reg+disp]
[reg*[2,4,8]+disp]
[reg+reg*[2,4,8]+disp]

This would be easy enough.

* FS/GS segments will need special handling.

That part is going to kick me in the balls.  This is absolutely correct; my IR translator won't handle this right now.  I can pretend it doesn't exist and work from there, but it's just going to be a prototype.

How to use the ROP compiler codebase in order to do the deobfuscation mechanics:

* Basically, I want to take my "candidate replacement sequences" and randomly evaluate them.
* For each "obfuscated" sequence, randomly evaluate it starting from the same state(s) as the replacement sequences.
* Merge the results into one hash table.
* Now we can evaluate predicates, such as 
* * Registers:  (vEaxAfter != vEaxAfter2) || ... 
* * Flags:  (vZFAfter != vZFAfter2) || ...
* What about memory?
* * It must read from the same locations.  
* * * THAT IS NOT TRUE, if the values are within N of the stack pointer.
* * So ... it must read from the same locations, apart from the stack.
* * Must it access them the same number of times?
* * * I can't see why it should.  Consider add [eax+4], X / sub [eax+4], ebx / sub [eax+4], X => sub [eax+4], ebx

*** Is there a cheap test we can do for memory locations? ***

In theory we should be able to bound the writes below ESP and then straight-up compare the hash tables.
We can make this faster by precomputing a hash of the contents.

When we issue the memory equality postcondition
* 


    



(, issue an extra constraint saying 


let make64 h32 l32 = Int64.(logor (shift_left (of_int h32) 32) (of_int l32))
let rol13_64 i = Int64.(logor (shift_left i 13) (shift_right_logical i 51))

(* keep:  int32 -> bool, rejects by address
   hash:  'a -> int32 -> int32 -> 'a, arbitrary
   post:  'a -> 'b, arbitrary *)
let hash_mem_ht ht keep hash post = 
  post (Hashtbl.fold (fun a v h -> if keep a then hash h a v else h) ht 0L)

let comparator_of_ht ht1 keep hash post = 
  let h = hash_mem_ht ht1 keep hash post in
 (fun ht2 -> hash_mem_ht ht2 keep hash = h && compare_mem_ht keep ht1 ht2)

  let _ = List.iter (fun i -> Printf.printf "%s\n" (PpIR.ppInstr i)) ir_ssa in
  let _ = Hashtbl.iter (fun v e -> Printf.printf "%s -> %s\n" (PpIR.ppVar v) (PpIR.ppExpr false e)) ht in
  let _ = Printf.printf "3\n" in
*)

(* So at this point it would be nice to get around to actually changing the 
   IRSSA functionality to incorporate the "afterize" functionality directly.
   What does that mean?
   After a sequence is converted to SSA, the convertor will "bottom out" at
   [].  From here, we should input a list of pairs:  (vNonSSA,vAfter).  
   Generate an assignment: vAfter = ht_ssa(vNonSSA).
   These just go directly onto the bottom of the SSA translated instructions.
   Perhaps two such lists:  non-memories, and memories.
   Afterwards, List.rev and return as usual. 
   Also, that functionality should 
   *)
(*

(*
let get_after2_values2 ht = get_after_values ht ((vGen,vGen2)::X86ToIRUtil.l_p_var_varafter2)
let get_after_values2  ht = get_after_values ht ((vGen,vGen1)::X86ToIRUtil.l_p_var_varafter)
*)

(*
let f_obfuscated2 h m = (h,m,get_after2_values2 h)
*)


*)