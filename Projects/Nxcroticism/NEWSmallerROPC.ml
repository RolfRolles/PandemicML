(* TO DO:  
1) Commented out propagate, because I changed the subregister stuff 
2) Need to eliminate dependent memory accesses (subregister stuff)
3) How did I implement "read safety" and "read correlation"?
4) Implement preserve behaviors
*)

open NEWGadget

(* Returns Some(x) if ht[key] exists, None otherwise *)
let ht_find_opt  ht key = let res = try Some(Hashtbl.find ht key) with Not_found -> None in res

type error = 
| X86SyntacticReject
| IRNoFinalJmp
| LocationPredictionFailed
| ReturnLocationInconsistent of bool (*bool: tried to verify? *)
| BadReturnLocation          of memloc
| StackDeltaInconsistent     of bool (*bool: tried to verify? *)
| WriteSafetyFailed
| ReadSafetyFailed
| NoInterestingBehaviors
| ReadWriteCorrelationFailed
| ReadOutsideStackFrame

let string_of_error = function
| X86SyntacticReject            -> "X86SyntacticReject"
| IRNoFinalJmp                  -> "IRNoFinalJmp"
| LocationPredictionFailed      -> "LocationPredictionFailed"     
| ReturnLocationInconsistent(b) -> "ReturnLocationInconsistent("^string_of_bool b^")"
| BadReturnLocation(m)          -> "BadReturnLocation("^string_of_memloc m^")"
| StackDeltaInconsistent(b)     -> "StackDeltaInconsistent("^string_of_bool b^")"   
| WriteSafetyFailed             -> "WriteSafetyFailed"
| ReadSafetyFailed              -> "ReadSafetyFailed"
| NoInterestingBehaviors        -> "NoInterestingBehaviors"
| ReadWriteCorrelationFailed    -> "ReadWriteCorrelationFailed"
| ReadOutsideStackFrame         -> "ReadOutsideStackFrame"

type 'a ver_err = (*'*)
| Error   of error
| NoError of 'a (*'*)

let mk_memadd r32 addr = let open IRUtil in mk_add (mk_evar r32) (mk_dword_of_int32 addr)
let vidx = IRUtil.new_var (IR.TypeReg_32)
let eidx = IRUtil.mk_evar vidx

let bytes s = IRUtil.bits s / 8
let below v1 d1 s1 v2 d2 = let open IRUtil in
  mk_ult (mk_memadd v1 (Int32.add d1 (Int32.of_int (bytes s1 - 1)))) (mk_memadd v2 d2)

let idx_not_within v1 d1 s1 = let open IRUtil in
  mk_and (mk_ult (mk_memadd v1 d1) (mk_dword_of_int32 0xfffffffcl))
 (mk_or (below v1 d1 s1 vidx 0l) (below vidx 0l (IR.TypeReg_8) v1 d1))

let distinct p ((v1,d1),s1) ((v2,d2),s2) = 
  if v1 = v2 
  then p 
  else IRUtil.mk_and p (IRUtil.mk_or (below v1 d1 s1 v2 d2) (below v2 d2 s2 v1 d1))

let x_distinct_list x ys = 
  let rec aux out = function
  | [] -> out
  | y::ys -> aux (distinct out x y) ys 
  in aux IRUtil.mk_true ys

let mk_distinction_assertion l1 l2 = let open IRUtil in 
  let rec aux1 out = function
  | [] -> out
  | x::xs -> aux1 (mk_and out (x_distinct_list x l2)) xs
  in 
  aux1 mk_true l1

let gadgets_of_x86 x86l n rng32 =
  (* Code for ESP deltas *)
  let esp_delta = IRUtil.mk_sub (IRUtil.mk_evar X86ToIRUtil.vEspAfter) X86ToIRUtil.eEsp in
  let mk_esp_delta d32 = let open IRUtil in mk_ne esp_delta (mk_dword_of_int32 d32) in
  let esp_delta_opt io = let open IRUtil in let open NEWIOStruct in
    let esp_delta = NEWIREval.eval_to_int32 (ht_of_io io) esp_delta in
    if all_ios_true io (mk_esp_delta esp_delta) then None else Some(esp_delta)
  in

  (* Reject bad x86/IR sequences, predict memory locations, find consistent 
     stack delta and return location, or return an error if any of that failed. *)
  let err e = Error(e) in
  match List.exists NEWSyntacticReject.x86_syntactic_reject x86l with
  | true                     -> err X86SyntacticReject
  | false                    -> (match NEWIOStruct.make_io_structs x86l rng32 n with
    | None                   -> err IRNoFinalJmp
    | Some(ir,e_jt,l_io)     -> (match NEWMemloc.predict_memlocs_opt l_io e_jt with 
      | None                 -> err LocationPredictionFailed
      | Some(_,None)         -> err (ReturnLocationInconsistent(false))
      | Some(io,Some(vr,dr)) -> if vr <> X86ToIRUtil.vEsp || dr < 0l then err (BadReturnLocation(vr,dr)) else
                               (match esp_delta_opt io with
        | None               -> err (StackDeltaInconsistent(false))
        | Some(fs)           -> 
        
  let do_load   emem ((v,d),s) vm   = IRUtil.mk_assign vm (IRUtil.mk_load emem (mk_memadd v d) s) in
  let do_load_l emem ((v,d),s) vm l = (do_load emem ((v,d),s) vm)::l in
  let emem = X86ToIRUtil.eMem in
  let ememafter = IRUtil.mk_evar X86ToIRUtil.vMemAfter in
  let l_r = Hashtbl.fold (do_load_l emem)      NEWIOStruct.(io.h_reads)  []  in
  let l_w = Hashtbl.fold (do_load_l ememafter) NEWIOStruct.(io.h_writes) l_r in
  let verify = NEWVerify.verify (ir@l_w) in

  let mk_verify_gen = Generator.mk_filtered_generator (fun g -> 
    verify (NEWGadgetGenerator.predicate_of_gadget io g)) 
  in 
  let mk_verify_gen_pred p = Generator.mk_filtered_generator (fun g -> 
    Printf.printf "%s\n" (string_of_gadget g); 
    verify (IRUtil.mk_and p (NEWGadgetGenerator.predicate_of_gadget io g))) 
  in

  let gga = NEWGadgetGenerator.generator_of_gadget_archetype io in

  let do_writes () = 
    let open IRUtil in
    let get_memlocs ht_in = 
      let mem_ht = Hashtbl.create 10 in
      Hashtbl.iter (fun g _ -> match g with 
        | Write(m,v) 
        | BinopWrite(m,_,v) -> Hashtbl.replace mem_ht (m,size_of_var v) ()
        | WriteConst(m,_,s) -> Hashtbl.replace mem_ht (m,s) ()
        | _ -> ())
        ht_in;
      mem_ht
    in
    let wdpred = NEWIOStruct.(mk_distinction_assertion io.l_writes io.l_writes) in
    let write_ver_gen = mk_verify_gen_pred wdpred in
    let write_ht = Hashtbl.create 10 in 
    let add g = Hashtbl.replace write_ht g () in
    Generator.iter add (write_ver_gen (gga NEWGadgetGenerator.TWriteConst));
    Generator.iter add (write_ver_gen (gga NEWGadgetGenerator.TWrite));
    Generator.iter add (write_ver_gen (gga (NEWGadgetGenerator.TBinopWrite(get_memlocs write_ht))));
    let pwrite = Hashtbl.fold (fun ((v,d),s) _ p -> mk_and p (idx_not_within v d s)) (get_memlocs write_ht) mk_true in
    let pwrite = mk_and pwrite (mk_ne (mk_load X86ToIRUtil.eMem eidx IR.TypeReg_8) (mk_load ememafter eidx IR.TypeReg_8)) in
    match verify pwrite with
    | false -> Some(write_ht)
    | true  -> None      
  in

  let do_reads retmem = 
    let rdpred = NEWIOStruct.(mk_distinction_assertion io.l_writes io.l_reads) in
    let read_ver_gen = mk_verify_gen_pred rdpred in
    let read_ht,c_ht,reg_ht = Hashtbl.create 10,Hashtbl.create 10,Hashtbl.create 10 in 
    let add g = Hashtbl.replace read_ht g () in
    Generator.iter (fun g -> match g with
      | Constant(v,_) -> Hashtbl.replace c_ht v (); Hashtbl.replace reg_ht g () 
      | _ -> ()) 
     (mk_verify_gen (gga NEWGadgetGenerator.TConstant));
    (* Verify return location *)
    match NEWIOStruct.(ht_find_opt io.h_reads ((vr,dr),IR.TypeReg_32)) with
    | None    -> err (ReturnLocationInconsistent(false))
    | Some(v) -> let open IRUtil in 
      let p = (mk_and rdpred (mk_ne (mk_evar v) (mk_load emem (mk_memadd vr dr) IR.TypeReg_32))) in
     (match verify p with     
      | true  -> err (ReturnLocationInconsistent(true))
      | false -> Generator.iter add (read_ver_gen (gga NEWGadgetGenerator.TRead));
        Generator.iter add (read_ver_gen (gga (NEWGadgetGenerator.TBinopRead(c_ht))));
        (* Verify read safety *)
       (match verify IRUtil.mk_true with
        | true(*-> err ReadSafetyFailed*)
        | false -> NoError(read_ht,c_ht,reg_ht)))
  in

  let do_regs reg_ht c_ht = 
    let add g = Hashtbl.replace reg_ht g () in
    Generator.iter add (mk_verify_gen (gga (NEWGadgetGenerator.TBinopReg(c_ht))));
    Generator.iter add (mk_verify_gen (gga NEWGadgetGenerator.TCopy));
    ()
  in
  let empty ht = 
    try Hashtbl.fold (fun _ _ _ -> raise Not_found) ht true 
    with Not_found -> false 
  in
  match verify (mk_esp_delta fs) with
  | true                 -> err (StackDeltaInconsistent(true))
  | false                -> (match do_writes () with
    | None               -> err WriteSafetyFailed
    | Some(w)            -> (match do_reads (vr,dr) with
      | NoError(r,c,reg) -> do_regs reg c; 
        if empty w && empty r && empty reg
        then err NoInterestingBehaviors 
        else NoError((vr,dr),fs,w,r,reg) (* Need to compute preserved set here *)
      | Error(e)         -> Error(e))))))

let _ = 
  let test = let open X86 in [{ pref=[];instr=(Mov,[Memexpr(Md(Mem32(SS,Some(Esp),None,Some(8l))));Immediate(Id(0l))])};{ pref=[];instr=(Ret,[])}] in
  let rng32 () = Random.int32 Int32.max_int in
  match gadgets_of_x86 test 3 rng32 with
  | Error(e)              -> Printf.printf "Error: %s\n" (string_of_error e)
  | NoError(m,fs,w,r,reg) -> Printf.printf "Success!\n"
  


   
