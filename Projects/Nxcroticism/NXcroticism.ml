(* NXcroticism.ml *)
open FrameworkUtil

(*
#use "c:\\paframework\\Projects\\Nxcroticism\\NXcroticism.ml";;
*)

(* Determine whether we should traverse backwards at this location *)
let return_pred _ b =
  (* Read byte from array (no need to check length in this situation; 1-byte opcode) *)
  (* let b = intarr.(i) in *)
  (* Does this byte correspond to a return instruction? *)
  (b = 0xc3l || b = 0xc2l)

let bruteforce_backwards_return = SequenceGenerator.bruteforce_backwards return_pred

(* Think about what conditions ought to cause us to stop.
   
   ESP is inconsistent?
   Write safety fails?
   Read safety fails?
   Return check fails?
   *)
let gc = GadgetCache.make ()

let rec verify_sequences children_ht ir_in x86l_in =
  let open NaryTree in
  Hashtbl.iter (fun i x -> match x with
  | Node(instr,ir,addr_ht,child_ht) -> 
    let ir = ir @ ir_in in
    let x86l = instr::x86l_in in
    let open VerifyCandidates in
    match SequenceVerifier.determine_sequence_behaviors addr_ht x86l ir 3 with
    (* These will be faulty in supersequences too *)
    | BadAssembly  | BadIR -> ()
    
    (* These ... might not be faulty? *)
    | NoReturnBehavior  | BadReturnBehavior | BadStackDelta
    | WriteSafetyFailed | ReadSafetyFailed  | NoInterestingBehaviors 
    | ReadWriteCorrelationFailed | TooManyWrites | ReadOutsideStackFrame -> 
      verify_sequences child_ht ir x86l

    | VerifiedBehaviors(v) ->
      let _ = f_printf "Sequence:\n\n" in
      List.iter (fun i -> f_printf "%s\n" (X86Disasm.string_of_x86instr i)) x86l;
      f_printf "\nVerified behaviors:\n";
      VerifyCandidates.print_verified_results v;
      f_printf "%!";
      CacheVerifiedResults.cache_verified_results gc v x86l;
      verify_sequences child_ht ir x86l)
    children_ht

let verify_all_sequences tree =
  let open NaryTree in 
  verify_sequences tree.roots []

let main () =
  let _ = Random.self_init () in
(*let (n,r)  = Args.get_args () in*)
  let n_tree = NaryTree.make () in
  List.iter
   (fun path ->
      let (imagebase,seclist) = PEParse.extract_executable_sections path in
      List.iter 
       (fun (_,va,len,b) ->
          bruteforce_backwards_return 
            n_tree
            20 
           (Int32.add imagebase va) 
           (fun i -> Int32.of_int (int_of_char (String.get b (Int32.to_int i)))) 
            len)
       seclist)
    ["c:\\temp\\depends.dll"];
  (*["c:\\Windows\\SysWOW64\\kernel32.dll"];*)
  let filtered_tree = NaryTree.filter_roots n_tree in
  let _ = verify_all_sequences filtered_tree [] in
  f_printf "Theorem prover time: %f%!\n" (!SequenceVerifier.f_tptime);
  LightTunnel.make_contrived_example gc
(*display_as_graph_in_IDA filtered_tree*)
            
(*
let _ = main ()
*)

let _ = 
  let _ = Random.self_init () in
  let addr_ht = Hashtbl.create 1 in
  let _ = Hashtbl.replace addr_ht 0x12345678l () in
  let faulting_sequence = 
    let open X86 in
    { pref = []; instr = (Or, [Memexpr(Mb(Mem32(DS,Some(Ecx),None,None)));GeneralReg(Gb(Al))]);}::
    { pref = []; instr = (Pop,[GeneralReg(Gd(Ebp))]) }::
    { pref = []; instr = (Pop,[GeneralReg(Gd(Ebx))]) }::
    { pref = []; instr = (Ret,[]) }::
    []
  in
  let ir = List.concat (List.map (X86ToIR.translate_instr 0l) faulting_sequence) in
  let _ = SequenceVerifier.determine_sequence_behaviors addr_ht faulting_sequence ir 4 in
  f_printf "%!\n"
