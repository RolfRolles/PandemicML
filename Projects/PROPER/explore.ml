(* #use "c:\\paframework\\Projects\\PROPER\\explore.ml";; *)

let bcv_debug = true;;

if bcv_debug then IDA.msg "BreakCV: 1\n";
#use "c:\\paframework\\framework.ml";;

if bcv_debug then IDA.msg "BreakCV: 2\n";
#directory "c:\\paframework\\Util\\";;
#load "ArrayUtil.cmo";;

if bcv_debug then IDA.msg "BreakCV: 3\n";
#directory "c:\\paframework\\idax86\\";;
#load "IDAX86Disasm.cmo";;
#load "IDAX86Graph.cmo";;

if bcv_debug then IDA.msg "BreakCV: 4\n";
#directory "c:\\paframework\\X86Analyze";;
#load "X86AnalyzeDefUse.cmo";;
#load "X86LocalConstant.cmo";;
#load "X86GlobalOpt.cmo";;

if bcv_debug then IDA.msg "BreakCV: 5\n";
#directory "c:\\paframework\\Projects\\BitwiseAI\\";;
#load "BitwiseAINewInterface.cmo";;

module C = X86CFG.X86CFGBuilder.C;;

(* let _ = static_explore_vm 500 (IDA.get_screen_ea ());; *)

let disasm ea abstract_state_hashtbl = 
  let x86,_,succ = X86Decode.decode ea in
  let ir = X86ToIR.translate ea in
  let bir = BitwiseAINewInterface.bitwise_abstract_interpret_instr_list abstract_state_hashtbl ir in
  let lastir = List.hd (List.rev bir) in
  let p () = (*List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i)) bir*) () in
  let open ASMUtil in
  let ret ea = [(ea,[x86],Hashtbl.copy abstract_state_hashtbl)] in
  let reti64 i = [(Int64.to_int32 i,[x86],Hashtbl.copy abstract_state_hashtbl)] in
  match succ with
  | Flow(f)   -> (*IDA.msg "%08lx: Implicit flow to %08lx\n" ea f; p (); *)ret f
  | ICall(f)  -> IDA.msg "%08lx: Indirect call encountered. STOP\n" ea; p (); []
  | Jmp(f)    -> (*IDA.msg "%08lx: Explicit flow to %08lx\n" ea f; p (); *)ret f
  | Call(f,_) -> (*IDA.msg "%08lx: Call encountered, following %08lx\n" ea f; p (); *)ret f
  | IJmp      ->
   (match lastir with
    | IR.Jmp(IR.Const(i,_)) -> (*IDA.msg "%08lx: IJmp evaluated to single branch, leading to %08Lx\n" ea i; p (); *)reti64 i
    | IR.Jmp(_)             -> IDA.msg "%08lx: IJmp evaluated to unknown Jmp: \"%s\". STOP\n" ea (PpIR.ppInstr lastir); p (); []
    | _                     -> IDA.msg "%08lx: x86 was IJmp, IR did not end in Jmp. STOP\n" ea; p (); [])
  | Return    -> 
   (match lastir with
    | IR.Jmp(IR.Const(i,_)) -> (*IDA.msg "%08lx: Ret evaluated to known target %08Lx\n" ea i; p (); *)reti64 i
    | IR.Jmp(_)             -> IDA.msg "%08lx: Ret evaluated to unknown Jmp: \"%s\". STOP\n" ea (PpIR.ppInstr lastir); p (); []
    | _                     -> IDA.msg "%08lx: x86 was Return, IR did not end in Jmp. STOP\n" ea; p (); [])
  | Jcc(f,_)  -> 
   (match lastir with
    | IR.Jmp(IR.Const(i,_)) -> (*IDA.msg "%08lx: Jcc evaluated to one-way branch, leading to %08Lx\n" ea i; p (); *)reti64 i
    | IR.Jmp(_)             -> IDA.msg "%08lx: Jcc evaluated to malformed Jmp: \"%s\". STOP\n" ea (PpIR.ppInstr lastir); p (); []
    | IR.CJmp(_,IR.Const(ta,_),IR.Const(fa,_)) 
                            -> IDA.msg "%08lx: Jcc did not evaluate to one-way branch; ta = %08Lx, fa = %08Lx\n" ea ta fa; 
                             p (); 
                            (reti64 ta)@(reti64 fa)
    | IR.CJmp(_,_,_)        -> IDA.msg "%08lx: Jcc evaluated to malformed CJmp: \"%s\". STOP\n" ea (PpIR.ppInstr lastir); p (); []
    | _                     -> IDA.msg "%08lx: x86 was Jcc, IR did not end in Jmp or Jcc. STOP\n" ea; p (); [])

let make_mem_with_static_contents begea endea =
  let arr = 
    Array.init 
   (Int32.to_int (Int32.sub endea begea)) 
   (fun i -> 
      BitwiseAINewInterface.abstract_bv_of_constant 
       (Int64.of_int32 (IDA.get_byte (Int32.add begea (Int32.of_int i))))
        IR.TypeReg_8)
  in
  let top_byte = Array.make 8 (BitwiseAINewInterface.AbsHalf) in
 (fun addr -> let a32 = Int64.to_int32 addr in if a32 >= begea && a32 < endea then arr.(Int32.to_int (Int32.sub a32 begea)) else top_byte)

let default_mem = BitwiseAINewInterface.AbsMem(make_mem_with_static_contents 0x00401000l 0x004171FFl);;

let static_explore_vm num startea = 
  (* Set up the initial valuations:  ESP = 0x10000, process' static memory image bound as abstract values *)
  let default_assignments = let open IR in [Assign(X86ToIRUtil.vEsp,Const(0x10000L,TypeReg_32))] in
  let abstract_state_hashtbl = Hashtbl.create 50 in
  Hashtbl.replace abstract_state_hashtbl X86ToIRUtil.vMem default_mem;
  let _ = BitwiseAINewInterface.bitwise_abstract_interpret_instr_list abstract_state_hashtbl default_assignments in
  
  (* Interface for knowing when not to process an instruction redundantly *)
  let ht_v2vmeips = Hashtbl.create 50 in
  let lookup_vertex_vmeip v vmeip =
    match (try Some(Hashtbl.find ht_v2vmeips v) with Not_found -> None) with
    | Some(ht) -> let res = try ignore(Hashtbl.find ht vmeip); true with Not_found -> false in res
    | None -> false
  in
  let insert_vertex_vmeip v vmeip = 
    let ht = try Hashtbl.find ht_v2vmeips v with Not_found -> let h = Hashtbl.create 50 in Hashtbl.replace ht_v2vmeips v h; h in
    Hashtbl.replace ht vmeip ()
  in

  let add_vertex cfg addr disasm = 
    try (cfg,C.find_vertex cfg addr) 
    with Not_found -> (C.create_vertex cfg addr disasm)
  in

  let extract_esi_opt ht = 
    match (try Some(Hashtbl.find ht X86ToIRUtil.vEsi) with Not_found -> None) with
    | Some(BitwiseAINewInterface.AbsValue(absval)) -> (match (BitwiseAINewInterface.make_constant_opt absval) with
      | Some(IR.Const(esi,_)) -> Some(Int64.to_int32 esi)
      | _ -> None)
    | _ -> None
  in

  (*  How do I want to write this algorithm?
  
      * After disassembling a vertex and obtaining its successors, add the vertex to the graph.
      * Before disassembling a vertex, check to see whether we've already seen this vertex under
      this ESI-valuation.  If we have, skip further processing of this vertex.
      * If the parent vertex option was not null, add the edge to the graph.
      * Associate the vertex with the present ESI value, if applicable.
  *)
  (* Recursively traverse starting from startea under the intial valuation *)
  
  let opt_and_show og title =
    let og = X86GlobalOpt.filter_jmp_x86graph og in
    let og = X86CFG.X86CFGBuilder.remove_empty_vertices og in
    let cg = X86CFG.X86CFGBuilder.merge_singleton_vertices og in
    let jg = X86GlobalOpt.filter_jmp_x86graph cg in
    let og = 
    (*let open X86ToIRUtil in
      optimize ~terminal:(None) jg*)jg
    in
    ignore(IDAX86Graph.show_x86_cfg og title)
  in

  let limit = 10000000 in
  let rec aux num graph = function
  | [] -> IDA.msg "Done iterating (natural)\n"; graph
  | _ when num = limit -> IDA.msg "Done iterating (forced)\n"; graph
  | (pv_opt,(ea,x86,ht))::xs -> 
    if num mod 1000 = 0 then 
     (IDA.msg "Iteration %d\n" num;
      opt_and_show graph (Printf.sprintf "Iteration %d (optimized)" num));
    let new_list = disasm ea ht in
    let graph,v = add_vertex graph ea x86 in
    let esi_opt = extract_esi_opt ht in
    if esi_opt <> None && lookup_vertex_vmeip v (Util.opt_get esi_opt)
    then aux num graph xs
    else
      let _ = 
        match esi_opt with
        | Some(esi) when lookup_vertex_vmeip v esi -> ()
        | Some(esi) -> insert_vertex_vmeip v esi
        | None -> ()
      in 
      let cfg = 
        match pv_opt with
        | Some(pv) -> C.add_edge graph pv v
        | None -> graph
      in
      aux (num+1) cfg ((List.map (fun a -> (Some(v),a)) new_list)@xs)
  in 
  let cfg = aux num (C.empty ()) [(None,(startea,[],abstract_state_hashtbl))] in
  cfg
  
  

(* OK, that was pretty successful.  Next steps:
   * For VMProtect, we want to make sure we never process the same instruction again if it corresponds to the same ESI value.
     This is already kind of problematic.  Let's run with it for now and see what happens.
     
     How do I pull this off:
     
     Idea:  build the control flow graph as we go.
     How:  
     * When I'm recursing through the instructions, I need to save the parent's address option.
       After I've processed that instruction, I add the instruction to the control flow graph.
       I also add an edge from the parent to the child.
       
     Problem:  we can't just terminate the search if we've seen an instruction already.  We need to process it again if the
     VM EIP points to a different location.
     
     Solution:  for each vertex in the graph, we want to keep an associative container signifying all of the EIP values that
     have been seen already.  If it doesn't match any existing entries, we process it again.
     
*)

