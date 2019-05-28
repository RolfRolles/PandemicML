module C = X86CFG.X86CFGBuilder.C;;

let modify_x86_graph fn cfg = 
  let vertices = C.G.fold_vertex (fun v list -> v::list) cfg [] in
  List.fold_left 
   (fun cfg v ->
      let ir = C.get_ir cfg v in
      let ir = fn ir in
      C.set_ir cfg v ir)
    cfg
    vertices;;

let filter_jmp_x86graph =
  let filter_jumps i = 
    let open X86 in 
    match i with
    | { pref = _; instr = (Jmp,[JccTarget(_,_)])} -> false 
    | _ -> true
  in modify_x86_graph (List.filter filter_jumps);;

let x86_local_cp cfg = 
  let all_vertices = C.G.fold_vertex (fun v l -> v::l) cfg [] in
  let new_graph,changed = 
    List.fold_left
     (fun (cfg,changed) v ->
        let new_ir,change = X86LocalConstant.local_constant_fold_x86 (C.get_ir cfg v) in
        ((C.set_ir cfg v new_ir),changed||change))
     (cfg,false)
      all_vertices
  in (new_graph,changed)  

let x86_dse ?(terminal=X86ToIRUtil.reserved_vars) cfg = 
  let module C = X86CFG.X86CFGBuilder.C in
  let sz = C.G.nb_vertex cfg * 2 in
  let empty = IRUtil.VarSet.empty in
  let terminal = IRUtil.var_setify terminal in
  let out_sets = Hashtbl.create sz in
  let  in_sets = Hashtbl.create sz in
  C.G.iter_vertex 
   (fun v -> 
      Hashtbl.replace out_sets v empty;
      Hashtbl.replace  in_sets v empty)
    cfg;
  let terminal_vertices = 
    C.G.fold_vertex
     (fun v l ->
        match (C.G.fold_succ (fun v l -> v::l) cfg v []) with
        | [] -> v::l
        | _  -> l)
      cfg
      []
  in
  List.iter (fun v -> Hashtbl.replace out_sets v terminal) terminal_vertices;
  let all_vertices = C.G.fold_vertex (fun v l -> v::l) cfg [] in
  let rec iterate = function
  | [] -> ()
  | x::xs ->
    let oldout = Hashtbl.find out_sets x in
    let out = 
      C.G.fold_succ
       (fun v s -> IRUtil.VarSet.union s (Hashtbl.find in_sets v))
        cfg
        x
        oldout 
    in
    let oldin = Hashtbl.find in_sets x in
    let ins = X86AnalyzeDefUse.block_dse_transfer (C.get_ir cfg x) out in
    if (not (IRUtil.VarSet.equal oldout out)) || (not (IRUtil.VarSet.equal oldin ins))
    then (Hashtbl.replace out_sets x out;
          Hashtbl.replace  in_sets x ins;
          iterate (C.G.fold_pred (fun v l -> v::l) cfg x xs))
    else iterate xs
  in iterate all_vertices;
  let new_graph,changed = 
    List.fold_left
     (fun (cfg,changed) v ->
        let ir = C.get_ir cfg v in
      (*IDA.msg "%08lx: processing\n" (C.G.V.label v);*)
        let new_ir,change = X86AnalyzeDefUse.remove_dead_instructions ir (Hashtbl.find out_sets v) in
        ((C.set_ir cfg v new_ir),changed||change))
     (cfg,false)
      all_vertices
  in (new_graph,changed)
  
