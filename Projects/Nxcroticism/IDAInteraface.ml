let display_as_graph_in_IDA filtered_tree = 
  let open NaryTree in 
  let rec f_count instr node num = 
    match node with
    | Node(_,_,_,child_ht) -> Hashtbl.fold f_count child_ht num + 1
  in
  let count = Hashtbl.fold f_count filtered_tree.roots 0 in
  let text_array = Array.init count (fun _ -> "") in
  (* Populate text_array *)
  let rec f_decorate instr node (index,lastnum,edges) = 
    match node with
    | Node(instr,ir,addr_ht,child_ht) -> 
      let address_text = Hashtbl.fold (fun addr _ str -> str^(Printf.sprintf "%08lx\n" addr)) addr_ht "" in
      let instr_text = IDAX86Disasm.colstr_of_x86instr instr in
      let ir_text = List.fold_left (fun str ir -> str^(PpIR.ppInstr ir)^"\n") "" ir in
      text_array.(index) <- address_text^"\n"^instr_text^"\n\n"^ir_text;
      let newindex,_,edges = Hashtbl.fold f_decorate child_ht (index+1,index,edges) in
      (newindex,lastnum,if lastnum = ~-1 then edges else ((index,lastnum)::edges))
  in
  let _,_,edges = Hashtbl.fold f_decorate filtered_tree.roots (0,~-1,[]) in

  let open IDAGraph in
  let rop_graph_viewer = {
    graph = ();
    sizer = (fun _ -> count);
    text  = (fun _ i -> text_array.(i));
    edges = (fun _ -> edges);
  }
  in IDAGraph.show_graph rop_graph_viewer "ROP Tree"

