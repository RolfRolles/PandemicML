let show_x86_cfg cfg title = 
  let open X86CFG.X86CFGBuilder.C in 
  let open IDAGraph in
  let open X86 in
  let rup i = (i * 3) / 2 in
  let n = rup (G.nb_vertex cfg) in
  let i2vmap = Hashtbl.create n in
  let v2imap = Hashtbl.create n in
  let _ = G.fold_vertex (fun v i -> Hashtbl.add i2vmap i v; Hashtbl.add v2imap v i; i+1) cfg 0 in
  let v2i v = 
    try Hashtbl.find v2imap v
    with Not_found -> (failwith ("Couldn't lookup v2i for"^(Printf.sprintf "%08lx" (G.V.label v))))
  in
  let i2v i = 
    try Hashtbl.find i2vmap i
    with Not_found -> (failwith ("Couldn't lookup i2v for "^string_of_int i))
  in
  let gv = 
  { graph = cfg;
    sizer = (fun cfg -> G.nb_vertex cfg);
    text  = 
     (fun cfg i -> 
        let l = get_ir cfg (i2v i) in
        let l = List.map (fun ip -> IDAX86Disasm.colstr_of_x86instr ip) l in
        Util.opt_get (StringUtil.intersperse_string "\n" l));
    edges = (fun cfg -> G.fold_edges (fun v1 v2 l -> (v2i v1,v2i v2)::l) cfg []); }
  in
  show_graph gv title
  
