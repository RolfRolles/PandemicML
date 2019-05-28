let show_x86_cfg cfg title = 
  let open X86CFG.X86CFGBuilder.C in 
  let open IDAGraph in
  let open X86 in
  let rup i = (i * 3) / 2 in
  let n = rup (G.nb_vertex cfg) in
  let i2vmap = Hashtbl.create n in
  let v2imap = Hashtbl.create n in
  let _ = G.fold_vertex (fun v i -> Hashtbl.add i2vmap i v; Hashtbl.add v2imap v i; IDA.msg "%08lx: %d\n" (G.V.label v) i; i+1) cfg 0 in
  let v2i v = 
    try Hashtbl.find v2imap v
    with Not_found -> (failwith ("Couldn't lookup v2i for"^(Printf.sprintf "%08lx" (G.V.label v))))
  in
  let i2v i = 
    try Hashtbl.find i2vmap i
    with Not_found -> (failwith ("Couldn't lookup i2v for "^string_of_int i))
  in
  let gv = 
  { graph = (i2v,v2i,cfg);
    sizer = (fun (_,_,cfg) -> let n = G.nb_vertex cfg in IDA.msg "%d vertices\n" n; n);
    text  = 
     (fun (i2v,v2i,cfg) i -> 
        IDA.msg "1\n";
        let v = i2v i in
        IDA.msg "2\n";
        let l = get_ir cfg v in
        IDA.msg "3\n";
        let l = List.map (fun ip -> X86Disasm.string_of_x86instr ip.instr) l in
        IDA.msg "4\n";
        let l = Util.opt_get (StringUtil.intersperse_string "\n" l) in
        IDA.msg "5\n";
        IDA.msg "%08lx: generating text for vertex %d: %s\n" (G.V.label v) i l;
        IDA.msg "6\n";
        l);
    edges = 
     (fun (i2v,v2i,cfg) -> 
       let el = G.fold_edges (fun v1 v2 l -> (v2i v1,v2i v2)::l) cfg [] in
       List.iter (fun (i1,i2) -> IDA.msg "Edge (%d,%d)\n" i1 i2) el;
       el); }
  in
  show_graph gv title