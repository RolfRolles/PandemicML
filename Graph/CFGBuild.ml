exception IndirectJump of int32

module type Language = sig
  type t
  val disasm : int32 -> t * ASMUtil.cfsuccessors
  val disasm_ex : int32 -> int32 -> t * ASMUtil.cfsuccessors * int32
end

module type S = sig
  type lang
  module C : CFG.CFG with type language = lang list
  val merge_singleton_vertices : C.G.t -> C.G.t
  val remove_empty_vertices : C.G.t -> C.G.t
  val build : ?stopfun:(int32 -> bool) -> int32 -> C.G.t * (int32, unit) Hashtbl.t
  val build_ex : ?start_graph:C.G.t -> int32 -> int32 -> C.G.t
  val get_order : C.G.t -> C.G.V.t -> C.G.V.t list
end

module MakeGraphBuilder(Lang : Language) = 
struct
  type lang = Lang.t
  module C = CFG.Make(Lang)

  let merge_singleton_vertices cfg = 
    (* Does the vertex have one parent, which in turn has one child? *)
    let can_collapse_into_pred v =
      match (C.G.fold_pred (fun v list -> v::list) cfg v []) with
      | [p] -> (match (C.G.fold_succ (fun v list -> v::list) cfg p []) with
        | [s] -> Some(p)
        | _ -> None)
      | _ -> None
    in
    let p2cmap = Hashtbl.create 50 in
    (* For every vertex, if we can collapse it into its parent, record this
       fact in the parent-to-child map (p2cmap).  Return a list of those 
       vertices for which the previous sentence is false.  These are the
       "tops"; those vertices which sit at the top of a chain of 
       single-parent-single-child vertices, or those that simply behave like
       regular vertices. *)
    let tops = 
      C.G.fold_vertex 
       (fun v top -> match can_collapse_into_pred v with
        | Some(p) -> Hashtbl.replace p2cmap p v; top
        | None    -> v::top)
        cfg 
        [] 
    in
    (* Given a starting parent, walk the p2cmap and return a list (in order)
       of those vertices that can be merged together. *)
    let collect_p2c_chain p =
      let rec aux p l =
        match (try Some(Hashtbl.find p2cmap p) with Not_found -> None) with
        | Some(c) -> aux c (c::l)
        | None -> List.rev l
      in aux p []
    in
    
    (* For every vertex in 'tops', collect an ordered list (as above) of its
       mergeable children. *)
    let chains = List.map (fun p -> (p,collect_p2c_chain p)) tops in
    let  ir c = C.get_ir cfg c in
    let rir c = List.rev (ir c) in
    
    (* For each parent vertex in 'chains', return a list of triples 
       containing:
       * The parent
       * The IR that will adorn the parent after we're done here
       * The list of successors of the final child vertex (or the parent 
         itself if there were no mergeable children). *)
    let parent_ir_succ_list = 
      List.map 
       (fun (p,cl) ->
          match cl with
          | [] -> (p, ir p, [])
          | _  -> 
            let last,il = List.fold_left
             (fun (last,il) c -> (c, (rir c)@il))
             (p,[])
              cl
            in
            let pir = (ir p)@(List.rev il) in
            let succs = C.G.fold_succ (fun v list -> v::list) cfg last [] in
            (p, pir, succs))
        chains
    in

    (* Obtain a new CFG with edges from the parent to the children, and with
       the IR collected from the children. *)
    let cfg = 
      List.fold_left 
       (fun cfg (p,ir,succs) -> 
          List.fold_left 
           (fun cfg c -> C.add_edge cfg p c) 
           (C.set_ir cfg p ir) 
            succs)
        cfg
        parent_ir_succ_list
    in
    
    (* Remove all of the mergeable children from the CFG; return the new CFG. *)
    List.fold_left
     (fun cfg (_,cl) -> 
        List.fold_left 
         (fun cfg c -> C.remove_vertex cfg c) 
          cfg 
          cl)
      cfg
      chains

  (* Remove all vertices with no IR upon them *)
  let remove_empty_vertices cfg =
    (* Gather all vertices with no IR upon them *)
    let empty_vertices = C.G.fold_vertex (fun v l -> match C.get_ir cfg v with | [] -> (C.G.V.label v)::l | _ -> l) cfg [] in
    List.fold_left 
     (fun cfg v -> 
        let v = C.find_vertex cfg v in
        (* Get all the predecessors and successors of the vertex *)
        let preds = C.G.fold_pred (fun v l -> v::l) cfg v [] in
        let succs = C.G.fold_succ (fun v l -> v::l) cfg v [] in
        (* Add edges from the predecessors to the successors *)
        let cfg = 
          List.fold_left 
           (fun cfg p ->
              List.fold_left 
               (fun cfg s -> C.add_edge cfg p s) 
                cfg
                succs)
            cfg
            preds
        in
        (* Remove the vertex from the graph *)
        C.remove_vertex cfg v)
      (* Do this for all empty vertices*)
      cfg
      empty_vertices

  (* Build starting from an address, and given a function that tells when to
     stop (which defaults to never stopping) *)
  let build ?(stopfun=(fun _ -> false)) start_ea = 
    (* Decoded instructions, so we only process them once *)
    let decoded    = Hashtbl.create 100 in
    (* Sucessors of a given address *)
    let successors = Hashtbl.create 100 in
    (* Leaders of basic blocks *)
    let leaders    = Hashtbl.create  50 in
    (* Called functions (can be ignored) *)
    let calltgts   = Hashtbl.create  20 in
  
    (* Add a leader if it is not a stop address *)
    let l a = if not (stopfun a) then Hashtbl.replace leaders a () in
    let d a = Hashtbl.find decoded a in  
  
    (* Add starting vertex to the set of leaders *)
    l start_ea;
  
    (* Extract flow information from instructions; process successors 
       recursively *)
    let rec process_insn addr =
      if 
       (* If we've seen the address before, or we should stop, don't process *)
       (try 
          ignore(d addr); true 
        with 
          Not_found -> stopfun addr)
      then () 
      else begin
        (* Disassemble the instruction *)
        let de,s = Lang.disasm addr in
        (* Add the decoded instruction to the table *)
        Hashtbl.add decoded    addr de;
        (* Add the address -> successors mapping to the table *)
        Hashtbl.add successors addr s;
        
        (* Process successors recursively *)
        let p = process_insn in
        let open ASMUtil in
        match s with
        (* Straight flow, process next address *)
        | Flow(a)   -> p a 
        | Call(a,r) -> Hashtbl.replace calltgts a (); p r
        | ICall(r)  -> p r
        (* Unconditional branch, process successor *)
        | Jmp(a)    -> l a; p a
        (* Conditional branch, process both directions *)
        | Jcc(t,f)  -> l t; l f; p t; p f
        (* Otherwise, stop *)
        | Return -> ()
        | IJmp -> () (*raise (IndirectJump(addr))*)
      end
    in 
  
    (* Bootstrap CFG building starting at the root address *)
    process_insn start_ea;
  
    let edges  = Hashtbl.create (Hashtbl.length leaders * 2) in
    let blocks = Hashtbl.create (Hashtbl.length leaders * 2) in
  
    (* Extract instructions in a basic block, and its successors *)
    let get_block b _ = 
      (* Extract basic blocks and successors.  Returns a reversed list of 
         instructions, and a list of successor addresses. *)
      let rec aux i a =
        (* If we should stop, no successors *)
        if stopfun a
        then (i,[])
        else
         ((* Put current instruction onto list *)
          let i = (d a)::i in
          let open ASMUtil in
          match (Hashtbl.find successors a) with
          (* For all flowing instructions, if the next address is not a leader,
             then continue to process recursively.  Otherwise, if it is a leader,
             this is the end of the basic block, and it has one successor (the
             subsequent address). *)
          | Flow(f)      
          | Call(_,f)
          | ICall(f) -> 
            if 
             (try 
               ignore(Hashtbl.find leaders f); true 
             with 
               Not_found -> false)
            then (i,[f]) 
            else aux i f
          (* Jumps terminate basic blocks and have one or two sucessors. *)
          | Jmp(f)   -> (i,[f])
          | Jcc(t,f) -> (i,[t;f])
          (* Returns and indirect jumps terminate and have zero successors. *)
          | Return   -> (i,[])
          | IJmp -> (i,[])) (*raise (IndirectJump(b))*)
      in 
      let dl,el = aux [] b in
      (* Remove all edges to addresses at which we should stop. *)
      let el = List.filter (fun x -> not (stopfun x)) el in
      (* Add the edges for this vertex. *)
      Hashtbl.add edges  b el;
      (* Add the statements for this vertex. *)
      Hashtbl.add blocks b (List.rev dl)
    in
    (* Second-stage CFG building *)
    Hashtbl.iter get_block leaders;

    (* Add all vertices to the graph *)
    let cfg = 
      Hashtbl.fold 
       (fun b l cfg -> fst(C.create_vertex cfg b l)) 
        blocks 
       (C.empty ()) 
    in
    
    (* Add all edges to the graph *)
    let cfg = Hashtbl.fold 
     (fun b l cfg -> 
        let f = C.find_vertex in
        let bv = f cfg b in
        List.fold_left (fun cfg s -> C.add_edge cfg bv (f cfg s)) cfg l)
      edges
      cfg
    in
    (cfg,calltgts)

  (* THIS FUNCTION IS ONLY USEFUL FOR OREANS' PRODUCTS *)
  (* Build starting from an address, and given a function a start key *)
  let build_ex ?(start_graph = C.empty()) start_ea start_state = 
    (* Decoded instructions, so we only process them once *)
    let decoded    = Hashtbl.create 100 in
    (* Sucessors of a given address *)
    let successors = Hashtbl.create 100 in
    (* Leaders of basic blocks *)
    let leaders    = Hashtbl.create  50 in
  
    (* Add a leader if it is not a stop address *)
    let l a = Hashtbl.replace leaders a () in
    let d a = Hashtbl.find decoded a in  
    let has_vertex a = try ignore(C.find_vertex start_graph a); true with Not_found -> false in
  
    (* Add starting vertex to the set of leaders *)
    l start_ea;
  
    (* Extract flow information from instructions; process successors 
       recursively *)
    let rec process_insn addr state =
      if 
       (* If we've seen the address before, or we should stop, don't process *)
       (try 
          ignore(d addr); true 
        with 
          Not_found -> has_vertex addr)
      then () 
      else begin
        (* Disassemble the instruction *)
        let de,s,new_state = Lang.disasm_ex addr state in
        (* Add the decoded instruction to the table *)
        Hashtbl.add decoded    addr de;
        (* Add the address -> successors mapping to the table *)
        Hashtbl.add successors addr s;
        
        (* Process successors recursively *)
        let p = process_insn in
        let open ASMUtil in
        match s with

        (* CODEVIRTUALIZER SPECIFIC *)
        | ICall(a)
        | Call(_,a)
        | Flow(a) -> p a new_state
        | Jmp(a) -> l a; p a 0l
        | Jcc(ta,fa) -> l ta; l fa; p fa new_state; p ta 0l
        | IJmp
        | Return -> ()
      end
    in 
  
    (* Bootstrap CFG building starting at the root address *)
    process_insn start_ea start_state;
  
    let edges  = Hashtbl.create (Hashtbl.length leaders * 2) in
    let blocks = Hashtbl.create (Hashtbl.length leaders * 2) in
  
    (* Extract instructions in a basic block, and its successors *)
    let get_block b _ = 
      (* Extract basic blocks and successors.  Returns a reversed list of 
         instructions, and a list of successor addresses. *)
      let rec aux i a =
        (* Put current instruction onto list *)
        let i = (d a)::i in
        let open ASMUtil in
        match (Hashtbl.find successors a) with
        (* For all flowing instructions, if the next address is not a leader,
           then continue to process recursively.  Otherwise, if it is a leader,
           this is the end of the basic block, and it has one successor (the
           subsequent address). *)
        | Flow(f)      
        | Call(f,_)
        | ICall(f) -> 
          if 
           (try 
             ignore(Hashtbl.find leaders f); true 
           with 
             Not_found -> has_vertex f)
          then (i,[f]) 
          else aux i f
        (* Jumps terminate basic blocks and have one or two sucessors. *)
        | Jmp(f)   -> (i,[f])
        | Jcc(t,f) -> (i,[t;f])
        (* Returns and indirect jumps terminate and have zero successors. *)
        | Return   -> (i,[])
        | IJmp -> (i,[]) (*raise (IndirectJump(b))*)
      in 
      let dl,el = aux [] b in
      (* Add the edges for this vertex. *)
      Hashtbl.add edges  b el;
      (* Add the statements for this vertex. *)
      Hashtbl.add blocks b (List.rev dl)
    in
    (* Second-stage CFG building *)
    Hashtbl.iter (fun a _ -> if has_vertex a then Hashtbl.remove leaders a else ()) leaders;
    Hashtbl.iter get_block leaders;

    (* Add all vertices to the graph *)
    let cfg = 
      Hashtbl.fold 
       (fun b l cfg -> fst(C.create_vertex cfg b l)) 
        blocks 
       (start_graph) 
    in
    
    (* Add all edges to the graph *)
    let cfg = Hashtbl.fold 
     (fun b l cfg -> 
        let f = C.find_vertex in
        let bv = f cfg b in
        List.fold_left (fun cfg s -> C.add_edge cfg bv (f cfg s)) cfg l)
      edges
      cfg
    in
    cfg

(* Only consider a subset of vertices.  For the first recursion, this is the 
   entire graph. *)
let rec get_order cfg first_vertex vertices_to_consider =
  let considertbl = Hashtbl.create 200 in
  List.iter (fun x -> Hashtbl.add considertbl x ()) vertices_to_consider;
  let consider x = 
    try 
      ignore(Hashtbl.find considertbl x); true 
    with 
      Not_found -> false 
  in
  
  (* SCC lowlink number *)
  let lowlink = Hashtbl.create 200 in
  List.iter (fun x -> Hashtbl.add lowlink x 0) vertices_to_consider;
  let lln v = 
    try 
      Hashtbl.find lowlink v 
    with 
      Not_found -> failwith (Printf.sprintf "%08lx: lln failed" (C.G.V.label v)) 
  in
  let setll v i = Hashtbl.replace lowlink v i in
  
  (* SCC index number *)
  let index   = Hashtbl.create 200 in
  List.iter (fun x -> Hashtbl.add index x (None)) vertices_to_consider;
  let idx v = 
    let i =  
      try
        Hashtbl.find index v
      with 
        Not_found -> failwith (Printf.sprintf "%08lx: idx failed" (C.G.V.label v))
    in
    match i with 
    | Some(x) -> x 
    | None -> failwith "get_order: idx lookup failed" 
  in
  let setidx v i = Hashtbl.replace index v (Some(i)) in
  let have_done v =
    try
      match Hashtbl.find index v with
      | None -> false
      | Some(_) -> true
    with
      Not_found -> failwith (Printf.sprintf "%08lx: have_done failed" (C.G.V.label v))
  
  in
  (* SCC instack counter *)
  let instack = Hashtbl.create 200 in
  List.iter (fun x -> Hashtbl.add instack x false) vertices_to_consider;
  let ins v = 
    try
      Hashtbl.find instack v
    with 
      Not_found -> failwith (
        let cstr = List.fold_left (fun s v -> s^(Printf.sprintf "%08lx\n" (C.G.V.label v))) "" vertices_to_consider in
        Printf.sprintf "%08lx: instack checking failed; consider = %B, %s" (C.G.V.label v) (consider v) cstr)

  in
  (* Stack *)
  let stack = Stack.create () in
  
  (* SCC min operator *)
  let min i j = if i <= j then i else j in
  
  let order = ref [] in
  
  (* Reference variables *)
  let cur_dfsnum = ref 0 in
  
  let rec visit cur_vertex = 
    incr cur_dfsnum;
    setidx cur_vertex !cur_dfsnum;
    setll  cur_vertex !cur_dfsnum;
    Hashtbl.replace instack cur_vertex true;
    Stack.push cur_vertex stack;
  
    (* Recurse into children.  Upon termination, update the LowLink number 
       of the current vertex if the child has a lower one. *)
    C.G.iter_succ 
     (fun child -> 
        if (consider child) && (not (have_done child))
        then (visit child; setll cur_vertex (min (lln cur_vertex) (lln child)))
        else if (consider child) && (not (idx child = 0)) && (ins child)
        then (setll cur_vertex (min (lln cur_vertex) (idx child))))
      cfg 
      cur_vertex;
    
    (* If the lowlink of the current vertex is the same as its index, then 
       this is the head of a SCR; pop all vertices off of the stack until 
       we reach the head, and store them in a list. *)
    if (lln cur_vertex) == (idx cur_vertex)
    then
     (let rec aux i l = 
        let child = Stack.pop stack in
        Hashtbl.replace instack child false;
        setidx child 0;
        if (child <> cur_vertex) then aux (i+1) (child::l) else (i+1,child::l)
      in
      (* Here's fr33ke's modification from the standard Tarjan SCR algorithm:
         If the size of the SCR is one, merely prepend the current vertex 
         onto the ordering.  If the size of the SCR is greater than one, 
         then we have a proper SCR, and we call the algorithm in which we
         currently reside recursively to obtain an ordering for the SCR. *)
      let scr_size,scr_vertices = aux 0 [] in
      if (scr_size = 1)
      then order := cur_vertex :: !order
      else
       (let keephash = Hashtbl.create (scr_size + 10) in
        List.iter (fun v -> Hashtbl.replace keephash v ()) scr_vertices;
        let keep v = 
          try 
            ignore(Hashtbl.find keephash v); true 
          with 
            Not_found -> false 
        in
        (* Select a vertex with the maximal number of predecessors outside of the SCC *)
        let first =
          let _,first = 
            List.fold_left
             (fun (hi,hiv) v ->
                let nhi = 
                  C.G.fold_pred 
                   (fun p n -> 
                      if (keep p)
                      then n 
                      else (n+1)) 
                    cfg 
                    v 
                    0 
                in
                if (nhi > hi)
                then (nhi,v)
                else (hi,hiv))
             (0,List.hd scr_vertices)
              scr_vertices
          in first
        in 
        let new_order = get_order cfg first scr_vertices in
        
        (order := (new_order)@(!order))))
      (*let s  = List.fold_left (fun s v -> s^(Printf.sprintf "%08lx\n" (C.G.V.label v))) "" scr_vertices in
        let s2 = List.fold_left (fun s v -> s^(Printf.sprintf "%08lx\n" (C.G.V.label v))) "" !order in
        let s3 = List.fold_left (fun s v -> s^(Printf.sprintf "%08lx\n" (C.G.V.label v))) "" new_order in
        failwith (Printf.sprintf "%08lx: SCC head\n%s\n Order %s\n New order %s\n" (C.G.V.label first) s s2 s3);()))*)
  in 

  (* Mark that first_vertex has already been visited, to terminate the 
     inevitable recursion. *)
(*visit first_vertex;*)
  let _ = setidx first_vertex 0 in

  (* Iterate over each of those of first_vertex's children which we are
     considering. *)
  C.G.iter_succ
   (fun c -> 
      if (consider c) && (not (have_done c))
      then visit c)
    cfg 
    first_vertex;
  (* Return the ordering *)
  first_vertex::(!order)

(* Bootstrap.  We begin by considering all vertices. *)
let get_order cfg first = 
  let vertices = C.G.fold_vertex (fun v l -> v::l) cfg [] in
  get_order cfg first vertices

end

type 'a s = (module S with type lang = 'a)