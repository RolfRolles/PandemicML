(*
#use "c:\\paframework\\Projects\\Nxcroticism\\Compiler.ml";;
*)
module VarVarComparator = struct
  type t = (IR.var * IR.var)
  let compare (l1,r1) (l2,r2) = 
    let open IR in
    let c1 = IRUtil.var_comparator l1 l2 in
    if c1 <> 0 then c1 else IRUtil.var_comparator r1 r2
end

module VarVarMap = Map.Make(VarVarComparator)

let get_successors g vsrc = 
  List.fold_left (fun acc vdst ->
    let res = try Some(VarVarMap.find (vsrc,vdst) g) with Not_found -> None in
    match res with
    | Some(x) -> (List.map (fun (l,s) -> (vsrc,vdst,s,l)) x)@acc
    | None -> acc)
    []
    X86ToIRUtil.l_v_general_registers(*_noesp*)

(* l_copies: [(vsrc,vdst,list of gadgets,set of clobbered registers)] *)
let mk_register_transfers l_copies =
  let s_v_empty = IRUtil.VarSet.empty in
  let l_free_transfer = [[],s_v_empty] in
  let free map v = VarVarMap.add (v,v) l_free_transfer map in
  let free_map = 
    List.fold_left 
     (fun map (v32,v16,v8p_opt) -> 
        let map = free (free map v32) v16 in
        match v8p_opt with
        | None -> map
        | Some(vh8,vl8) -> free (free map vh8) vl8)
      VarVarMap.empty 
      X86ToIRUtil.l_v_general_registers_parentage_noesp
  in
  let add_path map vs vd (l,s) =

    (* Does a path between the two variables exist already? *)
    let l_opt = try Some(VarVarMap.find (vs,vd) map) with Not_found -> None in
    match l_opt with

    (* No, add this path straight away and return. *)
    | None -> true,VarVarMap.add (vs,vd) [l,s] map

    (* Yes, a path does exist. *)
    | Some(l_paths) ->

      (* Check to see whether this path is inferior to an existing one.  I.e.,
         is there some other path between the same nodes that has a smaller
         clobber set than this one? *)
      if List.exists (fun (_,cs) -> IRUtil.VarSet.subset cs s) l_paths

      (* Yes, there was a path with a smaller clobber set. *)
      then false,map
      else  
        (* Remove any paths that are subsumed by the new one *)
        let filtered_paths = 
          List.filter 
           (fun (_,cs) -> not (IRUtil.VarSet.subset s cs)) 
            l_paths 
        in
        (* Add the new path *)
        true,VarVarMap.add (vs,vd) ((l,s)::filtered_paths) map
  in
  let initial_map = 
    List.fold_left 
     (fun map (vs,vd,l,s) -> snd(add_path map vs vd (l,s))) 
      free_map 
      l_copies 
  in
  
  (* Needs some alteration regarding l_edges in the lambda *)
  let enqueue_children wlist l_curstack g v =
    let l_edges = get_successors g v in
    
    (* Take successor vertex, clobber set, and list of gadgets *)
    let w_edges = List.map (fun (vp,vs,s,l) -> (vs,(vp,s,l)::l_curstack)) l_edges in
    w_edges @ wlist
  in
  
  let rec process_work_item g = function
  | [] -> g
  (* vcur:  the destination of the last write
     l_rev_visited:  the stack_entry list representing the path leading to this variable
  *)
  | (vcur,l_rev_visited)::items ->
    let g,items = 
      match l_rev_visited with
      | []    -> failwith "process_work_item: empty stack in work item"
  
      (* Had one parent frame.  This corresponds to a single edge in the graph,
         one that exists already.  In this case we should enqueue all of the 
         children onto the path, thereby making paths of length two, and thereby
         triggering the next pattern match. *)
      | _::[] -> g,enqueue_children items l_rev_visited g vcur
      
      (* Had more than one parent frame, i.e. a proper path that is not a single
         edge.  Consider all terminal subpaths represented by the stack. *)
      | (_,s_clobber_p,l_gadgets_p)::xs -> 
        let (g,items,_,_) = 
          List.fold_left 
           (fun (g,items,s_clobber_cum,l_gadgets_cum) (vgp,s_clobber_gp,l_gadgets_gp) -> 
              (* As we walk the path backwards, accumulate the clobber sets and
                 the trail of instructions that lead us to this point. *)
              let s_clobber_cum = IRUtil.VarSet.union s_clobber_cum s_clobber_gp in
              let l_gadgets_cum = l_gadgets_gp @ l_gadgets_cum in
  
              (* I think this is buggy.  Need to think more carefully about this.
                 Once we find a shorter path vgp -> vcur, what should we add to
                 the work list?
                 
                 What could have changed?
                 
                 vgp -> vcur -> x for all x
                 
                 So adding the children of vcur does work.  However it could 
                 create more work that is necessary.  Is there any reason not
                 to start a new stack that just contains vgp, vcur, and vcur's
                 children?
                 *)
              let badded,g = add_path g vgp vcur (l_gadgets_cum,s_clobber_cum) in
              let items = 
                if badded
                then enqueue_children items l_rev_visited g vcur
                else items
              in
              (g,items,s_clobber_cum,l_gadgets_cum))
           (g,items,s_clobber_p,l_gadgets_p)
            xs
        in
        g,items
    in
    process_work_item g items
  in
  let initial_work_items = List.map (fun (vs,vd,l,s) -> (vd,[(vs,s,l)])) l_copies in
  let g = process_work_item initial_map initial_work_items in
  g  

let dump_varset = IRUtil.VarSet.iter (fun v -> IDA.msg "%s, " (PpIR.ppVar v)) 

let dump_inner (l,s) = 
  let _ = List.iter (IDA.msg "%08lx  ") l in
  dump_varset s; 
  IDA.msg "\n"

let print_copy_matrix =
  VarVarMap.iter 
   (fun (vs,vd) l_ls -> 
      let _ = IDA.msg "%s->%s:\n" (PpIR.ppVar vs) (PpIR.ppVar vd) in
      List.iter dump_inner l_ls)

let test1 () =
  let open X86ToIRUtil in
  (* eax -> ebx -> ecx path subsumes eax -> ecx path *)
  let l_copies1 = 
   (vEax,vEbx,[0x12345678l],IRUtil.var_setify [])::
   (vEbx,vEcx,[0x23456789l],IRUtil.var_setify [vEdx])::
   (vEax,vEcx,[0x3456789Al],IRUtil.var_setify [vEdx;vEsi])::
    []
  in
  let m_copies1 = mk_register_transfers l_copies1 in
  let res = try Some(VarVarMap.find (vEax,vEcx) m_copies1) with Not_found -> None in
  match res with
  | None -> IDA.msg "test1(): path from eax -> ecx not found (should be one)!\n"
  | Some([[0x12345678l;0x23456789l],s]) when IRUtil.VarSet.cardinal s = 1 && IRUtil.VarSet.min_elt s = vEdx -> 
    IDA.msg "test1(): passed!\n"
  | Some(l) -> let _ = IDA.msg "test1(): failed; result was " in List.iter dump_inner l

let test2 () = 
  let open X86ToIRUtil in
  (* eax -> ebx -> ecx path is different than the eax -> ecx path *)
  let l_copies2 = 
   (vEax,vEbx,[0x12345678l],IRUtil.var_setify [])::
   (vEbx,vEcx,[0x23456789l],IRUtil.var_setify [vEbp])::
   (vEax,vEcx,[0x3456789Al],IRUtil.var_setify [vEdx;vEsi])::
    []
  in
  let m_copies2 = mk_register_transfers l_copies2 in
  let res = try Some(VarVarMap.find (vEax,vEcx) m_copies2) with Not_found -> None in
  match res with
  | None -> IDA.msg "test1(): path from eax -> ecx not found (should be two)!\n"
  | Some([l1,s1;l2,s2] as l) ->
   (match l1,l2 with
    | [0x12345678l;0x23456789l],[0x3456789Al]
      when (IRUtil.VarSet.cardinal s1 = 1 && IRUtil.VarSet.min_elt s1 = vEbp) &&
           (IRUtil.VarSet.cardinal s2 = 2 && IRUtil.VarSet.min_elt s2 = vEdx && IRUtil.VarSet.max_elt s2 = vEsi)
      -> IDA.msg "test2(): passed!\n"    | [0x3456789Al],[0x12345678l;0x23456789l]
      when (IRUtil.VarSet.cardinal s2 = 1 && IRUtil.VarSet.min_elt s2 = vEbp) &&
           (IRUtil.VarSet.cardinal s1 = 2 && IRUtil.VarSet.min_elt s1 = vEdx && IRUtil.VarSet.max_elt s1 = vEsi)
      -> IDA.msg "test2(): passed!\n"
    | _,_ ->   let _ = IDA.msg "test2(): failed; result was " in List.iter dump_inner l)
  | Some(l) -> let _ = IDA.msg "test2(): failed; result was " in List.iter dump_inner l

let test3 () = 
  let open X86ToIRUtil in
  (* Make sure that inferior paths do not subsume the free null paths *)
  let l_copies3 = 
   (vEax,vEbx,[1l],IRUtil.var_setify [vEcx;vEdx])::
   (vEbx,vEax,[2l],IRUtil.var_setify [vEbp;vEdi])::
   (vEax,vEcx,[3l],IRUtil.var_setify [vEbx;vEdi])::
   (vEcx,vEax,[4l],IRUtil.var_setify [vEdx;vEsi])::
   (vEbx,vEcx,[5l],IRUtil.var_setify [vEdi;vEsi])::
   (vEcx,vEbx,[6l],IRUtil.var_setify [vEax;vEdx])::
    []
  in
  let m_copies3 = mk_register_transfers l_copies3 in
  let ensure_empty_singleton var =
    let str = PpIR.ppVar var in
    let res = try Some(VarVarMap.find (var,var) m_copies3) with Not_found -> None in
    match res with
    | None -> IDA.msg "test3(): path from %s -> %s not found (should be one)!\n" str str
    | Some([[],s]) when IRUtil.VarSet.is_empty s -> IDA.msg "test3(): %s passed!\n" str
    | Some(l) -> let _ = IDA.msg "test3(): %s failed; result was " str in List.iter dump_inner l
  in
  let _ = ensure_empty_singleton vEax in
  let _ = ensure_empty_singleton vEbx in
  let _ = ensure_empty_singleton vEcx in
  ()

let test_mk_register_transfers () =
  let _ = test1 () in
  let _ = test2 () in
  let _ = test3 () in
  ()

let replicate_computation graph l_in vout l_gadget s_clobber =
  let l_succ = get_successors graph vout in
  List.fold_left (fun acc (_,vd,s,l) -> (vd,l_gadget@l,IRUtil.VarSet.union s s_clobber)::acc) l_in l_succ

let uncurry3 f (x,y,z)   = f x y z

let replicate_computations graph =
  List.fold_left (fun acc p -> uncurry3 (replicate_computation graph acc) p) []  

(* Now, despite the fact that the engineering is kind of shitty, I can pass in
   the register transfer instructions from the binary and output a "graph" that
   embeds the replication paths between registers. 
   
   I would like to have some nice high-level functionality where I can pass in
   a "computation" and the corresponding output register, and get back all other
   "computations" where the result is replicated to the other output registers.
   
   That's easy.  Given (vdst,lcomp,scomp) Simply iterate over the map for 
   \forall vother (vdst,vother) -> [(li,si)] 
   Return [(vother,lcomp@li,IRUtil.VarSet.union scomp si)].
   
   This is our set of replicated computations. 
   
   Great.  That's trivial.
   
   Now I can resume coding the munch component.
   *)

