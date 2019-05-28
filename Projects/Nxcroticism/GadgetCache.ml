(*
#use "c:\\paframework\\Projects\\Nxcroticism\\GadgetCache.ml";;
*)

open FrameworkUtil
open DataStructures
open Encodable

(* 
Next phase of this project:  gadget cache.

Interface to this module should have a number of functions like the following:

* accept_reg_copy
* accept_reg_binop
* accept_ ...

After we've discovered and verified the gadgets, we should then iterate over
them, inspecting them to make sure that their combinations of properties are
useful (for example, rejecting gadgets that write to stack memory, or read from
non-stack memory while also writing to non-stack memory).  

Then, for each useful behavior of each sequence, we call one of the accept 
functions, which puts the gadget into some internal container that prunes away
useless duplicates.

After we have added all gadgets to the cache, then we call "finalize", which
for example is responsible for saturating the transfer graph.

After all of that is done, we now have a nice little module that we can query
from the code generator function to materialize the gadgets that we need during
synthesis.

One thing I realize about this code:  the saturation component ignores the
stack depth.  I am going to need to reconsider this and change the code.

*)
let get_successors size g vsrc = 
  List.fold_left (fun acc vdst ->
    let res = try Some(VarVarMap.find (vsrc,vdst) g) with Not_found -> None in
    match res with
    | Some(x) -> (List.map (fun (l,s) -> (vsrc,vdst,s,l)) x)@acc
    | None -> acc)
    []
    (match size with
     | IR.TypeReg_32 -> X86ToIRUtil.l_v_general_registers32_noesp
     | IR.TypeReg_16 -> X86ToIRUtil.l_v_general_registers16_nosp
     | IR.TypeReg_8  -> X86ToIRUtil.l_v_general_registers8
     | _ -> invalid_arg "get_successors")
     

let initial_transfer_maps = 
  let free map v = VarVarMap.add (v,v) [[],IRUtil.VarSet.empty] map in
  let me = VarVarMap.empty in
  let m32,m16,m8 = 
    List.fold_left 
     (fun (map32,map16,map8) (v32,v16,v8p_opt) -> 
        let map32 = free map32 v32 in
        let map16 = free map16 v16 in
        let map8  = match v8p_opt with
        | None -> map8
        | Some(vh8,vl8) -> free (free map8 vh8) vl8
        in (map32,map16,map8))
      (me,me,me)
      X86ToIRUtil.l_v_general_registers_parentage_noesp
  in
  { val8 = m8; val16 = m16; val32 = m32; }

let add_to_transfer_map map vs vd (l,s) = 
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

(* I think I need to modify this.  Suppose we add a new edge a -c-> b to the 
   graph.  What could change in the graph?
   
   - Could have new paths like a -> b -> d
   - Could have new paths like e -> a -> b
   
   So I need to explore in both directions whenever a new path is created.
   Not only should I put the children of b onto the stack, but also I should
   put the parents of a.
   
   *)
let finalize_register_transfers size map =
  (* Needs some alteration regarding l_edges in the lambda *)
  let enqueue_children wlist l_curstack g v =
    let l_edges = get_successors size g v in
    
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
              let badded,g = add_to_transfer_map g vgp vcur (l_gadgets_cum,s_clobber_cum) in
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
  let initial_work_items = 
    VarVarMap.fold 
     (fun (vs,vd) l_ls list -> (List.map (fun (l,s) -> (vd,[(vs,s,l)])) l_ls)@list)
      map
      []
  in
  let g = process_work_item map initial_work_items in
  g  

let dump_varset = IRUtil.VarSet.iter (fun v -> f_printf "%s, " (PpIR.ppVar v)) 

let dump_inner (l,s) = 
  let _ = List.iter (f_printf "%08lx  ") l in
  dump_varset s; 
  f_printf "\n"

let print_copy_matrix =
  VarVarMap.iter 
   (fun (vs,vd) l_ls -> 
      let _ = f_printf "%s->%s:\n" (PpIR.ppVar vs) (PpIR.ppVar vd) in
      List.iter dump_inner l_ls)

(*  For the tests below. *)
let mk_register_transfers l_copies = 
  let initial_map = 
    List.fold_left 
     (fun map (vs,vd,l,s) -> snd(add_to_transfer_map map vs vd (l,s))) 
      initial_transfer_maps.val32
      l_copies 
  in
  finalize_register_transfers IR.TypeReg_32 initial_map

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
  | None -> f_printf "test1(): path from eax -> ecx not found (should be one)!\n"
  | Some([[0x12345678l;0x23456789l],s]) when IRUtil.VarSet.cardinal s = 1 && IRUtil.VarSet.min_elt s = vEdx -> 
    f_printf "test1(): passed!\n"
  | Some(l) -> let _ = f_printf "test1(): failed; result was " in List.iter dump_inner l

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
  | None -> f_printf "test1(): path from eax -> ecx not found (should be two)!\n"
  | Some([l1,s1;l2,s2] as l) ->
   (match l1,l2 with
    | [0x12345678l;0x23456789l],[0x3456789Al]
      when (IRUtil.VarSet.cardinal s1 = 1 && IRUtil.VarSet.min_elt s1 = vEbp) &&
           (IRUtil.VarSet.cardinal s2 = 2 && IRUtil.VarSet.min_elt s2 = vEdx && IRUtil.VarSet.max_elt s2 = vEsi)
      -> f_printf "test2(): passed!\n"    | [0x3456789Al],[0x12345678l;0x23456789l]
      when (IRUtil.VarSet.cardinal s2 = 1 && IRUtil.VarSet.min_elt s2 = vEbp) &&
           (IRUtil.VarSet.cardinal s1 = 2 && IRUtil.VarSet.min_elt s1 = vEdx && IRUtil.VarSet.max_elt s1 = vEsi)
      -> f_printf "test2(): passed!\n"
    | _,_ ->   let _ = f_printf "test2(): failed; result was " in List.iter dump_inner l)
  | Some(l) -> let _ = f_printf "test2(): failed; result was " in List.iter dump_inner l

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
    | None -> f_printf "test3(): path from %s -> %s not found (should be one)!\n" str str
    | Some([[],s]) when IRUtil.VarSet.is_empty s -> f_printf "test3(): %s passed!\n" str
    | Some(l) -> let _ = f_printf "test3(): %s failed; result was " str in List.iter dump_inner l
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

let replicate_computation graph size l_in vout l_gadget s_clobber =
  let l_succ = get_successors size graph vout in
  List.fold_left (fun acc (_,vd,s,l) -> (vd,l_gadget@l,IRUtil.VarSet.union s s_clobber)::acc) l_in l_succ

let replicate_computations graph size =
  List.fold_left (fun acc (a,b,c) -> replicate_computation graph size acc a b c) []  

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
   
   Now I can resume coding the munch component. *)

type 'a t =  (* ' *) 
{
  (* Make it a real graph structure / better data structure? *)
  mutable transfer_matrix: ('a  (* ' *) list * IRUtil.VarSet.t) list VarVarMap.t triplicate;
  mutable write_binop_by_binop: (IR.var * IR.var * int32 * encodable * IRUtil.VarSet.t) list BinopMap.t triplicate;
  mutable set_reg_by_reg: (encodable * IRUtil.VarSet.t) list IRUtil.VarMap.t triplicate;
  (*reg_binop_cache: *)
  
}

let make () =
  let e = BinopMap.empty in let wbbb = { val8 = e; val16 = e; val32 = e } in
  let e = IRUtil.VarMap.empty in let srbr = { val8 = e; val16 = e; val32 = e } in
  {
    transfer_matrix = initial_transfer_maps; 
    write_binop_by_binop = wbbb; 
    set_reg_by_reg = srbr;
  }

(* Maps segregated by size
   Index by:
   - Binops
*)
let accept_write_binop gc size vr b vm d32 enc s =
  let _ = f_printf "accept_write_binop\n%!" in
  let map = select_member size gc.write_binop_by_binop in
  let l_opt = try Some(BinopMap.find b map) with Not_found -> None in
  let new_l = 
    match l_opt with
    | Some(l) -> (vr,vm,d32,enc,s)::l
    | None -> [(vr,vm,d32,enc,s)]
  in
  let map = BinopMap.add b new_l map in
  gc.write_binop_by_binop <- replace_member size gc.write_binop_by_binop map;
  ()

let get_write_binops gc size b =
  let map = select_member size gc.write_binop_by_binop in
  let l_opt = try Some(BinopMap.find b map) with Not_found -> None in
  match l_opt with
  | Some(l) -> l
  | None -> []
  
(* Maps segregated by size
   Index by:
   - ?
*)
let accept_write_reg gc size vr vm d32 enc s =
  ()
  
(* Should index this by output register and also binop, maybe both *)
let accept_read_binop gc size vr b vm d32 enc s =
  ()

let accept_read_reg gc size vr vm d32 enc s =
(*let map = select_member size gc.read_regs in*)
  ()

let accept_reg_binop gc size vd vl b vr enc s =
  ()
  
let accept_set_reg_value gc size vr enc s =
  let _ = f_printf "accept_set_reg_value\n%!" in
  let map = select_member size gc.set_reg_by_reg in
  let l_opt = try Some(IRUtil.VarMap.find vr map) with Not_found -> None in
  let new_l = 
    match l_opt with
    | Some(l) -> (enc,s)::l
    | None -> [(enc,s)]
  in
  let map = IRUtil.VarMap.add vr new_l map in
  gc.set_reg_by_reg <- replace_member size gc.set_reg_by_reg map;
  ()

let get_set_reg gc size vr value =
  let map = select_member size gc.set_reg_by_reg in
  let l_opt = try Some(IRUtil.VarMap.find vr map) with Not_found -> None in
  match l_opt with
  | Some(l) -> List.map (fun (enc,s) -> ({ enc with constant_value = Some(value); },s)) l
  | None -> []

let accept_reg_copy gc size vdst vsrc enc s = 
  let _ = f_printf "accept_reg_copy\n%!" in
  let map = select_member size gc.transfer_matrix in
  let _,map = add_to_transfer_map map vsrc vdst ([enc],s) in
  gc.transfer_matrix <- replace_member size gc.transfer_matrix map;
  ()

let finalize_reg_copy gc =
  let _ = gc.transfer_matrix <- triplicate_map finalize_register_transfers gc.transfer_matrix in
  ()

  