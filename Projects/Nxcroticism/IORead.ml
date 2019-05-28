open DataStructures
open IOPair

(* 
 * * For each (address,candidateset,value,size) read
 * * Look up the value in the rout hash table
 * * If it matches, that's a potential read behavior
*)
let determine_read_reg32_behaviors size io =
  (* Get post val -> var hash table *)
  let rout = io.rev_post in
  List.fold_left 
   (fun set (a32,s_read,v32) ->
      (* For each (reg32,disp32) in s_read *)
      VarInt32Set.fold
       (fun (v,d32) set ->
          match ht_find_opt rout (IRUtil.mk_fixed_const_of_i32 v32 size) with
          (* Value is not present, return the existing set *)
          | None -> set
          (* Value is present, so add (vdst,vr32,d32) to the set*)
          | Some(l) -> List.fold_left (fun set el -> VarVarInt32Set.add (el,v,d32) set) set l)
        (* Fold over all common read addresses *)
        s_read
        (* Accumulator VarVarInt32 set *)
        set)
    (* List fold accumulator: empty VarVarInt32Set *)
    VarVarInt32Set.empty
    (* Fold over all reads *)
    io.reads
          
let determine_aggregate_read_reg32_behaviors size ioc =
  let f i = determine_read_reg32_behaviors size (select_member size i) in
  match ioc.l_io with
  | [] -> VarVarInt32Set.empty 
  | x::xs -> List.fold_left (fun s i -> VarVarInt32Set.inter s (f i)) (f x) xs

let determine_aggregate_read_reg32_behaviors = triplicate determine_aggregate_read_reg32_behaviors 

(* Ensure that vdst.post = mem.pre[vreg.pre + disp32] *)
let verify_read_reg tbl vdst vreg disp32 size =
  let open VerifyUtil in
  IRUtil.mk_ne (dest_expr tbl vdst size) (mk_load_disp32_pre vreg disp32 size)

let verify_read_behaviors p verify ssa_tbl size s_read_reg_t =
  VarVarInt32Set.filter 
   (fun (vdst,vreg,d32) -> verify (IRUtil.mk_and p (verify_read_reg ssa_tbl vdst vreg d32 size)))
   (select_member size s_read_reg_t)

let verify_read_behaviors p verify ssa_tbl s_read_reg_t =
  (* Given a read of, say, a 32-bit quantity, there will be no corresponding read
     behaviors for the 16- and 8-bit registers.  So for every verified 32-bit read,
     we should add the following:
     1)  A verified 16-bit read at the same address
     2)  A verified 8-bit read at the same address
     3)  A verified 8-bit read at address+1
     
     Similarly, for verified 16-bit reads, we should add:
     1)  A verified 8-bit read at the same address
     2)  A verified 8-bit read at address+1 *)
  let s_read_pairs_t = triplicate (verify_read_behaviors p verify ssa_tbl) s_read_reg_t in
(*dump_load_const s_read_pairs_t;*)

  let s_read_pairs16 = 
    VarVarInt32Set.fold 
     (fun (vr,vm,d32) set ->
        VarVarInt32Set.add (X86ToIRUtil.v16_of_v32_exn vr,vm,d32) set)
      s_read_pairs_t.val32
      s_read_pairs_t.val16
  in
  let s_read_pairs8 = 
    VarVarInt32Set.fold 
     (fun (vr,vm,d32) set ->
        match X86ToIRUtil.v8p_opt_of_v16_exn vr with
        | Some(vh,vl) -> VarVarInt32Set.add (vh,vm,Int32.succ d32) (VarVarInt32Set.add (vl,vm,d32) set)
        | None -> set)
      s_read_pairs16
      s_read_pairs_t.val8
  in
  { s_read_pairs_t with val16 = s_read_pairs16; val8 = s_read_pairs8; }
  
