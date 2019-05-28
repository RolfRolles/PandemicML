open DataStructures
open IOPair
open FrameworkUtil

(* Given the output values at the end of the emulation, reverse the hash table.
   * rout = reverse_ht
   * For each var/value pair (v,c) in ht_pre
   * * l = rout[v] option
   * * For each var x in l, where x <> v, (v,x) is a candidate copy
   * * Make a map Var -> [potential;copy;destination;variables]
*)
let determine_copy_behaviors io =
  let rout = io.rev_post in
  let vll = 
    Hashtbl.fold 
     (fun v c l -> 
        if v <> X86ToIRUtil.vEsp
        then
          match ht_find_opt rout c with
          | Some(vl) -> (v,IRUtil.var_setify (List.filter (fun x -> x <> v) vl))::l
          | None -> l
        else l)
      io.pre
      []
  in IRUtil.var_mapify vll

(* Determine all copy behaviors across a list of i/o pairs *)
let determine_aggregate_copy_behaviors size ioc = 
  let f i = determine_copy_behaviors (select_member size i) in
  match ioc.l_io with
  | [] -> IRUtil.VarMap.empty
  | x::xs -> List.fold_left (fun m i -> varmapset_inter m (f i)) (f x) xs
  
let determine_aggregate_copy_behaviors = triplicate determine_aggregate_copy_behaviors 

(* Ensure that vdst.post = mem.pre[vreg.pre + disp32] *)
let verify_reg_copy tbl vsrc vdst size = 
  let open VerifyUtil in
  IRUtil.mk_ne (src_expr vsrc size) (dest_expr tbl vdst size)

let verify_copy_behaviors verify ssa_tbl m_copies size set =
  IRUtil.VarMap.fold
   (fun invar s set ->
      IRUtil.VarSet.fold
       (fun outvar set ->
          if verify (verify_reg_copy ssa_tbl invar outvar size) 
          then VarVarSet.add (invar,outvar) set
          else set)
        s
        set)
    m_copies
    set

(* If it is the case that a 32-bit copy was verified, then it should
   always be the case that a corresponding 16-bit candidate was 
   identified.  So print a message if something fails (in which case,
   some weird bug is happening). *)
let prune_redundant_subregister_copies s_copy_pairs_in m_copies =
  let open IRUtil in
  VarVarSet.fold
   (fun (vsrc,vdst) map ->
      let set_opt = try Some(VarMap.find vsrc map) with Not_found -> None in
      let map = VarMap.remove vsrc map in
      match set_opt with
      | Some(s) -> 
        let s = VarSet.remove vdst s in 
        if VarSet.is_empty s
        then map
        else VarMap.add vsrc s map
      | None -> f_printf 
          "Pre-verified subregister copy %s -> %s (inherited from a verified larger copy) was not a candidate!\n"
          (PpIR.ppVar vsrc)
          (PpIR.ppVar vdst);
        map)
    s_copy_pairs_in
    m_copies

(* Given:
   s_copy_pairs_verified: the "larger" pre-verified facts
   size_larger:  the size of the variables in s_copy_pairs_verified
   s_copy_pairs:  the set of "smaller" pre-verified facts (returned modified)
   size_smaller: the size of the variables in s_copy_pairs *)

  (* Once we verify any 32-bit copies, we get the 16-bit ones for free, and the
     8-bit ones too (if the register supports 8-bit subdivisions). *)
let generate_smaller_copy_facts s_copy_pairs_verified f_derive_equalities_l =
  VarVarSet.fold 
   (fun (vsrc,vdst) set -> 
      let l = f_derive_equalities_l vsrc vdst in
      List.fold_left (fun set (vs,vd) -> VarVarSet.add (vs,vd) set) set l)
    s_copy_pairs_verified
    VarVarSet.empty 

let map_32bit_to_16bit vsrc vdst  = 
  let open X86ToIRUtil in
  [(v16_of_v32_exn vsrc, v16_of_v32_exn vdst)]

let map_16bit_to_8bit  vsrc vdst = 
  let open X86ToIRUtil in let open IRUtil in
  match v8p_opt_of_v16_exn vsrc,v8p_opt_of_v16_exn vdst with
  | Some(vsrch8,vsrcl8),Some(vdsth8,vdstl8) -> [(vsrcl8,vdstl8);(vsrch8,vdsth8)]
  | _,_ -> []

let verify_copy_behaviors verify ssa_tbl m_copies_t =
  let s_copy_pairs32  = verify_copy_behaviors verify ssa_tbl m_copies_t.val32 IR.TypeReg_32 VarVarSet.empty in
  let s_copy_pairs16  = generate_smaller_copy_facts s_copy_pairs32 map_32bit_to_16bit in
  let m_copies_16     = prune_redundant_subregister_copies s_copy_pairs16 m_copies_t.val16 in

  let s_copy_pairs16  = verify_copy_behaviors verify ssa_tbl m_copies_16 IR.TypeReg_16 s_copy_pairs16 in
  let s_copy_pairs8   = generate_smaller_copy_facts s_copy_pairs16 map_16bit_to_8bit in
  let m_copies_8      = prune_redundant_subregister_copies s_copy_pairs8 m_copies_t.val8 in

  let s_copy_pairs8   = verify_copy_behaviors verify ssa_tbl m_copies_8  IR.TypeReg_8  s_copy_pairs8  in
  { val32 = s_copy_pairs32; val16 = s_copy_pairs16; val8 = s_copy_pairs8; }
