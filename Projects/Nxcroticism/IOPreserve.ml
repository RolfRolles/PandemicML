open DataStructures
open IOPair

(* Given an i/o structure, take the reserved registers list (no ESP) and 
  consult the pre/post hash tables to determine which they are preserved. 
  
  OUTSTANDING QUESTION:  Should I include the stack variables in this?
*)
let determine_preserve_behaviors io =
  let open IRUtil in
  List.fold_left 
   (fun s v ->
      match ht_find_opt io.pre v,ht_find_opt io.post v with
      | Some(vpre),Some(vpost) when vpre = vpost -> VarSet.add v s
      | _,_ -> s)
    VarSet.empty
    X86ToIRUtil.l_v_general_registers32_noesp

(* Determine the preserved registers across an i/o structure list *)
let determine_aggregate_preserve_behaviors size ioc =
  let open IRUtil in
  let f i = determine_preserve_behaviors (select_member size i) in
  match ioc.l_io with
  | [] -> VarSet.empty
  | x::xs -> List.fold_left (fun s i -> VarSet.inter s (f i)) (f x) xs

let determine_aggregate_preserve_behaviors = triplicate determine_aggregate_preserve_behaviors 

(* Ensure that reg.post = reg.pre *)
let verify_reg_preserved tbl vreg size = 
  let open VerifyUtil in
  IRUtil.mk_ne (src_expr vreg size) (dest_expr tbl vreg size) 

let mk_conjoined_preserve_expr ssa_tbl size s_preserved = 
  IRUtil.VarSet.fold 
   (fun v e -> IRUtil.mk_and (verify_reg_preserved ssa_tbl v size) e) 
    s_preserved
    IRUtil.mk_true
  
let verify_preserved_behaviors verify ssa_tbl size s_preserved =
  if verify (mk_conjoined_preserve_expr ssa_tbl size s_preserved)
  then s_preserved
  else IRUtil.VarSet.filter (fun v -> verify (verify_reg_preserved ssa_tbl v size)) s_preserved

let verify_preserved_behaviors verify ssa_tbl s_preserved_t =
  let s_preserved32 = verify_preserved_behaviors verify ssa_tbl IR.TypeReg_32 s_preserved_t.val32 in
  let s_preserved16_pv = 
    IRUtil.VarSet.fold 
     (fun v32 set -> IRUtil.VarSet.add (X86ToIRUtil.v16_of_v32_exn v32) set) 
      s_preserved32 
      IRUtil.VarSet.empty
  in
  let s_preserved16 = IRUtil.VarSet.diff s_preserved_t.val16 s_preserved16_pv in
  let s_preserved16 = 
    if IRUtil.VarSet.is_empty s_preserved16
    then s_preserved16
    else verify_preserved_behaviors verify ssa_tbl IR.TypeReg_16 s_preserved16
  in
  let s_preserved16 = IRUtil.VarSet.union s_preserved16_pv s_preserved16 in

  let s_preserved8_pv = 
    IRUtil.VarSet.fold 
     (fun v16 set -> 
      match X86ToIRUtil.v8p_opt_of_v16_exn v16 with
      | Some(vh8,vl8) -> IRUtil.VarSet.add vl8 (IRUtil.VarSet.add vh8 set)
      | None -> set) 
      s_preserved16 
      IRUtil.VarSet.empty
  in
  let s_preserved8 = IRUtil.VarSet.diff s_preserved_t.val8 s_preserved8_pv in
  let s_preserved8 = 
    if IRUtil.VarSet.is_empty s_preserved8
    then s_preserved8
    else verify_preserved_behaviors verify ssa_tbl IR.TypeReg_8 s_preserved8
  in
  let s_preserved8 = IRUtil.VarSet.union s_preserved8_pv s_preserved8 in
  { val32 = s_preserved32; val16 = s_preserved16; val8 = s_preserved8; }

