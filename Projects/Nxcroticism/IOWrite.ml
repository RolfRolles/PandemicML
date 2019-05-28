open DataStructures
open IOPair

(* Virtually identical to last set of functions *)
let determine_write_reg32_behaviors size io =
  (* Was rout = ... io.post *)
  let rin = reverse_ht io.pre in
  List.fold_left 
   (fun set (a32,s_write,v32) ->
      VarInt32Set.fold
       (fun (v,d32) set ->
          match ht_find_opt rin (IRUtil.mk_fixed_const_of_i32 v32 size) with
          | None -> set
          | Some(l) -> List.fold_left (fun set el -> VarVarInt32Set.add (el,v,d32) set) set l)
        (* Was s_read *)
        s_write
        set)
    VarVarInt32Set.empty
    (* Was io.reads *)
    io.writes          

let determine_aggregate_write_reg32_behaviors size ioc =
  let f i = 
    let s_vvi = determine_write_reg32_behaviors size (select_member size i) in
  (*f_printf "Dumping write candidates:\n";
    dump_varvarint32set s_vvi;*)
    s_vvi    
  in
  let s_vvi = 
  match ioc.l_io with
  | [] -> VarVarInt32Set.empty 
  | x::xs -> List.fold_left (fun s i -> VarVarInt32Set.inter s (f i)) (f x) xs
  in
(*f_printf "Dumping final write candidates:\n";
  dump_varvarint32set s_vvi;*)
  s_vvi    

let determine_aggregate_write_reg32_behaviors = triplicate determine_aggregate_write_reg32_behaviors 

(* Ensure that vsrc.pre = mem.post[vmemreg.pre + disp32] *)
let verify_write_reg tbl vmemreg disp32 vsrc size = 
  let open VerifyUtil in
  IRUtil.mk_ne (mk_load_disp32_post tbl vmemreg disp32 size) (src_expr vsrc size)

(* No fancy behaviors for writes.  I.e. do not treat a 32-bit write as a 16-
   or 8-bit one. *)
let verify_write_behaviors p verify ssa_tbl size s_write_reg_t =
  VarVarInt32Set.filter 
   (fun (vsrc,vreg,d32) -> verify (IRUtil.mk_and p (verify_write_reg ssa_tbl vreg d32 vsrc size)))
   (select_member size s_write_reg_t)

let verify_write_behaviors p verify ssa_tbl s_write_reg_t =
  triplicate (verify_write_behaviors p verify ssa_tbl) s_write_reg_t

