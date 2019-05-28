open DataStructures
open IOPair

let make_io_out_varint32map io =
  Hashtbl.fold 
   (fun v c s -> match c with 
    | IR.Const(i64,_) -> VarInt32Set.add (v,Int64.to_int32 i64) s
    | _ -> failwith "make_io_out_varint32map: ill-formed ht_pre")
    io.post
    VarInt32Set.empty

let determine_aggregate_load_const32_behaviors size ioc =
  let f i = make_io_out_varint32map (select_member size i) in
  match ioc.l_io with
  | io::ios -> List.fold_left (fun s i -> VarInt32Set.inter s (f i)) (f io) ios
  | _ -> VarInt32Set.empty

let determine_aggregate_load_const32_behaviors = triplicate determine_aggregate_load_const32_behaviors 

(* Ensure that vdst.post = const32 *)
let verify_set_const tbl vdst const32 size = 
  let open VerifyUtil in
  IRUtil.mk_ne (dest_expr tbl vdst size) (IRUtil.mk_fixed_const_of_i32 const32 size)

let verify_set_const_behaviors verify ssa_tbl size s_set_const =
  VarInt32Set.filter 
   (fun (v,i32) -> verify (verify_set_const ssa_tbl v i32 size))
    s_set_const

let map_32bit_to_16bit var i32 = 
  [(X86ToIRUtil.v16_of_v32_exn var, Int32.logand i32 0xffffl)]

let map_16bit_to_8bit  var i32 = 
  let i8 i32 = Int32.logand 0xffl i32 in
  match X86ToIRUtil.v8p_opt_of_v16_exn var with
  | Some(vh8,vl8) -> [(vh8,i8 (Int32.shift_right_logical i32 8));(vl8,i8 i32)]
  | None -> []

let generate_smaller_set_const_facts s_set_const_verified f_derive_equalities_l =
  VarInt32Set.fold 
   (fun (var,i32) set -> 
      let l = f_derive_equalities_l var i32 in
      List.fold_left (fun set (v,i32) -> VarInt32Set.add (v,i32) set) set l)
    s_set_const_verified
    VarInt32Set.empty

let prune_redundant_set_const s_verified s_candidate =
  VarInt32Set.filter (fun p -> not (VarInt32Set.mem p s_verified)) s_candidate

let verify_set_const_behaviors verify ssa_tbl s_set_const_t =
  let s_set_const32    = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_32 s_set_const_t.val32 in

  let s_set_const16_pv = generate_smaller_set_const_facts s_set_const32 map_32bit_to_16bit in
  let s_set_const16    = prune_redundant_set_const s_set_const16_pv s_set_const_t.val16 in

  let s_set_const16    = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_16 s_set_const16 in
  let s_set_const16    = VarInt32Set.union s_set_const16 s_set_const16_pv in

  let s_set_const8_pv  = generate_smaller_set_const_facts s_set_const16 map_16bit_to_8bit in
  let s_set_const8     = prune_redundant_set_const s_set_const8_pv s_set_const_t.val8 in

  let s_set_const8  = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_8  s_set_const8  in
  let s_set_const8  = VarInt32Set.union s_set_const8 s_set_const8_pv in
  { val32 = s_set_const32; val16 = s_set_const16; val8 = s_set_const8; }

