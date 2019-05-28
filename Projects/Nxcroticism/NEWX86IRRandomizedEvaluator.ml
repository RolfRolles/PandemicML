let initialize_registers rng32 = 
  let ht = Hashtbl.create 100 in let add v v32 = Hashtbl.replace ht v v32 in

  (* Generate initial register values *)
  let _ =
    List.iter (fun (v32,v16,pv8o) ->
      let c32 = rng32 () in
      let _ = add v32 (IRUtil.mk_dword_of_int32 c32) in
      let _ = add v16 (IRUtil.mk_word_of_int32  c32) in
      let b = IRUtil.mk_byte_of_int32 in
      match pv8o with
      | Some(v8h,v8l) -> 
         let _ = add v8h (b (Int32.shift_right c32 8)) in
         add v8l (b c32)
      | None -> ())
      X86ToIRUtil.l_v_general_registers_parentage
  in

  (* Generate values for registers of the "non-general" variety *)
  let mk_rnd_const s = IRUtil.mk_fixed_const_of_i32 (rng32 ()) s in
  List.iter (fun v -> add v (mk_rnd_const IR.TypeReg_16)) X86ToIRUtil.l_v_segs;
  List.iter (fun v -> add v (mk_rnd_const IR.TypeReg_1 )) X86ToIRUtil.l_v_flags;   
  List.iter (fun v -> add v (mk_rnd_const IR.TypeReg_32)) X86ToIRUtil.l_v_control;
  List.iter (fun v -> add v (mk_rnd_const IR.TypeReg_32)) X86ToIRUtil.l_v_debug;
  ht

