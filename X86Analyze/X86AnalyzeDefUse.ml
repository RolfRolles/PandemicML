let rec get_def_uses x86instr =
  let open X86 in
  let extract_defs_uses () = 
    List.fold_left 
     (fun (defs,uses) -> function
      | IR.Assign(v,e)  -> (v::defs,(IRLocalOpt.extract_uses e)@uses)
      | IR.CJmp(c,t,nt) -> (defs,   (IRLocalOpt.extract_uses c)@(IRLocalOpt.extract_uses t)@(IRLocalOpt.extract_uses nt)@uses)
      | IR.Jmp(e)
      | IR.Halt(e)
      | IR.Assert(e)    -> (defs,   (IRLocalOpt.extract_uses e)@uses)
      | IR.Label(_)
      | IR.Special(_)
      | IR.Comment(_)   -> (defs, uses))
     ([],[])
     (IRLocalOpt.local_opt (X86ToIR.translate_instr 0l x86instr))
  in
  
  let aux () =
    match x86instr with
    | { pref = pref; instr = ((Movsb|Movsd|Movsw),_) } ->
      let esi_edi_mem  = [X86ToIRUtil.vEsi;X86ToIRUtil.vEdi;X86ToIRUtil.vMem] in
      let ecx_esdi_mem = (X86ToIRUtil.vEcx)::esi_edi_mem in
      if (List.mem (Rep) pref)
      then (ecx_esdi_mem,ecx_esdi_mem)
      else (esi_edi_mem,esi_edi_mem)
    | { pref = _; instr = (Mov,[GeneralReg(Gw(r16));_]) } ->
      let r32 = X86ToIRUtil.vr32_of_reg16 r16 in
      ([r32],List.filter (fun x -> x <> r32) (snd(extract_defs_uses ())))
    | { pref = _; instr = (Mov,[GeneralReg(Gb(r8 ));_]) } ->
      let r32 = X86ToIRUtil.vr32_of_reg8 r8 in
      ([r32],List.filter (fun x -> x <> r32) (snd(extract_defs_uses ())))
    (* Stupid, actually code IR translations for Rcr/Rcl *)
    | { pref = _; instr = ((Rcr|Rcl),ol) } ->
      let d,u = get_def_uses { pref = []; instr = (Ror,ol) } in
      (IRUtil.VarSet.elements d,IRUtil.VarSet.elements u)
    | _ ->
      let defs,uses = extract_defs_uses () in
      let filter = List.filter (fun x -> IRUtil.varno x < X86ToIRUtil.num_reserved_vars) in
      let reduce x = IRUtil.VarSet.elements (IRUtil.var_setify (filter x)) in
      (reduce defs,reduce uses)
  in 
  let defs,uses = aux () in
  (IRUtil.var_setify defs, IRUtil.var_setify uses)

module VarSet = IRUtil.VarSet

(* Terminology on "vars_out" might seem a bit confusing; this is the "out" set
   in the liveness data flow analysis, which is indeed the *input* to this 
   function.  The *output* is "vars_in", i.e., the vars that are live on input
   to the basic block. *)
let block_dse_transfer ir vars_out = 
  List.fold_right
   (fun instr varset ->
      let defset,useset = get_def_uses instr in
      VarSet.union (VarSet.diff varset defset) useset)
    ir
    vars_out

let remove_dead_instructions ir vars_out =
  let _,out_ir,changed = 
    List.fold_right
     (fun instr (varset,out_ir,changed) ->
      (*IDA.msg "%s\n" (X86Disasm.string_of_x86instr instr);*)
        let defset,useset = get_def_uses instr in
        let live_defs = VarSet.inter defset varset in
      (*VarSet.iter (fun v -> IDA.msg "  %s\n" (PpIR.ppVar v)) live_defs;*)
        if (not (VarSet.is_empty defset)) && (VarSet.is_empty live_defs)
        then (IDA.msg "%s\n" (X86Disasm.string_of_x86instr instr); (varset,out_ir,true))
        else (VarSet.union (VarSet.diff varset defset) useset,instr::out_ir,changed))
      ir
      (vars_out,[],false)
  in (out_ir,changed)
