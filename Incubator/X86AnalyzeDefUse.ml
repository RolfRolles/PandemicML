let get_def_uses x86instr =
  let open X86 in
  let aux (({pref = _; instr = (m,_)}) =
    match m with
    | Movsb
    | Movsd
    | Movsw ->
      let esi_edi  = [X86ToIRUtil.vEsi;X86ToIRUtil.vEdi] in
      let ecx_esdi = (X86ToIRUtil.vEcx)::esi_edi in
      if (List.mem (Rep) pref)
      then (ecx_esdi,ecx_esdi)
      else (esi_edi,esi_edi)
    | _ ->
      let defs,uses = 
        List.fold_left 
         (fun (defs,uses) -> function
          | Assign(v,e)  -> (v::defs,(IRLocalOpt.extract_uses e)@uses)
          | CJmp(c,t,nt) -> (defs,   (IRLocalOpt.extract_uses c)@(IRLocalOpt.extract_uses t)@(IRLocalOpt.extract_uses nt)@uses)
          | Jmp(e)
          | Halt(e)
          | Assert(e)    -> (defs,   (IRLocalOpt.extract_uses e)@uses)
          | Label(_)
          | Special(_)
          | Comment(_)   -> (defs, uses))
         ([],[])
         (X86ToIR.translate_instr 0l x86instr)
      in
      let filter = List.filter (fun x -> IRUtil.varno x < X86ToIRUtil.num_reserved_vars) in
      let reduce x = IRUtil.VarSet.elements (IRUtil.var_setify (filter x)) in
      (reduce defs,reduce uses)
  in aux x86instr

