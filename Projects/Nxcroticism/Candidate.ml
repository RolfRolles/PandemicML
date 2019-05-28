open FrameworkUtil
open DataStructures
open IOPair

type candidate_results =
{
  c_preserved_regs: IRUtil.VarSet.t triplicate;
  c_copied_regs: (IRUtil.VarSet.t IRUtil.VarMap.t) triplicate;
  c_set_const: VarInt32Set.t triplicate;
  c_mem_read_const: VarVarInt32Set.t triplicate;
  c_mem_write_reg: VarVarInt32Set.t triplicate;
  c_write_binops:   VarBinopVarInt32Set.t triplicate;
  c_read_binops:    VarBinopVarInt32Set.t triplicate;  
  c_reg_binops:     VarVarBinopVarSet.t triplicate;
}

let generate_sequence_behaviors_candidates ioc = 
  let c_write_binops,c_read_binops,c_reg_binops = IOBinop.determine_aggregate_binop_behaviors ioc in
  {
    c_preserved_regs = IOPreserve.determine_aggregate_preserve_behaviors ioc;
    c_copied_regs    = IOCopy.determine_aggregate_copy_behaviors ioc;
    c_set_const      = IOConstant.determine_aggregate_load_const32_behaviors ioc;
    c_mem_read_const = IORead.determine_aggregate_read_reg32_behaviors ioc;
    c_mem_write_reg  = IOWrite.determine_aggregate_write_reg32_behaviors ioc;
    c_write_binops   = c_write_binops;
    c_read_binops    = c_read_binops;  
    c_reg_binops     = c_reg_binops;
  }  

let dump_reg_copies t_s_cr =
  let dump_varvarset =
    VarVarSet.iter 
     (fun (vsrc,vdst) -> 
        f_printf "%s.final = %s.initial\n" 
         (PpIR.ppVar vdst) 
         (PpIR.ppVar vsrc)) 
  in
  if not
   (VarVarSet.is_empty t_s_cr.val8  && 
    VarVarSet.is_empty t_s_cr.val16 && 
    VarVarSet.is_empty t_s_cr.val32)
  then 
   (f_printf "Register-to-register copy behaviors:\n";
    dump_varvarset t_s_cr.val32;
    dump_varvarset t_s_cr.val16;
    dump_varvarset t_s_cr.val8)

let dump_set_const t_s_cr =
  let dump_varint32set =
    VarInt32Set.iter 
     (fun (vdst,disp32) -> f_printf "%s.final = 0x%lx\n" (PpIR.ppVar vdst) disp32) 
  in
  if not
   (VarInt32Set.is_empty t_s_cr.val8  &&
    VarInt32Set.is_empty t_s_cr.val16 && 
    VarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Set register to const32 behaviors:\n";
    dump_varint32set t_s_cr.val32;
    dump_varint32set t_s_cr.val16;
    dump_varint32set t_s_cr.val8)

let dump_load_const t_s_cr =
  let dump_varvarint32set =
    VarVarInt32Set.iter 
     (fun (vdst,vreg,disp32) -> 
        f_printf "%s.final = mem[%s.initial+0x%lx]\n" 
         (PpIR.ppVar vdst) (PpIR.ppVar vreg) disp32) 
  in
  if not
   (VarVarInt32Set.is_empty t_s_cr.val8  &&
    VarVarInt32Set.is_empty t_s_cr.val16 && 
    VarVarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Load const from [reg+disp32] behaviors:\n";
    dump_varvarint32set t_s_cr.val32;
    dump_varvarint32set t_s_cr.val16;
    dump_varvarint32set t_s_cr.val8)

let dump_write_const t_s_cr =
  let dump_varvarint32set =
    VarVarInt32Set.iter 
     (fun (vsrc,vreg,disp32) -> 
        f_printf "mem[%s.initial+0x%lx] = %s.initial\n" 
         (PpIR.ppVar vreg) disp32 (PpIR.ppVar vsrc)) 
  in
  if not
   (VarVarInt32Set.is_empty t_s_cr.val8  &&
    VarVarInt32Set.is_empty t_s_cr.val16 && 
    VarVarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Write reg32 to [reg+disp32] behaviors:\n";
    dump_varvarint32set t_s_cr.val32;
    dump_varvarint32set t_s_cr.val16;
    dump_varvarint32set t_s_cr.val8)

let dump_register_binop_behaviors s_reg_t =
  let dump_vvbvset =
    VarVarBinopVarSet.iter (fun (v,vl,b,vr) -> 
      f_printf "%s = %s %s %s\n" 
       (PpIR.ppVar v)
       (PpIR.ppVar vl)
       (PpIR.ppBinop b)
       (PpIR.ppVar vr))
  in
  if not
   (VarVarBinopVarSet.is_empty s_reg_t.val8  &&
    VarVarBinopVarSet.is_empty s_reg_t.val16 && 
    VarVarBinopVarSet.is_empty s_reg_t.val32)
  then 
   (f_printf "Register binop behaviors:\n";
    dump_vvbvset s_reg_t.val32;
    dump_vvbvset s_reg_t.val16;
    dump_vvbvset s_reg_t.val8)
  
let dump_read_binop_behaviors s_read_t =
  let dump_vbvd32set =
    VarBinopVarInt32Set.iter (fun (v,b,vr,d32) -> 
      f_printf "%s %s= [%s.pre+0x%lx]\n" 
       (PpIR.ppVar v)
       (PpIR.ppBinop b)
       (PpIR.ppVar vr)
        d32)
  in
  if not
   (VarBinopVarInt32Set.is_empty s_read_t.val8  &&
    VarBinopVarInt32Set.is_empty s_read_t.val16 && 
    VarBinopVarInt32Set.is_empty s_read_t.val32)
  then 
   (f_printf "Memory read binop behaviors:\n";
    dump_vbvd32set s_read_t.val32;
    dump_vbvd32set s_read_t.val16;
    dump_vbvd32set s_read_t.val8)

let dump_write_binop_behaviors s_write_t =
  let dump_vbvd32set =
    VarBinopVarInt32Set.iter (fun (v,b,vr,d32) -> 
      f_printf "[%s.pre+0x%lx] %s= %s\n" 
       (PpIR.ppVar vr)
        d32
       (PpIR.ppBinop b)
       (PpIR.ppVar v))
  in
  if not
   (VarBinopVarInt32Set.is_empty s_write_t.val8  &&
    VarBinopVarInt32Set.is_empty s_write_t.val16 && 
    VarBinopVarInt32Set.is_empty s_write_t.val32)
  then 
   (f_printf "Memory write binop behaviors:\n";
    dump_vbvd32set s_write_t.val32;
    dump_vbvd32set s_write_t.val16;
    dump_vbvd32set s_write_t.val8)

let dump_read_esp t_s_cr =
  let dump = 
    VarInt32Set.iter
     (fun (v,disp32) ->
        f_printf "%s = mem[ESP.initial+0x%lx]\n" 
         (PpIR.ppVar v) disp32)
  in
  if not
   (VarInt32Set.is_empty t_s_cr.val8  &&
    VarInt32Set.is_empty t_s_cr.val16 && 
    VarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Read reg from [ESP+disp32] behaviors:\n";
    dump t_s_cr.val32;
    dump t_s_cr.val16;
    dump t_s_cr.val8)

let dump_write_esp t_s_cr =
  let dump = 
    VarInt32Set.iter
     (fun (v,disp32) ->
        f_printf "mem[ESP.initial+0x%lx] = %s\n" 
          disp32 (PpIR.ppVar v))
  in
  if not
   (VarInt32Set.is_empty t_s_cr.val8  &&
    VarInt32Set.is_empty t_s_cr.val16 && 
    VarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Write reg to [ESP+disp32] behaviors:\n";
    dump t_s_cr.val32;
    dump t_s_cr.val16;
    dump t_s_cr.val8)
        
let dump_read_esp_binops t_s_cr =
  let dump = 
    VarBinopInt32Set.iter
     (fun (v,b,disp32) ->
        f_printf "%s %s= mem[ESP.initial+0x%lx]\n" 
         (PpIR.ppVar v) (PpIR.ppBinop b) disp32)
  in
  if not
   (VarBinopInt32Set.is_empty t_s_cr.val8  &&
    VarBinopInt32Set.is_empty t_s_cr.val16 && 
    VarBinopInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Read binop from ESP-source behaviors:\n";
    dump t_s_cr.val32;
    dump t_s_cr.val16;
    dump t_s_cr.val8)

let dump_write_esp_binops t_s_cr =
  let dump =
    VarBinopInt32Set.iter
     (fun (v,b,disp32) ->
        f_printf "mem[ESP.initial+0x%lx] %s= %s\n" 
          disp32 (PpIR.ppBinop b) (PpIR.ppVar v))
  in
  if not
   (VarBinopInt32Set.is_empty t_s_cr.val8  &&
    VarBinopInt32Set.is_empty t_s_cr.val16 && 
    VarBinopInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Write binop to ESP-source behaviors:\n";
    dump t_s_cr.val32;
    dump t_s_cr.val16;
    dump t_s_cr.val8)

let print_candidate_results cr = 
  let _ = f_printf "Preserved registers: {" in
  print_triplicate dump_varset cr.c_preserved_regs;
  f_printf "}\n";
  
  print_triplicate dump_vsmap cr.c_copied_regs;
  dump_set_const                cr.c_set_const;
  dump_load_const               cr.c_mem_read_const;
  dump_write_const              cr.c_mem_write_reg;
  dump_register_binop_behaviors cr.c_reg_binops;
  dump_read_binop_behaviors     cr.c_read_binops;
  dump_write_binop_behaviors    cr.c_write_binops;

  f_printf "\n%!"
