(* Set aside ESP and MEM (in variable and expression form *)
let vesp,eesp = X86ToIRUtil.vEsp,X86ToIRUtil.eEsp
let vmem,emem = X86ToIRUtil.vMem,X86ToIRUtil.eMem

(* Shortcut for mk_evar, lookup var's SSA terminal variable, make a dword constant *)
let mk_e = IRUtil.mk_evar 
let post tbl v = let nv = try Hashtbl.find tbl v with Not_found -> v in mk_e nv
let irdw_of_i32 i32 = IRUtil.mk_dword (Int64.of_int32 i32)

(* Load from either the intial or final memory *)
let mk_load_disp32_common emem vreg disp32 s =
  IRUtil.mk_load emem (IRUtil.mk_add (mk_e vreg) (irdw_of_i32 disp32)) s

let mk_load_disp32_pre = mk_load_disp32_common emem
let mk_load_disp32_post tbl = mk_load_disp32_common (post tbl vmem)

let src_expr vsrc = function
| IR.TypeReg_1  -> failwith "src_expr: 1-bit register"
| IR.TypeReg_8  -> IRUtil.VarMap.find vsrc X86ToIRUtil.m_v8_to_e8
| IR.TypeReg_16 -> IRUtil.VarMap.find vsrc X86ToIRUtil.m_v16_to_e16
| IR.TypeReg_32 -> mk_e vsrc
| IR.TypeReg_64 -> failwith "src_expr: 64-bit register"

let dest_expr tbl vdst = function
| IR.TypeReg_1  -> failwith "dest_expr: 1-bit register"
| IR.TypeReg_8  -> IRLocalOpt.replace_var_with_var tbl (IRUtil.VarMap.find vdst X86ToIRUtil.m_v8_to_e8)
| IR.TypeReg_16 -> IRLocalOpt.replace_var_with_var tbl (IRUtil.VarMap.find vdst X86ToIRUtil.m_v16_to_e16)
| IR.TypeReg_32 -> post tbl vdst
| IR.TypeReg_64 -> failwith "dest_expr: 64-bit register"
