open X86ToIRUtil

type x86_complete_state = 
{
  eax: int32;
  ecx: int32;
  edx: int32;
  ebx: int32;
  esp: int32;
  ebp: int32;
  esi: int32;
  edi: int32;
  
  eflags: int;
  
  memory: int32 array;
}

let fl2eflags sf zf af pf cf ofl =
  let sf = sf lsl 7 in
  let zf = zf lsl 6 in
  let af = af lsl 4 in
  let pf = pf lsl 2 in
  let cf = cf in
  let ofl = ofl lsl 11 in
  sf lor zf lor af lor pf lor cf lor ofl
  
let eflags2fl flags = 
  let cf  = flags land 1 in
  let ofl = (flags lsr 11) land 1 in
  let sf  = (flags lsr  7) land 1 in
  let zf  = (flags lsr  6) land 1 in
  let af  = (flags lsr  4) land 1 in
  let pf  = (flags lsr  2) land 1 in
  (sf,zf,af,pf,cf,ofl)

let do_flags_assign sf zf af pf cf ofl =
  let open X86ToIRUtil in
  let open IRUtil in
  let mk_flag b = mk_bit (Int64.of_int b) in
 [mk_assign vCF (mk_flag cf);
  mk_assign vPF (mk_flag pf);
  mk_assign vAF (mk_flag af);
  mk_assign vZF (mk_flag zf);
  mk_assign vSF (mk_flag sf);
  mk_assign vOF (mk_flag ofl)]
  
let do_vars3_assign eax ecx edx =
  let open X86ToIRUtil in
  let open IRUtil in
  let mk_dw d = mk_dword (Int64.of_int32 d) in
 [mk_assign vEax (mk_dw eax);
  mk_assign vEcx (mk_dw ecx);
  mk_assign vEdx (mk_dw edx)]

let do_vars_assign state =
  let open X86ToIRUtil in
  let open IRUtil in
  let mk_dw d = mk_dword (Int64.of_int32 d) in
 [mk_assign vEax (mk_dw state.eax);
  mk_assign vEcx (mk_dw state.ecx);
  mk_assign vEdx (mk_dw state.edx);
  mk_assign vEbx (mk_dw state.ebx);
  mk_assign vEsp (mk_dw state.esp);
  mk_assign vEbp (mk_dw state.ebp);
  mk_assign vEsi (mk_dw state.esi);
  mk_assign vEdi (mk_dw state.edi)]
  
let do_complete_vars_assign state = 
  let vars = do_vars_assign state in
  let sf,zf,af,pf,cf,ofl = eflags2fl state.eflags in
  let flags = do_flags_assign sf zf af pf cf ofl in
  vars@flags

let mk_initial_state3 sf zf af pf cf ofl eax ecx edx =
  let state = Hashtbl.create 50 in
  let open IR in
  Hashtbl.add state vCF  (Const(Int64.of_int cf,  TypeReg_1));
  Hashtbl.add state vPF  (Const(Int64.of_int pf,  TypeReg_1));
  Hashtbl.add state vAF  (Const(Int64.of_int af,  TypeReg_1));
  Hashtbl.add state vZF  (Const(Int64.of_int zf,  TypeReg_1));
  Hashtbl.add state vSF  (Const(Int64.of_int sf,  TypeReg_1));
  Hashtbl.add state vOF  (Const(Int64.of_int ofl, TypeReg_1));
  Hashtbl.add state vEax (Const(Int64.of_int32 eax, TypeReg_32));
  Hashtbl.add state vEcx (Const(Int64.of_int32 ecx, TypeReg_32));
  Hashtbl.add state vEdx (Const(Int64.of_int32 edx, TypeReg_32));
  state

let mk_initial_state state_in =
  let state = Hashtbl.create 50 in
  let open IR in
  let sf,zf,af,pf,cf,ofl = eflags2fl state_in.eflags in
  Hashtbl.add state vCF  (Const(Int64.of_int cf,  TypeReg_1));
  Hashtbl.add state vPF  (Const(Int64.of_int pf,  TypeReg_1));
  Hashtbl.add state vAF  (Const(Int64.of_int af,  TypeReg_1));
  Hashtbl.add state vZF  (Const(Int64.of_int zf,  TypeReg_1));
  Hashtbl.add state vSF  (Const(Int64.of_int sf,  TypeReg_1));
  Hashtbl.add state vOF  (Const(Int64.of_int ofl, TypeReg_1));
  Hashtbl.add state vEax (Const(Int64.of_int32 state_in.eax, TypeReg_32));
  Hashtbl.add state vEcx (Const(Int64.of_int32 state_in.ecx, TypeReg_32));
  Hashtbl.add state vEdx (Const(Int64.of_int32 state_in.edx, TypeReg_32));
  Hashtbl.add state vEbx (Const(Int64.of_int32 state_in.ebx, TypeReg_32));
  Hashtbl.add state vEsp (Const(Int64.of_int32 state_in.esp, TypeReg_32));
  Hashtbl.add state vEbp (Const(Int64.of_int32 state_in.ebp, TypeReg_32));
  Hashtbl.add state vEsi (Const(Int64.of_int32 state_in.esi, TypeReg_32));
  Hashtbl.add state vEdi (Const(Int64.of_int32 state_in.edi, TypeReg_32));
  state
