open X86

let mnem = Lea 
let mnem = Add

let random_lea_instructions = 
 [{ pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),None,None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,0),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,1),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,2),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,3),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,0),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,1),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,2),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,3),None)));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) }]

let assemble_to_file fname header instrlist footer =
  let oc = open_out_bin fname in
  let num_bytes_written = ref 0 in
  let ob list = List.iter (fun i32 -> incr num_bytes_written; output_byte oc (Int32.to_int i32)) list in
  ob header;
  ob (List.concat (List.map X86Encode.encode_instruction instrlist));
  ob footer;
  while !num_bytes_written < 0x200
  do
    ob [0l]
  done;
  close_out oc
  
let cv_jmp    = [0xebl;0x10l]
let cv_marker = [0x43l;0x56l;0x20l;0x20l]
let cv_meat start mutation_level = (if start then 12l else 13l)::[0l;0l;0l;mutation_level;0l;0l;0l;]

let cv_mutX_start lvl = List.concat ([cv_jmp;cv_marker;cv_meat true lvl;cv_marker])
let cv_end = List.concat ([cv_jmp;cv_marker;cv_meat false 0l;cv_marker])

let _ = assemble_to_file "blob.bin" (cv_mutX_start 0l) random_lea_instructions cv_end