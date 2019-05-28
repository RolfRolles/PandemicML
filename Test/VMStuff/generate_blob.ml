open X86

let mnem = Lea 
let mnem = Add

let random_dword_lhs_mem_instructions = 
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
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };
  { pref = []; instr = (mnem,[Memexpr(Md(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))));GeneralReg(Gd(Eax))]) };  
  ]

let random_word_lhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),None,None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,0),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,1),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,2),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,3),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,0),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,1),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,2),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,3),None)));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };
  { pref = []; instr = (mnem,[Memexpr(Mw(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))));GeneralReg(Gw(Ax))]) };  
  ]

let random_lo_byte_lhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),None,None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,0),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,1),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,2),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,3),None)));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Al))]) };  
  ]

let random_hi_byte_lhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),None,None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,0),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,1),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,2),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,3),None)));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };
  { pref = []; instr = (mnem,[Memexpr(Mb(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))));GeneralReg(Gb(Ah))]) };  
  ]

let random_dword_rhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),None,None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))))]) };  
  ]

let random_word_rhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),None,None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gw(Ax));Memexpr(Mw(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))))]) };  
  ]

let random_lo_byte_rhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),None,None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Al));Memexpr(Mb(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))))]) };  
  ]

let random_hi_byte_rhs_mem_instructions = 
 [{ pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),None,None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,0),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,1),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,2),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,3),None)))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),None,Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,0),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,1),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,2),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,Some(Edx,3),Some(Random.int32 Int32.max_int))))]) };
  { pref = []; instr = (mnem,[GeneralReg(Gb(Ah));Memexpr(Mb(Mem32(DS,None,None,Some(Random.int32 Int32.max_int))))]) };  
  ]

let dword_reg_instr = [{ pref = []; instr = (mnem,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]) }]
let dword_reg_imm_instr = [{ pref = []; instr = (mnem,[GeneralReg(Gd(Eax));Immediate(Id(Random.int32 Int32.max_int))]) }]

let assemble_to_file fname header instrlist footer separator =
  let oc = open_out_bin fname in
  let num_bytes_written = ref 0 in
  let ob list = List.iter (fun i32 -> incr num_bytes_written; output_byte oc (Int32.to_int i32)) list in
  ob header;
  let rec aux = function
  | [] -> ()
  | x::xs -> 
   (match separator with 
    | Some(s) -> ob s
    | None -> ());
    Printf.printf "%s\n" (X86Disasm.string_of_x86instr x);
    ob (X86Encode.encode_instruction x);
    aux xs
  in aux instrlist;
 (match separator with 
  | Some(s) -> ob s
  | None -> ());
  ob footer;
  while !num_bytes_written < 0x1200
  do
    ob [0l]
  done;
  close_out oc
  
  
let cv_jmp    = [0xebl;0x10l]
let cv_marker = [0x43l;0x56l;0x20l;0x20l]
let cv_meat start mutation_level = (if start then 12l else 13l)::[0l;0l;0l;mutation_level;0l;0l;0l;]
let cv_mutX_start lvl = List.concat ([cv_jmp;cv_marker;cv_meat true lvl;cv_marker])
let cv_end = List.concat ([cv_jmp;cv_marker;cv_meat false 0l;cv_marker])

let wl_jmp    = [0xebl;0x10l]
let wl_marker = [0x57l;0x4Cl;0x20l;0x20l]
let wl_meat start mutation_level = (if start then 12l else 13l)::[0l;0l;0l;0l;mutation_level;0l;0l;]
let wl_mutX_start lvl = List.concat ([wl_jmp;wl_marker;wl_meat true lvl;wl_marker])
let wl_end = List.concat ([wl_jmp;wl_marker;wl_meat false 0l;wl_marker])

(* Chaplaja VM *)
let chvm_start = [0xEBl; 0x0Fl; 0x43l; 0x68l; 0x61l; 0x70l; 0x6Cl; 0x6Al; 0x61l; 0x56l; 0x4Dl; 0x2Dl; 0x62l; 0x65l; 0x67l; 0x69l; 0x6El;]
let chvm_end   = [0xEBl; 0x0Dl; 0x43l; 0x68l; 0x61l; 0x70l; 0x6Cl; 0x6Al; 0x61l; 0x56l; 0x4Dl; 0x2Dl; 0x65l; 0x6El; 0x64l; ]

let separator  = X86Encode.encode_instruction { pref = []; instr = (Push,[Immediate(Id(0x0defacedl))]) }
let terminator = { pref = []; instr = (Push,[Immediate(Id(0xdefec8edl))]) }

let instructions = 
({pref = []; instr = (Xor,[GeneralReg(Gd(Eax));GeneralReg(Gd(Eax))])}::
  random_word_lhs_mem_instructions@
  random_lo_byte_lhs_mem_instructions@
  random_hi_byte_lhs_mem_instructions@	
  random_dword_rhs_mem_instructions@
  random_word_rhs_mem_instructions@
  random_lo_byte_rhs_mem_instructions@
  random_hi_byte_rhs_mem_instructions@
  dword_reg_instr@
  dword_reg_imm_instr@
  [terminator])


let _ = 
  assemble_to_file 
    "blob.bin" 
   (wl_mutX_start 0l) 
    instructions
    wl_end 
   (Some(separator))

let _ = 
  assemble_to_file 
    "blob.orig.bin" 
    []
    instructions
    []
   (Some(separator))


(*
let _ = 
  assemble_to_file 
    "blob.bin" 
    chvm_start
    instructions
    chvm_end 
   (None)

let _ = 
  assemble_to_file 
    "blob.orig.bin" 
    []
    instructions
    []
   (None)
*)