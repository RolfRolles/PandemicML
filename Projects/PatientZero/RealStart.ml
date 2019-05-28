let save_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[Memexpr(Mem32(Mb(DS,None,None,a32)));GeneralReg(Gd(Esp))]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 28l a32))]); }::
  { pref = []; instr = (Pushfd,[]); }::
  { pref = []; instr = (Pushad,[]); }::
  []
  
let restore_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 0x4l a32))]); }::
  { pref = []; instr = (Popad,[]); }::
  { pref = []; instr = (Popfd,[]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Memexpr(Mem32(Mb(DS,None,None,a32)))]); }::
  []
  
let mk_long_rel_jmp src tgt =
  let disp32 = Int32.sub tgt (Int32.add 5l src) in
  let b0,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b1,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b2,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b3        = Int32.logand disp32 0xffl in
  [0xe9l;b0;b1;b2;b3]

let assemble x = List.concat (List.map X86Encode.encode_instruction x)

let create_setup () =
  let o_init_t  = JITRegion.allocate 4096 in
  let o_blit_t  = JITRegion.allocate 4096 in
  let o_retn_t  = JITRegion.allocate 4096 in
  let o_stack_t = JITRegion.allocate 4096 in
  let o_mem_t   = JITRegion.allocate 4096 in
  
  let opt_get = match init_t with | Some((i32,_) as p) -> i32,p | None -> failwith ("Allocating "^s) in
  let c_init32  ,init_t  = opt_get o_init_t  in
  let c_blit32  ,blit_t  = opt_get o_blit_t  in
  let c_return32,retn_t  = opt_get o_retn_t  in
  let c_stack32 ,stack_t = opt_get o_stack_t in
  let c_mem32   ,mem_t   = opt_get o_mem_t   in  
    
  let l_init32 = 
    (assemble (save_macro c_mem32))@
    (assemble (restore_macro (Int32.add 0x28l c_mem32))) 
  in
  let l_retn32 = 
    (assemble (save_macro (Int32.add 0x50l c_mem32)))@
    (assemble (restore_macro c_mem32))@
    [0xc3l] 
  in
  let l_blit32 = 
    let rec aux list = function
    | 0 -> list@(mk_long_rel_jmp (Int32.add 0x100l c_blit32) c_return32)
    | n when n > 0 -> aux (0x90l::list) (n-1)
    | _ -> failwith "impossible jnklsdgkljfsa"
    in
    aux [] 256
  in
  if not (JITRegion.blit init_t (Array.of_list l_init32)) then failwith "couldn't blit preamble";
  if not (JITRegion.blit retn_t (Array.of_list l_retn32)) then failwith "couldn't blit postamble";
  if not (JITRegion.blit retn_t (Array.of_list l_blit32)) then failwith "couldn't blit blittable region";
  (init_t,blit_t,retn_t,stack_t,mem_t,JITRegion.blit blit_t,JITRegion.execute_complex init_t mem_t)
    
let get_outstate f_blit f_execute instr instate = 
  let a_instr_nop = Array.make 0x100 0x90l in
  let l_instr = assemble instr in
  let _ = List.fold_left (fun i i32 -> a_instr_nop.(i) <- i32; i+1) 0 l_instr in
  f_blit a_instr_nop;
  f_execute instate
  

  