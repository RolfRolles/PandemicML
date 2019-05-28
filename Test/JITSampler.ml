let save_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[Memexpr(Md(Mem32(DS,None,None,Some(a32))));GeneralReg(Gd(Esp))]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 0x28l a32))]); }::
  { pref = []; instr = (Pushfd,[]); }::
  { pref = []; instr = (Pushad,[]); }::
  []
  
let restore_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 0x4l a32))]); }::
  { pref = []; instr = (Popad,[]); }::
  { pref = []; instr = (Popfd,[]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Memexpr(Md(Mem32(DS,None,None,Some(a32))))]); }::
  []
  
let mk_long_rel_jmp src tgt =
  let disp32 = Int32.sub tgt (Int32.add 5l src) in
  let b0,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b1,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b2,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b3        = Int32.logand disp32 0xffl in
  [0xe9l;b0;b1;b2;b3]

let assemble = X86Encode.encode_instruction
let l_assemble x = List.concat (List.map assemble x)

let create_setup () =
  let o_init_t  = JITRegion.allocate 4096 in
  let o_blit_t  = JITRegion.allocate 4096 in
  let o_retn_t  = JITRegion.allocate 4096 in
  let o_stack_t = JITRegion.allocate 4096 in
  let o_mem_t   = JITRegion.allocate 4096 in

  let opt_get s = function | Some(i32,i) -> (i32,(i32,i)) | None -> failwith ("Allocating "^s) in
  let c_init32  ,init_t  = opt_get "init_t"  o_init_t  in
  let c_blit32  ,blit_t  = opt_get "blit_t"  o_blit_t  in
  let c_return32,retn_t  = opt_get "retn_t"  o_retn_t  in
  let c_stack32 ,stack_t = opt_get "stack_t" o_stack_t in
  let c_mem32   ,mem_t   = opt_get "mem_t"   o_mem_t   in  
    
  let l_init32 = 
    (l_assemble (save_macro c_mem32))@
    (l_assemble (restore_macro (Int32.add 0x28l c_mem32)))
  in
  let l_init32 = 
    l_init32@
    (mk_long_rel_jmp (Int32.add c_init32 (Int32.of_int (List.length l_init32))) c_blit32)
  in
  let l_retn32 = 
    (l_assemble (save_macro (Int32.add 0x50l c_mem32)))@
    (l_assemble (restore_macro c_mem32))@
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
  if not (JITRegion.blit blit_t (Array.of_list l_blit32)) then failwith "couldn't blit blittable region";
  (init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,JITRegion.blit blit_t,JITRegion.execute_complex init_t mem_t)
    
let blit_instr f_blit instr =
  let a_instr_nop = Array.make 0x100 0x90l in
  let l_instr = assemble instr in
  let _ = List.fold_left (fun i i32 -> a_instr_nop.(i) <- i32; i+1) 0 l_instr in
  f_blit a_instr_nop

let get_outstate f_blit f_execute instr instate = 
  blit_instr f_blit instr;
  f_execute instate
  
let mk_random_state esp =
  let rb () = Random.int 2 in
  let ri32 () = Random.int32 Int32.max_int in
  
  (* Make a random assignment to the flags, LHS and RHS of the binary operation *)
  let instate =
    let cf,zf,sf,af,pf,ofl,df = rb(),rb(),rb(),rb(),rb(),rb(),rb() in
    let open JITRegion in
    {
      eax = ri32();
      ecx = ri32();
      edx = ri32();
      ebx = ri32();
      esp = esp;
      ebp = ri32();
      esi = ri32();
      edi = ri32();
      eflags = Int32.of_int (X86Misc.fl2eflags sf zf af pf cf ofl df);
    }
  in
  instate

let print_x86ctx x = 
  let open JITRegion in
  Printf.printf "eax = %08lx ebx = %08lx ecx = %08lx edx = %08lx\n"
   (x.eax) (x.ebx) (x.ecx) (x.edx);
  Printf.printf "esp = %08lx ebp = %08lx esi = %08lx edi = %08lx\n"
   (x.esp) (x.ebp) (x.esi) (x.edi);
  Printf.printf "%s\n%!" (X86Misc.flags_str x.eflags)