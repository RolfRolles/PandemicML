open DataStructures
open IOPair
open FrameworkUtil

let nonzero_rand_i32 () = Int32.succ (Random.int32 (Int32.pred Int32.max_int))
let nonzero_rand_i32_4 () = Int32.logand (nonzero_rand_i32 ()) 0xffffffcl

(* Given:
   * ht_pre, a hash table containing the initial register context, and 
   * l_mem, a list of memory accesses of the form (a,v,s), 
   
   * For each memory access, produce a VarInt32Set that contains register
     displacements of the form [reg32+disp32].  These sets are generated
     implicitly, and are not returned in an individual form.
   * Produce a VarInt32Set for the entire list, which is the union of all
     of the individual sets.  Return this.
*)
let make_purported_register_displacements ht_pre l_mem =
  let set = ref VarInt32Set.empty in
  let newlist = List.map (fun (a,_,v) -> 
    let newset = Hashtbl.fold (fun v c set ->
      match c with
      | IR.Const(c,IR.TypeReg_32) -> VarInt32Set.add (v,Int32.sub a (Int64.to_int32 c)) set
      | _ -> failwith "make_purported_register_displacements: ill-formed ht_pre")
      ht_pre
      VarInt32Set.empty
    in
    set := VarInt32Set.union !set newset;
    (a,newset,v))
    l_mem
  in
  (newlist,!set)

let analyze_io_memaccesses io sio binter r_s_read r_s_write =
  let f = make_purported_register_displacements in
  let newread,s_read   = f io.val32.pre sio.reads in
  let newwrite,s_write = f io.val32.pre sio.writes in
 (if binter
  then (r_s_read  := VarInt32Set.inter !r_s_read  s_read;
        r_s_write := VarInt32Set.inter !r_s_write s_write)
  else
       (r_s_read := s_read; r_s_write := s_write));
 (newread,newwrite)
  
(* Given: 
   l_io, a list of io structures
   
   For each io:
   * Produce the union of the read candidates
   * Produce the union of the write candidates
   
   Insersect these sets across all ios to produce the common locations.
   Return the common reads, writes, and read/writes.
*)
let get_common_accessed_locations size l_io =
  let sel = select_member size in
  let rep = replace_member size in

  let read,write = ref VarInt32Set.empty, ref VarInt32Set.empty in
  let new_l_io = 
    match l_io with
    | (x,io)::ios -> 
      let sio = sel io in
      let newread,newwrite = analyze_io_memaccesses io sio false read write in
      let ios = List.map
       (fun (x,io) -> 
         let sio = sel io in
         let newread,newwrite = analyze_io_memaccesses io sio true read write in
         x,rep io { sio with reads = newread; writes = newwrite; })
        ios
      in 
      (x,(rep io { sio with reads = newread; writes = newwrite; }))::ios
    | _ -> []
  in
  new_l_io,!read,!write,VarInt32Set.inter !read !write

let get_common_accessed_locations size l_io = 
  let sel = select_member size in
  let rep = replace_member size in
  let new_l_io,read,write,readwrite = get_common_accessed_locations size l_io  in

  let map sset = List.map (fun (a,set,v) -> (a,VarInt32Set.inter set sset,v)) in
  let new_l_io = List.map 
    (fun (x,io) -> 
       let sio = sel io in 
       x,rep io { sio with reads = map read sio.reads; writes = map write sio.writes; }) 
     new_l_io
  in
  (new_l_io,read,write,readwrite)

let get_common_accessed_locations l_io =
  let l_io,r8, w8, c8  = get_common_accessed_locations IR.TypeReg_8  l_io in
  let l_io,r16,w16,c16 = get_common_accessed_locations IR.TypeReg_16 l_io in
  let l_io,r32,w32,c32 = get_common_accessed_locations IR.TypeReg_32 l_io in
  l_io,
  { val8 = r8; val16 = r16; val32 = r32; },
  { val8 = w8; val16 = w16; val32 = w32; },
  { val8 = c8; val16 = c16; val32 = c32; }

(* Given the (address,symbolic address,value) triples, union together the 
   symbolic address sets where the value matches the argument. *)
let ascertain_symbolic_memory_locations_from_value value io =
  List.fold_left 
   (fun acc (a,set,v) -> if v = value then VarInt32Set.union acc set else acc) 
    VarInt32Set.empty 
    io.val32.reads
    
(* [(v,io)] -> { Intersection of all locations that contained the respective v's },[io] *)
let ascertain_aggregate_symbolic_memory_locations l_io =
  match l_io with
  | (value,io)::ios ->
    let f value x = 
      match value with
      | Some(value) -> ascertain_symbolic_memory_locations_from_value value x
      | None -> VarInt32Set.empty
    in
    List.fold_left 
     (fun (list,set) (value,io) -> io::list,VarInt32Set.inter set (f value io))
     ([],(f value io))
      ios
  | [] -> [],VarInt32Set.empty

(* I should not have to do this, but apparently the constant folding function is not idempotent 
   That needs to be fixed, seriously.  But for the time being, let's just do this.
   DUPLICATED; ALSO IN IOBinop
    *)
let rec fold_constants_fixedpoint e =
  let n = IRLocalOpt.fold_expr_constants e in
  if e <> n
  then fold_constants_fixedpoint n
  else e

let obtain_smaller_ht_post ir_assign post = 
  let ht_post = Hashtbl.create 16 in
  List.iter (fun (x,y) -> 
    let re = IRLocalOpt.replace_var_with_expr post y in
    let pre = fold_constants_fixedpoint re in
    match pre with
    | IR.Const(_,_) as c -> Hashtbl.replace ht_post x c
    | _ -> failwith ("obtain_smaller_ht_post: did not obtain constant, got "^PpIR.ppExpr false pre^" from "^PpIR.ppExpr false re))
    ir_assign;
  ht_post

let get_ht_post16 =
  obtain_smaller_ht_post 
   (let open X86ToIRUtil in
   [vAx,eAx;vCx,eCx;vDx,eDx;vBx,eBx;vSp,eSp;vBp,eBp;vSi,eSi;vDi,eDi])

let get_ht_post8 =
  obtain_smaller_ht_post 
   (let open X86ToIRUtil in
   [vAl,eAl;vAh,eAh;vCl,eCl;vCh,eCh;vDl,eDl;vDh,eDh;vBl,eBl;vBh,eBh])

let segregate_address_list =
  List.fold_left (fun (l32,l16,l8) (a,v,s) ->
    match s with
    | IR.TypeReg_1  -> failwith "segregate_address_list: 1-bit mem access shouldn't happen"
    | IR.TypeReg_8  -> (l32,l16,(a,VarInt32Set.empty,v)::l8)
    | IR.TypeReg_16 -> (l32,(a,VarInt32Set.empty,v)::l16,l8)
    | IR.TypeReg_32 -> ((a,VarInt32Set.empty,v)::l32,l16,l8)
    | IR.TypeReg_64 -> failwith "segregate_address_list: 64-bit mem access unsupported")
   ([],[],[])

let make_io_struct ir e_jt = 
  (* Generate a dword-aligned ESP value *)
  let esp_val  = nonzero_rand_i32_4 () in
  let p_espval = X86ToIRUtil.vEsp,esp_val in
  
  (* Generate initial register values *)
  let l_varvals32,l_varvals16,l_varvals8 =
    List.fold_left (fun (r32,r16,r8) (v32,v16,pv8o) ->
      let c32 = nonzero_rand_i32 () in
      let c16 = Int32.logand c32 0xffffl in
      let r8 =
        match pv8o with
        | Some(v8h,v8l) -> (v8h,Int32.shift_right c16 8)::(v8l,Int32.logand 0xffl c16)::r8
        | None -> r8
     in 
     ((v32,c32)::r32,(v16,c16)::r16,r8))
     ([],[],[])
      X86ToIRUtil.l_v_general_registers_parentage_noesp
  in
  
  (* ht_post contains the initial values of the registers (no flags as of now) *)
  let ht_post = new_ht IR.TypeReg_32 (p_espval::l_varvals32) in
  
  (* ht_pre is a virgin copy of ht_post before emulation *)
  let ht_pre = Hashtbl.copy ht_post in

  (* Insert flag valuations into the post hash table, but not the pre one *)
  List.iter 
   (fun vf -> 
      Hashtbl.replace 
        ht_post 
        vf 
       (IRUtil.mk_bit (i64_of_bool (Random.bool ()))))
   (let open X86ToIRUtil in [vSF;vOF;vPF;vZF;vAF;vCF;vDF]);
  
  (* Make the memory context *)
  let memctx = Hashtbl.create 32 in
  
  (* Perform emulation, get the output values *)
  let reads,writes = IRRandomizedEvaluator.concrete_evaluate_jumpless_nonssa ir ht_post memctx in

  (* Determine the return location *)
  let c_jt = IRLocalOpt.replace_var_with_expr ht_post e_jt in
  let a_jt32_opt = match fold_constants_fixedpoint c_jt with
  | IR.Const(a32,IR.TypeReg_32) -> Some(Int64.to_int32 a32)
  | _ -> None

  in
  (* Clean up ht_post to remove temporary IR translator variables *)
  let _ = remove_irtrans_vars X86ToIRUtil.s_general_registers32 ht_post in
  
  (* Segregate the lists of reads and writes by size *)
  let r32,r16,r8 = segregate_address_list reads in
  let w32,w16,w8 = segregate_address_list writes in
  
  (* Make 16- and 8-bit versions of the pre and post hash tables *)
  let pre16,post16 = new_ht IR.TypeReg_16 l_varvals16, get_ht_post16 ht_post in
  let pre8,post8   = new_ht IR.TypeReg_8  l_varvals8,  get_ht_post8  ht_post in
  
  (* Create and return the IO structures *)
  let io pre post rev_post register_data reads writes = 
  { 
    pre = pre; post = post; rev_post = rev_post; 
    register_data = register_data; reads = reads; writes = writes; 
  }
  in
  let io32 = io ht_pre ht_post (reverse_ht ht_post) l_varvals32 r32 w32 in
  let io16 = io pre16  post16  (reverse_ht post16)  l_varvals16 r16 w16 in
  let io8  = io pre8   post8   (reverse_ht post8)   l_varvals8  r8  w8 in
  (a_jt32_opt,{ val32 = io32; val16 = io16; val8 = io8; })

(* Given:
   ir:    a list of non-ssa, jumpless IR
   n:     number of tests to execute
   e_jt:  the expression corresponding to the return location
   Return: an io_container structure with the IO pairs.
*)
let make_io_structs ir n e_jt =
  let rec aux i list =
    if i = n
    then list
    else aux (i+1) ((make_io_struct ir e_jt)::list)
  in
  let l_io = aux 0 [] in
  let l_io,read,write,readwrites = get_common_accessed_locations l_io in
  let l_io,s_retlocs = ascertain_aggregate_symbolic_memory_locations l_io in
  let retloc_option = match VarInt32Set.elements s_retlocs with 
  | (v,d32)::[] when v = X86ToIRUtil.vEsp -> (*f_printf "Retloc %ld\n" d32;*)Some(d32)
  | _ -> None
  in
  {
    l_io = l_io;
    retaddr_displacement_opt = retloc_option;
    common_reads = read;
    common_writes = write;
    common_readwrites = readwrites;
  }
  
