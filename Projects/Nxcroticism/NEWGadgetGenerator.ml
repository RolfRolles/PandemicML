open NEWGadget
open NEWIOStruct

let predicate_of_gadget io g = 
  let sizeof = IRUtil.size_of_var      in let n = IRUtil.mk_ne in 
  let e = IRUtil.mk_evar               in let mem h m s = Hashtbl.find h (m,s) in 
  let mv h m v = mem h m (sizeof v)    in let mrv m v = e (mv io.h_reads m v) in 
  let mwv m v = e (mv io.h_writes m v) in let mws m s = e (mem io.h_writes m s) in

  let open IR in match g with 
  | BinopReg(vres,vlhs,b,vrhs) -> n (e vres)  (Binop(e vlhs,b,e vrhs))
  | Constant(v,i)              -> n (e v)     (IRUtil.mk_fixed_const_of_i32 i (sizeof v))
  | Copy(vdst,vsrc)            -> n (e vdst)  (e vsrc)
  | Read(vdst,m)               -> n (e vdst)  (mrv m vdst)
  | BinopRead(vres,b,m)        -> n (e vres)  (Binop(e vres,b,mrv m vres))
  | Write(m,vdst)              -> n (e vdst)  (mwv m vdst)
  | WriteConst(m,c,s)          -> n (mws m s) (IRUtil.mk_fixed_const_of_i32 c s)
  | BinopWrite(m,b,v)          -> n (mwv m v) (Binop(mrv m v,b,e v))

let empirical_filter io = Generator.mk_filtered_generator (fun g -> all_ios_true io (predicate_of_gadget io g))

let l_binop   = let open IR in [Add;Mul;And;Or;Xor]
let l_shiftop = let open IR in [Sar;Shl;Shr]

let mk_binop (d,l,b,r) = BinopReg(d,l,b,r)

open GeneratorDSL

let ht_find_opt  ht key = let res = try Some(Hashtbl.find ht key) with Not_found -> None in res
let ht_find_bool ht key = match ht_find_opt ht key with | Some(_) -> true | None -> false

let binopreg_comm_generator c_ht ldst lsrc =
  let ht = Hashtbl.create 10 in
  let reject_comm (d,l,b,r) = 
    (ht_find_bool c_ht d) ||
    (l = r) ||
    (try (Hashtbl.find ht (l,b,r); true) with Not_found -> (Hashtbl.replace ht (r,b,l) (); false))
  in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,l_binop,lsrc,Some(reject_comm),mk_binop))

let binopreg_nc_generator c_ht ldst lsrc =  
  let open X86ToIRUtil in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,l_shiftop,l_v_general_registers8,Some(fun (d,l,b,r) -> (ht_find_bool c_ht d) || l = r),mk_binop))

let copy_generator ldst lsrc =
  let reject_copy (vdst,vsrc) = 
    try (IRUtil.VarMap.find vdst X86ToIRUtil.m_vafter_to_vbefore = vsrc)
    with Not_found -> failwith (Printf.sprintf "reject_copy:  LHS (%s) not an \"After\" variable" (PpIR.ppVar vdst))
  in
  GeneratorDSL.generator_dsl (G2(ldst,lsrc,Some(reject_copy),(fun (a,b) -> Copy(a,b))))
  
let constant_generator ht ldst =
  let m = List.map (fun v -> v,NEWIREval.eval_to_int32 ht (IRUtil.mk_evar v)) in
  GeneratorDSL.generator_dsl (G1(m ldst,None,(fun (v,i) -> Constant(v,i))))

let generate2 f = let open X86ToIRUtil in Generator.mk_sequential_generator 
 [f l_reg32_after l_v_general_registers32_noesp;
  f l_reg16_after l_v_general_registers16_nosp;
  f l_reg8_after  l_v_general_registers8;]

let generate1 f = let open X86ToIRUtil in Generator.mk_sequential_generator 
 [f l_reg32_after;f l_reg16_after;f l_reg8_after ;]

let generate_copy     () = generate2 copy_generator 
let generate_constant ht = generate1 (constant_generator ht)
let generate_binopreg ht = Generator.mk_sequential_generator 
 [generate2 (binopreg_comm_generator ht);generate2 (binopreg_nc_generator ht)]

(* REJECT IF vs IS A SUBREGISTER OF v *)
let write_generator lm lv =
  let reject_write (((v,d),s),vs) = IRUtil.size_of_var vs <> s in
  GeneratorDSL.generator_dsl (G2(lm,lv,Some(reject_write),(fun ((m,s),vs) -> Write(m,vs))))

(* REJECT IF vs IS A SUBREGISTER OF v *)
let read_generator lm lv =
  let reject_read (vs,((v,d),s)) = IRUtil.size_of_var vs <> s in
  GeneratorDSL.generator_dsl (G2(lv,lm,Some(reject_read),(fun (vd,(m,s)) -> Read(vd,m))))

let writeconst_generator io = let m = Hashtbl.fold (fun (m,s) v l -> 
  let x = NEWIREval.eval_to_int32 (ht_of_io io) (IRUtil.mk_evar v) in (m,x,s)::l) 
   (io.h_writes) []
  in
  GeneratorDSL.generator_dsl (G1(m,None,(fun (m,i,s) -> WriteConst(m,i,s))))

let mk_binopwrite ((m,_),b,r) = BinopWrite(m,b,r)
let mk_binopread  ((m,_),b,r) = BinopRead (r,b,m)

(* REJECT IF r IS A SUBREGISTER OF v *)
let reject_write_binop ht ((m,s),_,r) = IRUtil.size_of_var r <> s || ht_find_bool ht (m,IRUtil.size_of_var r)
let reject_read_binop  ht ((m,s),_,r) = IRUtil.size_of_var r <> s || ht_find_bool ht r

let binopmem_generator fr ht lb lm lrhs m = GeneratorDSL.generator_dsl (G3(lm,lb,lrhs,Some(fr ht),m))
let binopmem_generator fr ht m lm lrhs = Generator.mk_sequential_generator 
 [binopmem_generator fr ht l_binop lm lrhs m;binopmem_generator fr ht l_shiftop lm lrhs m]

type tgadget = 
| TBinopReg   of (IR.var,unit) Hashtbl.t
| TBinopRead  of (IR.var,unit) Hashtbl.t
| TBinopWrite of ((memloc * IR.typereg),unit) Hashtbl.t
| TConstant 
| TCopy 
| TRead 
| TWrite 
| TWriteConst

let generator_of_gadget_archetype io g =
  let aux = function
  | TBinopReg(ht)   -> generate_binopreg ht
  | TConstant       -> generate_constant (ht_of_io io)
  | TCopy           -> generate_copy ()
  | TRead           -> generate1 (read_generator  io.l_reads)
  | TBinopRead(ht)  -> generate2 (fun _ l -> binopmem_generator reject_read_binop  ht mk_binopread  io.l_reads  l)
  | TWrite          -> generate1 (write_generator io.l_writes)
  | TWriteConst     -> writeconst_generator io
  | TBinopWrite(ht) -> generate2 (fun _ l -> binopmem_generator reject_write_binop ht mk_binopwrite io.l_common l)
  in 
  empirical_filter io (aux g)

(*
let propagate g = let open IR in match g with 
| BinopWrite(_,_,_)
| Write(_,_)
| WriteConst(_,_,_) -> []

(* New gadget for each subregister *)
| Read(vdst,m)    -> List.map (fun (_,r) -> Read(r,m))                  (get_subregisters vdst)
| Constant(v,i)   -> List.map (fun (t,r) -> Constant(r,get_constant i)) (get_subregisters v)
| Copy(vdst,vsrc) -> List.map (fun (rl,rr,_) -> Copy(rl,rr))            (get_pairs vdst vsrc)

(* These binops filter to Low parts:  Add, Mul, Shl, Shr *)
| BinopReg(vres,vlhs,((Add|Mul) as b),vrhs) -> (*|Shl|Shr*)
  ListUtil.map_filter (fun (v,vl,vr,t) -> if is_low t then Some(BinopReg(v,vl,b,vr)) else None) (get_triples vres vlhs vrhs)

(* These binops filter to all subregisters:  And, Or, Xor *)
| BinopReg(vres,vlhs,((And|Or|Xor) as b),vrhs) -> 
  List.map (fun (v,vl,vr,_) -> BinopReg(v,vl,b,vr)) (get_triples vres vlhs vrhs)

| BinopRead(vres,((Add|Mul) as b),m) -> (*|Shl|Shr*)
  ListUtil.map_filter (fun (t,r) -> if is_low t then Some(BinopRead(r,b,m)) else None) (get_subregisters vres)

| BinopRead(vres,((And|Or|Xor) as b),m) -> List.map (fun (_,r) -> BinopRead(r,b,m)) (get_subregisters vres)
*)

