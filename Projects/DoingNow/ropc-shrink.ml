(*
***Life Goal (Strangely-Situated)***

I am going to make a lot of mistakes in life.
Mistakes serve to teach lessons.
Therefore I should make mistakes as quickly as possible.

***Goal***

Make ROPC as small as possible!

***Executive Summary***

The existing implementation contains a lot of boilerplate.  Each gadget type has its own .ml file for generating candiates and verifying them.  These files tend to contain:

* Functions for generating candidates by explicitly examining the I/O pairs
* Functions to determine the intersection of candidates
* Functions to generate verification conditions for the property
* Functions to derive further facts from a verified property

The boilerplate arises because:

* Candidates are generated manually from the I/O pairs, requiring special logic for each gadget type.
* Candidates are stored in data structures that differ among gadget types, obviating code reuse.
* Derivation logic is duplicated per gadget due to differing data structures and propagation rules.

The insight is to recognize that the verification condition itself contains all of the necessary information to test the property.  On a high-level, we can apply the generator paradigm to generate verification conditions, which eliminates the first three categories of the .ml files described above.  We can use common machinery to factor together propagation rules and strategies.  (The generator actually generates Gadget variants, which are then the common currency used by the rest of the common machinery.  Having a common type, the tedious custom data structures can be removed).
*)

(*
***TO DO***

1) Check out what I did in IOCopy and see if I can replace the subregister mess with that.

It looks feasible, actually.  generate_smaller_copy_facts embodies a strategy that would appear to generalize my incredibly tedious attemps to replace IR.vars in the Gadget definitions.  So perhaps I should revert this change.  At this point, I start to wonder if I should be using version control.
*)

(* Once we verify any 32-bit copies, we get the 16-bit ones for free, and the
   8-bit ones too (if the register supports 8-bit subdivisions). *)
let generate_smaller_copy_facts s_copy_pairs_verified f_derive_equalities_l =
  VarVarSet.fold 
   (fun (vsrc,vdst) set -> 
      let l = f_derive_equalities_l vsrc vdst in
      List.fold_left (fun set (vs,vd) -> VarVarSet.add (vs,vd) set) set l)
    s_copy_pairs_verified
    VarVarSet.empty 

let map_32bit_to_16bit vsrc vdst  = let open X86ToIRUtil in [(v16_of_v32_exn vsrc, v16_of_v32_exn vdst)]
let map_16bit_to_8bit  vsrc vdst =  let open X86ToIRUtil in 
  match v8p_opt_of_v16_exn vsrc,v8p_opt_of_v16_exn vdst with
  | Some(vsrch8,vsrcl8),Some(vdsth8,vdstl8) -> [(vsrcl8,vdstl8);(vsrch8,vdsth8)]
  | _,_ -> []

let s_copy_pairs16  = generate_smaller_copy_facts s_copy_pairs32 map_32bit_to_16bit in

(*
2) Simplify the framework's SSA.

DONE.  Except I will need to grep the codebase for IRSSA.bb_to_ssa.  This is
good: I wanted to get rid of this ugly wrinkle and replace it with a 
hard-coded constant stored in some well-documented constants file.
*)

(*
3) Check out I/O pair generation.
*)

let initialize_registers () = 
  let ht = Hashtbl.create 100 in let add v v32 = Hashtbl.replace ht v v32 in

  (* Generate initial register values *)
  let _ =
    List.iter (fun (v32,v16,pv8o) ->
      let c32 = nonzero_rand_i32 () in
      let _ = add v32 (IRUtil.mk_dword_of_int32 c32) in
      let _ = add v16 (IRUtil.mk_word_of_int32  c16) in
      let b = IRUtil.mk_byte_of_int32 in
      match pv8o with
      | Some(v8h,v8l) -> 
         let _ = Hashtbl.replace ht v8h (b (Int32.shift_right c32 8)) in
         Hashtbl.replace ht v8l (b (Int32.shift_right c32 8))
      | None -> ())
      X86ToIRUtil.l_v_general_registers_parentage
  in
  (* Generate values for registers of the "non-general" variety *)
  ht

let make_io_struct ir = 
  (* Make the memory context and register hash table *)
  let memctx = Hashtbl.create 32 and ht = initialize_registers () in
  
  (* Append the statements needed to make SSA easier *)
  let ir_ssa_after = X86ToIRUtil.mk_after ht_ssa in
  
  (* Perform emulation, get the output values reads := (a,v,s)::(!reads); *)
  let reads,writes = IRRandomizedEvaluator.concrete_evaluate_jumpless_nonssa ir_ssa_after ht memctx in

  (ht,reads,writes)
  
let mk_io_struct_caller ir e_jt =
  let ht,reads,writes = mk_io_struct ir in
(* XXXX USE THE EVALUATOR DIRECTLY HERE XXXX
  Determine the return location
  let c_jt = IRLocalOpt.replace_var_with_expr ht_post e_jt in
  let a_jt32_opt = match fold_constants_fixedpoint c_jt with
  | IR.Const(a32,IR.TypeReg_32) -> Some(Int64.to_int32 a32)
  | _ -> None
  in
*)

(* Segregate the lists of reads and writes by size
  let r32,r16,r8 = segregate_address_list reads in
  let w32,w16,w8 = segregate_address_list writes in
*)

  
(* 
4) Gadget generators
*)

(*
BinopReg:   4, After/Lhs/Binop/Rhs
* Rules:  
* * Add, Mul, And, Or, Xor:  LHS != RHS
* * Sar, Shl, Shr:
Copy:       2, After/Before
Constant:   2, After/Int32

BinopRead:  3, After/Binop/Memloc
Read:       2, After/Memloc
BinopWrite: 3, Memloc/Binop/Rhs
Write:      2, Memloc/Before
*)

let mk_binop (d,l,b,r) = BinopReg(d,l,b,r)

let binopreg_comm_generator ldst lsrc =
  let ht = Hashtbl.create 10 in
  let reject_comm ((d,l,b,r) as p) = 
    (l = r) ||
    (try (Hashtbl.find ht (l,b,r); true) with Not_found -> (Hashtbl.replace ht (r,b,l) false))
  in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,[Add;Mul;And;Or;Xor],lsrc,Some(reject_comm),mk_binop)

let binopreg_nc_generator ldst lsrc =  
  let open X86ToIRUtil in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,[Sar;Shl;Shr],l_v_general_registers8,Some(fun (d,l,b,r) -> l = r),mk_binop)

let copy_generator ldst lsrc =
  let reject_copy (vdst,vsrc) = 
    try (IRUtil.VarMap.find X86ToIRUtil.m_vafter_to_vbefore vdst = vsrc)
    with Not_found -> failwith "reject_copy:  LHS not an \"After\" variable"
  in
  GeneratorDSL.generator_dsl (G2(ldst,lsrc,Some(reject_copy),(fun (a,b) -> Copy(a,b)))
  
let constant_generator ldst =
  let m = List.map (fun v -> v,eval_to_int32 (mk_evar v)) in
  GeneratorDSL.generator_dsl (G1(m ldst,None,(fun (v,i) -> Constant(v,i))))

let generate2 f = Generator.mk_sequential_generator
 [f l_reg32_after X86ToIRUtil.l_v_general_registers32_noesp;
  f l_reg16_after X86ToIRUtil.l_v_general_registers16_nosp;
  f l_reg8_after  X86ToIRUtil.l_v_general_registers8;]

let generate1 f = Generator.mk_sequential_generator 
 [f l_reg32_after;f l_reg16_after;f l_reg8_after ;]

let generate_copy     () = generate2 copy_generator 
let generate_constant () = generate1 constant_generator
let generate_binopreg () = Generator.mk_sequentual_generator 
 [generate2 binop_comm_generator;generate2 binop_nc_generator]

| Write(m,vdst)
| BinopWrite(m,b,v)
| WriteConst(m,c,s)
| Read(vdst,m)
| BinopRead(vres,b,m)
