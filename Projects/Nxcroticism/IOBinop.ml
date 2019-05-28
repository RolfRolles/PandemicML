open FrameworkUtil
open DataStructures
open IOPair

(* I should not have to do this, but apparently the constant folding function is not idempotent 
   That needs to be fixed, seriously.  But for the time being, let's just do this.
   DUPLICATED; ALSO IN GenerateIOPairs
    *)
let rec fold_constants_fixedpoint e =
  let n = IRLocalOpt.fold_expr_constants e in
  if e <> n
  then fold_constants_fixedpoint n
  else e

let make_binop_behaviors_ht size io ht_mem_expr_to_var =
  let l_mem_exprs_val = 
    List.fold_left
     (fun l (a,set,v32) ->
        VarInt32Set.fold (fun p l -> (Hashtbl.find ht_mem_expr_to_var p,v32)::l) set l)
      []
      io.reads
  in
  let l_varvals = l_mem_exprs_val@(io.register_data) in
  let ht_results = Hashtbl.create 1024 in

  let add_result dw vbv =
    let set = try Hashtbl.find ht_results dw with Not_found -> VarBinopVarSet.empty in
    let set = VarBinopVarSet.add vbv set in
    Hashtbl.replace ht_results dw set
  in

  let perform_all lv l32 bl list = 
    List.iter
     (fun (rv,r32) ->
        if lv <> rv then
        List.iter 
         (fun bop -> 
            let e = 
              fold_constants_fixedpoint
               (IR.Binop(IRUtil.mk_fixed_const_of_i32 l32 size,bop,IRUtil.mk_fixed_const_of_i32 r32 size))
            in
            match e with
            | IR.Const(_,_) as c -> add_result c (lv,bop,rv)
            | _ -> (*f_printf "Couldn't fold %s\n" (PpIR.ppExpr e); *)())
          bl)
      list
  in
  let rec cbin  bl = function | [] -> () | (v,c)::rs -> perform_all v c bl rs;        cbin  bl rs in
  let rec ncbin bl = function | [] -> () | (v,c)::rs -> perform_all v c bl l_varvals; ncbin bl rs in
  let open IR in 
  cbin  [Add;Mul;And;Or;Xor]          l_varvals;
  ncbin [Sub(*;SDiv;UDiv;SMod;UMod*)] l_varvals;
  ncbin [Shl;Shr;Sar]                 l_varvals;
(*
  cbin  [EQ;NE]                       l_varvals;
  ncbin [ULT;ULE;SLT;SLE]             l_varvals;
*)
  ht_results

let determine_binop_behaviors size io ht_var_to_mem_expr ht_mem_expr_to_var =
  (* ht[var] -> IR.expr(dword) of final variable values *)
  let rout = io.rev_post in
  (* ht[dword] -> (var,binop,var) *)
  let results = make_binop_behaviors_ht size io ht_mem_expr_to_var in
  let add_to_map k vl map =
    match ht_find_opt results k with
    (* The variables in the list v matched the results of some binop, 
       specified in s_vbv as {(vlhs,bop,vrhs)} *)
    | Some(s_vbv) -> 
      List.fold_left (fun map v ->
        let s_existing = try IRUtil.VarMap.find v map with Not_found -> s_vbv in
        IRUtil.VarMap.add v (VarBinopVarSet.union s_vbv s_existing) map)
        map
        vl
    (* No match. *)
    | None -> map
  in
  let map = Hashtbl.fold add_to_map rout IRUtil.VarMap.empty in

  let map = List.fold_left (fun map (a32,set,v32) ->
    VarInt32Set.fold (fun p map -> 
      add_to_map (IRUtil.mk_fixed_const_of_i32 v32 size) [(Hashtbl.find ht_mem_expr_to_var p)] map)
      set
      map)
    map
    io.writes
  in
  map
      
let determine_aggregate_binop_behaviors size ioc =  
  let ht_var_to_mem_expr = Hashtbl.create 20 in
  let ht_mem_expr_to_var = Hashtbl.create 20 in
  let insert_vars set =
    VarInt32Set.fold (fun p set -> 
      let nv = ht_find_opt ht_mem_expr_to_var p in
      match nv with
      | None ->
        let nv = IRUtil.new_var size in
        Hashtbl.replace ht_var_to_mem_expr nv p;
        Hashtbl.replace ht_mem_expr_to_var p nv;
        IRUtil.VarSet.add nv set
      | Some(nv) -> IRUtil.VarSet.add nv set)
      set
      IRUtil.VarSet.empty
  in
  let s_readvars  = insert_vars (select_member size ioc.common_reads) in 
  let is_read   v = IRUtil.VarSet.mem v s_readvars in
  let s_writevars = insert_vars (select_member size ioc.common_writes) in 
  let is_write  v = IRUtil.VarSet.mem v s_writevars in
  let s_memvars   = IRUtil.VarSet.union s_readvars s_writevars in 
  let is_mem v    = IRUtil.VarSet.mem v s_memvars in 
  let f x = 
    let map =
    determine_binop_behaviors 
      size 
     (select_member size x) 
      ht_var_to_mem_expr 
      ht_mem_expr_to_var
    in
  (*f_printf "Var-Binop-Var map after an iteration\n";
    dump_vbvmap map;*)
    map
  in
  let vbvmap = 
    match ioc.l_io with
    | io::ios -> List.fold_left (fun s i -> varmapvbvset_inter s (f i)) (f io) ios
    | _ -> IRUtil.VarMap.empty
  in
(*f_printf "Var-Binop-Var map after all iterations\n";
  dump_vbvmap vbvmap;*)
  
  IRUtil.VarMap.fold (fun v s_vbv acc ->
    VarBinopVarSet.fold (fun (vl,binop,vr) ((write,read,reg) as acci) ->
      match (v,vl,vr) with
      (* This is [memloc] BINOP= vrreg *)
      | _ when is_write v && is_read vl && v = vl && not(is_mem vr) ->
        let (vr32,d32) = Hashtbl.find ht_var_to_mem_expr v in
        (VarBinopVarInt32Set.add (vr,binop,vr32,d32) write,read,reg)

      | _ when is_write v && is_read vr && v = vr && not(is_mem vl) ->
        let (vr32,d32) = Hashtbl.find ht_var_to_mem_expr v in
        (VarBinopVarInt32Set.add (vl,binop,vr32,d32) write,read,reg)

      (* This is vreg BINOP= [memloc] *)
      | _ when not(is_mem v) && v = vl && is_mem vr ->
        let (vr32,d32) = Hashtbl.find ht_var_to_mem_expr vr in
        (write,VarBinopVarInt32Set.add (vl,binop,vr32,d32) read,reg)

      | _ when not(is_mem v) && v = vr && is_mem vl ->
        let (vr32,d32) = Hashtbl.find ht_var_to_mem_expr vl in
        (write,VarBinopVarInt32Set.add (vr,binop,vr32,d32) read,reg)
      
      (* This is vreg = vlreg BINOP= vrreg *)
      | _ when not(is_mem v) && not(is_mem vl) && not(is_mem vr) ->
        (write,read,VarVarBinopVarSet.add (v,vl,binop,vr) reg)
      
      (* Upon investigation, these correspond to various things interacting
         with one another to produce a zero result.  They have never been
         interesting. *)
      | _ -> acci
      (*f_printf "Missed BINOP %s %s %s, investigate\n" 
          (PpIR.ppVar vl) 
          (PpIR.ppBinop binop) 
          (PpIR.ppVar vr); 
        (write,read,reg)*)
        )
      s_vbv
      acc)
  vbvmap
  (VarBinopVarInt32Set.empty,VarBinopVarInt32Set.empty,VarVarBinopVarSet.empty)

let determine_aggregate_binop_behaviors ioc = 
  let triple_t = triplicate determine_aggregate_binop_behaviors ioc in
  let w32,r32,reg32 = triple_t.val32 in
  let w16,r16,reg16 = triple_t.val16 in
  let w8 ,r8 ,reg8  = triple_t.val8  in
  {val8 = w8;   val16 =   w16; val32 =   w32;},
  {val8 = r8;   val16 =   r16; val32 =   r32;},
  {val8 = reg8; val16 = reg16; val32 = reg32;}

(* Ensure that vdst.post = vsrclhs bOP vsrcrhs *)
let verify_reg_binop tbl vdst vsrclhs b vsrcrhs size = 
  let open VerifyUtil in
  let esrclhs = src_expr vsrclhs size and esrcrhs = src_expr vsrcrhs size in
  IRUtil.mk_ne (dest_expr tbl vdst size) (IRUtil.mk_fn_of_binop b esrclhs esrcrhs)

(* Ensure that vdst.post = vdst.pre bOP mem.pre[vmemreg.pre + disp32] *)
let verify_read_binop tbl vdst b vmemreg disp32 size = 
  let open VerifyUtil in
  IRUtil.mk_ne 
   (dest_expr tbl vdst size) 
   (IRUtil.mk_fn_of_binop 
      b 
     (src_expr vdst size) 
     (mk_load_disp32_pre vmemreg disp32 size))

(* Ensure that mem.post[vmemreg.pre + disp32] = vsrclhs.pre bOP mem.pre[vmemreg.pre + disp32] *)
let verify_write_binop tbl vmemreg disp32 b vsrcrhs size = 
  let open VerifyUtil in
  IRUtil.mk_ne 
   (mk_load_disp32_post tbl vmemreg disp32 size)
   (IRUtil.mk_fn_of_binop 
      b 
     (mk_load_disp32_pre vmemreg disp32 size)
     (src_expr vsrcrhs size))

let verify_register_binop_behaviors verify ssa_tbl size s_vvbv =
  let open VerifyUtil in
  VarVarBinopVarSet.filter
   (fun (vdst,vsrclhs,binop,vsrcrhs) -> verify (verify_reg_binop ssa_tbl vdst vsrclhs binop vsrcrhs size))
    s_vvbv

let verify_memread_binop_behaviors p verify ssa_tbl size s_vbvi32 =
  VarBinopVarInt32Set.filter
   (fun (vdst,binop,vreg,d32) -> 
      verify (IRUtil.mk_and p (verify_read_binop ssa_tbl vdst binop vreg d32 size)))
    s_vbvi32

let verify_memwrite_binop_behaviors p verify ssa_tbl size s_vbvi32 =
  VarBinopVarInt32Set.filter
   (fun (vdst,binop,vreg,d32) -> 
      verify (IRUtil.mk_and p (verify_write_binop ssa_tbl vreg d32 binop vdst size)))
    s_vbvi32

(* This should probably filter behavior down.  I.e. if a larger register is 
   found to be a bitwise binop, then the subregisters will be binops too. *)
let verify_register_binop_behaviors verify ssa_tbl s_vvbv_t =
  triplicate_map (verify_register_binop_behaviors verify ssa_tbl) s_vvbv_t

let verify_memread_binop_behaviors p verify ssa_tbl s_vvbv_t =
  triplicate_map (verify_memread_binop_behaviors p verify ssa_tbl) s_vvbv_t

let verify_memwrite_binop_behaviors p verify ssa_tbl s_vvbv_t =
  triplicate_map (verify_memwrite_binop_behaviors p verify ssa_tbl) s_vvbv_t

