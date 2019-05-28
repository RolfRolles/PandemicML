module Z3 = Z3.V3

(* COPIED FROM NEWSmallerROPC.ml *)
let eval ht e = NEWIREval.expr e ht (fun _ _ -> failwith "Memory read in eval") (fun _ _ _ -> failwith "Memory write in eval")
let eval_to_int32 ht e = match (eval ht e) with | IR.Const(i,_) -> Int64.to_int32 i | _ -> failwith "eval_to_int32"
let is_true x = x = 1l
let is_eval_true ht e = is_true (eval_to_int32 ht e)

(* Put this in a new Z3Util module *)
let string_of_search_failure = function 
| Z3.NO_FAILURE       -> "NO_FAILURE"
| Z3.UNKNOWN          -> "UNKNOWN"
| Z3.TIMEOUT          -> "TIMEOUT"
| Z3.MEMOUT_WATERMARK -> "MEMOUT_WATERMARK"
| Z3.CANCELED         -> "CANCELED"
| Z3.NUM_CONFLICTS    -> "NUM_CONFLICTS"
| Z3.THEORY           -> "THEORY"
| Z3.QUANTIFIERS      -> "QUANTIFIERS"

(* Util *)
let accrue ht r x = 
  let l = 
    try Hashtbl.find ht r 
    with Not_found -> [] 
  in Hashtbl.replace ht r (x::l)

(* This code creates the JIT scaffolding necessary for testing *)
let init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,f_blit,f_execute = JITSampler.create_setup ()

let debug  = false
let debug2 = false

let generate_iop instate = let open RegUtil in
  let outstate = f_execute instate in
 (if debug2 then 
  let _ = Printf.printf "State before:\n" in let _ = JITSampler.print_x86ctx  instate in
  let _ = Printf.printf "State after:\n"  in         JITSampler.print_x86ctx outstate);
  let ht = Hashtbl.create 64 in
  List.iter (fun (_,r,fc,vb,va) -> let get = fst(qtype_getter_setter r) in 
    Hashtbl.replace ht vb (fc (get instate)); Hashtbl.replace ht va (fc (get outstate)))
    l_regflags;
  ht

let mk_random_state _ = JITSampler.mk_random_state c_stack32

let mk_exhaustive_gen_from_state brnd r s =
  let get,set = RegUtil.qtype_getter_setter r in let rs = ref (set 0l s) in
  let bwrapped = ref false in
  function 
  | false -> if !bwrapped then raise Generator.GenerationComplete else !rs 
  | true -> (if !bwrapped then (bwrapped := false; raise Generator.GenerationComplete));
    let r = !rs in rs := set (Int32.add 1l (get !rs)) (if brnd then mk_random_state () else !rs);
   (if (get !rs) = 0l then bwrapped := true);
    r

let mk_exhaustive_gen brnd r gen = Generator.(map_multiply (mk_exhaustive_gen_from_state brnd r) gen)

(* Make an input state where all 32 and 1-bit values respectively match *)
let instate_all_same val32 val1 = 
  let open JITRegion in
  { eax = val32; ebx = val32; ecx = val32; edx = val32; esp = c_stack32; ebp = val32; esi = val32; edi = val32; 
    eflags = Int32.of_int (X86Misc.fl2eflags val1 val1 val1 val1 val1 val1 val1); }

let create_exhaustive_generator l = 
  match (List.fold_left (fun acc r -> match acc with 
  | None    -> Some(mk_exhaustive_gen_from_state true r (mk_random_state ()))
  | Some(g) -> Some(mk_exhaustive_gen false r g)) None l) with
  | None -> failwith "create_exhaustive_generator:  empty lists"
  | Some(g) -> g

let mk_memoize_one g = 
  let r = ref (g false) in
  function
  | false -> !r
  | true  -> let x = g true in r := g false; x

let create_random_generator n ls = let open Generator in
  mk_sequential_generator [(mk_generator ls);mk_counting_generator n (mk_memoize_one mk_random_state)]

(* Duplicated from ListUtil *)
let score_sort l = List.rev_map snd (List.sort compare (List.rev_map (fun ((s,i) as p) -> (-(SynthesisIR.score s),p)) l))

let materialize_unfalsified_hypotheses g_io g_hypothesis = 
 let _ = (if debug then 
      let rec a g = 
        let _ = Printf.printf "C " in 
        match (try Some(g true) with Generator.GenerationComplete -> None) with 
        | Some (_,_,i) -> Printf.printf "%s\n" (PpIR.ppExpr false i); a g
        | None   -> ()
      in a g_hypothesis) in
  match (Generator.fold (fun ht_iop o -> match o with
  | Some(l) -> Some(List.filter (fun (r,s,i) -> 
    let b = is_eval_true ht_iop i in 
   (if debug && (b = false) then Printf.printf "[!] Invalidated %s\n" (PpIR.ppExpr false i));
    b) l)
  | None    -> Some(Generator.fold (fun (r,s,i) l -> 
   (if debug then Printf.printf "C %s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s)));
    let b = is_eval_true ht_iop i in
   (if debug && (b = false) then Printf.printf "[!] Invalidated %s\n" (PpIR.ppExpr false i));
    if b then (r,s,i)::l else l) g_hypothesis []))
  g_io None) with
  | None    -> failwith "generate_unfalsified_hypotheses: g_io did not generate"
  | Some(l) -> let ht = Hashtbl.create 64 in 
    List.iter (fun (r,s,i) -> accrue ht r (s,i)) l;
    Hashtbl.fold (fun r c l -> (r,score_sort c)::l) ht []

(* Cheap code to determine register output constant-ness *)
type 'a cplattice = (* ' *)
| Constant of 'a (* ' *)
| Overdefined

let analyze gen =
  let hmod = Hashtbl.create 64 in 
  let modified h = List.iter (fun (_,r,_,vb,va) -> 
    if Hashtbl.find h vb <> Hashtbl.find h va then Hashtbl.replace hmod r ()) RegUtil.l_regflags
  in
  let constantness h acc =
    let join x y = match x with | Constant(z) when z = y -> x | _ -> Overdefined in
    let rep h f = List.iter (fun (_,r,_,_,va) -> Hashtbl.replace h r (f r va)) RegUtil.l_regflags in
    match acc with
    | None     -> let h2 = Hashtbl.create 64 in rep h2 (fun _ v -> Constant(Hashtbl.find h v)); Some(h2)
    | Some(h2) -> rep h2 (fun r v -> join (Hashtbl.find h2 r) (Hashtbl.find h v)); acc
  in
  match Generator.fold (fun ht acc -> modified ht; constantness ht acc) gen None with
  | None -> failwith "analyze: gen didn't generate"
  | Some(hconst) -> Hashtbl.fold (fun k v acc -> match v with 
    | Overdefined -> acc 
    | Constant((IR.Const(i,s) as c)) -> (k,Some[SynthesisIR.Const(i,s),c])::acc
    | Constant(e) -> failwith ("analyze: non-constant expression "^(PpIR.ppExpr false e))) 
    hconst [],
    Hashtbl.fold (fun e _ l -> e::l) hmod []

let distinguish z3 ((s1,i1) as p1) ((s2,i2) as p2) l =
  let z3ast = Z3SymbolicExecute.z3ast_of_irexpr z3 (IRUtil.mk_ne i1 i2) in
  let _ = Z3.assert_cnstr z3 z3ast in
  let (result,m) = Z3.check_and_get_model z3 in
  let res = match result with
  | Z3.L_UNDEF -> failwith (string_of_search_failure (Z3.get_search_failure z3))
  | Z3.L_FALSE -> SynthesisIR.(if score s1 <= score s2 then Some(p1,p2::l) else Some(p2,p1::l))
  | Z3.L_TRUE  -> let state = Array.fold_left (fun s fd -> 
      let name  = Z3.get_symbol_string z3 (Z3.get_decl_name z3 fd) in
      let b,ast = Z3.eval_func_decl z3 m fd in
     (if (not b) then failwith "distinguish: couldn't evaluate constant");
      let b,i64 = Z3.get_numeral_int64 z3 ast in
     (if (not b) then failwith "distinguish: couldn't evaluate constant to int64");
      let set = try Hashtbl.find RegUtil.ht_str2setter name 
        with Not_found -> failwith ("distinguish: could not find "^name) 
      in
      set (Int64.to_int32 i64) s) 
     (mk_random_state ()) 
     (Z3.get_model_constants z3 m)
    in 
   (match is_eval_true (generate_iop state) i1, is_eval_true (generate_iop state) i2 with
    | true,true -> 
      Printf.printf 
        "Input did not distinguish?\nChal: %s\nCurr: %s\nState before:\n" 
          (PpIR.ppExpr false i1) (PpIR.ppExpr false i2); 
      JITSampler.print_x86ctx state;
      Printf.printf "State after:\n";
      None
    | true,false  -> Some(p1,l)
    | false,true  -> Some(p2,[])
    | false,false -> None)
  in
  Z3.del_model z3 m;
  res

let deathmatch l = 
  let z3 = Z3.mk_context_x [|("SOFT_TIMEOUT","10000");("MODEL","true")|] in
  let res = List.fold_left (fun acc (r,l) -> 
    let res = List.fold_left (fun acc chal -> match acc with 
    | None -> Some(chal,[])
    | Some(cand,l) -> distinguish z3 cand chal l)
    None l
    in
    match res with
    | Some(x,l) -> (r,Some(x::l))::acc
    | None      -> (r,None)::acc)
    [] l
  in
  Z3.del_context z3;
  res

let uncurry7 func (a,b,c,d,e,f,g) = func a b c d e f g

type eval_method = 
| Exhaustive of RegUtil.qtype list
| Randomized of int * (JITRegion.x86ctx list)

type 'a output_method = Autodetect | Provided of 'a (* ' *)

let analyze_mk_stategen instr evalm =
  let _ = JITSampler.blit_instr f_blit instr in
  Generator.mk_transform_generator generate_iop (match evalm with
  | Exhaustive(linp) -> create_exhaustive_generator linp
  | Randomized(n,ls) -> create_random_generator n ls)

let analyze_instr instr outv evalm l_tmpl =
  let sgen = analyze_mk_stategen instr evalm in
  let l_const,l_mod = match outv with
  | Autodetect -> analyze sgen
  | Provided(c,m) -> c,m
  in
  let l_const = List.filter X86.(RegUtil.(fun (q,_) -> match q with 
    | Reg(Gd(Esp)) | Reg(Gw(Sp)) -> false 
    | _ -> true)) 
    l_const 
  in
 (if debug then 
   (Printf.printf "Constants\n"; 
    List.iter (fun (x,_) -> Printf.printf "  %s\n" (RegUtil.string_of_qtype x)) l_const;
    Printf.printf "Modified\n"; 
    List.iter (fun x -> Printf.printf "  %s\n" (RegUtil.string_of_qtype x)) l_mod));
  let l_mod_regs,l_mod_flags = 
    List.partition 
     (function | RegUtil.Reg(_) -> true | RegUtil.Flag(_) -> false) 
     (List.filter (fun q -> not(List.mem_assoc q l_const)) l_mod)
  in
  let analyze l_mod l_tmpl = 
    let hgen = List.map (uncurry7 (SynthesisIR.generator_of_expr_highest l_mod)) l_tmpl in
    let hgen = Generator.mk_sequential_generator hgen in
    let l_unfalsified = materialize_unfalsified_hypotheses sgen hgen in
    match evalm with
    | Exhaustive(_) -> List.map (fun (r,l) -> if l = [] then (r,None) else (r,Some(l))) l_unfalsified
    | Randomized(_) -> deathmatch l_unfalsified
  in
  let l_reg_exprs = analyze l_mod_regs l_tmpl in
  let l_reg_subst = List.fold_left (fun acc (r,o) -> match o with 
    | Some([]) -> failwith "analyze_instr: impossible"
    | Some((s,i)::xs) -> Printf.printf "adding %s->%s\n" (RegUtil.string_of_qtype r) (PpIR.ppExpr false i); (Hashtbl.find RegUtil.ht_reg2var r,s)::acc
    | None -> acc)
    []
    l_reg_exprs
  in
  let l_tmpl = List.map (fun (l,h,a,b,c,d,e) -> 
    let ht = Hashtbl.copy h in List.iter (fun (v,x) -> accrue ht (SynthesisIR.Var(v)) x) l_reg_subst;
    (l,ht,a,b,c,d,e)) 
    l_tmpl
  in
  let l_flag_exprs = analyze l_mod_flags l_tmpl in
 (l_flag_exprs@l_reg_exprs@l_const)
    
(* Analyze all I/O pairs for a several fixed hypotheses *)
let analyze_instr_hypothesis_list instr evalm list =
  materialize_unfalsified_hypotheses 
   (analyze_mk_stategen instr evalm) 
   (Generator.mk_generator (List.map (fun (reg,hyp) -> 
     (reg,hyp,IRUtil.(mk_eq (mk_evar (Hashtbl.find RegUtil.ht_reg2aftervar reg)) (SynthesisIR.convert_expr hyp))))
     list))

(* Analyze all I/O pairs for a single hypothesis *)
let analyze_instr_single_hypothesis instr evalm reg hyp =
  analyze_instr_hypothesis_list instr evalm [reg,hyp]

(* No register required *)
let analyze_instr_single_hypothesis_noreg instr evalm hyp =
  materialize_unfalsified_hypotheses 
   (analyze_mk_stategen instr evalm) 
   (Generator.mk_generator [(RegUtil.Flag(X86.X86F_C),hyp,SynthesisIR.convert_expr hyp)])

let print_results res = List.iter (fun (r,o) -> 
  Printf.printf "%s: " (RegUtil.string_of_qtype r);
  match o with
  | None       -> Printf.printf "COULD NOT SYNTHESIZE\n"
  | Some([])   -> Printf.printf "Synthesis Error:\n";
  | Some((s,_)::_) -> Printf.printf "%s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s)))
  res
