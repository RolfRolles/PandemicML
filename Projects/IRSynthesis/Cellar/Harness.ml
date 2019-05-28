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

(* This code creates the JIT scaffolding necessary for testing *)
let init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,f_blit,f_execute = JITSampler.create_setup ()

let generate_iop instate = let open RegUtil in
  let outstate = f_execute instate in
  let ht = Hashtbl.create 64 in
  List.iter (fun (_,r,fc,vb,va) -> let get = fst(qtype_getter_setter r) in 
    Hashtbl.replace ht vb (fc (get instate)); Hashtbl.replace ht va (fc (get outstate)))
    l_regflags;
  ht

let mk_random_state _ = JITSampler.mk_random_state c_stack32

let mk_exhaustive_gen_from_state brnd r s =
  let get,set = RegUtil.qtype_getter_setter r in let rs = ref (set 0l s) in
  function 
  | false -> !rs 
  | true -> rs := set (Int32.add 1l (get !rs)) !rs;
    if (get !rs) = 0l 
    then begin
     (if brnd 
      then rs := (set 0l (mk_random_state ())));
      raise Generator.GenerationComplete
    end
    else !rs

(* FIGURE OUT WHY THIS GOES INTO AN INFINITE LOOP
let mk_exhaustive_gen_from_state base r s =
  let get,set = RegUtil.qtype_getter_setter r in let rs = ref (set 0l s) in
  function 
  | false -> !rs 
  | true  -> let incval = Int32.add 1l (get !rs) in
   (rs := if base then (set incval (mk_random_state ())) else (set incval !rs));
    if incval = 0l then raise Generator.GenerationComplete else !rs
*)
let mk_exhaustive_gen brnd r gen = Generator.(map_multiply (mk_exhaustive_gen_from_state brnd r) gen)

(* Make an input state where all 32 and 1-bit values respectively match *)
let instate_all_same val32 val1 = 
  let open JITRegion in
  { eax = val32; ebx = val32; ecx = val32; edx = val32; esp = c_stack32; ebp = val32; esi = val32; edi = val32; 
    eflags = Int32.of_int (X86Misc.fl2eflags val1 val1 val1 val1 val1 val1 val1); }

let create_exhaustive_generator l = 
  match (List.fold_left (fun acc r -> match acc with 
  | None    -> Some(mk_exhaustive_gen_from_state true r (instate_all_same 0l 0))
  | Some(g) -> Some(mk_exhaustive_gen false r g)) None l) with
  | None -> failwith "create_exhaustive_generator:  empty lists"
  | Some(g) -> g

let mk_memoize_one _ = failwith "Unimplemented"

let create_random_generator n ls = let open Generator in
  mk_sequential_generator [(mk_generator ls);mk_counting_generator n (mk_memoize_one mk_random_state)]

(* Duplicated from ListUtil *)
let score_sort l = List.rev_map snd (List.sort compare (List.rev_map (fun ((s,i) as p) -> (SynthesisIR.score s,p)) l))

let materialize_unfalsified_hypotheses g_io g_hypothesis = 
  match (Generator.fold (fun ht_iop o -> match o with
  | Some(l) -> Some(List.filter (fun (r,s,i) -> is_eval_true ht_iop i) l)
  | None    -> Some(Generator.fold (fun (r,s,i) l -> if is_eval_true ht_iop i then (r,s,i)::l else l) g_hypothesis []))
  g_io None) with
  | None    -> failwith "generate_unfalsified_hypotheses: g_io did not generate"
  | Some(l) -> let ht = Hashtbl.create 64 in 
    let accrue r x = let l = try Hashtbl.find ht r with Not_found -> [] in x::l in
    List.iter (fun (r,s,i) -> Hashtbl.replace ht r (accrue r (s,i))) l;
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

let uncurry6 func (a,b,c,d,e,f) = func a b c d e f

type eval_method = Exhaustive | Randomized of int * (JITRegion.x86ctx list)
type 'a output_method = Autodetect | Provided of 'a (* ' *)

let analyze_instr instr outv linp evalm l_tmpl =
  let _ = JITSampler.blit_instr f_blit instr in
  let sgen = Generator.mk_transform_generator generate_iop (match evalm with
  | Exhaustive       -> create_exhaustive_generator linp
  | Randomized(n,ls) -> create_random_generator n ls)
  in
  let l_const,l_mod = match outv with
  | Autodetect -> analyze sgen
  | Provided(c,m) -> c,m
  in
  let hgen = List.map (uncurry6 (SynthesisIR.generator_of_expr_highest l_mod)) l_tmpl in
  let hgen = Generator.mk_sequential_generator hgen in
  let l_unfalsified = materialize_unfalsified_hypotheses sgen hgen in
  (* What is l_unfalsified? At present it is a list of SynthesisIR,IR expression 
     pairs that were equal across all states.  So what do I want to do with that? 
     I guess I am going to need to destructure these expressions (ugly) and then...
     
     I think the answer to my question lies in the conjunction of two other 
     question/answers.
     
     1) What information does deathmatch need to do its work?
     2) What information is the caller expecting?
     
     1) In the existing code, deathmatch does the following:
     * * Given a list of candidates *for one particular register*, loop:
  *)
  match evalm with
  | Exhaustive    -> l_const@(List.map (fun (r,l) -> if l = [] then (r,None) else (r,Some(l))) l_unfalsified)
  | Randomized(_) -> l_const@(deathmatch l_unfalsified)

let print_results = List.iter (fun (r,o) -> 
  Printf.printf "%s: " (RegUtil.string_of_qtype r);
  match o with
  | None -> Printf.printf "COULD NOT SYNTHESIZE\n"
  | Some(l) -> Printf.printf "Candidates:\n";
    List.iter (fun (s,i) -> Printf.printf "%s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s))) l)


  let instr = X86.({pref=[];instr=(Xor,[GeneralReg(al);GeneralReg(cl)])}) in
  let l_tmpl = X86ToIRUtil.(SynthesisIR.([
    Binop(Var(vAl),Xor,Var(vBl)),ht_fill [Var(vBl),[Var(vCl)]],ht_fill [],ht_fill [],ht_fill [],ht_fill [];
    
    ]))
  in
  let _ = Printf.printf "2\n" in
  let res = analyze_instr instr Autodetect (RegUtil.([Reg(al);Reg(cl)])) Exhaustive l_tmpl in
  let _ = Printf.printf "3\n" in
  print_results res

