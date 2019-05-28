open X86Predicate
open DataStructures

(* This code creates the JIT scaffolding necessary for testing *)
let init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,f_blit,f_execute = JITSampler.create_setup ()

(* This code applies all test functions against expressions that have been
   turned into statements.  *)
let apply_all l_evalstmt stmt =
  if l_evalstmt = [] then invalid_arg "apply_all";
  let rec aux = function
  | (f,_,_)::fs -> if f stmt then aux fs else (*(Printf.printf "Eliminated %s\n" (string_of_stmt stmt);*)false
  | [] -> true
  in 
  aux l_evalstmt

let expand_single_and_apply_all l_evalstmt l_f_stmt acc expr =
  let stmts = List.rev_map (fun f -> f expr) l_f_stmt in
  List.fold_left (fun acc stmt -> if apply_all l_evalstmt stmt then stmt::acc else acc) acc stmts

let expand_and_apply_all l_evalstmt l_expr l_f_stmt =
  List.fold_left (expand_single_and_apply_all l_evalstmt l_f_stmt) [] l_expr

(* This code makes a function (stmt -> bool) given an input state *)
let make_fixed_test_statement_evaluator q_clobbered x86_cp instate =
  let outstate = f_execute instate in
  let f_stmt = 
    let open X86PredicateEvaluator in
    eval_stmt (y_combinator (eval_expr instate outstate)) instate outstate 
  in
  let q_clobbered = IOAnalysis.get_clobbered f_stmt q_clobbered in
  let x86_cp = IOAnalysis.join_x86_cp x86_cp (IOAnalysis.x86_cp_of_x86_state outstate) in
  (f_stmt,q_clobbered,x86_cp,instate,outstate)

(* Make an input state where all 32 and 1-bit values respectively match *)
let instate_all_same val32 val1 = 
  let open JITRegion in
  { eax = val32; ebx = val32; ecx = val32; edx = val32; esp = c_stack32; ebp = val32; esi = val32; edi = val32; 
    eflags = Int32.of_int (X86Misc.fl2eflags val1 val1 val1 val1 val1 val1 val1); }
    
(* Make such a function from a random input state *)
let make_random_test_statement_evaluator q_clobbered x86_cp =
  let instate = JITSampler.mk_random_state c_stack32 in
  make_fixed_test_statement_evaluator q_clobbered x86_cp instate

let expand_state_by_flag state f =
  let  mask32 = X86Misc.mask32_of_x86_flags f in
  let nmask32 = Int32.lognot mask32 in
  let open JITRegion in
  let flag_set  = { state with eflags = Int32.logor  state.eflags  mask32; } in
  let flag_nset = { state with eflags = Int32.logand state.eflags nmask32; } in
  (flag_set,flag_nset)

let expand_state_by_flags state l_f =
  let rec aux list = function
  | f::fs -> 
    let rec aux2 outlist = function
    | s::ss -> let s,n = expand_state_by_flag s f in aux2 (s::n::outlist) ss
    | [] -> outlist
    in aux (aux2 [] list) fs
  | [] -> list
  in aux [state] l_f

let expand_and_evaluate instate l_f list q cp =
  let instates = expand_state_by_flags instate l_f in
  List.fold_left 
   (fun (list,q,cp) instate -> 
      let f_stmt,q,cp,_,o = make_fixed_test_statement_evaluator q cp instate in
      ((f_stmt,instate,o)::list,q,cp))
   (list,q,cp)
    instates

(* Delete this when done testing *)
let exhaustive_al_cl l_f =
  let rec aux eax ecx q cp list =
    match eax,ecx with
    | 0x100l,0x100l -> (list,q,cp)
    | 0x100l,_ -> aux 0x0l (Int32.succ ecx) q cp list
    | _,_ -> 
      let instate = JITSampler.mk_random_state c_stack32 in
      let instate = { instate with JITRegion.eax = eax; JITRegion.ecx = ecx; } in
      let list,q,cp = expand_and_evaluate instate l_f list q cp in
      aux (Int32.succ eax) ecx q cp list
  in
  let list,q_clob,x86_cp = aux 0l 0l IOAnalysis.empty_clob IOAnalysis.default_x86_cp [] in
  let q_l_f_stmt,q_const = IOAnalysis.finalize_analysis q_clob x86_cp in
 (list,q_l_f_stmt,q_const)

(* Delete this when done testing *)
let exhaustive_al l_f =
  let rec aux eax q cp list =
    match eax with
    | 0x100l -> (list,q,cp)
    | _ -> 
      let instate = JITSampler.mk_random_state c_stack32 in
      let instate = { instate with JITRegion.eax = eax; } in      
      let list,q,cp = expand_and_evaluate instate l_f list q cp in
      aux (Int32.succ eax) q cp list
  in
  let list,q_clob,x86_cp = aux 0l IOAnalysis.empty_clob IOAnalysis.default_x86_cp [] in
  let q_l_f_stmt,q_const = IOAnalysis.finalize_analysis q_clob x86_cp in
 (list,q_l_f_stmt,q_const)

(* Delete this when done testing *)
let exhaustive_ax l_f =
  let rec aux eax q cp list =
    match eax with
    | 0x10000l -> (list,q,cp)
    | _ -> 
      let instate = JITSampler.mk_random_state c_stack32 in
      let instate = { instate with JITRegion.eax = eax; } in      
      let list,q,cp = expand_and_evaluate instate l_f list q cp in
      aux (Int32.succ eax) q cp list
  in
  let list,q_clob,x86_cp = aux 0l IOAnalysis.empty_clob IOAnalysis.default_x86_cp [] in
  let q_l_f_stmt,q_const = IOAnalysis.finalize_analysis q_clob x86_cp in
 (list,q_l_f_stmt,q_const)

(* Delete this when done testing *)
let exhaustive_ax_cx4 l_f =
  let rec aux eax ecx q cp list =
    match eax,ecx with
    | 0x10000l,0x10l -> (list,q,cp)
    | 0x10000l,_ -> aux 0x0l (Int32.succ ecx) q cp list
    | _,_ -> 
      let instate = JITSampler.mk_random_state c_stack32 in
      let instate = { instate with JITRegion.eax = eax; JITRegion.ecx = ecx; } in
      let list,q,cp = expand_and_evaluate instate l_f list q cp in
      aux (Int32.succ eax) ecx q cp list
  in
  let list,q_clob,x86_cp = aux 0l 0l IOAnalysis.empty_clob IOAnalysis.default_x86_cp [] in
  let q_l_f_stmt,q_const = IOAnalysis.finalize_analysis q_clob x86_cp in
 (list,q_l_f_stmt,q_const)

(* Generate N randomized statement evaluators, along with some fixed evaluators.
   Throughout this process, discover which locations are outputs (i.e. their
   output value differs from their input value), and which registers and flags
   are always set to a constant value. 
   
   Return the list of evaluators, a quadruple of lists of functions that make
   statements from expressions (i.e., if eax is the only output location, then
   the 32-bit component of the quadruple will be 
   (fun x -> RegEquals(X86Reg(X86.Gd(X86.Eax)),x))), and a quadruple containing
   sets of those locations that are constant. *)
let mk_random_test_statement_evaluators n =
  let rec aux i list q_clob x86_cp =
    if i = n
    then list,q_clob,x86_cp
    else 
      let f_stmt,q_clob,x86_cp,instate,outstate = make_random_test_statement_evaluator q_clob x86_cp in
      aux (i+1) ((f_stmt,instate,outstate)::list) q_clob x86_cp
  in 
  let list,q_clob,x86_cp = aux 0 [] IOAnalysis.empty_clob IOAnalysis.default_x86_cp in
  let rec aux2 list q_clob x86_cp = function
  | x::xs -> 
    let f_stmt,q_clob,x86_cp,instate,outstate = make_fixed_test_statement_evaluator q_clob x86_cp x in
    aux2 ((f_stmt,instate,outstate)::list) q_clob x86_cp xs 
  | [] -> list,q_clob,x86_cp
  in
  let list,q_clob,x86_cp = 
    let open JITRegion in 
    aux2
      list 
      q_clob 
      x86_cp
     [instate_all_same 0l 0;
      instate_all_same 0l 1;
      instate_all_same 0x80000000l 0;
      instate_all_same 0x80000000l 1;
      instate_all_same 0xffffffffl 0;
      instate_all_same 0xffffffffl 1;
   { (instate_all_same 0x80000000l 0) with eax = 0l; };
   { (instate_all_same 0x80000000l 1) with eax = 0l; };
      ]
  in
  let q_l_f_stmt,q_const = IOAnalysis.finalize_analysis q_clob x86_cp in
 (list,q_l_f_stmt,q_const)

(* Some syntactic pruning to discard candidates with undesirable patterns 
   (constants-in-disguise, arithmetic identities). *)
let rec keep = function
| X86Flag(_) -> true
| X86Reg(_) -> true
| BitImm(_) -> true
| Imm(_) -> true

(* Don't do constant folding *)
| Binop(Imm(_),_,Imm(_)) -> false
| Binop(BitImm(_),_,BitImm(_)) -> false

(* Equivalent to smaller Sub *)
| Binop(Binop(l1,Or,r1),Add,Binop(l2,And,r2)) when (l1 = l2 && r1 = r2) || (l1 = r2 && r1 = l2) -> false
| Binop(Binop(l1,And,r1),Add,Binop(l2,Or,r2)) when (l1 = l2 && r1 = r2) || (l1 = r2 && r1 = l2) -> false
| Binop(Binop(l1,Add,r1),And,Binop(l2,Or,r2)) when (l1 = l2 && r1 = r2) || (l1 = r2 && r1 = l2) -> false
| Binop(Binop(l1,Or,r1),And,Binop(l2,Add,r2)) when (l1 = l2 && r1 = r2) || (l1 = r2 && r1 = l2) -> false

(* Sets to zero *)
| Binop(Unop(Not,l),And,r) when l = r -> false
| Binop(l,And,Unop(Not,r)) when l = r -> false
| Binop(l,Xor,r) when l = r -> false
| Binop(l,Sub,r) when l = r -> false
| Binop(Binop(l1,o1,r1),Xor,Binop(l2,o2,r2)) when o1 = o2 && is_commutative o1 && l1 = r2 && r1 = l2 -> false

(* Equivalent to smaller Add *)
| Binop(_,Sub,Unop(Neg,_)) -> false

(* Equivalent to smaller Sub *)
| Binop(Unop(Neg,_),Add,_) -> false
| Binop(_,Add,Unop(Neg,_)) -> false

(* Produces -1 *)
| Binop(Unop(Not,l),Add,r) when l = r -> false
| Binop(l,Add,Unop(Not,r)) when l = r -> false
| Binop(Unop(Not,l),Or,r)  when l = r -> false
| Binop(l,Or,Unop(Not,r))  when l = r -> false
| Binop(Unop(Not,l),Xor,r) when l = r -> false
| Binop(l,Xor,Unop(Not,r)) when l = r -> false
| Binop(Unop(Dec,l),Sub,r) when l = r -> false
| Binop(l,Sub,Unop(Inc,r)) when l = r -> false

(* Equivalent to smaller term-in-itself*) 
| Binop(l,Or, r) when l = r -> false
| Binop(l,And,r) when l = r -> false
| Binop(Binop(l1,o1,r1),Or, Binop(l2,o2,r2)) when o1 = o2 && is_commutative o1 && l1 = r2 && r1 = l2 -> false
| Binop(Binop(l1,o1,r1),And,Binop(l2,o2,r2)) when o1 = o2 && is_commutative o1 && l1 = r2 && r1 = l2 -> false

| Binop(l,o,r) -> keep l && keep r

(* Don't fold constants *)
| Unop(_,Imm(_)) -> false
| Unop(_,BitImm(_)) -> false

(* Equivalent to swapped Sub *)
| Unop(Neg,Binop(_,Sub,_)) -> false

| Unop(_,e) -> keep e
| Bitop(_,Imm(_)) -> false
| Bitop(_,e) -> keep e

| Extend(_,_,e) -> keep e
| ITE(b,t,f) -> keep b && keep t && keep f

(* If we end up with multiple equivalent candidates, we want to select the 
   "smallest" one according to some metric.  This is that metric. *)
let rec score expr = let open X86Predicate in match expr with 
| X86Flag(_)         -> 1
| X86Reg(_)          -> 1
| BitImm(_)          -> 1
| Imm(X86.Id(i32))   -> if i32 = 0l then 1 else 3
| Imm(X86.Iw(i16))   -> if i16 = 0l then 1 else 3
| Imm(X86.Ib(i8 ))   -> if i8  = 0l then 1 else 3
| Binop(l,(Eq|Ne),r) -> 1 + (score l) + (score r)
| Binop(l,(And|Or|Xor),r)      -> 3 + (score l) + (score r)
| Binop(l,(Slt|Sle|Ult|Ule),r) -> 7 + (score l) + (score r)
| Binop(l,_,r)       -> 15 + (score l) + (score r)
| Unop(_,e)          -> 1 + (score e)
| Bitop(FifthBit,e)  -> 1 + (score e)
| Bitop(Parity,e)    -> 1 + (score e)
| Bitop(SignBit,e)   -> 50 + (score e)
| Bitop(SecondBit,e) -> 50 + (score e)
| Extend(Low,_,e)    -> 1000 + (score e)
| Extend(_,_,e)      -> 20 + (score e)
| ITE(b,t,f)         -> 1000 + (score b) + (score t) + (score f)

(* Given a quadruple of candidates, for each location, collect all expressions
   and store them in a list.  Return a quadruple of hash tables with this
   information in them. *)
let refine_candidates q_cand =
  let ht32 = Hashtbl.create 17 in
  let ht16 = Hashtbl.create 17 in
  let ht8  = Hashtbl.create 17 in
  let ht1  = Hashtbl.create 17 in
  let add h k v =
    let res = try Hashtbl.find h k with Not_found -> [] in
    Hashtbl.replace h k (v::res)
  in
  let aux = function
  | FlagEquals(f,e) -> add ht1 f e
  | RegEquals(X86.Gd(r32),e) -> add ht32 r32 e
  | RegEquals(X86.Gw(r16),e) -> add ht16 r16 e
  | RegEquals(X86.Gb(r8) ,e) -> add ht8  r8  e
  | ImmEquals(_,e) -> Printf.printf "Skipping imm equals for now\n"
  in
  List.iter aux q_cand.val32;
  List.iter aux q_cand.val16;
  List.iter aux q_cand.val8;
  List.iter aux q_cand.val1;
  {
    val32 = ht32;
    val16 = ht16;
    val8  = ht8;
    val1  = ht1;
  }

(* While distinguishing candidates, if the theorem prover returns a query, this
   function is responsible for turning that query into an x86 state. *)
let reify_counterexample l = 
  let instate = JITSampler.mk_random_state c_stack32 in
  let update f32 p b =
    let i32 = Int32.shift_left 1l p in
    if b
    then Int32.logor f32 i32
    else Int32.logand f32 (Int32.lognot i32)
  in
  let open JITRegion in
  let aux s = function
  | RegEquals(X86.Gd(X86.Eax),Imm(X86.Id(v32))) -> { s with eax = v32; }
  | RegEquals(X86.Gd(X86.Ebx),Imm(X86.Id(v32))) -> { s with ebx = v32; }
  | RegEquals(X86.Gd(X86.Ecx),Imm(X86.Id(v32))) -> { s with ecx = v32; }
  | RegEquals(X86.Gd(X86.Edx),Imm(X86.Id(v32))) -> { s with edx = v32; }
  | RegEquals(X86.Gd(X86.Esp),Imm(X86.Id(v32))) -> { s with esp = v32; }
  | RegEquals(X86.Gd(X86.Ebp),Imm(X86.Id(v32))) -> { s with ebp = v32; }
  | RegEquals(X86.Gd(X86.Esi),Imm(X86.Id(v32))) -> { s with esi = v32; }
  | RegEquals(X86.Gd(X86.Edi),Imm(X86.Id(v32))) -> { s with edi = v32; }
  | FlagEquals(X86.X86F_C,BitImm(b)) -> { s with eflags = update s.eflags  0 b }
  | FlagEquals(X86.X86F_P,BitImm(b)) -> { s with eflags = update s.eflags  2 b }
  | FlagEquals(X86.X86F_A,BitImm(b)) -> { s with eflags = update s.eflags  4 b }
  | FlagEquals(X86.X86F_Z,BitImm(b)) -> { s with eflags = update s.eflags  6 b }
  | FlagEquals(X86.X86F_S,BitImm(b)) -> { s with eflags = update s.eflags  7 b }
  | FlagEquals(X86.X86F_D,BitImm(b)) -> { s with eflags = update s.eflags 10 b }
  | FlagEquals(X86.X86F_O,BitImm(b)) -> { s with eflags = update s.eflags 11 b }
  | _ -> invalid_arg "reify_counterxample"
  in 
  List.fold_left aux instate l

(* Takes an (expr * expr list) option and an expr.  Either:
   1) Eliminate the first and return Some(second)
   2) Eliminate the second and return o_current
   3) Decide that they are indistinct and return Some(first,second::list) *)
let deathmatch f_stmt_maker f_dist f o_current e_challenger =
  match o_current with
  | None -> Some(e_challenger,[])
  | Some(e_current,l_indistinct) -> 
    let x = f_dist f e_challenger e_current in
  (*Printf.printf "Distinguishing %s from %s\n" (string_of_expr e_current) (string_of_expr e_challenger);*)
    match x with
    | [] -> 
      if score e_current <= score e_challenger
      then Some(e_current,e_challenger::l_indistinct)
      else Some(e_challenger,e_current::l_indistinct)
    | l  -> let c = reify_counterexample l in
      let outstate = f_execute c in
      let f_stmt = 
        let open X86PredicateEvaluator in
        eval_stmt (y_combinator (eval_expr c outstate)) c outstate 
      in
      let s_el  = f_stmt_maker f e_challenger in
      let b_el  = f_stmt s_el in
      let s_el2 = f_stmt_maker f e_current in
      let b_el2 = f_stmt s_el2 in
    (*Printf.printf "Distinguishing %s from %s using counterexample:\n" (string_of_stmt s_el) (string_of_stmt s_el2);
      List.iter (fun s -> Printf.printf "%s\n" (string_of_stmt s)) l;*)
      match b_el,b_el2 with
      (* Indicates a bug *)
      | true,true   -> 
        Printf.printf 
          "Input did not distinguish?\nChal: %s\nCurr: %s\nState before:\n" 
            (string_of_stmt s_el)
            (string_of_stmt s_el2); 
        JITSampler.print_x86ctx c;
        Printf.printf "State after:\n";
        JITSampler.print_x86ctx outstate;
        List.iter (fun s -> Printf.printf "%s\n" (string_of_stmt s)) l;
        None
      | true,false  -> 
      (*Printf.printf "Eliminated %s\n" (string_of_stmt s_el2);*)
        Some(e_challenger,[])
      | false,true  -> 
      (*Printf.printf "Eliminated %s\n" (string_of_stmt s_el);*)
        Some(e_current,l_indistinct)
      (* Possible for non-booleans *)
      | false,false -> None

let deathmatch_single f_stmt_maker f_dist expr1 = deathmatch f_stmt_maker f_dist (Some(expr1,[]))

(* This is the SMT magic.  Given a hash table location -> [candidates], winnow
   and merge the candidates based upon whether the theorem prover says that 
   they are identical.  The output is one pruned list of candidates, sorted
   by score, where the candidates passed all the checks and are assumed after
   this point to represent the true behaviors of the instructions. *)
let minimize_candidates ht_cand f_stmt_maker f_dist =
  let ht_new = Hashtbl.create 17 in
  Hashtbl.iter (fun f l -> 
    let scres = 
      match (List.fold_left (deathmatch f_stmt_maker f_dist f) (None) l) with
      | Some(x,y) -> x::y
      | None -> []
    in
    let scres  = List.map (fun x -> (score x,x)) scres in
    let sorted = List.sort (fun (s1,_) (s2,_) -> Pervasives.compare s1 s2) scres in
  (*List.iter (fun (s,e) -> Printf.printf "%d: %s\n" s (string_of_expr e)) sorted;*)
    let res    = List.map snd sorted in
    Hashtbl.replace ht_new f res)
    ht_cand;
  ht_new

(* Applies minimization to candidate of a specified size *)
let minimize_candidates_by_size d q sz = 
  let open X86Predicate in
  let open Z3Integration in
  match sz with
  | S32 -> { q with val32 = (minimize_candidates q.val32 (fun x e -> RegEquals(X86.Gd(x),e)) (distinguish_reg32 d)); }
  | S16 -> { q with val16 = (minimize_candidates q.val16 (fun x e -> RegEquals(X86.Gw(x),e)) (distinguish_reg16 d)); }
  | S8  -> { q with val8  = (minimize_candidates q.val8  (fun x e -> RegEquals(X86.Gb(x),e)) (distinguish_reg8  d)); }
  | S1  -> { q with val1  = (minimize_candidates q.val1  (fun x e -> FlagEquals(x,e))        (distinguish_flag  d)); }

(* This function takes as input a list of statement evaluators, a list of
   statement-making functions as described previously, a sequential generator
   object, and a size.  It generates all candidate expressions, generates all
   statements corresponding to those expressions being equal to some final
   location, and then applies the statement evaluators to them.  Those 
   statements that satisfy all evaluators are returned in a list. *)
let try_all l_evalstmt q_l_f_stmt seq_g esize = 
  let rec aux quad =
    let _,e = seq_g#yield_current () in
  (*Printf.printf "%s\n" (string_of_expr e);*)
    let quad =
      if keep e
      then 
        update_quad 
          quad 
         (expand_single_and_apply_all l_evalstmt (select_quad q_l_f_stmt esize) (select_quad quad esize) e)
          esize
      else quad
    in
    let continue =
      try (seq_g#increase_current (); true)
      with X86PredicateGenerator.Lapsed -> false
    in
    if continue then aux quad else quad
  in
  aux { val32 = []; val16 = []; val8 = []; val1 = [] }

(* Print the verified behaviors for all locations. *)
let print_results q_res =
  let print ht f_descr = 
    Hashtbl.iter (fun f l -> List.iter (fun x -> Printf.printf "%s: %s\n" (f_descr f) (string_of_expr x)) l)
    (*let str = match l with x::_ -> string_of_expr x | [] -> "NONE!" in
      Printf.printf "%s: %s\n" (f_descr f) str)*)
      ht
  in
  print q_res.val32 X86Disasm.string_of_x86_reg32;
  print q_res.val16 X86Disasm.string_of_x86_reg16;
  print q_res.val8  X86Disasm.string_of_x86_reg8;
  print q_res.val1  X86Disasm.string_of_x86_flags

(* The top-level function.  Given a quadruple of templates, a quadruple of 
   registers and a quadruple of atoms for use in filling the templates, and
   an x86 instruction, JIT the instruction, make N=5000 randomized evaluators
   based upon it, generate all expressions as specified by the templates and
   filling values, then apply the SMT-based minimization.  Repeat for the 
   flags. Return a hash table containing the final candidates. *)
let divine_semantics f_make bsmt q_templates q_reg_atoms q_imm_atoms instr size = 
  let _ = JITSampler.blit_instr f_blit instr in
  let _ = Printf.printf "%s:\n%!" (X86Disasm.string_of_x86instr instr) in
  let l_evalstmt,q_l_f_stmt,q_const = f_make () in
  let blah2 = Z3Integration.mk_blah2 [("MODEL", "true");("SOFT_TIMEOUT", "10000")] in

  let q = 
    let f size = function
    | [] -> { val32 = []; val16 = []; val8 = []; val1 = []; }
    | x  -> try_all l_evalstmt q_l_f_stmt (X86PredicateGenerator.seq_generator x q_reg_atoms q_imm_atoms) size
    in
    match size with
    | S32 -> f size q_templates.val32
    | S16 -> f size q_templates.val16
    | S8  -> f size q_templates.val8
    | S1  -> f size q_templates.val1
  in
  let q_r_cand = refine_candidates q in
  let q_r_cand = if bsmt then minimize_candidates_by_size blah2 q_r_cand size else q_r_cand in 
  
  let res = 
    match q_templates.val1 with
    | [] -> q_r_cand
    | _  ->
      let el = 
        match size with 
        | S32 -> q_reg_atoms.val32 
        | S16 -> q_reg_atoms.val16 
        | S8  -> q_reg_atoms.val8 
        | S1  -> q_reg_atoms.val1
      in
      let el =
        try
          match size with
          | S32 -> List.hd (Hashtbl.find q_r_cand.val32 X86.Eax)::el
          | S16 -> List.hd (Hashtbl.find q_r_cand.val16 X86.Ax)::el
          | S8  -> List.hd (Hashtbl.find q_r_cand.val8  X86.Al)::el
          | S1  -> el
        with _ -> el
      in
      let q_reg_atoms_extended = update_quad q_reg_atoms el size in
(*    Printf.printf "Chose %s for the base component\n" (string_of_expr expr);*)
      
      let seq_g = X86PredicateGenerator.seq_generator q_templates.val1 q_reg_atoms_extended q_imm_atoms in
      let q = try_all l_evalstmt q_l_f_stmt seq_g S1 in
    (*List.iter (fun s -> Printf.printf "%s\n%!" (string_of_stmt s)) q.val1;
      
      Printf.printf "Candidates\n";
      List.iter (fun s -> Printf.printf "%s\n" (string_of_stmt s)) q.val1;
      Printf.printf "\n";*)

      let q1 = { val32 = []; val16 = []; val8 = []; val1 = q_const.val1@q.val1; } in
      let q_r_cand1 = refine_candidates q1 in
      let q_r_cand1 = if bsmt then minimize_candidates_by_size blah2 q_r_cand1 S1 else q_r_cand1 in 
      { val32 = q_r_cand.val32; val16 = q_r_cand.val16; val8 = q_r_cand.val8; val1 = q_r_cand1.val1; }
  in
  print_results res;
  res

let divine_semantics_exhaustive_8bit bcf = divine_semantics (fun _ -> exhaustive_al_cl bcf) false
let divine_semantics_random n = divine_semantics (fun _ -> mk_random_test_statement_evaluators n) true

let infinite_test l =
  let rec aux i j =
    if i = 1000000
    then 
      let j = j + 1 in
      Printf.printf "%dM tests done\n%!" j;
      aux 0 j
    else
      let rec aux2 = function
      | (i,l)::xs ->
        let _ = JITSampler.blit_instr f_blit i in
        let instate = JITSampler.mk_random_state c_stack32 in
        let f_stmt,_,_,_,_ = make_fixed_test_statement_evaluator IOAnalysis.empty_clob IOAnalysis.default_x86_cp instate in
        List.iter 
         (fun stmt -> 
            if not (f_stmt stmt) 
            then
              let _ = Printf.printf "%s :: %s FAILED!\n" (X86Disasm.string_of_x86instr i) (string_of_stmt stmt) in
              JITSampler.print_x86ctx instate)
          l;
        aux2 xs
      | [] -> aux (i+1) j
      in aux2 l
  in
  aux 0 0
