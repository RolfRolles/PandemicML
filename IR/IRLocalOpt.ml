open IR
open PpIR
open IRUtil

(* Recursively extract every variable that is used in an expression; return a list *)
let rec extract_uses = function
| Cast(_,_,e)
| Unop(_,e) -> 
  extract_uses e

| Binop(e1,_,e2)
| Load(e1,e2,_) -> 
  let l1 = extract_uses e1 in
  let l2 = extract_uses e2 in
  l1@l2
                  
| Store(e1,e2,e3,_) -> 
  let l1 = extract_uses e1 in
  let l2 = extract_uses e2 in
  let l3 = extract_uses e3 in
  (l1@l2@l3)

| Var(v) -> [v]

| Let(v,e1,e2) -> 
  let l1 = extract_uses e1 in
  let l2 = extract_uses e2 in
  v::(l1@l2)

| Const(_) -> []

let rec replace_var_with_var tbl e = 
  let rec aux = function
  | Cast(a,b,e)       -> Cast(a, b, aux e)
  | Unop(a,e)         -> Unop(a, aux e)
  | Binop(e1,o,e2)    -> Binop(aux e1, o, aux e2)
  | Load(e1,e2,r)     -> Load(aux e1, aux e2, r)
  | Store(e1,e2,e3,r) -> Store(aux e1, aux e2, aux e3, r)
  | Var(v) as e       -> let v = try Var(Hashtbl.find tbl v) with Not_found -> e in v
  | Const(_) as c     -> c
  | Let(v,e1,e2)      -> Let(v, aux e1, aux e2)
  in aux e

(* b:  replace assignment targets *)
let replace_instr_var_with_var b tbl = function
| Assign(a,e)     -> Assign((if b then (try Hashtbl.find tbl a with Not_found -> a) else a), replace_var_with_var tbl e)
| CJmp(e1,e2,e3)  -> CJmp(replace_var_with_var tbl e1, replace_var_with_var tbl e2, replace_var_with_var tbl e3)
| Jmp(e)          ->  Jmp(replace_var_with_var tbl e)
| Halt(e)         -> Halt(replace_var_with_var tbl e)
| Assert(e)       -> Assert(replace_var_with_var tbl e)
| Label(_)
| Special(_)
| Comment(_) as i -> i

let rec replace_var_with_expr tbl e = 
  let rec aux = function
  | Cast(a,b,e)       -> Cast(a, b, aux e)
  | Unop(a,e)         -> Unop(a, aux e)
  | Binop(e1,o,e2)    -> Binop(aux e1, o, aux e2)
  | Load(e1,e2,r)     -> Load(aux e1, aux e2, r)
  | Store(e1,e2,e3,r) -> Store(aux e1, aux e2, aux e3, r)
  | Var(v) as e       -> let v = try Hashtbl.find tbl v with Not_found -> e in v
  | Const(_) as c     -> c
  | Let(v,e1,e2)      -> Let(v, aux e1, aux e2)
  in aux e

let replace_instr_var_with_expr tbl = function
| Assign(a,e)     -> Assign(a, replace_var_with_expr tbl e)
| CJmp(e1,e2,e3)  -> CJmp(replace_var_with_expr tbl e1, replace_var_with_expr tbl e2, replace_var_with_expr tbl e3)
| Jmp(e)          ->  Jmp(replace_var_with_expr tbl e)
| Halt(e)         -> Halt(replace_var_with_expr tbl e)
| Assert(e)       -> Assert(replace_var_with_expr tbl e)
| Label(_)
| Special(_)
| Comment(_) as i -> i

(* Returns a pair:  (Some(defined var) or None, {used vars}) *)
let extract_uses_and_defs = function
| Assign(v,e)    -> (Some v, var_setify  (extract_uses e))
| CJmp(e1,e2,e3) -> (None  , var_setify ((extract_uses e1)@(extract_uses e2)@(extract_uses e3)))
| Jmp(e)
| Halt(e)
| Assert(e)  -> (None, var_setify (extract_uses e))
| Label(_)
| Special(_)
| Comment(_) -> (None, VarSet.empty)

let i64_of_bool = function
| true  -> 1L
| false -> 0L

let sign_extend_byte = function
| x when x <= 0x7FL -> x
| x -> Int64.logor  x 0xFFFFFFFFFFFFFF00L
let sign_extend_word = function
| x when x <= 0x7FFFL -> x
| x -> Int64.logor  x 0xFFFFFFFFFFFF0000L
let sign_extend_dword = function
| x when x <= 0x7FFFFFFFL -> x
| x -> Int64.logor  x 0xFFFFFFFF00000000L

let ucomp clhs crhs c = 
  let c = match c with
  | ULT -> ( < )
  | ULE -> ( <= )
  | _ -> invalid_arg "ucomp"
  in
  let b = 
    if clhs >= 0L && crhs >= 0L then c clhs crhs else
    if clhs <  0L && crhs >= 0L then true else
    if crhs <  0L && clhs >= 0L then false else
    not (c clhs crhs)
  in Const(i64_of_bool b,TypeReg_1)

let scomp clhs crhs c s = 
  let c = match c with
  | SLT -> ( < )
  | SLE -> ( <= )
  | _ -> invalid_arg "scomp"
  in 
  let b = match s with
  | TypeReg_1  -> invalid_arg "scomp: Could probably handle this, but will it ever happen?"
  | TypeReg_8  -> c (sign_extend_byte  clhs) (sign_extend_byte  crhs)
  | TypeReg_16 -> c (sign_extend_word  clhs) (sign_extend_word  crhs)
  | TypeReg_32 -> c (sign_extend_dword clhs) (sign_extend_dword crhs)
  | TypeReg_64 -> c clhs crhs
  in Const(i64_of_bool b,TypeReg_1)
  
let sign_extend c = function
| TypeReg_1  -> invalid_arg "sign_extend: TypeReg_1"
| TypeReg_8  -> sign_extend_byte c
| TypeReg_16 -> sign_extend_word c
| TypeReg_32 -> sign_extend_dword c
| TypeReg_64 -> c
  
let do_signed_cast c1 s1 s =
  let c =
    if (Int64.logand (mk_sign_const_i64 s1) c1) = 0L 
    then c1
    else Int64.logor c1 (Int64.logxor (mk_max_const_i64 s) (mk_max_const_i64 s1))
  in Const(c,s)
  
let do_high_cast c1 s1 s =
  let s1 = IRUtil.bits s1 in
  let s' = IRUtil.bits s  in
  if  s'> s1 then failwith "do_high_cast: typechecking prevents this";
  Const(Int64.shift_right_logical c1 (s1-s'),s) (* ' *)

let rec fold_expr_constants = function
| Binop(_,And,Const(0L,s))              -> Const(0L,s)
| Binop(x,And,Const(-1L,s))             -> fold_expr_constants x
| Binop(Const(0L,s),Xor,x)              -> fold_expr_constants x
| Binop(x,Sub,Const(0L,s))              -> fold_expr_constants x
| Binop(Const(0L,s),And,_)              -> Const(0L,s)
| Binop(Const(0L,s),Add,x)              -> fold_expr_constants x
| Binop(Const(0L,s),Sub,x)              -> Unop(Neg,fold_expr_constants x)
| Binop(Const(0L,s),Or ,x)              -> fold_expr_constants x
| Binop(Const(i,s),Add,Unop(Not,x))     -> Binop(Const(Int64.sub i 1L,s),Add,Unop(Neg,fold_expr_constants x))
| Unop(Neg,Unop(Neg,x))                 -> fold_expr_constants x
| Binop(Const(c1,s1),Add,Const(c2,s2))  -> Const(truncate_to s1 (Int64.add c1 c2),s1)
| Binop(Const(c1,s1),Sub,Const(c2,s2))  -> Const(truncate_to s1 (Int64.sub c1 c2),s1)
| Binop(Const(c1,s1),And,Const(c2,s2))  -> Const(Int64.logand c1 c2,s1)
| Binop(Const(c1,s1),Xor,Const(c2,s2))  -> Const(Int64.logxor c1 c2,s1)
| Binop(Const(c1,s1), Or,Const(c2,s2))  -> Const(Int64.logor  c1 c2,s1)
| Binop(Const(c1,s1), EQ,Const(c2,s2))  -> let res = if c1 =  c2 then 0x1L else 0x0L in Const(res,TypeReg_1)
| Binop(Const(c1,s1), NE,Const(c2,s2))  -> let res = if c1 <> c2 then 0x1L else 0x0L in Const(res,TypeReg_1)
| Binop(Const(c1,s1),ULT,Const(c2,s2))  -> ucomp c1 c2 (ULT)
| Binop(Const(c1,s1),ULE,Const(c2,s2))  -> ucomp c1 c2 (ULE)
| Binop(Const(c1,s1),SLT,Const(c2,s2))  -> scomp c1 c2 (SLT) s1
| Binop(Const(c1,s1),SLE,Const(c2,s2))  -> scomp c1 c2 (SLE) s1
| Binop(Const(c1,s1),Mul,Const(c2,s2))  -> Const(truncate_to s1 (Int64.mul c1 c2),s1)
| Unop(Not,Const(c1,s1))                -> Const(truncate_to s1 (Int64.logxor (-1L) c1),s1)
| Unop(Neg,Const(c1,s1))                -> Const(truncate_to s1 (Int64.add 1L (Int64.logxor (-1L) c1)),s1)
| Cast(_,s,e) when IRTypeCheck.type_of_integer_type (IRTypeCheck.typecheck_expr e) = s -> fold_expr_constants e
| Cast(Low,s,Const(c1,s1))              -> Const(truncate_to s c1,s)
| Cast(Unsigned,s,Const(c1,s1))         -> Const(c1,s)
| Cast(Signed,s,Const(c1,s1))           -> do_signed_cast c1 s1 s
| Cast(High,s,Const(c1,s1))             -> do_high_cast c1 s1 s
| Binop(Const(c1,s1),Shl,Const(c2,s2))  -> Const(truncate_to s1 (Int64.shift_left c1 (Int64.to_int c2)),s1)
| Binop(Const(c1,s1),Shr,Const(c2,s2))  -> Const(truncate_to s1 (Int64.shift_right_logical c1 (Int64.to_int c2)),s1)
| Binop(Const(c1,s1),Sar,Const(c2,s2))  -> Const(truncate_to s1 (Int64.shift_right (sign_extend c1 s1) (Int64.to_int c2)),s1)
| Binop(x,Add,Const(c,s))               -> Binop(Const(c,s),Add,fold_expr_constants x)
| Binop(x,Or ,Const(c,s))               -> Binop(Const(c,s),Or, fold_expr_constants x)
| Binop(x,Add,Binop(y,Add,z))           -> Binop(Binop(fold_expr_constants x,Add,fold_expr_constants y),Add,fold_expr_constants z)
| Binop(x,Or ,Binop(y,Or ,z))           -> Binop(Binop(fold_expr_constants x,Or ,fold_expr_constants y),Or ,fold_expr_constants z)
| Binop(Const(c1,s1),Xor,Binop(Const(c2,s2),Xor,z))          -> Binop(Const(Int64.logxor c1 c2,s1),Xor,fold_expr_constants z)
| Binop(x,Xor,Const(c,s))               -> Binop(Const(c,s),Xor,fold_expr_constants x)
(* These three cause an infinite loop, must resolve *)
(*
| Binop(x,Sub,Const(c1,s1))             -> Binop(Const(truncate_to s1 (Int64.logxor (-1L) c1),s1),Add,x)
*)
(* Have to AND the shiftand *)
| Binop(x,o,y)                          -> Binop(fold_expr_constants x, o, fold_expr_constants y)
| Unop(o,x)                             -> Unop(o, fold_expr_constants x)
| Load(e1,e2,tr)                        -> Load(fold_expr_constants e1, fold_expr_constants e2, tr)
| Cast(a,b,e)                           -> Cast(a, b, fold_expr_constants e)
| Store(e1,e2,e3,r)                     -> Store(fold_expr_constants e1, fold_expr_constants e2, fold_expr_constants e3, r)
| Var(v) as e                           -> e
| Const(_) as c                         -> c
| Let(v,e1,e2)                          -> Let(v, fold_expr_constants e1, fold_expr_constants e2)

let bool2e = function
| true  -> Const(0x1L,TypeReg_1)
| false -> Const(0x0L,TypeReg_1)

let fold_const_binop b = match b with
| Binop(Const(c1,s1),Add,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.add c1 c2),s1),true)
| Binop(Const(c1,s1),Sub,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.sub c1 c2),s1),true)
| Binop(Const(c1,s1),And,Const(c2,s2))  -> (Const(Int64.logand c1 c2,s1),true)
| Binop(Const(c1,s1),Xor,Const(c2,s2))  -> (Const(Int64.logxor c1 c2,s1),true)
| Binop(Const(c1,s1), Or,Const(c2,s2))  -> (Const(Int64.logor  c1 c2,s1),true)
| Binop(Const(c1,s1), EQ,Const(c2,s2))  -> (bool2e (c1 =  c2),true)
| Binop(Const(c1,s1), NE,Const(c2,s2))  -> (bool2e (c1 <> c2),true)
| Binop(Const(c1,s1),ULT,Const(c2,s2))  -> (ucomp c1 c2 (ULT),true)
| Binop(Const(c1,s1),ULE,Const(c2,s2))  -> (ucomp c1 c2 (ULE),true)
| Binop(Const(c1,s1),SLT,Const(c2,s2))  -> (scomp c1 c2 (SLT) s1,true)
| Binop(Const(c1,s1),SLE,Const(c2,s2))  -> (scomp c1 c2 (SLE) s1,true)
| Binop(Const(c1,s1),Mul,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.mul c1 c2),s1),true)
| Binop(Const(c1,s1),Shl,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_left c1 (Int64.to_int c2)),s1),true)
| Binop(Const(c1,s1),Shr,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right_logical c1 (Int64.to_int c2)),s1),true)
| Binop(Const(c1,s1),Sar,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right (sign_extend c1 s1) (Int64.to_int c2)),s1),true)
| _ -> (b,false)

let fold_const_unop u = match u with
| Unop(Not,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.logxor (-1L) c1),s1),true)
| Unop(Neg,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.add 1L (Int64.logxor (-1L) c1)),s1),true)
| _ -> (u,false)

let fold_const_cast c = match c with
| Cast(Low,s,Const(c1,s1))              -> (Const(truncate_to s c1,s),true)
| Cast(Unsigned,s,Const(c1,s1))         -> (Const(c1,s),true)
| Cast(Signed,s,Const(c1,s1))           -> (do_signed_cast c1 s1 s,true)
| Cast(High,s,Const(c1,s1))             -> (do_high_cast c1 s1 s,true)
| _ -> (c,false)
  
let ic = function | Const(_,_) -> true | _ -> false

let fold_instr_constants = function
| Assign(a,e)     -> Assign(a,fold_expr_constants e)
| CJmp(e1,e2,e3)  -> CJmp(fold_expr_constants e1, fold_expr_constants e2, fold_expr_constants e3)
| Jmp(e)          -> Jmp(fold_expr_constants e)
| Halt(e)         -> Halt(fold_expr_constants e)
| Assert(e)       -> Assert(fold_expr_constants e)
| Label(_)
| Special(_)
| Comment(_) as i -> i

(* O(N) local dead statement elimination *)
let local_dse instrs live =
  let live = var_setify live in
  (* Reverse the list of instructions, as this analysis works backwards *)
  let revinstrs = List.rev instrs in
  (* Calculate a list of booleans which indicates whether or not to kill the 
     corresponding instruction *)
  let _,kill_list = 
    List.fold_left
      (fun (live_vars,kill_list) instr ->
        let d,u = extract_uses_and_defs instr in
        (* Only kill a statement if it is an assign to a variable that is not live *)
        let kill_statement = match d with
        | Some(d) -> not (VarSet.mem d live_vars)
        | None -> false
        in
        (* Remove the defined variable, then add all the used ones, to the live set *)
        let live_vars = 
          if (not kill_statement) 
          then 
            match d with 
            | Some(d) -> VarSet.union u (VarSet.remove d live_vars)
            | None    -> VarSet.union u live_vars
          else live_vars
        in
        (live_vars,kill_statement::kill_list))
      (* Start with the live set from above (all registers and memory), and the empty list *)
      (live,[])
      (* Iterate over the reversed list of instructions *)
      revinstrs
  in
  (* Walk along the kill list and the instructions simultaneously, and do not add
     the instruction to the resulting list if we're supposed to kill it.  Returns
     the instructions in reversed order from what they're passed in as, and as they're
     passed in reversed to begin with, it returns them in the proper order. *)
  let rec remove_instrs instrlist killlist outlist = 
    match (instrlist,killlist) with
    | i::is,k::ks -> remove_instrs is ks (if (not k) then (i::outlist) else (outlist))
    | [], []      -> outlist
    | _,_         -> failwith "something terrible happened"
  in
  (* Finally, return the list of instructions minus the dead statements. *)
  remove_instrs revinstrs (List.rev kill_list) []

let extract_constant_and_copy = function
| Assign(v1,Var(v2))         -> Some(v1,Some(v2),None)
| Assign(v1,(Const(_) as c)) -> Some(v1,None,Some(c))
| Assign(v1,_)               -> Some(v1,None,None)
| _                          -> None

(* O(N) local copy and constant propagation *)
let local_opt v2cmap instrs =
  (* "Left to right" mapping.  If we have x = y, then this map stores x -> y, 
     until either x or y have been reassigned. *)
  let l2rmap = Hashtbl.create 50 in
  (* "Right to left" mapping.  For each variable y, this map stores the set of 
     all of the variables x such that x = y was executed within the basic block, 
     with neither x nor y having been reassigned up until this point. *)
  let r2lmap = Hashtbl.create 50 in
  (* "Var to constant" mapping.  For each constant assignment x = c, this map 
     stores x -> c, until x is reassigned. *)
  let v2cmap = v2cmap in
  
  (* When a variable y gets reassigned, we query r2lmap for the set of variables 
     to which y had been assigned, then remove each of those mappings from l2rmap.
     We then remove the set from r2lmap, and mark y as no longer being constant. *)
  let remove_mappings v1 =
    let set = try Hashtbl.find r2lmap v1 with Not_found -> VarSet.empty in
    VarSet.iter (fun v -> Hashtbl.remove l2rmap v) set;
    Hashtbl.remove r2lmap v1;
    Hashtbl.remove v2cmap v1;
    Hashtbl.remove l2rmap v1
  in
  
  (* This gets called when we have an assignment v1 = v2.  We remove any
     existing mappings pertaining to v1, record the assignment, and update
     the r2lmap. *)
  let update_mappings v1 v2 =
    remove_mappings v1;
    Hashtbl.replace l2rmap v1 v2;
    let set = try Hashtbl.find r2lmap v2 with Not_found -> VarSet.empty in
    Hashtbl.replace r2lmap v2 (VarSet.add v1 set)
  in
  
  (* Do copy and constant propagation simultaneously. *)
  let rec aux outlist = function
  | [] -> List.rev outlist
  | i::is -> 
    (* For each expression contained within the current instruction, begin by
       propagating all copies and constants, and then folding any constant 
       subexpressions. *)
    let i = replace_instr_var_with_expr v2cmap i in
    let i = replace_instr_var_with_var false l2rmap i in
    
    (* This should not be necessary; apparently my fold_instr_constants is broken *)
    let rec fold_constants_loop i =
      let i' = fold_instr_constants i in (* ' *)
      if i <> i' 
      then fold_constants_loop i'
      else i
    in
    let i = fold_constants_loop i in
    
    (* After transforming the instruction, extract any assignments of the form 
       v1 = v2 or v1 = c, where c is a constant. *)
   (match (extract_constant_and_copy i) with
    (* If we had v1 = v2, update the maps accordingly. *)
    | Some(v1,Some(v2),None)   -> update_mappings v1 v2
    (* If we had v1 = c, update the maps accordingly. *)
    | Some(v1,None,Some(c))    -> remove_mappings v1; Hashtbl.replace v2cmap v1 c
    (* If we had v1 = [anything other than constant or copy], update maps *)
    | Some(v1,None,None)       -> remove_mappings v1
    (* This is impossible *)
    | Some(v1,Some(_),Some(_)) -> failwith "impossible"
    (* Not an assignment *)
    | None                     -> ());

    (* Tail recurse *)
    aux (i::outlist) is
  in
  aux [] instrs

let local_opt_state_out instrs =
  let ht = Hashtbl.create 50 in
  let instrs = local_opt ht instrs in
  (ht,instrs)

let local_opt_state_in = local_opt

let local_opt instrs = let (_,i) = local_opt_state_out instrs in i

let forward_substitute instrs =
  (* Assign(v,e) ::= v -> {constituents of e} *)
  let l2rmap = Hashtbl.create 50 in 
  (* Assign(v,e) ::= forall x in e. x -> v *)
  let r2lmap = Hashtbl.create 50 in
  (* Assign(v,e) ::= v -> e *)
  let v2emap = Hashtbl.create 50 in
  
  (* The "destructive" part.  Every time we have an assignment to a variable v, 
     we need to look up the set of variables x such that x = f(v,...) in the 
     r2lmap, then remove those variables from each of the l2r sets, remove the 
     r2l mapping, and remove the v2e mapping. *)
  let remove_mappings v =
    let r2lset = try Hashtbl.find r2lmap v with Not_found -> VarSet.empty in
    VarSet.iter 
      (fun r -> 
        let l2rset = try Hashtbl.find l2rmap r with Not_found -> VarSet.empty in
        Hashtbl.replace l2rmap r (VarSet.remove v l2rset))
      r2lset;
    Hashtbl.remove r2lmap v;
    Hashtbl.remove l2rmap v;
    Hashtbl.remove v2emap v

  in
  (* The "constructive" part.  Every time we have an assignment v := e, we
     first need to remove the old mappings for v, and then we need to add
     new mappings. [BE LESS VAGUE] *)
  let update_mappings v e =
    remove_mappings v;
    let vars = var_setify (extract_uses e) in
    Hashtbl.replace l2rmap v vars;
    VarSet.iter 
      (fun x -> 
        let set = try Hashtbl.find r2lmap x with Not_found -> VarSet.empty in
        Hashtbl.replace r2lmap x (VarSet.add v set))
      vars;
    Hashtbl.replace v2emap v e
  
  in
  let replace = replace_var_with_expr v2emap in
   
  let rec fold_constants_loop i =
    let i' = fold_instr_constants i in (* ' *)
    if i <> i' 
    then fold_constants_loop i'
    else i
  in
  let rec aux outlist = function
  | [] -> List.rev outlist
  | i::is -> 
  (*print_endline (ppInstr i);*)
    let i =
    (match i with
     | Assign(v,e)     -> 
      (match v with 
       | Variable(i,_) when i > 21 -> let e = replace e in update_mappings v e; Assign(v,e)
       | _ -> remove_mappings v; Assign(v,e))
     | CJmp(e1,e2,e3)  -> CJmp(replace e1, replace e2, replace e3)
     | Jmp(e)          ->  Jmp(replace e)
     | Halt(e)         -> Halt(replace e)
     | Assert(e)       -> Assert(replace e)
     | Label(_)
     | Special(_)
     | Comment(_) as i -> i)
     in aux ((fold_constants_loop i)::outlist) is
  in aux [] instrs





(* Removed code

This part removed from local_dse:
(* This code was really bad, because it implicitly hardcoded an assumption about
   the set of reserved variables that should be preserved at the end of a basic
   block.  In fact, the code as it is written is out-of-synch with the rest of
   the framework!
   
   Instead, we explicitly pass the variables in as parameter 'live'.

  (* Make a set of variables numbered base to base+n-1, of the specified size *)
  let make_reg_set base n size = 
    let rec aux i set =
      if i == n 
      then set
      else aux (i+1) (VarSet.add (Variable(base+i, size)) set)
    in aux 0 VarSet.empty
  in
  (* Make the general 32-bit registers, the segment registers, and the flags *)
  let r32 = make_reg_set  0 8 TypeReg_32 in
  let rsr = make_reg_set  8 6 TypeReg_16 in
  let rfl = make_reg_set 14 7 TypeReg_1  in
  (* The set of live variables is all of the above plus memory *)
  let live = VarSet.add (Mem(21,Little,TypeReg_32)) (VarSet.union (VarSet.union r32 rfl) rsr) in
*)

let rec fold_expr_constants e = 
  let fold1 ?(changed=false) e1 = 
    let everchanged = ref changed in
    let rec aux e1 = 
      let ef,b = fold_expr_constants e1 in
      if b then (everchanged := true; if not (ic ef) then aux ef else ef) else e1
    in
    if ic e then (e,false) else (aux e1,!everchanged)
  in
  let refold1 ?(changed=false) expr fn oe =
    let x',b = fold1 expr in
    let x''  = fn x' in
    if (b || changed) then fold1 ~changed:true x'' else (oe,changed)
  in
  let refold2 ?(changed=false) e1 e2 fn oe =
    let e1',b1 = fold1 e1 in
    let e2',b2 = fold1 e2 in
    let x = fn e1' e2' in
    if (b1 || b2 || changed) then fold1 ~changed:true x else (oe,changed)
  in
  let refold3 ?(changed=false) e1 e2 e3 fn oe =
    let e1',b1 = fold1 e1 in
    let e2',b2 = fold1 e2 in
    let e3',b3 = fold1 e3 in
    let x = fn e1' e2' e3' in
    if (b1 || b2 || b3 || changed) then fold1 ~changed:true x else (oe,changed)
  in
  let aux expr = match expr with
  (* Constant folding *)
  | Binop(Const(c1,s1),Add,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.add c1 c2),s1),true)
  | Binop(Const(c1,s1),Sub,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.sub c1 c2),s1),true)
  | Binop(Const(c1,s1),And,Const(c2,s2))  -> (Const(Int64.logand c1 c2,s1),true)
  | Binop(Const(c1,s1),Xor,Const(c2,s2))  -> (Const(Int64.logxor c1 c2,s1),true)
  | Binop(Const(c1,s1), Or,Const(c2,s2))  -> (Const(Int64.logor  c1 c2,s1),true)
  | Binop(Const(c1,s1), EQ,Const(c2,s2))  -> (bool2e (c1 =  c2),true)
  | Binop(Const(c1,s1), NE,Const(c2,s2))  -> (bool2e (c1 <> c2),true)
  | Binop(Const(c1,s1),ULT,Const(c2,s2))  -> (ucomp c1 c2 (ULT),true)
  | Binop(Const(c1,s1),ULE,Const(c2,s2))  -> (ucomp c1 c2 (ULE),true)
  | Binop(Const(c1,s1),SLT,Const(c2,s2))  -> (scomp c1 c2 (SLT) s1,true)
  | Binop(Const(c1,s1),SLE,Const(c2,s2))  -> (scomp c1 c2 (SLE) s1,true)
  | Binop(Const(c1,s1),Mul,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.mul c1 c2),s1),true)
  | Binop(Const(c1,s1),Shl,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_left c1 (Int64.to_int c2)),s1),true)
  | Binop(Const(c1,s1),Shr,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right_logical c1 (Int64.to_int c2)),s1),true)
  | Binop(Const(c1,s1),Sar,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right (sign_extend c1 s1) (Int64.to_int c2)),s1),true)
  | Unop(Not,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.logxor (-1L) c1),s1),true)
  | Unop(Neg,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.add 1L (Int64.logxor (-1L) c1)),s1),true)
  | Cast(Low,s,Const(c1,s1))              -> (Const(truncate_to s c1,s),true)
  | Cast(Unsigned,s,Const(c1,s1))         -> (Const(c1,s),true)
  | Cast(Signed,s,Const(c1,s1))           -> (do_signed_cast c1 s1 s,true)
  | Cast(High,s,Const(c1,s1))             -> (do_high_cast c1 s1 s,true)

  (* Simplifications *)
  | Binop(_,And,Const(0L,s))              -> (Const(0L,s),true)
  | Binop(x,And,Const(i,s)) when i = (mk_max_const_i64 s) -> fold1 ~changed:true x
  | Binop(Const(0L,s),Xor,x)              -> fold1 ~changed:true x
  | Binop(x,Sub,Const(0L,s))              -> fold1 ~changed:true x
  | Binop(Const(0L,s),And,_)              -> (Const(0L,s),true)
  | Binop(Const(0L,s),Add,x)              -> fold1 ~changed:true x
  | Binop(Const(0L,s),Sub,x)              -> refold1 ~changed:true x (fun x -> Unop(Neg,x)) expr
  | Binop(Const(0L,s),Or ,x)              -> fold1 ~changed:true x
  | Binop(Const(i,s),Add,Unop(Not,x))     -> refold1 ~changed:true x (fun x -> Binop(Const(truncate_to s (Int64.sub i 1L),s),Add,Unop(Neg,x))) expr
  | Unop(Neg,Unop(Neg,x))                 -> fold1 ~changed:true x
  | Cast(_,s,e) when IRTypeCheck.type_of_integer_type (IRTypeCheck.typecheck_expr e) = s -> fold1 ~changed:true e

  (* Reassociation *)
  | Binop(x,Add,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Add,x)) expr
  | Binop(x,Or ,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Or, x)) expr
  | Binop(x,Add,Binop(y,Add,z))           -> refold3 ~changed:true x y z (fun x y z -> Binop(Binop(x,Add,y),Add,z)) expr
  | Binop(x,Or ,Binop(y,Or ,z))           -> refold3 ~changed:true x y z (fun x y z -> Binop(Binop(x,Or ,y),Or ,z)) expr
  | Binop(Const(c1,s1),Xor,Binop(Const(c2,s2),Xor,z)) -> refold1 ~changed:true z (fun z -> Binop(Const(Int64.logxor c1 c2,s1),Xor,z)) expr
  | Binop(x,Xor,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Xor,x)) expr
  | Binop(x,Sub,Const(c1,s1))             -> refold1 ~changed:true x (fun x -> Binop(Const(truncate_to s1 (Int64.succ (Int64.logxor (-1L) c1)),s1),Add,x)) expr
  
  (* Constant fold constituents *)
  | Binop(x,o,y)                          -> refold2 x y (fun x y -> Binop(x, o, y)) expr
  | Unop(o,x)                             -> refold1 x (fun x -> Unop(o, x)) expr
  | Load(e1,e2,tr)                        -> refold2 e1 e2 (fun e1 e2 -> Load(e1, e2, tr)) expr
  | Cast(a,b,e)                           -> refold1 e (fun e -> Cast(a, b, e)) expr
  | Store(e1,e2,e3,r)                     -> refold3 e1 e2 e3 (fun e1 e2 e3 -> Store(e1, e2, e3, r)) expr
  | Var(v)                                -> (expr,false)
  | Const(_)                              -> (expr,false)
  | Let(v,e1,e2)                          -> refold2 e1 e2 (fun e1 e2 -> Let(v, e1, e2)) expr
  in
  aux e
  
let fold_expr_constants e = let e,_ = fold_expr_constants e in e
*)
(*
let rec reassociate expr = 
  match expr with
| Binop(t1,((Add|Mul|And|Or|Xor|NE|EQ) as o),t2) -> 
  let t1,t2 = reassociate t1, reassociate t2 in
  let c1,c2 = ic t1,ic t2 in
  let te = Binop(t1,o,t2) in
  if (c1 && c2)     then (let e,_ = binop_const_fold te in e) else 
  if (not c1 && c2) then (reassociate Binop(t2,o,t1)) else
  reassociate te
| Binop(t1,Sub,t2) ->
  let t1,t2 = reassociate t1, reassociate t2 in
  let c1,c2 = is_constant t1, is_constant t2 in
  let te = Binop(t1,Sub,t2) in
  if (c1 && c2)     then (let e,_ = binop_const_fold te in e) else 
  if (not c1 && c2) then (reassociate Binop(Unop(Neg(t2)),Sub,t1)) else
  reassociate te
| Binop(t1,Add,Binop(t2,Add,t3)) -> reassociate (Binop(Binop(t1,Add,t2),Add,t3))
| Binop(t1,Mul,Binop(t2,Mul,t3)) -> reassociate (Binop(Binop(t1,Mul,t2),Mul,t3))
| Binop(Binop(c1,Add,t),Add,c2) when ic c1 && ic c2 -> reassociate (Binop(binop_const_fold (Binop(c1,Add,c2)),Add,t))
| Binop(Binop(c1,Mul,t),Mul,c2) when ic c1 && ic c2 -> reassociate (Binop(binop_const_fold (Binop(c1,Mul,c2)),Mul,t))
| Binop(Binop(c1,Add,t),Mul,c2) when ic c1 && ic c2 -> reassociate (Binop(binop_const_fold (Binop(c1,Mul,c2)),Add,Binop(c2,Mul,t)))
| Binop(c1,Mul,Binop(c2,Add,t)) when ic c1 && ic c2 -> reassociate (Binop(binop_const_fold (Binop(c1,Mul,c2)),Add,Binop(c1,Mul,t)))
| Binop(Binop(t1,Add,t2),Mul,c)
| Binop(c,Mul,Binop(t1,Add,t2)) when ic c           -> reassociate (Binop(Binop(c, Mul,t1)),Add,Binop(c, Mul,t2))
| Binop(Binop(t1,Sub,t2),Mul,c)
| Binop(c,Mul,Binop(t1,Sub,t2)) when ic c           -> reassociate (Binop(Binop(c, Mul,t1)),Sub,Binop(c, Mul,t2))
| Binop(Binop(t1,Add,t2),Mul,t3)                    -> reassociate (Binop(Binop(t1,Mul,t3)),Add,Binop(t2,Mul,t3))
| Binop(t1,Mul,Binop(t1,Add,t2))                    -> reassociate (Binop(Binop(t1,Mul,t2)),Add,Binop(t1,Mul,t3))
| Binop(Binop(t1,Sub,t2),Mul,t3)                    -> reassociate (Binop(Binop(t1,Mul,t3)),Sub,Binop(t2,Mul,t3))
| Binop(t1,Mul,Binop(t1,Sub,t2))                    -> reassociate (Binop(Binop(t1,Mul,t2)),Sub,Binop(t1,Mul,t3))
*)

