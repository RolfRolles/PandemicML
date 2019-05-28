(*
#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\Projects\\Nxcroticism\\Verify.ml";;
#use "c:\\paframework\\Projects\\Nxcroticism\\ProbabilisticCandidateGeneration.ml";;
#use "c:\\paframework\\Projects\\Nxcroticism\\NXcroticism.ml";;

  OUTSTANDING QUESTION:  
  * How should we exclude extraneous memory reads / writes?

     Collect up all registers that are used as a source of reads, writes, or memory binops 
     s_read_reg_t
     s_write_reg_t
     s_write_binop_t
     s_read_binop_t
     
     Generate random values for these registers
     Abstract interpret the IR under this valuation
     Get the transformed IR back
     Examine the reads/writes
     Are any of them ...
     
     I don't know if that's going to work.  Consider something like this:
     
     add dword ptr ss:[ebp+FFFFFFEE], esi
     pop ebx
     mov byte ptr ds:[eax], dl
     ret

     We want to reject this sequence because it has two memory writes, despite
     the fact that we can bound each of them.  This test is not going to help
     us determine that.  Let's think more about this problem.
     
     The following sequences are acceptable:
     
     * Any amount of memory reads from the snippet's stack frame
     * At most one non-stack-based memory read, where the destination is known
     * OR, at most one write, where the destination is known and is not the 
       stack
     
     So if we AI the instructions with a stack pointer valuation, we should
     find that there are no store instructions where the address is known.
     If we know the store address, it means either there was a hard-coded 
     address (which should have been rejected already and therefore could
     not happen), or a write went to the stack.  We can reject in that case.
     
     Now collect up the list of all of the reg+disp locations that were read 
     from, and exclude ESP.  Put it in 
     
     If this list is a singleton, then
     
  TO DO:  ADD FUNCTIONALITY
  * Split reads into those that come from the stack, and those that do not.
  * * Set.partition will work handily here.
  * Division and modulus binops
  * Control-transfer gadgets i.e. mov dword ptr ss:[esp], eax / ret
  * Add unops
  * Add write constants
  * Add memory copy
  * Extend the memory location predictor to [regb+regi*sf+const]

  
  TO DO:  OPTIMIZE
  * Propagate register and read bitwise binop behaviors.
  * Further strategies for excluding the 0-result binop false positive 
    explosion.
  * Should we also exclude binops like add eax, eax?

  TO DO:  REFACTOR
  * The code is nearly mature enough to separate out into modules.

  * * IN THE FRAMEWORK:
  * * * IRDataStructures for all of the maps, sets, hashtable manipulations, etc
  * * * Move stuff specific to X86 registers into X86ToIRUtil
  * * * IRConcreteEvaluator

  * * IN THIS PROJECT:
  * * * IOPairGenerator
  
  Not sure how to split the rest of this stuff, but here's what we have:
  
  * Candidate generators
  * Property verifiers
  * X86 syntactic rejection -- move this into the main module
  * Top-level function to invoke all of the rest of the above.
  
  I think it's wise to have the candidate generator and the verifier decoupled,
  so that we can get back unverified results, if only for the sake of printing
  them out as debug information.
  
  Maybe we should make modules like the following:
  
  IOESPDisplacement.determine_one : io -> t
  IOESPDisplacement.determine_all : io -> t
  IOESPDisplacement.determine_triplicate : io -> t triplicate
  IOESPDisplacement.verify : t -> ?
  IOESPDisplacement.verify_triplicate : t triplicate -> ?
  IOESPDisplacement.print -- for debug information?
  
  DONE:
  TO DO:  ADD FUNCTIONALITY
  * Extend to 16- and 8-bit versions
  * Add verification of binops
  
  TO DO:  TEST
  * Make sure that all of the verification queries are returning properly.
  * * Some debugging later, so far so good.
    
  TO DO:  OPTIMIZE
  * Would GCL/WP make the verification faster?
  * * This turned out not to be a problem once I set the logic.

*)
(*module Z3 = Z3.V3*)


(*
let f_printf fmt_etc = IDA.msg fmt_etc
*)
let logfile = open_out "c:\\temp\\nxc-log.txt"
let f_printf fmt_etc = Printf.fprintf logfile fmt_etc

(* Returns Some(x) if ht[key] exists, None otherwise *)
let ht_find_opt ht key =
  let res =
    try Some(Hashtbl.find ht key)
    with Not_found -> None
  in res

(* For hash tables where the values are lists, add the k/v pair *)
let ht_add_to_value_list ht key value =
  match ht_find_opt ht key with
  | None    -> Hashtbl.replace ht key [value]
  | Some(l) -> Hashtbl.replace ht key (value::l)

(* Given an ('a,'b) hash table, make a ('b,'a list) hash table *)
let reverse_ht ht =
  let nht = Hashtbl.create (Hashtbl.length ht) in
  Hashtbl.iter (fun k v -> ht_add_to_value_list nht v k) ht;
  nht

(* Given a list ('a,int32), make an ('a,IR.expr) hash table w/Const(int32,s) values  *)
let new_ht s pairs = 
  let ht = Hashtbl.create (List.length pairs * 2) in
  List.iter (fun (x,y) -> Hashtbl.replace ht x (IRUtil.mk_fixed_const_of_i32 y s)) pairs;
  ht

(* Compute the intersection of two IR.var -> IR.VarSet maps *)
let varmapset_inter m1 m2 =
  let open IRUtil in
  let pm = VarMap.mapi
   (fun k s1 ->
      let res = 
        try Some(VarMap.find k m2)
        with Not_found -> None
      in match res with
      | None -> VarSet.empty
      | Some(s2) -> VarSet.inter s1 s2)
    m1
  in VarMap.filter (fun _ s -> not (VarSet.is_empty s)) pm

(* Remove those variables that are not contained within varset from the 
   context hash table*)
let remove_irtrans_vars varset ht =
  let vl = Hashtbl.fold 
   (fun k _ l -> 
      if IRUtil.VarSet.mem k varset
      then l
      else k::l)
    ht
    []
  in
  List.iter (Hashtbl.remove ht) vl;
  ht

(* Triplicate stuff *)
type 'a triplicate =
{
  val8: 'a;
  val16: 'a;
  val32: 'a;
}

let select_member s t = match s with
| IR.TypeReg_8  -> t.val8 
| IR.TypeReg_16 -> t.val16
| IR.TypeReg_32 -> t.val32
| _ -> invalid_arg "select_member"

let replace_member s t v = match s with
| IR.TypeReg_8  -> { t with val8  = v; }
| IR.TypeReg_16 -> { t with val16 = v; }
| IR.TypeReg_32 -> { t with val32 = v; }
| _ -> invalid_arg "replace_member"

let triplicate f x =
  { val8 = f IR.TypeReg_8 x; val16 = f IR.TypeReg_16 x; val32 = f IR.TypeReg_32 x }

let triplicate_map f x =
  { val8 = f IR.TypeReg_8 x.val8; val16 = f IR.TypeReg_16 x.val16; val32 = f IR.TypeReg_32 x.val32 }

let print_triplicate f t =
  f t.val32;
  f t.val16;
  f t.val8

(* Fold each individually, return them separately *)
let triple_fold_triple f_outer f_inner acc_t t =
{ val8  = f_outer f_inner t.val8  acc_t.val8; 
  val16 = f_outer f_inner t.val16 acc_t.val16; 
  val32 = f_outer f_inner t.val32 acc_t.val32; }

let fold_triple f_outer f_inner acc t =
  let acc8  = f_outer f_inner t.val8  acc  in
  let acc16 = f_outer f_inner t.val16 acc8 in
  f_outer f_inner t.val32 acc16

(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)

let int_of_binop = function
| IR.Add  -> 0
| IR.Sub  -> 1
| IR.Mul  -> 2
| IR.SDiv -> 3
| IR.UDiv -> 4
| IR.SMod -> 5
| IR.UMod -> 6
| IR.Shl  -> 7
| IR.Shr  -> 8
| IR.Sar  -> 9
| IR.And  -> 10
| IR.Or   -> 11
| IR.Xor  -> 12
| IR.EQ   -> 13
| IR.NE   -> 14
| IR.ULT  -> 15
| IR.ULE  -> 16
| IR.SLT  -> 17
| IR.SLE  -> 18

module VarBinopVarComparator = struct
  type t = (IR.var * IR.binop * IR.var)
  let compare (l1,b1,r1) (l2,b2,r2) = 
    let open IR in
    let c1 = IRUtil.var_comparator l1 l2 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator r1 r2 in
    if c3 <> 0 then c3 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarBinopVarSet = Set.Make(VarBinopVarComparator)

module VarVarBinopVarComparator = struct
  type t = (IR.var * IR.var * IR.binop * IR.var)
  let compare (v1,l1,b1,r1) (v2,l2,b2,r2) = 
    let open IR in
    let c1 = IRUtil.var_comparator v1 v2 in
    if c1 <> 0 then c1 else
    let c1 = IRUtil.var_comparator l1 l2 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator r1 r2 in
    if c3 <> 0 then c3 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarVarBinopVarSet = Set.Make(VarVarBinopVarComparator)

module VarBinopVarInt32Comparator = struct
  type t = (IR.var * IR.binop * IR.var * int32)
  let compare (v11,b1,v12,i321) (v21,b2,v22,i322) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator v12 v22 in
    if c3 <> 0 then c3 else
    let c2 = Int32.compare i321 i322 in
    if c2 <> 0 then c2 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarBinopVarInt32Set = Set.Make(VarBinopVarInt32Comparator)

(* Compute the intersection of two IR.var -> IR.VarBinopVarSet maps 
   Duplicated from before, figure out how to use first-class modules *)
let varmapvbvset_inter m1 m2 =
  let open IRUtil in
  let pm = VarMap.mapi
   (fun k s1 ->
      let res = 
        try Some(VarMap.find k m2)
        with Not_found -> None
      in match res with
      | None -> VarBinopVarSet.empty
      | Some(s2) -> VarBinopVarSet.inter s1 s2)
    m1
  in VarMap.filter (fun _ s -> not (VarBinopVarSet.is_empty s)) pm

module VarInt32Comparator = struct
  type t = (IR.var * int32)
  let compare (v1,d1) (v2,d2) = 
    let c1 = IRUtil.var_comparator v1 v2 in
    if c1 <> 0 then c1 else Int32.compare d1 d2
end

module VarInt32Set = Set.Make(VarInt32Comparator)

module VarVarComparator = struct
  type t = (IR.var * IR.var)
  let compare (v11,v12) (v21,v22) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else IRUtil.var_comparator v12 v22
end

module VarVarSet = Set.Make(VarVarComparator)

module VarVarInt32Comparator = struct
  type t = (IR.var * IR.var * int32)
  let compare (v11,v12,i321) (v21,v22,i322) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator v12 v22 in
    if c3 <> 0 then c3 else
    Int32.compare i321 i322
end

module VarVarInt32Set = Set.Make(VarVarInt32Comparator)

let dump_vbvset =
  VarBinopVarSet.iter (fun (vl,b,vr) -> 
    f_printf "%s %s %s, " 
     (PpIR.ppVar vl)
     (PpIR.ppBinop b)
     (PpIR.ppVar vr))

let dump_vbvmap =
  IRUtil.VarMap.iter (fun v s_vbv -> 
    f_printf "%s -> {" (PpIR.ppVar v);
    dump_vbvset s_vbv;
    f_printf " }\n")

let dump_varset = IRUtil.VarSet.iter (fun v -> f_printf "%s, " (PpIR.ppVar v)) 

let dump_vsmap =
  IRUtil.VarMap.iter (fun v s_v -> 
    f_printf "%s -> {" (PpIR.ppVar v);
    dump_varset s_v;
    f_printf " }\n")

(* This shit is duplicated from IRLocalOpt, twice! *)
let i64_of_bool = function | true  -> 1L | false -> 0L

(* Given an (IR.var,'a) list, make an IR.var -> 'a map.  What does it do with duplicates? *)
let var_mapify l = List.fold_left (fun a (v,x) -> IRUtil.VarMap.add v x a) IRUtil.VarMap.empty l

(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* /NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)

(* Frequently-needed variables (registers, esp); all reserved variables as a set *)
let vregs,esp = let open X86ToIRUtil in [vEax;vEcx;vEdx;vEbx;vEbp;vEsi;vEdi],vEsp
let vregs_noesp_parentage =
  let open X86ToIRUtil in
 [(vEax,vAx,Some(vAh,vAl));(vEcx,vCx,Some(vCh,vCl));(vEdx,vDx,Some(vDh,vDl));
  (vEbx,vBx,Some(vBh,vBl));(vEbp,vBp,None);(vEsi,vSi,None);(vEdi,vDi,None)]

(*let reserved_vars_set = IRUtil.var_setify X86ToIRUtil.reserved_vars*)
let reserved_vars_set = IRUtil.var_setify (esp::vregs)

(* Evaluator functionality.  This function is factored out so it can be used
   elsewhere. *)
let write_mem memctx a32 v32 s =
  let n = (IRUtil.bits s)/8 in
  let rec aux v32 i =
    if i = n
    then ()
    else
      let addr = Int32.add a32 (Int32.of_int i) in
      Hashtbl.replace memctx addr (Int32.logand v32 0xffl);
      aux (Int32.shift_right_logical v32 8) (i+1)
  in aux v32 0

(* Given:

   ir: a list of IR statements.  Not in SSA form, and no jumps.
   regctx: (IR.var,IR.expr) Hashtbl.t, the register context
   memctx: (int32,int32) Hashtbl.t, the memory context
   
   Evaluate the statements in the given contexts, computing information about 
   the memory behavior of the sequence, as described below:
   
   Returns a pair (and modifies regctx/memctx): 
   reads:  (int32 address,int32 value, int32 size) list
   writes:  (int32 address,int32 value, int32 size) list
*)
let concrete_evaluate_jumpless_nonssa ir regctx memctx = 
  let open IR in let open IRUtil in
  (* All this shit is duplicated from IRLocalOpt *)
  let bool2e = function | true  -> Const(0x1L,TypeReg_1) | false -> Const(0x0L,TypeReg_1) in
  let sign_extend_byte = function
  | x when x <= 0x7FL -> x
  | x -> Int64.logor  x 0xFFFFFFFFFFFFFF00L
  in
  let sign_extend_word = function
  | x when x <= 0x7FFFL -> x
  | x -> Int64.logor  x 0xFFFFFFFFFFFF0000L
  in
  let sign_extend_dword = function
  | x when x <= 0x7FFFFFFFL -> x
  | x -> Int64.logor  x 0xFFFFFFFF00000000L
  in
  let sign_extend c = function
  | TypeReg_1  -> invalid_arg "sign_extend: TypeReg_1"
  | TypeReg_8  -> sign_extend_byte c
  | TypeReg_16 -> sign_extend_word c
  | TypeReg_32 -> sign_extend_dword c
  | TypeReg_64 -> c
  in
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
  in
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
  in
  let do_signed_cast c1 s1 s =
    let c =
      if (Int64.logand (mk_sign_const_i64 s1) c1) = 0L 
      then c1
      else Int64.logor c1 (Int64.logxor (mk_max_const_i64 s) (mk_max_const_i64 s1))
    in Const(c,s)
  in  
  let do_high_cast c1 s1 s =
    let s1 = IRUtil.bits s1 in
    let s' = IRUtil.bits s  in
    if  s'> s1 then failwith "do_high_cast: typechecking prevents this";
    Const(Int64.shift_right_logical c1 (s1-s'),s) (* ' *)
  in

  (* End duplication *)
  let cc = function | Const(c,s) -> c | _ -> failwith "concrete_evaluate_jumpless_nonssa::cc: typechecking prevents this" in
  let cs = function | Const(c,s) -> s | _ -> failwith "concrete_evaluate_jumpless_nonssa::cs: typechecking prevents this" in
  
  let reads = ref [] in
  let uninit_reads = ref [] in
  let uninit = Hashtbl.create 50 in
  let writes = ref [] in

  let read_mem a32 s =
    let n = (IRUtil.bits s)/8 in
    let rec aux c uni i =
      if i < n
      then 
        let addr = Int32.add a32 (Int32.of_int i) in
        let byte = ht_find_opt memctx addr in
        let byte,uninit = 
          match byte with
          | Some(b) -> (b,match ht_find_opt uninit addr with | Some(_) -> true | None -> false)
          | None -> 
            let new_byte = Int32.succ (Random.int32 0xFFl) in
            Hashtbl.replace memctx addr new_byte;
            Hashtbl.replace uninit addr new_byte;
            (new_byte,true)
        in
        let c = Int32.logor (Int32.shift_left byte (i*8)) c in
        aux c (uninit or uni) (i+1)
      else (c,uni)
    in
    let c,uni = aux 0l false 0 in
   (if uni
    then uninit_reads := (a32,c,s)::(!uninit_reads));
    c
  in
  
  let rec expr = function
  | Var(v)             -> Hashtbl.find regctx v
  | Const(_,_) as c    -> c

  | Binop(l,Add,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.add (cc l) (cc r)),(cs l))
  | Binop(l,Mul,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.mul (cc l) (cc r)),(cs l))
  | Binop(l,And,r)     -> let l,r = expr l,expr r in Const(Int64.logand (cc l) (cc r),(cs l))
  | Binop(l,Xor,r)     -> let l,r = expr l,expr r in Const(Int64.logxor (cc l) (cc r),(cs l))
  | Binop(l, Or,r)     -> let l,r = expr l,expr r in Const(Int64.logor  (cc l) (cc r),(cs l))

  | Binop(l,Sub,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.sub (cc l) (cc r)),(cs l))
  | Binop(l,UDiv,r)    -> failwith "concrete_evaluate_jumpless_nonssa::expr: UDIV unimplemented"
  | Binop(l,SDiv,r)    -> failwith "concrete_evaluate_jumpless_nonssa::expr: SDIV unimplemented"
  | Binop(l,UMod,r)    -> failwith "concrete_evaluate_jumpless_nonssa::expr: UMOD unimplemented"
  | Binop(l,SMod,r)    -> failwith "concrete_evaluate_jumpless_nonssa::expr: SMOD unimplemented"

  | Binop(l,Shl,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.shift_left (cc l) (Int64.to_int (cc r))),(cs l))
  | Binop(l,Shr,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.shift_right_logical (cc l) (Int64.to_int (cc r))),(cs l))
  | Binop(l,Sar,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.shift_right (sign_extend (cc l) (cs l)) (Int64.to_int (cc r))),(cs l))

  | Binop(l, EQ,r)     -> let l,r = expr l,expr r in bool2e ((cc l) =  (cc r))
  | Binop(l, NE,r)     -> let l,r = expr l,expr r in bool2e ((cc l) <> (cc r))

  | Binop(l,ULT,r)     -> let l,r = expr l,expr r in ucomp (cc l) (cc r) (ULT)
  | Binop(l,ULE,r)     -> let l,r = expr l,expr r in ucomp (cc l) (cc r) (ULE)
  | Binop(l,SLT,r)     -> let l,r = expr l,expr r in scomp (cc l) (cc r) (SLT) (cs l)
  | Binop(l,SLE,r)     -> let l,r = expr l,expr r in scomp (cc l) (cc r) (SLE) (cs l)

  | Unop(Not,e)        -> let e = expr e in Const(truncate_to (cs e) (Int64.logxor (-1L) (cc e)),(cs e))
  | Unop(Neg,e)        -> let e = expr e in Const(truncate_to (cs e) (Int64.add 1L (Int64.logxor (-1L) (cc e))),(cs e))

  | Cast(Low,s,e)      -> let e = expr e in Const(truncate_to s (cc e),s)
  | Cast(Unsigned,s,e) -> let e = expr e in Const((cc e),s)
  | Cast(Signed,s,e)   -> let e = expr e in do_signed_cast (cc e) (cs e) s
  | Cast(High,s,e)     -> let e = expr e in do_high_cast (cc e) (cs e) s

  | Load(_,a,s)        -> 
    let a = Int64.to_int32 (cc (expr a)) in
    let v = read_mem a s in
    reads := (a,v,s)::(!reads);
    IRUtil.mk_fixed_const (Int64.of_int32 v) s

  | Store(m,a,t,s)     -> 
    let a,t = Int64.to_int32 (cc (expr a)), Int64.to_int32 (cc (expr t)) in
    write_mem memctx a t s;
    writes := (a,t,s)::(!writes);
    m
  | Let(_,_,_)         -> failwith "concrete_evaluate_jumpless_nonssa::expr: LET unimplemented"
  in
  
  let stmt = function
  | Assign(Mem(_,_,_),e) -> let _ = expr e in ()
  | Assign(v,e) -> Hashtbl.replace regctx v (expr e)
  | Label(_) -> ()
  | Comment(_) -> ()
  | Assert(_) -> ()
  | Jmp(e) -> failwith "concrete_evaluate_jumpless_nonssa::stmt: JMP encountered"
  | CJmp(e,t,f) -> failwith "concrete_evaluate_jumpless_nonssa::stmt: CJMP encountered"
  | Halt(_) -> failwith "concrete_evaluate_jumpless_nonssa::stmt: HALT never used"
  | Special(_) -> failwith "concrete_evaluate_jumpless_nonssa::stmt: SPECIAL never used"
  in
  List.iter stmt ir;
  (!reads,!writes)
  
type quantized_memory_location = IR.var * int32
type s_structured_memory_location = VarInt32Set.t

(* The result of interpreting the IR in a given context. *)
type io =
{
  (* The 32-bit registers before emulation. *)
  pre:  (IR.var,IR.expr) Hashtbl.t;
  
  (* The 32-bit registers after emulation. *)
  post: (IR.var,IR.expr) Hashtbl.t;
  
  (* Value -> variables containing that value after emulation. *)
  rev_post: (IR.expr,IR.var list) Hashtbl.t;
  
  (* Register context. (variable, value32) list. Not strictly necessary. *)
  register_data: (IR.var * int32) list;

  (* Values read from memory during emulation. (address, candidate set, value) list *)
  reads: (int32 * s_structured_memory_location * int32) list;

  (* Values read from memory during emulation. (address, candidate set, value) list *)
  writes: (int32 * s_structured_memory_location * int32) list;
}


let print_var_expr_ht = 
  Hashtbl.iter (fun v e -> 
    f_printf "%s -> %s\n" (PpIR.ppVar v) (PpIR.ppExpr true e))

let print_memaccess_set =
  VarInt32Set.iter (fun (v,i32) -> f_printf "[%s.pre+0x%lx] " (PpIR.ppVar v) i32)

let print_memaccess_list =
  List.iter (fun (a32,set,v32) ->
    f_printf "[0x%lx] -> 0x%lx (candidates = {" a32 v32;
    print_memaccess_set set;
    f_printf "})\n")

(* Needs refactoring since the addition of register_data and stack_value *)
let print_io io =
  f_printf "Dumping I/O pair\n\n";
  f_printf "pre context:\n";
  print_var_expr_ht io.pre;
  f_printf "\npost context:\n";
  print_var_expr_ht io.post;
  f_printf "\nmemory reads:\n";
  print_memaccess_list io.reads;
  f_printf "\nmemory writes:\n";
  print_memaccess_list io.writes

type io_container =
{
  (* A list of io structures for the randomized runs *)
  l_io: io triplicate list;
  
  (* The set of memory locations read, in terms of [reg32+disp32] *)
  common_reads: s_structured_memory_location triplicate;
  
  (* The set of memory locations written, in terms of [reg32+disp32] *)
  common_writes: s_structured_memory_location triplicate;

  (* The set of memory locations that were both read AND written *)
  common_readwrites: s_structured_memory_location triplicate;
}

let print_io_container ioc =
  let _ = f_printf "\nCommon reads (8-bit): " in
  print_memaccess_set ioc.common_reads.val8;
  let _ = f_printf "\nCommon reads (16-bit): " in
  print_memaccess_set ioc.common_reads.val16;
  let _ = f_printf "\nCommon reads (32-bit): " in
  print_memaccess_set ioc.common_reads.val32;
  let _ = f_printf "\nCommon writes (8-bit): " in
  print_memaccess_set ioc.common_writes.val8;
  let _ = f_printf "\nCommon writes (16-bit): " in
  print_memaccess_set ioc.common_writes.val16;
  let _ = f_printf "\nCommon writes (32-bit): " in
  print_memaccess_set ioc.common_writes.val32;
  let _ = f_printf "\nCommon read/writes (8-bit): " in
  print_memaccess_set ioc.common_readwrites.val8;
  let _ = f_printf "\nCommon read/writes (16-bit): " in
  print_memaccess_set ioc.common_readwrites.val16;
  let _ = f_printf "\nCommon read/writes (32-bit): " in
  print_memaccess_set ioc.common_readwrites.val32;
  f_printf "\n";
  List.iter (print_triplicate print_io) ioc.l_io;
  f_printf "\n%!"

let get_ht_in_val io r = 
  match ht_find_opt io.pre r with
  | Some(IR.Const(i,_)) -> Int64.to_int32 i
  | _ -> failwith "determine_binop_behaviors: bad ht_in"

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
      (* Might want to keep this, might not. Omit ESP from consideration. *)
    (*if v = esp then set else*)
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
    | io::ios -> 
      let sio = sel io in
      let f = make_purported_register_displacements in
      let newread,s_read   = f io.val32.pre sio.reads in
      let newwrite,s_write = f io.val32.pre sio.writes in
      read := s_read; write := s_write;
      let ios = List.map
       (fun io -> 
         let sio = sel io in
         let newread,s_read = f io.val32.pre sio.reads in
         let newwrite,s_write = f io.val32.pre sio.writes in
         read  := VarInt32Set.inter !read s_read;
         write := VarInt32Set.inter !write s_write;
         rep io { sio with reads = newread; writes = newwrite; })
        ios
      in 
      (rep io { sio with reads = newread; writes = newwrite; })::ios
    | _ -> []
  in
  let map sset = List.map (fun (a,set,v) -> (a,VarInt32Set.inter set sset,v)) in
  let new_l_io = List.map 
    (fun io -> 
       let sio = sel io in 
       rep io { sio with reads = map !read sio.reads; writes = map !write sio.writes; }) 
     new_l_io
  in
  (new_l_io,!read,!write,VarInt32Set.inter !read !write)

let get_common_accessed_locations l_io =
  let l_io,r8, w8, c8  = get_common_accessed_locations IR.TypeReg_8  l_io in
  let l_io,r16,w16,c16 = get_common_accessed_locations IR.TypeReg_16 l_io in
  let l_io,r32,w32,c32 = get_common_accessed_locations IR.TypeReg_32 l_io in
  l_io,
  { val8 = r8; val16 = r16; val32 = r32; },
  { val8 = w8; val16 = w16; val32 = w32; },
  { val8 = c8; val16 = c16; val32 = c32; }

(* I should not have to do this, but apparently the constant folding function is not idempotent 
   That needs to be fixed, seriously.  But for the time being, let's just do this. *)
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

let nonzero_rand_i32 () = Int32.succ (Random.int32 (Int32.pred Int32.max_int))
let nonzero_rand_i32_4 () = Int32.logand  (nonzero_rand_i32 ()) 0xffffffcl

let make_io_struct ir = 
  (* Probably needs alterating, to remind myself later *)
  let remove_irtrans_vars = remove_irtrans_vars (*X86ToIRUtil.*)reserved_vars_set  in

  (* Generate a dword-aligned ESP value *)

  let esp_val = nonzero_rand_i32_4 () in
  let p_espval  = esp,esp_val in
  
  (* Make the memory context *)
  let memctx = Hashtbl.create 32 in
  
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
      vregs_noesp_parentage
  in
  
  (* ht_post contains the initial values of the registers (no flags as of now) *)
  let ht_post = new_ht IR.TypeReg_32 (p_espval::l_varvals32) in
  
  (* ht_pre is a virgin copy of ht_post before emulation *)
  let ht_pre = Hashtbl.copy ht_post in

  List.iter 
   (fun vf -> 
      Hashtbl.replace 
        ht_post 
        vf 
       (IRUtil.mk_bit (i64_of_bool (Random.bool ()))))
   (let open X86ToIRUtil in [vSF;vOF;vPF;vZF;vAF;vCF;vDF]);
  
  (* Perform emulation, get the output values *)
  let reads,writes = concrete_evaluate_jumpless_nonssa ir ht_post memctx in
  
  let _ = remove_irtrans_vars ht_post in
  let r32,r16,r8 = segregate_address_list reads in
  let w32,w16,w8 = segregate_address_list writes in
  let pre16,post16 = new_ht IR.TypeReg_16 l_varvals16, get_ht_post16 ht_post in
  let pre8,post8   = new_ht IR.TypeReg_8  l_varvals8,  get_ht_post8  ht_post in
  let io pre post rev_post register_data reads writes = 
  { 
    pre = pre; post = post; rev_post = rev_post; 
    register_data = register_data; reads = reads; writes = writes; 
  }
  in
  let io32 = io ht_pre ht_post (reverse_ht ht_post) l_varvals32 r32 w32 in
  let io16 = io pre16  post16  (reverse_ht post16)  l_varvals16 r16 w16 in
  let io8  = io pre8   post8   (reverse_ht post8)   l_varvals8  r8  w8 in
  { val32 = io32; val16 = io16; val8 = io8; }

(* Given:
   ir:  a list of non-ssa, jumpless IR
   n:  number of tests to execute
   Return:
*)
let make_io_structs ir n =
  let rec aux i list =
    if i = n
    then list
    else aux (i+1) ((make_io_struct ir)::list)
  (* Bootstrap i/o generation *)
  in
  let l_io = aux 0 [] in
  let l_io,read,write,readwrites = get_common_accessed_locations l_io in
  {
    l_io = l_io;
    common_reads = read;
    common_writes = write;
    common_readwrites = readwrites;
  }
  
(* Given an i/o structure, get the ESP differential as an option *)
let determine_esp_delta io =
  let open IR in
  match ht_find_opt io.post esp, ht_find_opt io.pre esp with
  | Some(Const(inval,TypeReg_32)),Some(Const(outval,TypeReg_32)) -> 
    Some(Int64.to_int32 (Int64.sub inval outval))
  | _,_ -> None
  
(* Given a list of i/o structures, get the ESP differential across all of them
   as an option *)
let determine_aggregate_esp_delta ioc =
  let rec aux acc = function 
  | x::xs ->	
      let res = determine_esp_delta x.val32 in
     (match res,acc with
      | Some(i32),Some(acc) when i32 = acc -> aux res xs
      | Some(_),None -> aux res xs
      | _,_ -> res)
  | [] -> acc
  in aux (None) ioc.l_io

(* Given an i/o structure, take the reserved registers list (no ESP) and 
  consult the pre/post hash tables to determine which they are preserved. 
  
  OUTSTANDING QUESTION:  Should I include the stack variables in this?
*)
let determine_preserve_behaviors io =
  let open IRUtil in
  List.fold_left 
   (fun s v ->
      match ht_find_opt io.pre v,ht_find_opt io.post v with
      | Some(vpre),Some(vpost) when vpre = vpost -> VarSet.add v s
      | _,_ -> s)
    VarSet.empty
    vregs

(* Determine the preserved registers across an i/o structure list *)
let determine_aggregate_preserve_behaviors size ioc =
  let open IRUtil in
  let f i = determine_preserve_behaviors (select_member size i) in
  match ioc.l_io with
  | [] -> VarSet.empty
  | x::xs -> List.fold_left (fun s i -> VarSet.inter s (f i)) (f x) xs

let determine_aggregate_preserve_behaviors = triplicate determine_aggregate_preserve_behaviors 

(* This function works by reversing the post hash table.  Seeing as we do this
   several times, it's probably a good idea to just put that construct directly
   into the i/o structure. 
   
   Given the output values at the end of the emulation, reverse the hash table.
   * rout = reverse_ht
   * For each var/value pair (v,c) in ht_pre
   * * l = rout[v] option
   * * For each var x in l, where x <> v, (v,x) is a candidate copy
   * * Make a map Var -> [potential;copy;destination;variables]
*)
let determine_copy_behaviors io =
  let rout = io.rev_post in
  let vll = 
    Hashtbl.fold 
     (fun v c l -> 
        if v <> esp 
        then
          match ht_find_opt rout c with
          | Some(vl) -> (v,IRUtil.var_setify (List.filter (fun x -> x <> v) vl))::l
          | None -> l
        else l)
      io.pre
      []
  in (*IRUtil.*)var_mapify vll

(* Determine all copy behaviors across a list of i/o pairs *)
let determine_aggregate_copy_behaviors size ioc = 
  let f i = determine_copy_behaviors (select_member size i) in
  match ioc.l_io with
  | [] -> IRUtil.VarMap.empty
  | x::xs -> List.fold_left (fun m i -> varmapset_inter m (f i)) (f x) xs
  
let determine_aggregate_copy_behaviors = triplicate determine_aggregate_copy_behaviors 

(* 
 * * For each (address,candidateset,value,size) read
 * * Look up the value in the rout hash table
 * * If it matches, that's a potential read behavior
*)
let determine_read_reg32_behaviors size io =
  (* Get post val -> var hash table *)
  let rout = io.rev_post in
  List.fold_left 
   (fun set (a32,s_read,v32) ->
      (* For each (reg32,disp32) in s_read *)
      VarInt32Set.fold
       (fun (v,d32) set ->
          match ht_find_opt rout (IRUtil.mk_fixed_const_of_i32 v32 size) with
          (* Value is not present, return the existing set *)
          | None -> set
          (* Value is present, so add (vdst,vr32,d32) to the set*)
          | Some(l) -> List.fold_left (fun set el -> VarVarInt32Set.add (el,v,d32) set) set l)
        (* Fold over all common read addresses *)
        s_read
        (* Accumulator VarVarInt32 set *)
        set)
    (* List fold accumulator: empty VarVarInt32Set *)
    VarVarInt32Set.empty
    (* Fold over all reads *)
    io.reads
          
let determine_aggregate_read_reg32_behaviors size ioc =
  let f i = determine_read_reg32_behaviors size (select_member size i) in
  match ioc.l_io with
  | [] -> VarVarInt32Set.empty 
  | x::xs -> List.fold_left (fun s i -> VarVarInt32Set.inter s (f i)) (f x) xs

let determine_aggregate_read_reg32_behaviors = triplicate determine_aggregate_read_reg32_behaviors 

(* Virtually identical to last set of functions *)
let determine_write_reg32_behaviors size io =
  (* Was rout = ... io.post *)
  let rin = reverse_ht io.pre in
  List.fold_left 
   (fun set (a32,s_write,v32) ->
      VarInt32Set.fold
       (fun (v,d32) set ->
          match ht_find_opt rin (IRUtil.mk_fixed_const_of_i32 v32 size) with
          | None -> set
          | Some(l) -> List.fold_left (fun set el -> VarVarInt32Set.add (el,v,d32) set) set l)
        (* Was s_read *)
        s_write
        set)
    VarVarInt32Set.empty
    (* Was io.reads *)
    io.writes
          
let dump_varvarint32set =
  VarVarInt32Set.iter 
   (fun (vsrc,vreg,disp32) -> 
      f_printf "mem[%s.initial+0x%lx] = %s.initial\n" 
       (PpIR.ppVar vreg) disp32 (PpIR.ppVar vsrc)) 

let determine_aggregate_write_reg32_behaviors size ioc =
  let f i = 
    let s_vvi = determine_write_reg32_behaviors size (select_member size i) in
  (*f_printf "Dumping write candidates:\n";
    dump_varvarint32set s_vvi;*)
    s_vvi    
  in
  let s_vvi = 
  match ioc.l_io with
  | [] -> VarVarInt32Set.empty 
  | x::xs -> List.fold_left (fun s i -> VarVarInt32Set.inter s (f i)) (f x) xs
  in
(*f_printf "Dumping final write candidates:\n";
  dump_varvarint32set s_vvi;*)
  s_vvi    

let determine_aggregate_write_reg32_behaviors = triplicate determine_aggregate_write_reg32_behaviors 

let make_io_out_varint32map io =
  Hashtbl.fold 
   (fun v c s -> match c with 
    | IR.Const(i64,_) -> VarInt32Set.add (v,Int64.to_int32 i64) s
    | _ -> failwith "make_io_out_varint32map: ill-formed ht_pre")
    io.post
    VarInt32Set.empty

let determine_aggregate_load_const32_behaviors size ioc =
  let f i = make_io_out_varint32map (select_member size i) in
  match ioc.l_io with
  | io::ios -> List.fold_left (fun s i -> VarInt32Set.inter s (f i)) (f io) ios
  | _ -> VarInt32Set.empty

let determine_aggregate_load_const32_behaviors = triplicate determine_aggregate_load_const32_behaviors 

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
    VarBinopVarSet.fold (fun (vl,binop,vr) (write,read,reg) ->
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
      
      | _ -> 
        f_printf "Missed BINOP %s %s %s, investigate\n" 
          (PpIR.ppVar vl) 
          (PpIR.ppBinop binop) 
          (PpIR.ppVar vr); 
        (write,read,reg))
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

type candidate_results =
{
  c_preserved_regs: IRUtil.VarSet.t triplicate;
  c_copied_regs: (IRUtil.VarSet.t IRUtil.VarMap.t) triplicate;
  c_set_const: VarInt32Set.t triplicate;
  c_mem_read_const: VarVarInt32Set.t triplicate;
  c_mem_write_reg: VarVarInt32Set.t triplicate;
  c_write_binops:   VarBinopVarInt32Set.t triplicate;
  c_read_binops:    VarBinopVarInt32Set.t triplicate;  
  c_reg_binops:     VarVarBinopVarSet.t triplicate;
}

let dump_reg_copies t_s_cr =
  let dump_varvarset =
    VarVarSet.iter 
     (fun (vsrc,vdst) -> 
        f_printf "%s.final = %s.initial\n" 
         (PpIR.ppVar vdst) 
         (PpIR.ppVar vsrc)) 
  in
  if not
   (VarVarSet.is_empty t_s_cr.val8  && 
    VarVarSet.is_empty t_s_cr.val16 && 
    VarVarSet.is_empty t_s_cr.val32)
  then 
   (f_printf "Register-to-register copy behaviors:\n";
    dump_varvarset t_s_cr.val32;
    dump_varvarset t_s_cr.val16;
    dump_varvarset t_s_cr.val8)

let dump_set_const t_s_cr =
  let dump_varint32set =
    VarInt32Set.iter 
     (fun (vdst,disp32) -> f_printf "%s.final = 0x%lx\n" (PpIR.ppVar vdst) disp32) 
  in
  if not
   (VarInt32Set.is_empty t_s_cr.val8  &&
    VarInt32Set.is_empty t_s_cr.val16 && 
    VarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Set register to const32 behaviors:\n";
    dump_varint32set t_s_cr.val32;
    dump_varint32set t_s_cr.val16;
    dump_varint32set t_s_cr.val8)

let dump_load_const t_s_cr =
  let dump_varvarint32set =
    VarVarInt32Set.iter 
     (fun (vdst,vreg,disp32) -> 
        f_printf "%s.final = mem[%s.initial+0x%lx]\n" 
         (PpIR.ppVar vdst) (PpIR.ppVar vreg) disp32) 
  in
  if not
   (VarVarInt32Set.is_empty t_s_cr.val8  &&
    VarVarInt32Set.is_empty t_s_cr.val16 && 
    VarVarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Load const from [reg+disp32] behaviors:\n";
    dump_varvarint32set t_s_cr.val32;
    dump_varvarint32set t_s_cr.val16;
    dump_varvarint32set t_s_cr.val8)

let dump_write_const t_s_cr =
  let dump_varvarint32set =
    VarVarInt32Set.iter 
     (fun (vsrc,vreg,disp32) -> 
        f_printf "mem[%s.initial+0x%lx] = %s.initial\n" 
         (PpIR.ppVar vreg) disp32 (PpIR.ppVar vsrc)) 
  in
  if not
   (VarVarInt32Set.is_empty t_s_cr.val8  &&
    VarVarInt32Set.is_empty t_s_cr.val16 && 
    VarVarInt32Set.is_empty t_s_cr.val32)
  then 
   (f_printf "Write reg32 to [reg+disp32] behaviors:\n";
    dump_varvarint32set t_s_cr.val32;
    dump_varvarint32set t_s_cr.val16;
    dump_varvarint32set t_s_cr.val8)

let dump_register_binop_behaviors s_reg_t =
  let dump_vvbvset =
    VarVarBinopVarSet.iter (fun (v,vl,b,vr) -> 
      f_printf "%s = %s %s %s\n" 
       (PpIR.ppVar v)
       (PpIR.ppVar vl)
       (PpIR.ppBinop b)
       (PpIR.ppVar vr))
  in
  if not
   (VarVarBinopVarSet.is_empty s_reg_t.val8  &&
    VarVarBinopVarSet.is_empty s_reg_t.val16 && 
    VarVarBinopVarSet.is_empty s_reg_t.val32)
  then 
   (f_printf "Register binop behaviors:\n";
    dump_vvbvset s_reg_t.val32;
    dump_vvbvset s_reg_t.val16;
    dump_vvbvset s_reg_t.val8)
  
let dump_read_binop_behaviors s_read_t =
  let dump_vbvd32set =
    VarBinopVarInt32Set.iter (fun (v,b,vr,d32) -> 
      f_printf "%s %s= [%s.pre+0x%lx]\n" 
       (PpIR.ppVar v)
       (PpIR.ppBinop b)
       (PpIR.ppVar vr)
        d32)
  in
  if not
   (VarBinopVarInt32Set.is_empty s_read_t.val8  &&
    VarBinopVarInt32Set.is_empty s_read_t.val16 && 
    VarBinopVarInt32Set.is_empty s_read_t.val32)
  then 
   (f_printf "Memory read binop behaviors:\n";
    dump_vbvd32set s_read_t.val32;
    dump_vbvd32set s_read_t.val16;
    dump_vbvd32set s_read_t.val8)

let dump_write_binop_behaviors s_write_t =
  let dump_vbvd32set =
    VarBinopVarInt32Set.iter (fun (v,b,vr,d32) -> 
      f_printf "[%s.pre+0x%lx] %s= %s\n" 
       (PpIR.ppVar vr)
        d32
       (PpIR.ppBinop b)
       (PpIR.ppVar v))
  in
  if not
   (VarBinopVarInt32Set.is_empty s_write_t.val8  &&
    VarBinopVarInt32Set.is_empty s_write_t.val16 && 
    VarBinopVarInt32Set.is_empty s_write_t.val32)
  then 
   (f_printf "Memory write binop behaviors:\n";
    dump_vbvd32set s_write_t.val32;
    dump_vbvd32set s_write_t.val16;
    dump_vbvd32set s_write_t.val8)


let print_candidate_results cr = 
  let _ = f_printf "Preserved registers: {" in
  print_triplicate dump_varset cr.c_preserved_regs;
  f_printf "}\n";
  
  print_triplicate dump_vsmap cr.c_copied_regs;
  dump_set_const                cr.c_set_const;
  dump_load_const               cr.c_mem_read_const;
  dump_write_const              cr.c_mem_write_reg;
  dump_register_binop_behaviors cr.c_reg_binops;
  dump_read_binop_behaviors     cr.c_read_binops;
  dump_write_binop_behaviors    cr.c_write_binops;

  f_printf "\n%!"

let generate_sequence_behaviors_candidates ioc = 
  let c_write_binops,c_read_binops,c_reg_binops = determine_aggregate_binop_behaviors ioc in
  {
    c_preserved_regs = determine_aggregate_preserve_behaviors ioc;
    c_copied_regs    = determine_aggregate_copy_behaviors ioc;
    c_set_const      = determine_aggregate_load_const32_behaviors ioc;
    c_mem_read_const = determine_aggregate_read_reg32_behaviors ioc;
    c_mem_write_reg  = determine_aggregate_write_reg32_behaviors ioc;
    c_write_binops   = c_write_binops;
    c_read_binops    = c_read_binops;  
    c_reg_binops     = c_reg_binops;
  }  

(* So what do I need to do here?
   Now that I have split everything up by size, I need to reflect that in this
   function.  Except I want to be smart about it instead of re-verifying facts
   that follow directly from what I've already verified.
   
   What that means in practice:
   
   COPIES:
   
   If a 32-bit register is verified as a copy, then it is immediately verified
   that the 16- and 8-bit (if applicable) subregisters are copied.  Therefore,
   we do not need to verify these.  However, it could be the case that there
   are 16-bit copies, or 8-bit copies, with no corresponding 32-bit copy, so we
   want to be sure that we account for that as well.
   
   So:  
   1) Verify 32-bit first.  Add any 16-bit and 8-bit copies to the verified 
   sets, and remove those from the potential copy sets.  
   2) Verify 16-bit next.  Add any 8-bit copies to the verified sets, and then
   remove those from the potential copy sets.
   3) Verify 8-bit copies.   
   
   BINOPS:
   
   The same comments above apply to the bitwise operators AND, OR, and XOR.
   If any of those behaviors are verified for the larger 32- or 16-bit 
   registers, add them to the smaller verified sets and remove them from the
   potential sets.
   
   Those same comments do not apply to other binops.  Verify those 
   independently.
   
   SET CONST32:
   
   * Any 32-bit load can be used as a 16- or 8-bit load.  Add those and then
     remove them from the potential loads.  ACTUALLY, THEY WILL NOT BE IN THE
     POTENTIAL LOAD SETS, SINCE THE READ EMULATION ONLY TRACKS MEMORY ACCESSES
     BY THE SIZE OF THE READ.  BUT THIS GIVES US THE OPPORTUNITY TO ADD THEM
     ANYWAY.
   * Same comments for 16-bit loads.
   * Do 8-bit loads.

   READ REG32:
   
   * Exactly the same comments as the previous case, since they are the same
     case.
   
   WRITE REG32:
   * This one is trickier.  We should not treat a 32-bit write as being 
     synonymous with an 8-bit write, since we want to bound the side effects.
     So just verify these as they are.
   
   PRESERVE:
   
   Exactly the same comments as for copies.  Don't bother verifying that
   the 16- or 8-bit subregisters are preserved, if we have verified the
   larger 32- or 16-bit superregisters are preserved.  I.e., once a register
   is verified as being preserved, mark the subregisters as preserved, and
   also remove them from the potential set (so we don't have to verify them
   again).
   
   *)
let verify_copy_behaviors verify ssa_tbl m_copies size set =
  IRUtil.VarMap.fold
   (fun invar s set ->
      IRUtil.VarSet.fold
       (fun outvar set ->
          if verify (verify_reg_copy ssa_tbl invar outvar size) 
          then VarVarSet.add (invar,outvar) set
          else set)
        s
        set)
    m_copies
    set

(* If it is the case that a 32-bit copy was verified, then it should
   always be the case that a corresponding 16-bit candidate was 
   identified.  So print a message if something fails (in which case,
   some weird bug is happening). *)
let prune_redundant_subregister_copies s_copy_pairs_in m_copies =
  let open IRUtil in
  VarVarSet.fold
   (fun (vsrc,vdst) map ->
      let set_opt = try Some(VarMap.find vsrc map) with Not_found -> None in
      let map = VarMap.remove vsrc map in
      match set_opt with
      | Some(s) -> 
        let s = VarSet.remove vdst s in 
        if VarSet.is_empty s
        then map
        else VarMap.add vsrc s map
      | None -> f_printf 
          "Pre-verified subregister copy %s -> %s (inherited from a verified larger copy) was not a candidate!\n"
          (PpIR.ppVar vsrc)
          (PpIR.ppVar vdst);
        map)
    s_copy_pairs_in
    m_copies

(* Given:
   s_copy_pairs_verified: the "larger" pre-verified facts
   size_larger:  the size of the variables in s_copy_pairs_verified
   s_copy_pairs:  the set of "smaller" pre-verified facts (returned modified)
   size_smaller: the size of the variables in s_copy_pairs *)

  (* Once we verify any 32-bit copies, we get the 16-bit ones for free, and the
     8-bit ones too (if the register supports 8-bit subdivisions). *)
let generate_smaller_copy_facts s_copy_pairs_verified f_derive_equalities_l =
  VarVarSet.fold 
   (fun (vsrc,vdst) set -> 
      let l = f_derive_equalities_l vsrc vdst in
      List.fold_left (fun set (vs,vd) -> VarVarSet.add (vs,vd) set) set l)
    s_copy_pairs_verified
    VarVarSet.empty 

let map_32bit_to_16bit vsrc vdst  = 
  let open X86ToIRUtil in
  [(v16_of_v32_exn vsrc, v16_of_v32_exn vdst)]

let map_16bit_to_8bit  vsrc vdst = 
  let open X86ToIRUtil in let open IRUtil in
  match v8p_opt_of_v16_exn vsrc,v8p_opt_of_v16_exn vdst with
  | Some(vsrch8,vsrcl8),Some(vdsth8,vdstl8) -> [(vsrcl8,vdstl8);(vsrch8,vdsth8)]
  | _,_ -> []

let verify_copy_behaviors verify ssa_tbl m_copies_t =
  let s_copy_pairs32  = verify_copy_behaviors verify ssa_tbl m_copies_t.val32 IR.TypeReg_32 VarVarSet.empty in
  let s_copy_pairs16  = generate_smaller_copy_facts s_copy_pairs32 map_32bit_to_16bit in
  let m_copies_16     = prune_redundant_subregister_copies s_copy_pairs16 m_copies_t.val16 in

  let s_copy_pairs16  = verify_copy_behaviors verify ssa_tbl m_copies_16 IR.TypeReg_16 s_copy_pairs16 in
  let s_copy_pairs8   = generate_smaller_copy_facts s_copy_pairs16 map_16bit_to_8bit in
  let m_copies_8      = prune_redundant_subregister_copies s_copy_pairs8 m_copies_t.val8 in

  let s_copy_pairs8   = verify_copy_behaviors verify ssa_tbl m_copies_8  IR.TypeReg_8  s_copy_pairs8  in
  { val32 = s_copy_pairs32; val16 = s_copy_pairs16; val8 = s_copy_pairs8; }

let verify_read_behaviors p verify ssa_tbl size s_read_reg_t =
  VarVarInt32Set.filter 
   (fun (vdst,vreg,d32) -> verify (IRUtil.mk_and p (verify_read_reg ssa_tbl vdst vreg d32 size)))
   (select_member size s_read_reg_t)

let verify_read_behaviors p verify ssa_tbl s_read_reg_t =
  (* Given a read of, say, a 32-bit quantity, there will be no corresponding read
     behaviors for the 16- and 8-bit registers.  So for every verified 32-bit read,
     we should add the following:
     1)  A verified 16-bit read at the same address
     2)  A verified 8-bit read at the same address
     3)  A verified 8-bit read at address+1
     
     Similarly, for verified 16-bit reads, we should add:
     1)  A verified 8-bit read at the same address
     2)  A verified 8-bit read at address+1 *)
  let s_read_pairs_t = triplicate (verify_read_behaviors p verify ssa_tbl) s_read_reg_t in
(*dump_load_const s_read_pairs_t;*)

  let s_read_pairs16 = 
    VarVarInt32Set.fold 
     (fun (vr,vm,d32) set ->
        VarVarInt32Set.add (X86ToIRUtil.v16_of_v32_exn vr,vm,d32) set)
      s_read_pairs_t.val32
      s_read_pairs_t.val16
  in
  let s_read_pairs8 = 
    VarVarInt32Set.fold 
     (fun (vr,vm,d32) set ->
        match X86ToIRUtil.v8p_opt_of_v16_exn vr with
        | Some(vh,vl) -> VarVarInt32Set.add (vh,vm,Int32.succ d32) (VarVarInt32Set.add (vl,vm,d32) set)
        | None -> set)
      s_read_pairs16
      s_read_pairs_t.val8
  in
  { s_read_pairs_t with val16 = s_read_pairs16; val8 = s_read_pairs8; }
  
(* 
  Need to make this an implication instead of a conjunction
  I.e., 
  
  IRUtil.mk_implies (mk_memory_fence vreg d32) X
  
  Need to think about how this is going to behave with respect to validity
  
  So let's say that X is to be checked for validity
  I.e., X should be unsatisfiable
  
  What happens if we do Y => X?
  
  !Y || X
  
  If !Y is true, then the formula is valid
  If !Y is false, i.e., Y is true, then X must be checked for validity according to the logical assertions in Y.
  
  So we should just straight-up validity check 
  
  fence => phi.
*)
let fence_amount32 = 0x1000l

let mk_memory_fence vreg d32 =
  let add = IRUtil.mk_add (IRUtil.mk_evar vreg) (IRUtil.mk_dword_of_int32 d32) in
  let dfence = IRUtil.mk_dword_of_int32 fence_amount32 in
  let eesp = X86ToIRUtil.eEsp in
  IRUtil.mk_or 
   (IRUtil.mk_sle (IRUtil.mk_add add dfence) eesp)
   (IRUtil.mk_sle (IRUtil.mk_add eesp dfence) add)


(* No fancy behaviors for writes.  I.e. do not treat a 32-bit write as a 16-
   or 8-bit one. *)
let verify_write_behaviors p verify ssa_tbl size s_write_reg_t =
  VarVarInt32Set.filter 
   (fun (vsrc,vreg,d32) -> verify (IRUtil.mk_and p (verify_write_reg ssa_tbl vreg d32 vsrc size)))
   (select_member size s_write_reg_t)

let verify_write_behaviors p verify ssa_tbl s_write_reg_t =
  triplicate (verify_write_behaviors p verify ssa_tbl) s_write_reg_t

let verify_set_const_behaviors verify ssa_tbl size s_set_const =
  VarInt32Set.filter 
   (fun (v,i32) -> verify (verify_set_const ssa_tbl v i32 size))
    s_set_const

let map_32bit_to_16bit var i32 = 
  [(X86ToIRUtil.v16_of_v32_exn var, Int32.logand i32 0xffffl)]

let map_16bit_to_8bit  var i32 = 
  let i8 i32 = Int32.logand 0xffl i32 in
  match X86ToIRUtil.v8p_opt_of_v16_exn var with
  | Some(vh8,vl8) -> [(vh8,i8 (Int32.shift_right_logical i32 8));(vl8,i8 i32)]
  | None -> []

let generate_smaller_set_const_facts s_set_const_verified f_derive_equalities_l =
  VarInt32Set.fold 
   (fun (var,i32) set -> 
      let l = f_derive_equalities_l var i32 in
      List.fold_left (fun set (v,i32) -> VarInt32Set.add (v,i32) set) set l)
    s_set_const_verified
    VarInt32Set.empty

let prune_redundant_set_const s_verified s_candidate =
  VarInt32Set.filter (fun p -> not (VarInt32Set.mem p s_verified)) s_candidate

let verify_set_const_behaviors verify ssa_tbl s_set_const_t =
  let s_set_const32    = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_32 s_set_const_t.val32 in

  let s_set_const16_pv = generate_smaller_set_const_facts s_set_const32 map_32bit_to_16bit in
  let s_set_const16    = prune_redundant_set_const s_set_const16_pv s_set_const_t.val16 in

  let s_set_const16    = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_16 s_set_const16 in
  let s_set_const16    = VarInt32Set.union s_set_const16 s_set_const16_pv in

  let s_set_const8_pv  = generate_smaller_set_const_facts s_set_const16 map_16bit_to_8bit in
  let s_set_const8     = prune_redundant_set_const s_set_const8_pv s_set_const_t.val8 in

  let s_set_const8  = verify_set_const_behaviors verify ssa_tbl IR.TypeReg_8  s_set_const8  in
  let s_set_const8  = VarInt32Set.union s_set_const8 s_set_const8_pv in
  { val32 = s_set_const32; val16 = s_set_const16; val8 = s_set_const8; }

let mk_conjoined_preserve_expr ssa_tbl size s_preserved = 
  IRUtil.VarSet.fold 
   (fun v e -> IRUtil.mk_and (verify_reg_preserved ssa_tbl v size) e) 
    s_preserved
    IRUtil.mk_true
  
let verify_preserved_behaviors verify ssa_tbl size s_preserved =
  if verify (mk_conjoined_preserve_expr ssa_tbl size s_preserved)
  then s_preserved
  else IRUtil.VarSet.filter (fun v -> verify (verify_reg_preserved ssa_tbl v size)) s_preserved

let verify_preserved_behaviors verify ssa_tbl s_preserved_t =
  let s_preserved32 = verify_preserved_behaviors verify ssa_tbl IR.TypeReg_32 s_preserved_t.val32 in
  let s_preserved16_pv = 
    IRUtil.VarSet.fold 
     (fun v32 set -> IRUtil.VarSet.add (X86ToIRUtil.v16_of_v32_exn v32) set) 
      s_preserved32 
      IRUtil.VarSet.empty
  in
  let s_preserved16 = IRUtil.VarSet.diff s_preserved_t.val16 s_preserved16_pv in
  let s_preserved16 = 
    if IRUtil.VarSet.is_empty s_preserved16
    then s_preserved16
    else verify_preserved_behaviors verify ssa_tbl IR.TypeReg_16 s_preserved16
  in
  let s_preserved16 = IRUtil.VarSet.union s_preserved16_pv s_preserved16 in

  let s_preserved8_pv = 
    IRUtil.VarSet.fold 
     (fun v16 set -> 
      match X86ToIRUtil.v8p_opt_of_v16_exn v16 with
      | Some(vh8,vl8) -> IRUtil.VarSet.add vl8 (IRUtil.VarSet.add vh8 set)
      | None -> set) 
      s_preserved16 
      IRUtil.VarSet.empty
  in
  let s_preserved8 = IRUtil.VarSet.diff s_preserved_t.val8 s_preserved8_pv in
  let s_preserved8 = 
    if IRUtil.VarSet.is_empty s_preserved8
    then s_preserved8
    else verify_preserved_behaviors verify ssa_tbl IR.TypeReg_8 s_preserved8
  in
  let s_preserved8 = IRUtil.VarSet.union s_preserved8_pv s_preserved8 in
  { val32 = s_preserved32; val16 = s_preserved16; val8 = s_preserved8; }

let verify_register_binop_behaviors verify ssa_tbl size s_vvbv =
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

type verified_results =
{
  stack_displacement: int32;
  preserved_regs:     IRUtil.VarSet.t triplicate;
  copied_regs:        VarVarSet.t triplicate;
  set_const:          VarInt32Set.t triplicate;
  mem_read_const:     VarVarInt32Set.t triplicate;
  mem_write_reg:      VarVarInt32Set.t triplicate;
  write_binops:       VarBinopVarInt32Set.t triplicate;
  read_binops:        VarBinopVarInt32Set.t triplicate;  
  reg_binops:         VarVarBinopVarSet.t triplicate;
}

let print_verified_results vr = 
  let _ = f_printf "\nESP displacement: 0x%lx\n" vr.stack_displacement in
  
  f_printf "Preserved registers: {";
  print_triplicate dump_varset vr.preserved_regs;
  f_printf "}\n";
  
  dump_reg_copies               vr.copied_regs;
  dump_set_const                vr.set_const;
  dump_load_const               vr.mem_read_const;
  dump_write_const              vr.mem_write_reg;
  dump_register_binop_behaviors vr.reg_binops;
  dump_read_binop_behaviors     vr.read_binops;
  dump_write_binop_behaviors    vr.write_binops;
  f_printf "\n"

(* Makes the following condition, which should be UNSAT to be true: 
  (For all w in l_w_byte_addr32. (idx != w)) && mempre[idx] != mempost[idx] *)
let mk_write_safety_predicate l_byte_expr vmempost =
  let vidx     = IRUtil.new_var IR.TypeReg_32 in
  let eidx     = IRUtil.mk_evar vidx in
  let emempost = IRUtil.mk_evar vmempost in
  let addrne be = IRUtil.mk_ne eidx be in
  let snd   = IRUtil.mk_ne 
    (IRUtil.mk_load X86ToIRUtil.eMem eidx IR.TypeReg_8)
    (IRUtil.mk_load emempost         eidx IR.TypeReg_8)
  in 
  let addrexpr = match l_byte_expr with
  | []     -> IRUtil.mk_true
  | hd::tl -> List.fold_left (fun p a32 -> IRUtil.mk_and p (addrne a32)) (addrne hd) tl
  in 
  IRUtil.mk_and snd addrexpr

let generate_memloc_set access_t access_binop_t = 
  (* Generate list of verified write locations *)
  let memlocs_t = { val8 = []; val16 = []; val32 = []; } in
  let memlocs_t = 
    triple_fold_triple 
      VarVarInt32Set.fold 
     (fun (_,vr,d32)   acc -> (vr,d32)::acc) 
      memlocs_t 
      access_t 
  in
  let memlocs_t = 
    triple_fold_triple 
      VarBinopVarInt32Set.fold 
     (fun (_,_,vr,d32) acc -> (vr,d32)::acc) 
      memlocs_t 
      access_binop_t 
  in
  
  let expr_set size list = 
    let n = Int32.of_int (IRUtil.bits size / 8) in
    let rec aux i acc = 
      if i = n
      then acc
      else 
        let set = 
          List.fold_left 
           (fun set (vr,d32) -> VarInt32Set.add (vr,Int32.add d32 i) set) 
            acc 
            list 
        in
        aux (Int32.succ i) set
    in
    aux 0l VarInt32Set.empty
  in
  let memlocs_t = triplicate_map expr_set memlocs_t in
  VarInt32Set.union (VarInt32Set.union memlocs_t.val8 memlocs_t.val16) memlocs_t.val32
  
let generate_memloc_list s_memaddrs s_write_reg_t s_write_binop_t = 
  VarInt32Set.elements s_memaddrs

let mk_addr_expr v32 d32 = IRUtil.mk_add (IRUtil.mk_evar v32) (IRUtil.mk_dword_of_int32 d32)

let generate_memexpr_list l_memaddrs =
  List.map (fun (v32,d32) -> mk_addr_expr v32 d32) l_memaddrs

let mk_distinction_assertion s_memlocs1 s_memlocs2 =
  VarInt32Set.fold (fun (vout,dout) p -> 
    VarInt32Set.fold (fun (vin,din) p ->
      if vin <> vout
      then IRUtil.mk_and (IRUtil.mk_ne (mk_addr_expr vout dout) (mk_addr_expr vin din)) p
      else p)
      s_memlocs1
      p)
    s_memlocs2
    IRUtil.mk_true
    
let mk_write_distinction_assertion s_memlocs = 
  mk_distinction_assertion s_memlocs s_memlocs
  
let has_unconstrained_memaccesses s_memregs ir =
  let memregasg = List.map (fun v -> (v,nonzero_rand_i32 ())) (IRUtil.VarSet.elements s_memregs) in
  let ht = new_ht IR.TypeReg_32 memregasg in
  let oir = IRLocalOpt.local_opt_state_in ht ir in
(*List.iter (fun i -> f_printf "%s\n" (PpIR.ppInstr i)) bir;*)
  
  let open IR in  
  let rec extract_memaddrs = function
  | Cast(_,_,e)
  | Unop(_,e)        -> extract_memaddrs e
  | Load(e1,a,_)     -> a::(extract_memaddrs e1)
  | Let(_,e1,e2)
  | Binop(e1,_,e2)   -> (extract_memaddrs e1)@(extract_memaddrs e2)                  
  | Store(e1,a,e3,_) -> a::((extract_memaddrs e1)@(extract_memaddrs e3))
  | Var(_)
  | Const(_)         -> []
  in

  let extract_memaddrs = function
  | Assign(_,e)     
  | Jmp(e)          
  | Halt(e)         
  | Assert(e)       -> extract_memaddrs e
  | CJmp(e1,e2,e3)  -> (extract_memaddrs e1)@(extract_memaddrs e2)@(extract_memaddrs e3)
  | Label(_)
  | Special(_)
  | Comment(_)      -> []
  in

  let memaddrs = List.fold_left (fun acc i -> (extract_memaddrs i)@acc) [] oir in
  List.exists (function | Const(_,_) -> false | _ -> true) memaddrs

(* So in which order should I verify these properties?
   DEPENDENCIES:
   
   Set const:  Technically, this could be affected by reads and writes, i.e., false negatives.  But 
   it is very useful to do this first, so as to reduce the number of queries regarding binops.  At 
   worst, we could issue this query again after verifying the reads and writes.

   Write reg binops: affects reads.
   Memwrite  binops: affects reads.
   
   Read reg:     this is affected by prior writes.
   Read binops:  affected by prior writes.

   Copy:       I guess technically this could go by way of a write to memory, followed by a read
   Reg binops: I guess technically this could go by way of a write to memory, followed by a read
   
   So, verify writes first.  Then verify write safety.
   Verify reads next.  Then verify read safety.
   
   Then verify everything else.
   *)


(* let _ = IDA.msg "2\n" in*)

(* Remove binops such as add [eax], al and add al, [eax] where the value register
   depends upon the address register. *)
let remove_dependent_mem_binops s_mem_binop_t =
  let had_dep = ref false in
  let s_w32 = 
    VarBinopVarInt32Set.filter 
     (fun (vv,_,vr,_)  -> if vv <> vr then true else (had_dep := true; false))
      s_mem_binop_t.val32 
  in
  let s_w16 = 
    VarBinopVarInt32Set.filter 
     (fun (v16,_,vr,_) -> 
        if v16 <> (X86ToIRUtil.v16_of_v32_exn vr) then true else (had_dep := true; false))
      s_mem_binop_t.val16 
  in
  let s_w8 = 
    VarBinopVarInt32Set.filter 
     (fun (v8,_,vr,_) -> 
        match X86ToIRUtil.v8p_opt_of_v32_exn vr with
        | Some(vh8,vl8) -> 
          if v8 <> vh8 && v8 <> vl8 then true else (had_dep := true; false)
        | None -> true) 
      s_mem_binop_t.val8
  in
  (!had_dep,{ val8 = s_w8; val16 = s_w16; val32 = s_w32; })

let verify_candidate_sequence_behaviors ir candidates disp32 verify ssa_tbl (*z3debug*) =
  let s_c_write_memlocs = generate_memloc_set candidates.c_mem_write_reg candidates.c_write_binops in
  let pwrite = mk_write_distinction_assertion s_c_write_memlocs in
  
  f_printf "Write indistinction predicate: %s\n" (PpIR.ppExpr false pwrite);

  let b_w_dep,c_write_binops = remove_dependent_mem_binops candidates.c_write_binops in
  let s_write_reg_t          = verify_write_behaviors pwrite verify ssa_tbl candidates.c_mem_write_reg in
  let s_write_binop_t        = verify_memwrite_binop_behaviors pwrite verify ssa_tbl c_write_binops in  
  
  let s_write_memlocs = generate_memloc_set s_write_reg_t s_write_binop_t in

  let bwritesafe = 
    match ht_find_opt ssa_tbl X86ToIRUtil.vMem with
    (* Syntactically did not write to memory; must be write-safe *)
    | None -> true
    (* Syntactically did write to memory, so check further *)
    | Some(memvar) ->
      let memlocs = generate_memexpr_list (VarInt32Set.elements s_write_memlocs) in
    (*f_printf "Memlocs: ";
      List.iter (fun e -> f_printf "%s " (PpIR.ppExpr false e)) memlocs;
      f_printf "\n";*)
      let res = verify (mk_write_safety_predicate memlocs memvar) in
      f_printf "\nWrite safety predicate: %b\n" res;
      res
  in
  
  (* If we weren't write-safe, there's no point in verifying anything else. *)
  
  if not bwritesafe
  then None
  else 
  
    let s_set_const_t = verify_set_const_behaviors verify ssa_tbl candidates.c_set_const in
    (* Remove register binops where the destination is proven to be a zero constant *)
    let filter_zero_const size set =
      let s_v_const = select_member size s_set_const_t in
      VarBinopVarInt32Set.filter (fun (vr,_,_,_) -> not (VarInt32Set.mem (vr,0l) s_v_const)) set
    in
    let c_read_binops = triplicate_map filter_zero_const candidates.c_read_binops in
    let b_r_dep,c_read_binops  = remove_dependent_mem_binops c_read_binops in
    
    let filter_zero_const size set =
      let s_v_const = select_member size s_set_const_t in
      VarVarBinopVarSet.filter (fun (vr,_,_,_) -> not (VarInt32Set.mem (vr,0l) s_v_const)) set
    in

    let s_c_read_memlocs = generate_memloc_set candidates.c_mem_read_const c_read_binops in
    let pread = mk_distinction_assertion s_c_read_memlocs s_write_memlocs in
    
    let s_read_binop_t = verify_memread_binop_behaviors pread verify ssa_tbl c_read_binops in
    let s_read_reg_t   = verify_read_behaviors pread verify ssa_tbl candidates.c_mem_read_const in    
    
    let s_read_memlocs = generate_memloc_set s_read_reg_t s_read_binop_t in
    
    let collect_memregs = VarInt32Set.fold (fun (v,_) set -> IRUtil.VarSet.add v set) in
    
    let memregs = collect_memregs s_write_memlocs (IRUtil.VarSet.singleton esp) in
    let memregs = collect_memregs s_read_memlocs  memregs in
    
    let bhas_unconstrained_memaccesses = has_unconstrained_memaccesses memregs ir in
    
    f_printf "Has unconstrained memaccesses: %b\n" bhas_unconstrained_memaccesses;
    
    if bhas_unconstrained_memaccesses
    then None
    else
      let c_reg_binops = triplicate_map filter_zero_const candidates.c_reg_binops in
      let s_copy_pairs_t         = verify_copy_behaviors verify ssa_tbl candidates.c_copied_regs in
      let s_reg_binop_t          = verify_register_binop_behaviors verify ssa_tbl c_reg_binops in
      
      let vve_t t  = let e = VarVarSet.is_empty           in e t.val32 && e t.val16 && e t.val8 in
      let vie_t t  = let e = VarInt32Set.is_empty         in e t.val32 && e t.val16 && e t.val8 in
      let vvie_t t = let e = VarVarInt32Set.is_empty      in e t.val32 && e t.val16 && e t.val8 in
      let vvbv_t t = let e = VarVarBinopVarSet.is_empty   in e t.val32 && e t.val16 && e t.val8 in
      let vbvi_t t = let e = VarBinopVarInt32Set.is_empty in e t.val32 && e t.val16 && e t.val8 in
      
      (* Don't compute the preserved set unless it has some other interesting behaviors *)
      let s_preserved = 
        if not 
           (* Did it have any verifiable behaviors at all? *)
           ((vve_t s_copy_pairs_t)   &&
            (vie_t s_set_const_t)    &&
            (vvie_t s_read_reg_t)    &&
            (vvie_t s_write_reg_t)   &&
            (vbvi_t s_write_binop_t) &&
            (vbvi_t s_read_binop_t)  &&
            (vvbv_t s_reg_binop_t))
        then verify_preserved_behaviors verify ssa_tbl candidates.c_preserved_regs
        else { val8 = IRUtil.VarSet.empty; val16 = IRUtil.VarSet.empty; val32 = IRUtil.VarSet.empty; }
      in
      Some({
        stack_displacement = disp32;
        preserved_regs     = s_preserved;
        copied_regs        = s_copy_pairs_t;
        set_const          = s_set_const_t;
        mem_read_const     = s_read_reg_t;
        mem_write_reg      = s_write_reg_t;
        write_binops       = s_write_binop_t;
        read_binops        = s_read_binop_t;
        reg_binops         = s_reg_binop_t;
      })
  
(* Reject a sequence immediately if:
* x86 syntactically uses:
* * FS/GS prefix
* * Hard-coded memory accesses
* * 16-bit memory locations when instruction is not LEA
* * Memory locations that are not 8/16/32 bits
* * Control/debug/segment/FPU/XMM/MMX registers
* * Far pointers
* * Jump targets
*)
let x86_syntactic_reject { X86.pref = pref; X86.instr = (m,o); } = 
  let open X86 in
  let is_lea = match m with | Lea -> true | _ -> false in

  let reject_operand = function
  (* Don't reject general registers or immediates *)
  | GeneralReg(_) -> false
  | Immediate(_) -> false
  
  (* Reject anything with an FS prefix *)
  | Memexpr(Mb(Mem32(FS,_,_,_))) -> true
  | Memexpr(Mw(Mem32(FS,_,_,_))) -> true
  | Memexpr(Md(Mem32(FS,_,_,_))) -> true

  (* Reject anything with a GS prefix *)
  | Memexpr(Mb(Mem32(GS,_,_,_))) -> true
  | Memexpr(Mw(Mem32(GS,_,_,_))) -> true
  | Memexpr(Md(Mem32(GS,_,_,_))) -> true  

  (* Reject anything with a hard-coded memory address *)
  | Memexpr(Mb(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Mw(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Md(Mem32(_,None,None,Some(_)))) -> true

  (* Reject 16-bit memory addresses when the instruction is not LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) when not is_lea -> true
  | Memexpr(Mw(Mem16(_,_,_,_))) when not is_lea -> true
  | Memexpr(Md(Mem16(_,_,_,_))) when not is_lea -> true
  
  (* Accept 16-bit memory access if instruction is LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) -> false
  | Memexpr(Mw(Mem16(_,_,_,_))) -> false
  | Memexpr(Md(Mem16(_,_,_,_))) -> false
  
  (* Accept 8/16/32-bit memory access if not rejected above *)
  | Memexpr(Mb(_)) -> false
  | Memexpr(Mw(_)) -> false
  | Memexpr(Md(_)) -> false
  
  (* Reject "large" memory accesses *)
  | Memexpr(Mf(_)) -> true
  | Memexpr(Mq(_)) -> true
  | Memexpr(Mt(_)) -> true
  | Memexpr(Mdq(_)) -> true

  (* Reject anything with a control/debug/segment/FPU/MMX/XMM register *)
  | ControlReg(_) -> true
  | DebugReg(_) -> true
  | SegReg(_) -> true
  | FPUReg(_) -> true
  | MMXReg(_) -> true
  | XMMReg(_) -> true
  
  (* Reject anything with a far pointer *)
  | FarTarget(_) -> true
  
  (* Reject jumps (should have been rejected already anyway) *)
  | JccTarget(_) -> true
  
  in
  (* Apply the test above to all operands *)
  let rec aux = function
  | [] -> false
  | o::os -> if reject_operand o then true else aux os
  in 
  
  (* Reject direction flag manipulations *)
  if m = X86.Cld || m = X86.Std || m = X86.Sti || m = X86.Cli
  then true
  else aux o

let determine_sequence_behaviors x86l ir n =
  if (List.exists x86_syntactic_reject x86l)
  then None
  else
  begin
    let ir = match List.rev ir with
    | [] -> f_printf "IR list empty?"; ir
    | IR.Jmp(_)::is
    | IR.CJmp(_,_,_)::is -> List.rev is
    | _ -> ir 
    in
    
  (*f_printf "IR:\n%!";
    List.iter (fun i -> f_printf "%s\n" (PpIR.ppInstr i)) ir;*)
    let ioc = make_io_structs ir n in
  (*print_io_container ioc;*)
    
    let ssa_tbl,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars ir in

  (*f_printf "IR:\n%!";
    List.iter (fun i -> f_printf "%s\n" (PpIR.ppInstr i)) ir_ssa;*)

    (*let z3ctx = Z3SymbolicExecute.mk_context () in
    List.iter (Z3SymbolicExecute.instr_to_z3 z3ctx) ir_ssa;*)
    
    let verify pc = 
      let before = Sys.time () in
      let z3ctx = Z3.mk_context_x [|("MODEL", "true");("SOFT_TIMEOUT", "10000")|] in
      let _ = Z3.set_logic z3ctx "QF_ABV" in
      let str = Z3SymbolicExecute.symbolic_execute z3ctx ir_ssa pc in
    (*let ctxt_str = Z3.context_to_string     z3ctx in*)
      (* blaaah, what a piece of shit *)
      let res = (str = "unsat\n") in
      
      Z3.del_context z3ctx;
      let after = Sys.time () in
      f_printf "Verified in %f: %s was %b\n" (after -. before) (PpIR.ppExpr false pc) res;
    (*f_printf "Solver context: %s\n" ctxt_str;*)
      res
    in

(*
    let verify pc = 
      let _  = Z3.push z3ctx in
      let before = Sys.time () in
      let _ = Z3SymbolicExecute.symbolic_execute z3ctx [] pc in
      let res = match Z3.check z3ctx with
      | Z3.L_FALSE -> true
      | _ -> false
      in
      let after = Sys.time () in
      let _ = Z3.pop z3ctx 1 in
      f_printf "Verified in %f: %s was %b\n" (after -. before) (PpIR.ppExpr false pc) res;
      res
    in
*)

    (* Reject a sequence immediately if:
     * ESP-delta is not:
     * * consistent
     * * positive
     * * a multiple of 4
     *)
    match determine_aggregate_esp_delta ioc with
    | None -> None
    | Some(disp32) -> 
      if (Int32.logand 3l disp32 <> 0l) || (disp32 <= 0l)
      then None
      else
       (if (verify (verify_stack_differential ssa_tbl disp32))
        then 
          let _ = List.iter (fun x -> f_printf "%s\n" (X86Disasm.string_of_x86instr x)) x86l in
          let candidates = generate_sequence_behaviors_candidates ioc in
        (*print_candidate_results candidates;*)
          let res = verify_candidate_sequence_behaviors ir candidates disp32 verify(*(fun _ -> false)*) ssa_tbl (*z3ctx*) in
          f_printf "%!\n";
        (*Z3.del_context z3ctx;*)
        (*print_verified_results res;*)
          res
        else None)
  end

(*
(* Functions for generating random int32s, and pairing them with values *)
let nonzero_rand_i32 () = Int32.succ (Random.int32 (Int32.pred Int32.max_int))
let nonzero_rand_i32_4 () = Int32.logand  (nonzero_rand_i32 ()) 0xffffffcl

(* Generate dwords to store in [esp_val-this,esp_val+this] *)
let stack_range_size32 = 0x20

(* As yet, l_stack_variables is unused *)
let l_stack_variables,m_stack_variables = 
  let rec aux (vl,vdl) disp =
    if disp = stack_range_size32 
    then (vl,vdl)
    else 
      let v = IRUtil.new_var IR.TypeReg_32 in
      aux (v::vl,(v,disp)::vdl) (disp+4)
  in 
  let vl,vdl = aux ([],[]) ~-stack_range_size32 in
  vl,var_mapify vdl

let print_stack_context = 
  List.iter (fun (v,i32) -> 
    let d = IRUtil.VarMap.find v m_stack_variables in
    f_printf "%s(%d) -> 0x%lx\n" (PpIR.ppVar v) d i32)

    c_write_binops   = c_write_binops;
    c_read_binops    = c_read_binops;  
    c_reg_binops     = c_reg_binops;


  let bwrite_read_reg_t = determine_aggregate_binop_behaviors ioc in
  f_printf "Binop behaviors:\n";
(*print_triplicate dump_binop_behaviors bwrite_read_reg_t;*)

let mk_memory_distinction_assertion s_
  let s_memaddrs = generate_memloc_set s_write_reg_t s_write_binop_t in
  let l_memaddrs = generate_memexpr_list s_memaddrs in


(* For two distinct memories, assert:
  (For all w in l_w_byte_addr32. vmempre1[w] = vmempre2[w]) && 
  (For all general registers reg. reg.post_1_ssa != reg.post_2.ssa)
  
   If this formula is not satisfiable, then the semantics with respect to 
   register output behavior is the same.
   
  *)
let mk_read_safety_predicate l_byte_expr vmempre1 ssaht1 vmempre2 ssaht2 =
  let emempre1 = IRUtil.mk_evar vmempre1 in
  let emempre2 = IRUtil.mk_evar vmempre2 in
  let hd,tl = List.hd l_byte_expr, List.tl l_byte_expr in
  let samemem ae = 
    IRUtil.mk_ne 
     (IRUtil.mk_load emempre1 ae IR.TypeReg_8)
     (IRUtil.mk_load emempre2 ae IR.TypeReg_8)
  in
  let psamemem = List.fold_left (fun c a -> IRUtil.mk_and c (samemem a)) (samemem hd) tl in
  let samereg vreg = IRUtil.mk_ne (IRUtil.mk_evar (Hashtbl.find ssaht2 vreg)) (IRUtil.mk_evar (Hashtbl.find ssaht1 vreg)) in
  let psamereg = List.fold_left (fun c v -> IRUtil.mk_and c (samereg v)) (samereg esp) vregs in
  IRUtil.mk_and psamemem psamereg
  
let generate_new_read_sequence ir =
  let vmem = X86ToIRUtil.vMem in
  let vnewmem = IRUtil.dup_var vmem in
  let ht = Hashtbl.create 5 in
  Hashtbl.replace ht vmem vnewmem;
  let newir = List.map (IRLocalOpt.replace_instr_var_with_var ht) ir in
  let ssa_tbl,ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars newir in
  (vnewmem,ssa_tbl,ir_ssa)

(*
  let p_distinct_mem_addrs =
    let rec aux p = function
    | x::xs -> aux (List.fold_left (fun p y -> IRUtil.mk_and (IRUtil.mk_ne x y) p) p xs) xs
    | [] -> p
    in aux IRUtil.mk_true l_memexprs 
  in
  p_distinct_mem_addrs
*)


*)