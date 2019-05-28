type l_memacc = (IR.var * memloc * IR.typereg) list
type io =
{
  l_io:     ((IR.var,IR.expr) Hashtbl.t * l_memacc * l_memacc) list;
  l_reads:  (memloc * size) list;
  h_reads:  ((memloc * IR.typereg),IR.var) Hashtbl.t;
  l_writes: (memloc * size) list;
  h_writes: ((memloc * IR.typereg),IR.var) Hashtbl.t;
  l_common: (memloc * size) list;
}

let initialize_registers rng32 = 
  let ht = Hashtbl.create 100 in let add v v32 = Hashtbl.replace ht v v32 in

  (* Generate initial register values *)
  let _ =
    List.iter (fun (v32,v16,pv8o) ->
      let c32 = rng32 () in
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
  
  (* Perform emulation, get the output values *)
  let reads,writes = IRRandomizedEvaluator.concrete_evaluate_jumpless_nonssa ir_ssa_after ht memctx in

  (ht,reads,writes)

type memloc = IR.var * int32
type reg = IR.var
type gadget =
| BinopReg of reg * reg * binop * reg
| BinopRead of reg * binop * memloc
| BinopWrite of memloc * binop * reg
| Constant of reg * int32
| Copy of reg * reg
| Read of reg * memloc
| Write of memloc * reg
| WriteConst of memloc * int32

let predicate_of_gadget io e = 
  let sizeof = IRUtil.size_of_var in let n = IRUtil.mk_ne in let e = IRUtil.mk_evar in
  let mem h m s = Hashtbl.find h (m,s) in let mv  h m v = mem h m (sizeof v) in
  let mrv = mv  io.h_reads in let mwv = mv  io.h_writes in 
  let mws = mem io.h_reads in let mws = mem io.h_writes in

  let open IR in match e with 
  | BinopReg(vres,vlhs,b,vrhs) -> n  vres     (Binop(e vlhs,b,e vrhs))
  | Constant(v,i)              -> n  v        (mk_fixed_const_of_i32 i (sizeof v))
  | Copy(vdst,vsrc)            -> n  vdst      vsrc
  | Read(vdst,m)               -> n  vdst     (mrv m vdst)
  | BinopRead(vres,b,m)        -> n  vres     (Binop(vres,b,mrv m vres))
  | Write(m,vdst)              -> n  vdst     (mwv m vdst)
  | WriteConst(m,c,s)          -> n (mws m s) (mk_fixed_const_of_i32 c s)
  | BinopWrite(m,b,v)          -> n (mwv m v) (Binop(read_before m (sizeof v),b,v))

let propagate = function
| BinopWrite(m,b,v)
| Write(m,vdst)
| WriteConst(m,c,s) -> []

(* New gadget for each subregister *)
| Read(vdst,m)    -> List.map (fun (_,r) -> Read(r,m))                  (get_subregisters vdst)
| Constant(v,i)   -> List.map (fun (t,r) -> Constant(r,get_constant i)) (get_subregisters v)
| Copy(vdst,vsrc) -> List.map (fun (rl,rr,_) -> Copy(rl,rr))            (get_pairs vdst vsrc)

(* These binops filter to Low parts:  Add, Mul, Shl, Shr *)
| BinopReg(vres,vlhs,(Add|Mul) as b,vrhs) -> (*|Shl|Shr*)
  ListUtil.map_filter (fun (v,vl,vr,t) -> if is_low t then Some(BinopReg(v,vl,b,vr)) else None) (get_triples vres vlhs vrhs)

(* These binops filter to all subregisters:  And, Or, Xor *)
| BinopReg(vres,vlhs,(And|Or|Xor) as b,vrhs) -> 
  List.map (fun (v,vl,vr,_) -> BinopReg(v,vl,b,vr)) (get_triples vres vlhs vrhs)

| BinopRead(vres,(Add|Mul) as b,m) -> (*|Shl|Shr*)
  ListUtil.map_filter (fun (t,r) -> if is_low t then Some(BinopRead(r,b,m)) else None) (get_subregisters vres)

| BinopRead(vres,(And|Or|Xor) as b,m) -> List.map (fun (_,r) -> BinopRead(r,b,m)) (get_subregisters vres)

type abi =
{
  addresses: Int32Set.t;
  framesize: int32;
  retpos:    int32;
  preserve:  IRUtil.Var.t;
}

let is_const s i64 = function
| IR.Const(i,sz) -> sz == s && i == i64
| e -> failwith ("is_const:  "^(PpIR.ppExpr e)^" did not evaluate to a constant")

let is_false = is_const (TypeReg_1) 0L
let is_eval_false ht e = is_false (IREvaluator.eval ht e)
let all_ios_false io e = List.forall (fun (h,r,w) -> is_eval_false h e) io.l_io
let empirical_filter io = mk_filtered_generator (fun g -> all_ios_false io (predicate_of_gadget g))

let mk_memsub r32 addr = mk_sub (expr_of_reg32 v32) (mk_dword32 addr)
let mk_memadd r32 addr = mk_add (expr_of_reg32 v32) (mk_dword32 addr)
let mk_memloc ht v32 addr = (v32,eval_to_int32 ht (mk_memsub v32 addr))
let mk_memlocs ht addr = List.map (fun x -> mk_memloc ht x addr)
let filter_memlocs ht addr = List.filter (fun (v,i) -> is_eval_false ht (mk_ne (mk_dword32 addr) (mk_memadd v i)))

(* Returns Some(x) if ht[key] exists, None otherwise *)
let ht_find_opt ht key = let res = try Some(Hashtbl.find ht key) with Not_found -> None in res

let predict_memlocs_opt l_io =
  let l_m h a = mk_memlocs h a X86ToIRUtil.l_v_general_registers32 in

  (* Main loop, over I/O pairs, to convert memory addresses to locations. *)
  let rec aux b ptr ptw tr tw = function

  (* #1: collect the memory locations+size accessed by all I/O pairs. *)
  | (h,r,w)::xs ->

    (* Add (memloc,size) pairs to t.  If it's not the first time through the
       loop, then check to see whether they are in pt before adding them. *)
    let inner pt t = List.fold_left (fun first_time (a,v,s) ->
      List.iter (fun m -> if first_time || (ht_find_opt pt (m,s) <> None) then Hashtbl.replace t (m,s) ()) (l_m h a);
      false)
      true
    in 

    (* Process the reads and writes into their respective hash tables *)
    let _,_ = inner ptr tr r,inner ptw tw w in

    (* Rotate the hash tables, progressively filtering the memory locations. *)
    aux false tr tw (Hashtbl.create 10) (Hashtbl.create 10) xs

  (* #2: affix every memory access with its unique memory location (or raise Not_found). *)
  | [] ->
    (* Create variables for the common locations; make new hash tables with these mappings *)
    let map ht = Hashtbl.fold (fun (m,s) _ (h,l) -> Hashtbl.replace h (m,s) (IRUtil.new_var s); (h,(m,s)::l)) 
      ht 
     (Hashtbl.create 10,[]) 
    in
    let tr,lrm,tw,lwm = map ptr,map ptw in
    
    (* lrw:  locations that were both read from and written to (same size). *)
    let lrwm = Hashtbl.fold (fun p r l -> match ht_find_opt tw p with | None -> l | Some(_) -> p::l) tr [] in
    
    (* Transform the I/O pairs. *)
    let rec outer outlist = function
    | (h,r,w)::xs ->
      
      (* Locate the unique memory location for each memory access, insert it 
         into the transformed output list.  Raise Not_found if such a location
         does not exist uniquely. *)
      let rec inner outlist t = function
      | [] -> outlist
      | (a,v,s)::avs -> 
        let ht_expand ht k = match ht_find_opt ht k with | Some(x) -> Some(k,x) | x -> x in

        (* Iterate through the potential memory locations, keeping only one. *)
        let rec innermost r l = match r,l with
        | None,[]       -> raise Not_found
        | Some(x),[]    -> x
        | None,m::ms    -> innermost (ht_expand t (m,s)) ms
        | Some(_),m::ms -> match ht_expand t (m,s) with | None -> innermost r ms | Some(_) -> raise Not_found 
        in
        
        (* Get the single value.  We don't get here if an exception was thrown. *)
        let (m,s),vm = innermost (None) (l_m h a) in
        
        (* Insert the variable->value mappings into the main I/O pair hash table *)
        Hashtbl.replace h vm (mk_fixed_const_of_i32 v s);
        
        (* Transform the memory address lists. *)
        inner ((vm,m,s)::outlist) t avs
      in
      
      (* Transform next I/O pair. *)
      outer ((h,inner [] tr r,inner [] tw w)::outlist) xs
    
    (* Return the new output *)
    | [] -> { l_io = outlist; l_reads = lrm; h_reads = tr; l_writes = lwm; h_writes = tw; l_common = lrwm; }
  in
  try Some(aux true (Hashtbl.create 10) (Hashtbl.create 10) (Hashtbl.create 10) (Hashtbl.create 10) l_io)
  with Not_found -> None
 
let l_binop   = [Add;Mul;And;Or;Xor]
let l_shiftop = [Sar;Shl;Shr]

let mk_binop (d,l,b,r) = BinopReg(d,l,b,r)

let binopreg_comm_generator ldst lsrc =
  let ht = Hashtbl.create 10 in
  let reject_comm ((d,l,b,r) as p) = 
    (l = r) ||
    (try (Hashtbl.find ht (l,b,r); true) with Not_found -> (Hashtbl.replace ht (r,b,l) (); false))
  in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,l_binop,lsrc,Some(reject_comm),mk_binop))

let binopreg_nc_generator ldst lsrc =  
  let open X86ToIRUtil in
  GeneratorDSL.generator_dsl (G4(ldst,lsrc,l_shiftop,l_v_general_registers8,Some(fun (d,l,b,r) -> l = r),mk_binop))

let copy_generator ldst lsrc =
  let reject_copy (vdst,vsrc) = 
    try (IRUtil.VarMap.find X86ToIRUtil.m_vafter_to_vbefore vdst = vsrc)
    with Not_found -> failwith "reject_copy:  LHS not an \"After\" variable"
  in
  GeneratorDSL.generator_dsl (G2(ldst,lsrc,Some(reject_copy),(fun (a,b) -> Copy(a,b))))
  
let constant_generator ldst =
  let m = List.map (fun v -> v,eval_to_int32 (mk_evar v)) in
  GeneratorDSL.generator_dsl (G1(m ldst,None,(fun (v,i) -> Constant(v,i))))

let generate2 f = let open X86ToIRUtil in Generator.mk_sequential_generator 
 [f l_reg32_after l_v_general_registers32_noesp;
  f l_reg16_after l_v_general_registers16_nosp;
  f l_reg8_after  l_v_general_registers8;]

let generate1 f = let open X86ToIRUtil in Generator.mk_sequential_generator 
 [f l_reg32_after;f l_reg16_after;f l_reg8_after ;]

let generate_copy     () = generate2 copy_generator 
let generate_constant () = generate1 constant_generator
let generate_binopreg () = Generator.mk_sequentual_generator 
 [generate2 binop_comm_generator;generate2 binop_nc_generator]

let write_generator lm lv =
  let reject_write ((vm,m,s),vs) = IRUtil.size_of_var vs <> s in
  GeneratorDSL.generator_dsl (G2(lm,lv,Some(reject_write),(fun ((vm,m,s),vs) -> Write(m,vs))))

let read_generator lv lm =
  let reject_read (vs,(vm,m,s)) = IRUtil.size_of_var vs <> s in
  GeneratorDSL.generator_dsl (G2(lm,lv,Some(reject_read),(fun (vd,(vm,m,s)) -> Read(vd,m))))

(* NOT DONE *)
let writeconst_generator ldst =
  let m = List.map (fun v -> v,eval_to_int32 (mk_evar v)) in
  GeneratorDSL.generator_dsl (G1(m ldst,None,(fun (v,i) -> WriteConst(m,c,s))))

let mk_binopwrite (m,b,r) = BinopWrite(m,b,r)
let mk_binopread  (m,b,r) = BinopRead (r,b,m)

let reject_binopmem ((m,s),_,r) = IRUtil.size_of_var r <> s
let binopmem_comm_generator lm lrhs m = GeneratorDSL.generator_dsl (G3(lm,l_binop,lrhs,Some(reject_binopmem),m))
let binopmem_nc_generator lm lrhs m = GeneratorDSL.generator_dsl (G3(lm,l_shiftop,lrhs,Some(reject_binopmem),m))

(*
BinopWrite:  a binop where the destination is a common memory location, the 
LHS is that same location, and the RHS is a register.
So essentially, I need a G3(common memlocs,binop,registers).

BinopRead:  a binop where the destination is a register, the RHS is that same
register, and the RHS is a memory location.
So essentially, I need a G3(registers,binop,read memlocs).

I can sort of re-use the existing binop machinery.
*)

(*
// Memory load block
vMemR1 = load(vMem,memexpr,sizeof(vMemR1)) // so on

// SSA IR translation goes here, as usual

// SSA register block
vEaxAfter = SSA(vEax) // so on
vAxAfter = Cast(Low, TypeReg_16,vEaxAfter) // so on
vAlAfter = Cast(Low, TypeReg_8 ,VAxAfter)  // so on
vAhAfter = Cast(High,TypeReg_8 ,VAxAfter)  // so on
// Flags, segment registers, CRx/DRx, etc.
vMemAfter = SSA(vMem)

// Memory write block
vMemW1 = load(vMemAfter,memexpr,sizeof(vMemW1)) // so on

SSA sequence:
[SSA IR]@[SSA variable assignments]

Final IR sequence:
[SSA sequence]@[Memory loads and writes]@[address distinction]

After this come the assertions.

So we need to make the memory variable assignments.
How do we do that?

We need to have randomly evaluated already.
Now perform location prediction:
* Return a list of memloc types:  (reg32,disp)
*)

(*
What now?

I think that I get the memory locations with the contents in lists divided up by size.

* Generate the seed memory location from the first I/O pair

What do I want the output to look like?
On first blush, I will need the set of memlocs that matched for any variable
How do I correlate them with the individual memory locations?
I suppose I could make a second pass.
That's probably what I do already.

* Fold those locations Filter all subsequent locations against that

class GadgetGenerator
{
  public:
  gadget Yield();
  bool Increase();
  void Initialize(pair);
};

BinopGenerator:  makes 32/16/8-bit reg_after = reg_before BINOP reg_before
Should probably distinguish between commutative and non-commutative operations
No binop should operate upon the same two registers simultaneously.

ConstantGenerator:  
Call Initialize with one pair
Internally generates a list of expressions (r == [eval r])

Copy:
Given a list of register variables before and after
Fill in the template for vAfter == vBefore
Need to handle the case where vAfter refers to the same register as vBefore

ESP:
Call Initialize with one pair
Generate a hard-coded single expression (ESP_after - ESP_before == [eval (ESP_after - ESP_before)])

Preserve:
Nominally, this is trivial.

Read:

IOBinop overview:

Generate candidates:

Generate the results of every real binop, and store it in a hash table
Look at the result hash tables and see if they match any of those table entries
Intersect the results across every pair
Divide them by access pattern
Apply to all sizes

Verify:

Three functions to verify binops (reg/read/write)
Those three functions as set reductions
Those three functions as triplicates

GENERIC PROPAGATION STRATEGY:

It seems like there could be logic to infer inequalities after verification.
Then we could put these "pre-verified" facts into a hash table.
We could check the table before verifying anything.
Then, apply a Hashtbl.map to retrieve the inferred facts.
Boom!

How to make this go really fast:

* For each category, decide what its generator representation looks like, and all its wrinkles.

Binops:

vOut = vIn1 Binop vIn2

We could have that:

vOut is a reg_after, vIn1 and vIn2 are reg_before
vOut is a mem_after, vIn1 is the same location before, and vIn2 is a reg_before
vOut is a reg_after, vIn1 is a reg_before, vIn2 is a mem_before

Propagation rules:

REGISTER CASE:

* Things that filter down to smaller sizes: Add, Mul, And, Or, Xor, Shl, Shr
* Things that don't: Sar, Sub

MEMORY WRITE CASE:

* Don't want to use dword writes in place of word/byte writes

MEMORY READ CASE:

* It's fine to propagate dword reads into word and byte reads

+ Constants:

Given a list of variables [x] and one randomized evaluation pair, map into [Binop(x,Eq,eval x)]
This makes sense for memory writes, but not memory reads.

Propagation rules:

Registers can cascade.

+ Copy:

Given lists of proper sizes, generate (eax_after = ebx_before) (i.e. no registers equal).

Propagation rules:

Subregister copies cascade.

+ ESP:

Given one randomized evaluation pair, compute (ESP_after - ESP-before).
The singular expression is (ESP_after - ESP-before) = [(ESP_after - ESP-before)].

+ Preserve:

On the surface it's simple.  (x_after == x_before).
However, to save theorem prover time, we should conjoin our facts and try to verify that.
If it fails, then verify them individually.  How will that work exactly?

+ Read: Generate reg_after == mem_loc_before.
Propagation rules:  Subregister copies cascade.

+ Write:  Same, with no cascade.

Let's think about just the register case for now.

type subreg = | Low1632 | Low832 | High832 | Low816 | High816

let is_low = function 
| Low1632 | Low832  | Low816  -> true
| High832 | High816 -> false

let get_subregisters = function
| Gd(Eax) -> [Low1632,Gw(Ax);High832,Gb(Ah);Low832,Gb(Al)]
| Gd(Ecx) -> [Low1632,Gw(Cx);High832,Gb(Ch);Low832,Gb(Cl)]
| Gd(Edx) -> [Low1632,Gw(Dx);High832,Gb(Dh);Low832,Gb(Dl)]
| Gd(Ebx) -> [Low1632,Gw(Bx);High832,Gb(Bh);Low832,Gb(Bl)]
| Gd(Esp) -> [Low1632,Gw(Sp)]
| Gd(Ebp) -> [Low1632,Gw(Bp)]
| Gd(Esi) -> [Low1632,Gw(Si)]
| Gd(Edi) -> [Low1632,Gw(Di)]
| Gw(Ax)  -> [Low816,Gb(Ah);Low816,Gb(Al)]
| Gw(Cx)  -> [Low816,Gb(Ch);Low816,Gb(Cl)]
| Gw(Dx)  -> [Low816,Gb(Dh);Low816,Gb(Dl)]
| Gw(Bx)  -> [Low816,Gb(Bh);Low816,Gb(Bl)]
| Gw(Sp) | Gw(Bp) | Gw(Si) | Gw(Di)
| Gb(Ah) | Gb(Ch) | Gb(Dh) | Gb(Bh)
| Gb(Al) | Gb(Cl) | Gb(Dl) | Gb(Bl)  -> []

let get_subreg t r = 
  let gr32 r16 r8h r8l = match t with 
  | Low1632 -> Some(r16) | Low832 -> Some(r8l) | High832 -> Some(r8h)
  | Low816 | High816 -> None
  in
  let ar32 r16 = match t with 
  | Low1632 -> Some(r16)
  | Low832 | High832 | Low816 | High816 -> None
  in
  let gr16 r8h r8l = match t with 
  | Low816 -> Some(r8l) | High816 -> Some(r8h)
  | Low1632 | Low832 | High832 -> None
  in
  match r with
  | Gd(Eax) -> gr32 (Gw(Ax)) (Gb(Ah)) (Gb(Al)) 
  | Gd(Ecx) -> gr32 (Gw(Cx)) (Gb(Ch)) (Gb(Cl))
  | Gd(Edx) -> gr32 (Gw(Dx)) (Gb(Dh)) (Gb(Dl)) 
  | Gd(Ebx) -> gr32 (Gw(Bx)) (Gb(Bh)) (Gb(Bl))
  | Gd(Esp) -> ar32 (Gw(Sp)) 
  | Gd(Ebp) -> ar32 (Gw(Bp))
  | Gd(Esi) -> ar32 (Gw(Si)) 
  | Gd(Edi) -> ar32 (Gw(Di))
  | Gw(Ax)  -> gr16 (Gb(Ah)) (Gb(Al)) 
  | Gw(Cx)  -> gr16 (Gb(Ch)) (Gb(Cl))
  | Gw(Dx)  -> gr16 (Gb(Dh)) (Gb(Dl)) 
  | Gw(Bx)  -> gr16 (Gb(Bh)) (Gb(Bl))
  | Gw(Sp) | Gw(Bp) | Gw(Si) | Gw(Di) | Gb(Ah) | Gb(Ch) | Gb(Dh) | Gb(Bh) 
  | Gb(Al) | Gb(Cl) | Gb(Dl) | Gb(Bl)  -> None

let get_constant i32 = function
| Low1632           -> Int32.logand 0xffffl i32
| Low832  | Low816  -> Int32.logand 0xffl   i32
| High832 | High816 -> Int32.logand 0xffl  (Int32.shift_right_logical i32 8)

let get_pairs r1 r2 =
  ListUtil.map_filter 
   (fun (t,rl) -> match get_subreg t r2 with | Some(rr) -> Some(rl,rr,t) | _ -> None)
   (get_subregisters r1)

let get_triples r1 r2 r3 =
  ListUtil.map_filter
   (fun (r1,r2,t) -> match get_subreg t r3 with | Some(rr) -> Some(r1,r2,rr,t) | _ -> None)
   (get_pairs r1 r2)


*)