    (* New ideas for consolidating this huge function:
     
     * verify setting registers to constants
     * use the constant registers to filter out the register binops
     * use the constant registers to filter out the read binops
       ? It seems easy enough to verify constant registers, generating a set
         of them, and then use this set to filter out candidates during the
         generation phase.

     * remove "dependent" write binops
     * remove "dependent" read binops
       ? We could get rid of dependent memory addresses by eliminating them in
         the gadget generator filters.  This type of approach could cause some
         confusion; gadgets would not even be generated, therefore memory 
         location prediction would fail.  As long as that's understood, we're
         clear.  If not, we could remove them separately.

     * verify the frame size
     * look at all writes, make "written memlocs"
     * generate "write distinction predicate"
     * "verify write behaviors"
     * "verify write binops"

     * make a new write location set consisting of the verified write behaviors
     * verify write safety
     * generate a "read/write distinction" predicate
     * verify return location
     * verify read binops
     * verify reads
     * make sure the reads and writes are "correlated"
     * verify "read safety"

     * verify register binops
     * verify copies
     * see if there were any interesting behaviors at all
     * if so, return them
     *)

    (* HISTORICAL:
     * verify the frame size
     * generate candidates
     * look at all writes, make "written memlocs"
     * generate "write distinction predicate"
     * remove "dependent" write binops
     * "verify write behaviors"
     * "verify write binops"
     * make a new write location set consisting of the verified write behaviors
     * verify write safety

     * verify setting registers to constants
     * use the constant registers to filter out the read binops
     * remove "dependent" read binops
     * generate a "read/write distinction" predicate
     * verify return location
     * verify read binops
     * verify reads
     * make sure the reads and writes are "correlated"
     * verify "read safety"

     * use the constant registers to filter out the register binops
     * verify register binops
     * verify copies
     * see if there were any interesting behaviors at all
     * if so, return them
     *)

(*  
type abi =
{
  addresses: Int32Set.t;
  framesize: int32;
  retpos:    int32;
  preserve:  IRUtil.Var.t;
}
*)

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

let filter_memlocs ht addr = List.filter (fun (v,i) -> is_eval_false ht (mk_ne (mk_dword_of_int32 addr) (mk_memadd v i)))

let is_const s i64 = function
| IR.Const(i,sz) -> sz == s && i == i64
| e -> failwith ("is_const:  "^(PpIR.ppExpr false e)^" did not evaluate to a constant")

*)

(*  
    Printf.printf "Table begins:\n\n";
    Hashtbl.iter (fun v e -> Printf.printf "%s->%s\n" (PpIR.ppVar v) (PpIR.ppExpr false e)) ht;
    Printf.printf "\n\nTable ends\n\n";
    List.iter (fun s -> Printf.printf "%s\n" (PpIR.ppInstr s)) ir_ssa;
    List.iter (fun s -> Printf.printf "%s\n" (PpIR.ppInstr s)) ir_ssa_after;
    Hashtbl.iter (fun v e -> Printf.printf "%s->%s\n" (PpIR.ppVar v) (PpIR.ppExpr false e)) ht;
    Printf.printf "%!";
    Printf.printf "%s\n" (string_of_gadget g); 

2) Enforce non-empty lists in gadget generator creation (or change the generator -- ***SHUDDER*** ) DONE?
*)
  (*Printf.printf "Frame size = %ld, return location = %s\n" fs (string_of_memloc m);
    Generator.iter (fun v -> Printf.printf "%s\n" (string_of_gadget v)) (generator_of_gadget_archetype io TWriteConst);
    NoError(m,fs,ir,io)*)

