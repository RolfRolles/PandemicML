(* 
TO DO:

Implement Ult,Ule,Slt,Sle
Finish enumerator

When computing binops, don't compute the commutative ones twice.
So:  divide the binops into commutative versus non-commutative, and compute 
only the non-commutative binops twice.
*)

let save_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[Memexpr(Md(Mem32(DS,None,None,Some(a32))));GeneralReg(Gd(Esp))]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 0x28l a32))]); }::
  { pref = []; instr = (Pushfd,[]); }::
  { pref = []; instr = (Pushad,[]); }::
  []
  
let restore_macro a32 =
  let open X86 in
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Immediate(Id(Int32.add 0x4l a32))]); }::
  { pref = []; instr = (Popad,[]); }::
  { pref = []; instr = (Popfd,[]); }::
  { pref = []; instr = (Mov,[GeneralReg(Gd(Esp));Memexpr(Md(Mem32(DS,None,None,Some(a32))))]); }::
  []
  
let mk_long_rel_jmp src tgt =
  let disp32 = Int32.sub tgt (Int32.add 5l src) in
  let b0,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b1,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b2,disp32 = Int32.logand disp32 0xffl,Int32.shift_right_logical disp32 8 in
  let b3        = Int32.logand disp32 0xffl in
  [0xe9l;b0;b1;b2;b3]

let assemble = X86Encode.encode_instruction
let l_assemble x = List.concat (List.map assemble x)

let create_setup () =
  let o_init_t  = JITRegion.allocate 4096 in
  let o_blit_t  = JITRegion.allocate 4096 in
  let o_retn_t  = JITRegion.allocate 4096 in
  let o_stack_t = JITRegion.allocate 4096 in
  let o_mem_t   = JITRegion.allocate 4096 in

  let opt_get s = function | Some(i32,i) -> (i32,(i32,i)) | None -> failwith ("Allocating "^s) in
  let c_init32  ,init_t  = opt_get "init_t"  o_init_t  in
  let c_blit32  ,blit_t  = opt_get "blit_t"  o_blit_t  in
  let c_return32,retn_t  = opt_get "retn_t"  o_retn_t  in
  let c_stack32 ,stack_t = opt_get "stack_t" o_stack_t in
  let c_mem32   ,mem_t   = opt_get "mem_t"   o_mem_t   in  
    
  let l_init32 = 
    (l_assemble (save_macro c_mem32))@
    (l_assemble (restore_macro (Int32.add 0x28l c_mem32)))
  in
  let l_init32 = 
    l_init32@
    (mk_long_rel_jmp (Int32.add c_init32 (Int32.of_int (List.length l_init32))) c_blit32)
  in
  let l_retn32 = 
    (l_assemble (save_macro (Int32.add 0x50l c_mem32)))@
    (l_assemble (restore_macro c_mem32))@
    [0xc3l] 
  in
  let l_blit32 = 
    let rec aux list = function
    | 0 -> list@(mk_long_rel_jmp (Int32.add 0x100l c_blit32) c_return32)
    | n when n > 0 -> aux (0x90l::list) (n-1)
    | _ -> failwith "impossible jnklsdgkljfsa"
    in
    aux [] 256
  in
  if not (JITRegion.blit init_t (Array.of_list l_init32)) then failwith "couldn't blit preamble";
  if not (JITRegion.blit retn_t (Array.of_list l_retn32)) then failwith "couldn't blit postamble";
  if not (JITRegion.blit blit_t (Array.of_list l_blit32)) then failwith "couldn't blit blittable region";
  (init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,JITRegion.blit blit_t,JITRegion.execute_complex init_t mem_t)
    
let get_outstate f_blit f_execute instr instate = 
  let a_instr_nop = Array.make 0x100 0x90l in
  let l_instr = assemble instr in
  let _ = List.fold_left (fun i i32 -> a_instr_nop.(i) <- i32; i+1) 0 l_instr in
  f_blit a_instr_nop;
  f_execute instate
  
let fl2eflags sf zf af pf cf ofl df =
  let sf = sf lsl 7 in
  let zf = zf lsl 6 in
  let af = af lsl 4 in
  let pf = pf lsl 2 in
  let cf = cf in
  let ofl = ofl lsl 11 in
  let df = df lsl 10 in
  sf lor zf lor af lor pf lor cf lor ofl lor df
  
let eflags2fl flags = 
  let cf  = flags land 1 in
  let ofl = (flags lsr 11) land 1 in
  let df  = (flags lsr 10) land 1 in
  let sf  = (flags lsr  7) land 1 in
  let zf  = (flags lsr  6) land 1 in
  let af  = (flags lsr  4) land 1 in
  let pf  = (flags lsr  2) land 1 in
  (sf,zf,af,pf,cf,ofl,df)

let mk_random_state esp =
  let rb () = Random.int 2 in
  let ri32 () = Random.int32 Int32.max_int in
  
  (* Make a random assignment to the flags, LHS and RHS of the binary operation *)
  let instate =
    let cf,zf,sf,af,pf,ofl,df = rb(),rb(),rb(),rb(),rb(),rb(),rb() in
    let open JITRegion in
    {
      eax = ri32();
      ecx = ri32();
      edx = ri32();
      ebx = ri32();
      esp = esp;
      ebp = ri32();
      esi = ri32();
      edi = ri32();
      eflags = Int32.of_int (fl2eflags sf zf af pf cf ofl df);
    }
  in
  instate

let flags_str f32 = 
  let (sf,zf,af,pf,cf,ofl,df) = eflags2fl (Int32.to_int f32) in
  let str = "XXXXXXX" in
  str.[0] <- if cf  = 1 then 'C' else 'c';
  str.[1] <- if pf  = 1 then 'P' else 'p';
  str.[2] <- if af  = 1 then 'A' else 'a';
  str.[3] <- if sf  = 1 then 'S' else 's';
  str.[4] <- if zf  = 1 then 'Z' else 'z';
  str.[5] <- if ofl = 1 then 'O' else 'o';
  str.[6] <- if ofl = 1 then 'D' else 'd';
  str

let print_x86ctx x = 
  let open JITRegion in
  Printf.printf "eax = %08lx ebx = %08lx ecx = %08lx edx = %08lx\n"
   (x.eax) (x.ebx) (x.ecx) (x.edx);
  Printf.printf "esp = %08lx ebp = %08lx esi = %08lx edi = %08lx\n"
   (x.esp) (x.ebp) (x.esi) (x.edi);
  Printf.printf "%s\n%!" (flags_str x.eflags)

type binop =
| Add
(*
| Adc
| Sbb
*)
| Sub
| Xor
| And
| Or
| Eq
| Ne
| Slt
| Sle
| Ult
| Ule

let l_binop = [Add;Sub;(*Adc;Sbb;*)Xor;And;Or] (*;Eq;Ne;Slt;Sle;Ult;Ule]*)
let log_binops = [Xor;And;Or]

type unop =
| Inc
| Dec
| Not
| Neg

let l_unop = [Inc;Dec;Not;Neg]
let log_unops = [Neg]

type bitop =
| Parity
| SignBit
| SecondBit (* From the top, i.e. y in xyzzzzzz *)

let l_bitop = [Parity;SignBit;SecondBit]

(* I should probably put the flags directly into the structures 
   This might necessitate some second-level of structures.  I.e.,
   the easiest way to do it would be to use the existing structures
   within the already-coded functionality, and just have it marshall
   and demarshall the flags into/from another structure on the way in 
   and out. *)
type expr =
| X86Flag of X86.x86_flags
| X86Reg  of X86.x86_general_reg
| BitImm  of bool
| Imm     of X86.x86_immediate
| Binop   of expr * binop * expr
| Unop    of unop * expr
| Bitop   of bitop * expr

let string_of_binop = function
| Add -> "Add"
| Sub -> "Sub"
(*
| Adc -> "Adc"
| Sbb -> "Sbb"
*)
| Xor -> "Xor"
| And -> "And"
| Or  -> "Or"
| Eq  -> "Eq"
| Ne  -> "Ne"
| Slt -> "Slt"
| Sle -> "Sle"
| Ult -> "Ult"
| Ule -> "Ule"

let is_commutative = function
| Add
(*
| Adc
*)
| Xor
| And
| Or 
| Eq 
| Ne  -> true
| Sub
(*
| Sbb
*)
| Slt
| Sle
| Ult
| Ule -> false

let string_of_unop = function
| Inc -> "Inc"
| Dec -> "Dec"
| Not -> "Not"
| Neg -> "Neg"

let string_of_bitop = function
| Parity    -> "Parity"
| SignBit   -> "SignBit"
| SecondBit -> "SecondBit"

let rec string_of_expr = function
| X86Flag(f) -> "X86Flag("^X86Disasm.string_of_x86_flags f^")"
| X86Reg(X86.Gd(r)) -> "X86Reg(Gd("^X86Disasm.string_of_x86_reg32 r^"))"
| X86Reg(X86.Gw(r)) -> "X86Reg(Gw("^X86Disasm.string_of_x86_reg16 r^"))"
| X86Reg(X86.Gb(r)) -> "X86Reg(Gb("^X86Disasm.string_of_x86_reg8  r^"))"
| BitImm(b)         -> "BitImm("^string_of_bool b^")"
| Imm(X86.Id(i))    -> Printf.sprintf "Imm(Id(0x%lx))" i
| Imm(X86.Iw(i))    -> Printf.sprintf "Imm(Iw(0x%lx))" i
| Imm(X86.Ib(i))    -> Printf.sprintf "Imm(Ib(0x%lx))" i
| Binop(l,o,r)      -> Printf.sprintf "Binop(%s,%s,%s)" (string_of_expr l) (string_of_binop o) (string_of_expr r)
| Unop(o,e)         -> Printf.sprintf "Unop(%s,%s)" (string_of_unop o) (string_of_expr e)
| Bitop(o,e)        -> Printf.sprintf "Bitop(%s,%s)" (string_of_bitop o) (string_of_expr e)

type esize =
| S32
| S16
| S8
| S1

let truncate_to i32 = function
| S32 -> (i32,S32)
| S16 -> (Int32.logand 0xffffl i32,S16)
| S8  -> (Int32.logand 0xffl i32,S8)
| S1  -> (Int32.logand 1l i32,S1)

let eval_expr instate outstate aux2 expr =
  let open JITRegion in
  let (sf,zf,af,pf,cf,ofl,df) = eflags2fl (Int32.to_int instate.eflags) in
  let aux = function

  | X86Reg(X86.Gd(X86.Eax)) -> (instate.eax,S32)
  | X86Reg(X86.Gd(X86.Ecx)) -> (instate.ecx,S32)
  | X86Reg(X86.Gd(X86.Edx)) -> (instate.edx,S32)
  | X86Reg(X86.Gd(X86.Ebx)) -> (instate.ebx,S32)
  | X86Reg(X86.Gd(X86.Esp)) -> (instate.esp,S32)
  | X86Reg(X86.Gd(X86.Ebp)) -> (instate.ebp,S32)
  | X86Reg(X86.Gd(X86.Esi)) -> (instate.esi,S32)
  | X86Reg(X86.Gd(X86.Edi)) -> (instate.edi,S32)

  | X86Reg(X86.Gw(X86.Ax)) -> truncate_to instate.eax S16
  | X86Reg(X86.Gw(X86.Cx)) -> truncate_to instate.ecx S16
  | X86Reg(X86.Gw(X86.Dx)) -> truncate_to instate.edx S16
  | X86Reg(X86.Gw(X86.Bx)) -> truncate_to instate.ebx S16
  | X86Reg(X86.Gw(X86.Sp)) -> truncate_to instate.esp S16
  | X86Reg(X86.Gw(X86.Bp)) -> truncate_to instate.ebp S16
  | X86Reg(X86.Gw(X86.Di)) -> truncate_to instate.esi S16
  | X86Reg(X86.Gw(X86.Si)) -> truncate_to instate.edi S16

  | X86Reg(X86.Gb(X86.Al)) -> truncate_to instate.eax S8
  | X86Reg(X86.Gb(X86.Cl)) -> truncate_to instate.ecx S8
  | X86Reg(X86.Gb(X86.Dl)) -> truncate_to instate.edx S8
  | X86Reg(X86.Gb(X86.Bl)) -> truncate_to instate.ebx S8
  | X86Reg(X86.Gb(X86.Ah)) -> truncate_to (Int32.shift_right_logical instate.eax 8) S8
  | X86Reg(X86.Gb(X86.Ch)) -> truncate_to (Int32.shift_right_logical instate.ecx 8) S8
  | X86Reg(X86.Gb(X86.Dh)) -> truncate_to (Int32.shift_right_logical instate.edx 8) S8
  | X86Reg(X86.Gb(X86.Bh)) -> truncate_to (Int32.shift_right_logical instate.ebx 8) S8

  | Imm(X86.Id(i32))    -> (i32,S32)
  | Imm(X86.Iw(i16))    -> (i16,S16)
  | Imm(X86.Ib(i8 ))    -> (i8 ,S8)
  | BitImm(b)           -> (if b then 1l else 0l),S1
  | X86Flag(X86.X86F_C) -> (Int32.of_int cf,S1)
  | X86Flag(X86.X86F_P) -> (Int32.of_int pf,S1)
  | X86Flag(X86.X86F_A) -> (Int32.of_int af,S1)
  | X86Flag(X86.X86F_S) -> (Int32.of_int sf,S1)
  | X86Flag(X86.X86F_Z) -> (Int32.of_int zf,S1)
  | X86Flag(X86.X86F_O) -> (Int32.of_int ofl,S1)
  | X86Flag(X86.X86F_D) -> (Int32.of_int df,S1)
  | Binop(l,b,r)    ->
    let (lv,ls),(rv,rs) = aux2 l,aux2 r in
    if ls <> rs then failwith "eval_expr: binop size mismatch";
   (match b with
    | Add -> truncate_to (Int32.add lv rv) ls
    | Sub -> truncate_to (Int32.sub lv rv) ls
  (*| Adc -> truncate_to (Int32.add lv (Int32.add rv (Int32.of_int cf))) ls
    | Sbb -> truncate_to (Int32.sub lv (Int32.add rv (Int32.of_int cf))) ls*)
    | Xor -> truncate_to (Int32.logxor lv rv) ls
    | And -> truncate_to (Int32.logand lv rv) ls
    | Or  -> truncate_to (Int32.logor  lv rv) ls
    | Eq  -> (if lv = rv then 1l else 0l),S1
    | Ne  -> (if lv = rv then 0l else 1l),S1
    | Slt -> 0l,S1 
    | Sle -> 0l,S1
    | Ult -> 0l,S1
    | Ule -> 0l,S1)
  | Unop(o,e) ->
    let ev,es = aux2 e in
   (match o with
    | Inc -> truncate_to (Int32.add ev 1l) es
    | Dec -> truncate_to (Int32.sub ev 1l) es
    | Not -> truncate_to (Int32.lognot ev) es
    | Neg -> truncate_to (Int32.sub 0l ev) es)
  | Bitop(o,e) ->
    let ev,es = aux2 e in
    let parity i =
      let rec aux p i n =
        if n = 8
        then i
        else aux (p lxor i) (i lsr 1) (n + 1)
      in 
      let p = aux 0 i 0 in
      Int32.of_int p      
    in

    let get_sign_const = function
    | S32 -> 0x80000000l
    | S16 -> 0x8000l
    | S8  -> 0x80l
    | S1  -> 0x1l
    in
    let get_second_const = function
    | S32 -> 0x40000000l
    | S16 -> 0x4000l
    | S8  -> 0x40l
    | S1  -> 0x0l
    in
   (match o with
    | Parity    -> (parity (Int32.to_int (Int32.logand 0xffl ev)),S1)
    | SignBit   -> (if (Int32.logand (get_sign_const   es) ev <> 0l) then 1l else 0l),S1
    | SecondBit -> (if (Int32.logand (get_second_const es) ev <> 0l) then 1l else 0l),S1)
  in
  aux expr    

let weak_memoize_rec f =
  let m = BatInnerWeaktbl.create 1023 in
  let rec f2 x = 
    try BatInnerWeaktbl.find m x with Not_found ->
    let f_x = f f2 x in
    BatInnerWeaktbl.add m x f_x;
    f_x
  in
  f2
  
let strong_memoize_rec f =
  let m = Hashtbl.create 1023 in
  let rec f2 x = 
    try Hashtbl.find m x with Not_found ->
    let f_x = f f2 x in
    Hashtbl.add m x f_x;
    f_x
  in
  f2

let flush_amt = 10000

let strong_flush_memoize_rec f =
  let m = Hashtbl.create 1023 in
  let n = ref 0 in
  let rec f2 x = 
    try Hashtbl.find m x with Not_found ->
   (if !n = flush_amt
    then (n := 0; Hashtbl.clear m));
    let f_x = f f2 x in
    Hashtbl.add m x f_x;
    incr n;
    f_x
  in
  f2

let rec y_combinator f x =
  f (y_combinator f) x;;  


type stmt = 
| FlagEquals of X86.x86_flags * expr
| RegEquals of X86.x86_general_reg * expr
| ImmEquals of X86.x86_immediate * expr

let string_of_stmt = function
| FlagEquals(f,e)        -> "FlagEquals("^X86Disasm.string_of_x86_flags f^","^string_of_expr e^")"
| RegEquals(X86.Gd(r),e) -> "RegEquals(Gd("^X86Disasm.string_of_x86_reg32 r^","^string_of_expr e^"))"
| RegEquals(X86.Gw(r),e) -> "RegEquals(Gw("^X86Disasm.string_of_x86_reg16 r^","^string_of_expr e^"))"
| RegEquals(X86.Gb(r),e) -> "RegEquals(Gb("^X86Disasm.string_of_x86_reg8  r^","^string_of_expr e^"))"
| ImmEquals(X86.Id(i),e) -> Printf.sprintf "ImmEquals(Id(0x%lx),%s)" i (string_of_expr e)
| ImmEquals(X86.Iw(i),e) -> Printf.sprintf "ImmEquals(Iw(0x%lx),%s)" i (string_of_expr e)
| ImmEquals(X86.Ib(i),e) -> Printf.sprintf "ImmEquals(Ib(0x%lx),%s)" i (string_of_expr e)

let eval_stmt eval_expr instate outstate stmt = 
  let open JITRegion in
(*let _ = Printf.printf "Evaluating statement: %s\n" (string_of_stmt stmt) in*)
  let (sf,zf,af,pf,cf,ofl,df) = eflags2fl (Int32.to_int outstate.eflags) in
  match stmt with 
  | FlagEquals(f,e) ->
    let ev,es = eval_expr e in
    if es <> S1 then failwith "Expression did not evaluate to a 1-bit value";
   (match f with
    | X86.X86F_C -> Int32.of_int cf  = ev
    | X86.X86F_P -> Int32.of_int pf  = ev
    | X86.X86F_A -> Int32.of_int af  = ev
    | X86.X86F_S -> Int32.of_int sf  = ev
    | X86.X86F_Z -> Int32.of_int zf  = ev
    | X86.X86F_O -> Int32.of_int ofl = ev
    | X86.X86F_D -> Int32.of_int df  = ev)
  | RegEquals(r,e) -> 
    let ev,es = eval_expr e in
   (match r with
    
    | X86.Gd(_) when es <> S32 -> failwith "expression did not evaluate to a 32-bit value"
    | X86.Gd(X86.Eax) -> ev = outstate.eax
    | X86.Gd(X86.Ecx) -> ev = outstate.ecx
    | X86.Gd(X86.Edx) -> ev = outstate.edx
    | X86.Gd(X86.Ebx) -> ev = outstate.ebx
    | X86.Gd(X86.Esp) -> ev = outstate.esp
    | X86.Gd(X86.Ebp) -> ev = outstate.ebp
    | X86.Gd(X86.Esi) -> ev = outstate.esi
    | X86.Gd(X86.Edi) -> ev = outstate.edi
    
    | X86.Gw(_) when es <> S16 -> failwith "expression did not evaluate to a 16-bit value"
    | X86.Gw(X86.Ax) -> fst (truncate_to outstate.eax S16) = ev
    | X86.Gw(X86.Cx) -> fst (truncate_to outstate.ecx S16) = ev
    | X86.Gw(X86.Dx) -> fst (truncate_to outstate.edx S16) = ev
    | X86.Gw(X86.Bx) -> fst (truncate_to outstate.ebx S16) = ev
    | X86.Gw(X86.Sp) -> fst (truncate_to outstate.esp S16) = ev
    | X86.Gw(X86.Bp) -> fst (truncate_to outstate.ebp S16) = ev
    | X86.Gw(X86.Di) -> fst (truncate_to outstate.esi S16) = ev
    | X86.Gw(X86.Si) -> fst (truncate_to outstate.edi S16) = ev
    
    | X86.Gb(_) when es <> S8 -> failwith "expression did not evaluate to a 8-bit value"
    | X86.Gb(X86.Al) -> fst (truncate_to outstate.eax S8) = ev
    | X86.Gb(X86.Cl) -> fst (truncate_to outstate.ecx S8) = ev
    | X86.Gb(X86.Dl) -> fst (truncate_to outstate.edx S8) = ev
    | X86.Gb(X86.Bl) -> fst (truncate_to outstate.ebx S8) = ev
    | X86.Gb(X86.Ah) -> fst (truncate_to (Int32.shift_right_logical outstate.eax 8) S8) = ev
    | X86.Gb(X86.Ch) -> fst (truncate_to (Int32.shift_right_logical outstate.ecx 8) S8) = ev
    | X86.Gb(X86.Dh) -> fst (truncate_to (Int32.shift_right_logical outstate.edx 8) S8) = ev
    | X86.Gb(X86.Bh) -> fst (truncate_to (Int32.shift_right_logical outstate.ebx 8) S8) = ev)

  | ImmEquals(X86.Id(i32),e) ->
    let ev,es = eval_expr e in
    if es <> S32 then failwith "expression did not evaluate to a 32-bit value";
    i32 = ev
    
  | ImmEquals(X86.Iw(i16),e) ->
    let ev,es = eval_expr e in
    if es <> S16 then failwith "expression did not evaluate to a 16-bit value";
    i16 = ev

  | ImmEquals(X86.Ib(i8),e) ->
    let ev,es = eval_expr e in
    if es <> S8  then failwith "expression did not evaluate to a 8-bit value";
    i8 = ev
  
type ('a,'b,'c,'d) quadruple =
{
  val32: 'a;
  val16: 'b;
  val8: 'c;
  val1: 'd;
}

let expr_reg_atoms = 
  let open X86 in
  {
    val32 = 
    [
      X86Reg(Gd(Eax));X86Reg(Gd(Ecx));X86Reg(Gd(Edx));X86Reg(Gd(Ebx));
      X86Reg(Gd(Esp));X86Reg(Gd(Ebp));X86Reg(Gd(Esi));X86Reg(Gd(Edi));
    ];
    val16 =
    [
      X86Reg(Gw(Ax));X86Reg(Gw(Cx));X86Reg(Gw(Dx));X86Reg(Gw(Bx));
      X86Reg(Gw(Sp));X86Reg(Gw(Bp));X86Reg(Gw(Si));X86Reg(Gw(Di));
    ];
    val8 = 
    [
      X86Reg(Gb(Al));X86Reg(Gb(Cl));X86Reg(Gb(Dl));X86Reg(Gb(Bl));
      X86Reg(Gb(Ah));X86Reg(Gb(Ch));X86Reg(Gb(Dh));X86Reg(Gb(Bh));
    ];
    val1 = 
    [
      X86Flag(X86.X86F_C);X86Flag(X86.X86F_P);X86Flag(X86.X86F_A);X86Flag(X86.X86F_S);
      X86Flag(X86.X86F_Z);X86Flag(X86.X86F_O);X86Flag(X86.X86F_D);
    ];
  }

let expr_reg_atoms_eax_ebx = 
  let open X86 in
  { val32 = [X86Reg(Gd(Eax));X86Reg(Gd(Ebx));]; val16 = []; val8 =  []; val1 =  []; }

let expr_no_imm_atoms = 
  let open X86 in
  { val32 = []; val16 = []; val8 = []; val1 = []; }

let expr_imm_atoms = 
  let open X86 in
  {
    val32 = [Imm(Id(0x80000000l));Imm(Id(0x40000000l));Imm(Id(0x00000000l));Imm(Id(0xffffffffl));];
    val16 = [Imm(Iw(0x8000l));Imm(Iw(0x4000l));Imm(Iw(0x0000l));Imm(Iw(0xffffl));];
    val8  = [Imm(Ib(0x80l));Imm(Ib(0x40l));Imm(Ib(0x00l));Imm(Ib(0xffl));];
    val1  = [BitImm(true);BitImm(false)];
  }


let apply_all l_evalstmt stmt =
  if l_evalstmt = [] then invalid_arg "wither";
  let rec aux = function
  | f::fs -> if f stmt then aux fs else false
  | [] -> true
  in 
  aux l_evalstmt

let expand_single_and_apply_all l_evalstmt l_f_stmt acc expr =
  let stmts = List.rev_map (fun f -> f expr) l_f_stmt in
  List.fold_left (fun acc stmt -> if apply_all l_evalstmt stmt then stmt::acc else acc) acc stmts

let wither_single l_evalstmt l_f_stmt expr = expand_single_and_apply_all l_evalstmt l_f_stmt [] expr

let expand_and_apply_all l_evalstmt l_expr l_f_stmt =
  List.fold_left (expand_single_and_apply_all l_evalstmt l_f_stmt) [] l_expr

let wither stmt_reg_funs l_evalstmt q_sized_exprs =
{ 
  val32 = expand_and_apply_all l_evalstmt q_sized_exprs.val32 stmt_reg_funs.val32;
  val16 = expand_and_apply_all l_evalstmt q_sized_exprs.val16 stmt_reg_funs.val16;
  val8  = expand_and_apply_all l_evalstmt q_sized_exprs.val8  stmt_reg_funs.val8;
  val1  = expand_and_apply_all l_evalstmt q_sized_exprs.val1  stmt_reg_funs.val1;
}

exception Lapsed

class virtual ['a] generator (name:string) default_list = (* ' *)
  object(self)
    (* Probably needs to be initialized in the constructor *)

    val name = name
    method get_name () = name

    val mutable current_list = ( default_list : 'a list ) (*'*)
    val default_list = ( default_list : 'a list ) (*'*)

    val mutable cached_yield = ( None : expr option )

    method virtual increase_children : unit -> unit
    method virtual refresh_yield : unit -> unit
    
    (* Need to think about exception behavior *)
    method increase () = 
      cached_yield <- None; 
      match current_list with
      | [x] -> 
        current_list <- default_list; 
       (try
          self#increase_children ()
        with Lapsed ->
          raise Lapsed)
      | x::xs -> 
        current_list <- xs
      | [] -> failwith "impossible"
    
    method yield () =
      match cached_yield with
      | Some(y) -> y
      | None -> self#refresh_yield (); self#yield ()
  end;;

class atom_generator name default_list = (* ' *)
  object(self)
    constraint 'a = expr (* ' *)
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = raise Lapsed
    method refresh_yield () = 
      cached_yield <- Some(List.hd current_list)
  end;;

class virtual ['a] unop_generator name default_list child = (* ' *)
  object(self)
    val child = child
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = 
      try child#increase ()
      with Lapsed ->
        raise Lapsed
  end;;

class flag_unop_generator name default_list child = 
  object(self) 
    constraint 'a = bitop
    inherit ['a] unop_generator name default_list child  

    method refresh_yield () = 
      cached_yield <- Some(Bitop(List.hd current_list,child#yield ()))
  end;;

class  reg_unop_generator name default_list child = 
  object(self) 
    constraint 'a = unop  
    inherit ['a] unop_generator name default_list child 

    method refresh_yield () = 
      cached_yield <- Some(Unop(List.hd current_list,child#yield ()))
  end;;

class binop_generator name default_list lchild rchild = (* ' *)
  object(self)
    val lchild = lchild (*( lchild : ['b] generator ) (* ' *)*)
    val rchild = rchild (*( rchild : ['c] generator ) (* ' *)*)
    constraint 'a = binop
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = 
      try lchild#increase ()
      with Lapsed ->
        try rchild#increase ()
        with Lapsed ->
          raise Lapsed

    method refresh_yield () = 
      cached_yield <- Some(Binop(lchild#yield (),List.hd current_list,rchild#yield ()))
  end;;

class sequential_generator l_gens =
  object(self)
    val mutable list = (l_gens :> (esize * 'a generator) list) (* ' *)

    method increase_current () =
      match list with
      | [_,x]     -> (try (x#increase ();false) with Lapsed -> raise Lapsed)
      | (_,x)::((_,y)::_ as xs) -> 
        (try (x#increase ();false)
         with Lapsed -> 
           list <- xs; 
           Printf.printf "Increased generator to %s\n%!" (y#get_name ());
           true)
      | []        -> failwith "impossible"

    method yield_current () =
      match list with
      | (s,x)::xs -> (s,x#yield ())
      | []    -> failwith "impossible"
  end;;

let generator_of_expr expr regs imms =
  let rec aux expr = 
    let s = string_of_expr expr in 
    match expr with
    | X86Reg(X86.Gd(_)) -> ( new atom_generator s regs.val32 :> 'a generator ) (* ' *)
    | X86Reg(X86.Gw(_)) -> new atom_generator s regs.val16
    | X86Reg(X86.Gb(_)) -> new atom_generator s regs.val8
    | X86Flag(_)        -> new atom_generator s regs.val1

    | Imm(X86.Id(_)) -> new atom_generator s imms.val32
    | Imm(X86.Iw(_)) -> new atom_generator s imms.val16
    | Imm(X86.Ib(_)) -> new atom_generator s imms.val8
    | BitImm(_)      -> new atom_generator s imms.val1
    
    | Binop(l,Add,r) -> ( new binop_generator s l_binop (aux l) (aux r) :> 'a generator ) (* ' *)
    | Binop(l,Eq,r)  -> ( new binop_generator s log_binops (aux l) (aux r) :> 'a generator ) (* ' *)
    | Unop(Inc,e) -> ( new reg_unop_generator s l_unop (aux e) :> 'a generator ) (* ' *)
    | Unop(Not,e) -> ( new reg_unop_generator s log_unops (aux e) :> 'a generator ) (* ' *)
    | Bitop(_,e) -> ( new flag_unop_generator s l_bitop (aux e) :> 'a generator ) (* ' *)
    | _ -> failwith ("generator_of_expr: error in %s"^(string_of_expr expr))
    
  in aux expr

let generators list q_regs q_imms =
  List.map (fun (s,e) -> (s,generator_of_expr e q_regs q_imms)) list
  
let seq_generator list q_regs q_imms = 
  new sequential_generator (generators list q_regs q_imms)

let l_archetypes_32_1 = 
 [
  (S32,  X86Reg(X86.Gd(X86.Eax)));
  (S32, Unop(Inc,X86Reg(X86.Gd(X86.Eax))));
 ]

let l_archetypes_32_2 = 
 [
  (S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))));
  (S32,Binop(Unop(Inc,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax))));
  (S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Unop(Inc,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
 ]

let l_archetypes_32_3 =
 [
  (S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(Unop(Inc,X86Reg(X86.Gd(X86.Eax))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))))));
  (S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax))))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax))));
  (S32,Binop(Binop(Unop(Inc,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax)))),Add,X86Reg(X86.Gd(X86.Eax))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax)))));
 ]

let l_archetypes_32_4 = 
 [
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(Binop(Unop(Inc,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax)))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,Binop(Unop(Inc,X86Reg(X86.Gd(X86.Eax))),Add,X86Reg(X86.Gd(X86.Eax)))));
  (S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Unop(Inc,X86Reg(X86.Gd(X86.Eax))))));
  (S32,Unop(Inc,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))))));
 ]

let l_archetypes_16_1 = 
 [
  (S16,  X86Reg(X86.Gw(X86.Ax)));
  (S16, Unop(Inc,X86Reg(X86.Gw(X86.Ax))));
 ]

let l_archetypes_16_2 = 
 [
  (S16,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))));
  (S16,Binop(Unop(Inc,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax))));
  (S16,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax)))));
  (S16,Unop(Inc,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
 ]

let l_archetypes_16_3 =
 [
  (S32,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S32,Binop(Unop(Inc,X86Reg(X86.Gw(X86.Ax))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S32,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))))));
  (S32,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax))))));
  (S32,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax))));
  (S32,Binop(Binop(Unop(Inc,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax))));
  (S32,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax)))),Add,X86Reg(X86.Gw(X86.Ax))));
  (S32,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax)))));
 ]

let l_archetypes_16_4 =
 [
  (S16,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S16,Binop(Binop(Unop(Inc,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S16,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax)))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S16,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,Binop(Unop(Inc,X86Reg(X86.Gw(X86.Ax))),Add,X86Reg(X86.Gw(X86.Ax)))));
  (S16,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Unop(Inc,X86Reg(X86.Gw(X86.Ax))))));
  (S16,Unop(Inc,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax))))));
 ]

let l_archetypes_8_1 = 
 [
  (S8,  X86Reg(X86.Gb(X86.Al)));
  (S8, Unop(Inc,X86Reg(X86.Gb(X86.Al))));
 ]

let l_archetypes_8_2 = 
 [
  (S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))));
  (S8,Binop(Unop(Inc,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al))));
  (S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al)))));
  (S8,Unop(Inc,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
 ]

let l_archetypes_8_3 =
 [
  (S32,Binop(X86Reg(X86.Gb(X86.Al)),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
  (S32,Binop(Unop(Inc,X86Reg(X86.Gb(X86.Al))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
  (S32,Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))))));
  (S32,Binop(X86Reg(X86.Gb(X86.Al)),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al))))));
  (S32,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al))));
  (S32,Binop(Binop(Unop(Inc,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al))));
  (S32,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al)))),Add,X86Reg(X86.Gb(X86.Al))));
  (S32,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al)))));
 ]

let l_archetypes_8_4 =
 [
  (S8,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
  (S8,Binop(Binop(Unop(Inc,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
  (S8,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al)))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
  (S8,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,Binop(Unop(Inc,X86Reg(X86.Gb(X86.Al))),Add,X86Reg(X86.Gb(X86.Al)))));
  (S8,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,Unop(Inc,X86Reg(X86.Gb(X86.Al))))));
  (S8,Unop(Inc,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))))));
 ]

(* Duplicated from X86Encode, factor out *)
let int_of_reg8 = function
| X86.Al -> 0
| X86.Cl -> 1
| X86.Dl -> 2
| X86.Bl -> 3
| X86.Ah -> 4
| X86.Ch -> 5
| X86.Dh -> 6
| X86.Bh -> 7

let int_of_reg16 = function
| X86.Ax -> 0
| X86.Cx -> 1
| X86.Dx -> 2
| X86.Bx -> 3
| X86.Sp -> 4
| X86.Bp -> 5
| X86.Si -> 6
| X86.Di -> 7

let int_of_reg32 = function
| X86.Eax -> 0
| X86.Ecx -> 1
| X86.Edx -> 2
| X86.Ebx -> 3
| X86.Esp -> 4
| X86.Ebp -> 5
| X86.Esi -> 6
| X86.Edi -> 7

let int_of_x86flag = function
| X86.X86F_C -> 0
| X86.X86F_P -> 1
| X86.X86F_A -> 2
| X86.X86F_S -> 3
| X86.X86F_Z -> 4
| X86.X86F_O -> 5
| X86.X86F_D -> 6

module Reg32Comparator = struct
  type t = X86.x86_reg32
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg32 b1) (int_of_reg32 b2)
end
module Reg32Set = Set.Make(Reg32Comparator)

module Reg16Comparator = struct
  type t = X86.x86_reg16
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg16 b1) (int_of_reg16 b2)
end
module Reg16Set = Set.Make(Reg16Comparator)

module Reg8Comparator = struct
  type t = X86.x86_reg8
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg8 b1) (int_of_reg8 b2)
end
module Reg8Set = Set.Make(Reg8Comparator)

module FlagComparator = struct
  type t = X86.x86_flags
  let compare b1 b2 = 
    Pervasives.compare (int_of_x86flag b1) (int_of_x86flag b2)
end
module FlagSet = Set.Make(FlagComparator)

let l_clobbered32 =
  let m r32 = (r32,RegEquals(X86.Gd(r32),X86Reg(X86.Gd(r32)))) in
  [
    m X86.Eax; m X86.Ecx; m X86.Edx; m X86.Ebx;
    m X86.Esp; m X86.Ebp; m X86.Esi; m X86.Edi;
  ]

let l_clobbered16 =
  let m r32 = (r32,RegEquals(X86.Gw(r32),X86Reg(X86.Gw(r32)))) in
  [
    m X86.Ax; m X86.Cx; m X86.Dx; m X86.Bx;
    m X86.Sp; m X86.Bp; m X86.Si; m X86.Di;
  ]

let l_clobbered8 =
  let m r32 = (r32,RegEquals(X86.Gb(r32),X86Reg(X86.Gb(r32)))) in
  [
    m X86.Al; m X86.Cl; m X86.Dl; m X86.Bl;
    m X86.Ah; m X86.Ch; m X86.Dh; m X86.Bh;
  ]

let l_clobbered1 =
  let m f = (f,FlagEquals(f,X86Flag(f))) in
  [
    m X86.X86F_C; m X86.X86F_P; m X86.X86F_A;
    m X86.X86F_S; m X86.X86F_Z; m X86.X86F_O;
    m X86.X86F_D;
  ]

let get_clobbered f_stmt list = 
  List.fold_left (fun acc (r,el) -> if not (f_stmt el) then r::acc else acc) [] list

let get_clobbered f_stmt q_clob =
  let f list = get_clobbered f_stmt list in
  { 
    val32 = List.fold_left (fun set el -> Reg32Set.add el set) q_clob.val32 (f l_clobbered32);
    val16 = List.fold_left (fun set el -> Reg16Set.add el set) q_clob.val16 (f l_clobbered16);
    val8  = List.fold_left (fun set el -> Reg8Set.add  el set) q_clob.val8  (f l_clobbered8);
    val1  = List.fold_left (fun set el -> FlagSet.add  el set) q_clob.val1  (f l_clobbered1);
  }

let init_t,blit_t,retn_t,stack_t,c_stack32,mem_t,f_blit,f_execute = create_setup ()

let make_fixed_test_statement_evaluator instr q_clobbered instate =
  let outstate = get_outstate f_blit f_execute instr instate in
  let f_stmt = eval_stmt (y_combinator (eval_expr instate outstate)) instate outstate in
  let q_clobbered = get_clobbered f_stmt q_clobbered in
  (f_stmt,q_clobbered)

let make_random_test_statement_evaluator instr q_clobbered =
  let instate = mk_random_state c_stack32 in
  make_fixed_test_statement_evaluator instr q_clobbered instate

let mk_random_test_statement_evaluators instr n =
  let rec aux i list q_clob =
    if i = n
    then list,q_clob
    else 
      let f_stmt,q_clob = make_random_test_statement_evaluator instr q_clob in
      aux (i+1) (f_stmt::list) q_clob
  in 
  let blank = 
  { 
    val32 = Reg32Set.empty; 
    val16 = Reg16Set.empty; 
    val8  = Reg8Set.empty; 
    val1  = FlagSet.empty 
  }
  in
  let list,q_clob = aux 0 [] blank in
  let q_l_f_stmt =
  {
    val32 = Reg32Set.fold (fun r32 l -> (fun x ->  RegEquals(X86.Gd(r32),x))::l) q_clob.val32 [];
    val16 = Reg16Set.fold (fun r32 l -> (fun x ->  RegEquals(X86.Gw(r32),x))::l) q_clob.val16 [];
    val8  = Reg8Set.fold  (fun r32 l -> (fun x ->  RegEquals(X86.Gb(r32),x))::l) q_clob.val8  [];
    val1  = FlagSet.fold  (fun r32 l -> (fun x -> FlagEquals(r32,x))::l)     q_clob.val1  [];
  }
  in
  (list,q_l_f_stmt)

let select_quad q = function
| S32 -> q.val32
| S16 -> q.val16
| S8  -> q.val8
| S1  -> q.val1

let update_quad q el = function
| S32 -> { q with val32 = el::q.val32; }
| S16 -> { q with val16 = el::q.val16; }
| S8  -> { q with val8  = el::q.val8 ; }
| S1  -> { q with val1  = el::q.val1 ; }

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

let _ = 
  let instr = 
    let open X86 in 
    { pref = []; instr = (Add,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]); }
  in
  let _ = Printf.printf "%s:\n" (X86Disasm.string_of_x86instr instr) in
  let l_evalstmt,q_l_f_stmt = mk_random_test_statement_evaluators instr 50 in
  let l_f_stmt = q_l_f_stmt.val32 in
    
  let seq_g = seq_generator l_archetypes_32_2 expr_reg_atoms_eax_ebx expr_imm_atoms in
  let rec aux quad =
    let s,e = seq_g#yield_current () in
    let quad =
      if keep e
      then { quad with val32 = expand_single_and_apply_all l_evalstmt l_f_stmt quad.val32 e }
      else quad
    in
    let continue =
      try (
       (if seq_g#increase_current ()
        then List.iter (fun s -> Printf.printf "%s\n%!" (string_of_stmt s)) quad.val32);
        true)
      with Lapsed -> false
    in
    if continue
    then aux quad
    else quad
  in
  let q = aux { val32 = []; val16 = []; val8 = []; val1 = [] } in
  List.iter (fun s -> Printf.printf "%s\n%!" (string_of_stmt s)) q.val32
