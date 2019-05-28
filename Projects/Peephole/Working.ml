(*
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\BreakCV.ml";;
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\CVMain.ml";;

#use "c:\\paframework\\Projects\\Peephole\\Working.ml";;

*)

(* DETAILED PLAN:

   * Generate
   * * Take a test executable and virtualize it many times
   * * * Using the command-line version of CV, turn all options up
   * * Find VM_jumpinto (start address)
   * * Follow VM_jumpinto and analyze the VM that is found there
   * * Take every handler, unobfuscated, strip jumps
   * * Reassemble these to [md5]-[length].bin.
   * * Repeat many times.

   * Harvest
   * * Disassemble all sequences from disk.
     * Select 2-length sequences
     * .... i-length sequences ....
   * * Canonicalize all sequences in the list
   * * * If canoncicalization fails, skip -- ???
   * * Store canonical sequences in a hash table
*)

let num_allowable_non_esp_registers = 2
let sequence_occurrence_threshhold = 0
let _,cfg = CVVMAnalyze.get_handler_cfg_exn 0x0040646Cl 0xffffffffl

type abs_x86_reg8 = 
| AbsReg8 of int

let string_of_abs_x86_reg8 = function
| AbsReg8(i) -> "AbsReg8("^string_of_int i^")"

type abs_x86_reg16 =
| AbsReg16 of int
| AbsSp

let string_of_abs_x86_reg16 = function
| AbsReg16(i) -> "AbsReg16("^string_of_int i^")"
| AbsSp       -> "AbsSp"

type abs_x86_reg32 =
| AbsReg32 of int
| AbsEsp

let string_of_abs_x86_reg32 = function
| AbsReg32(i) -> "AbsReg32("^string_of_int i^")"
| AbsEsp      -> "AbsEsp"

type abs_x86_general_reg =
| AGb of abs_x86_reg8
| AGw of abs_x86_reg16
| AGd of abs_x86_reg32

let string_of_abs_x86_general_reg = function
| AGb(r) -> string_of_abs_x86_reg8  r
| AGw(r) -> string_of_abs_x86_reg16 r
| AGd(r) -> string_of_abs_x86_reg32 r

type abs_imm =
(*
| AbsImmNeg4
| AbsImmNeg2
*)
| AbsImmNeg1
| AbsImm0
| AbsImm1
| AbsImm2
| AbsImm4
(*
| AbsImm6
| AbsImm8
*)
| AbsImmArbitrary of int
| AbsImmNegArbitrary of int

let string_of_abs_imm = function
(*
| AbsImmNeg4
| AbsImmNeg2
*)
| AbsImmNeg1 -> "AbsImmNeg1"
| AbsImm0    -> "AbsImm0"
| AbsImm1    -> "AbsImm1"
| AbsImm2    -> "AbsImm2"
| AbsImm4    -> "AbsImm4"
(*
| AbsImm6
| AbsImm8
*)
| AbsImmArbitrary(i)    -> "AbsImmArbitrary("^string_of_int i^")"
| AbsImmNegArbitrary(i) -> "AbsImmNegArbitrary("^string_of_int i^")"

type abs_x86_immediate =
| AIb of abs_imm
| AIw of abs_imm
| AId of abs_imm

let string_of_abs_x86_immediate = function
| AIb(a) -> "AIb("^string_of_abs_imm a^")"
| AIw(a) -> "AIw("^string_of_abs_imm a^")"
| AId(a) -> "AId("^string_of_abs_imm a^")"

type abs_x86_addr_expr =
| AbsMem16 of X86.x86_segreg * abs_x86_reg16 option * abs_x86_reg16 option * abs_x86_immediate option
| AbsMem32 of X86.x86_segreg * abs_x86_reg32 option * (abs_x86_reg32 * int) option * (abs_x86_immediate option)

let string_of_abs_x86_addr_expr = function
| AbsMem16(seg,bo,so,dop) -> 
  let bs = match bo  with Some(b) -> [string_of_abs_x86_reg16 b]    | None -> [] in
  let ss = match so  with Some(s) -> bs@[string_of_abs_x86_reg16 s] | None -> bs in
  let ds = match dop with Some(d) -> ss@[string_of_abs_x86_immediate d]     | None -> ss in
  let s  = StringUtil.intersperse_string "+" ds in
  X86Disasm.string_of_x86_segreg seg^":["^Util.opt_get s^"]"
| AbsMem32(seg,bo,so,dop) -> 
  let bs = match bo with Some(b) -> [string_of_abs_x86_reg32 b]    | None -> [] in
  let ss = match so with 
  | Some(sr,sf) -> 
    let l = [string_of_abs_x86_reg32 sr] in
    let l = if sf <> 0 then l@[X86Disasm.string_of_scalefac sf] else l in  
    bs@[Util.opt_get (StringUtil.intersperse_string "*" l)] 
  | None -> bs in
  let ds = match dop with Some(d) -> ss@[string_of_abs_x86_immediate d]     | None -> ss in
  let s  = StringUtil.intersperse_string "+" ds in
  X86Disasm.string_of_x86_segreg seg^":["^Util.opt_get s^"]"

type abs_x86_mem_expr =
| AMb  of abs_x86_addr_expr
| AMw  of abs_x86_addr_expr
| AMd  of abs_x86_addr_expr
| AMf  of abs_x86_addr_expr
| AMq  of abs_x86_addr_expr
| AMt  of abs_x86_addr_expr
| AMdq of abs_x86_addr_expr

let string_of_abs_x86_memexpr = function
| AMb(m)  ->    "byte ptr "^string_of_abs_x86_addr_expr m
| AMw(m)  ->    "word ptr "^string_of_abs_x86_addr_expr m
| AMd(m)  ->   "dword ptr "^string_of_abs_x86_addr_expr m
| AMf(m)  ->   "fword ptr "^string_of_abs_x86_addr_expr m
| AMq(m)  ->   "qword ptr "^string_of_abs_x86_addr_expr m
| AMt(m)  ->   "tbyte ptr "^string_of_abs_x86_addr_expr m
| AMdq(m) -> "xmmword ptr "^string_of_abs_x86_addr_expr m

type abs_x86operand =
| AbsGeneralReg of abs_x86_general_reg
| AbsControlReg of X86.x86_control_reg
| AbsDebugReg of X86.x86_debug_reg
| AbsSegReg of X86.x86_segreg
| AbsFPUReg of X86.x86_fpureg
| AbsMMXReg of X86.x86_mmxreg
| AbsXMMReg of X86.x86_xmmreg
| AbsImmediate of abs_x86_immediate
| AbsMemexpr of abs_x86_mem_expr
| AbsJccTarget of int32 * int32
| AbsFarTarget of X86.x86_far_target

let string_of_abs_x86operand = function
| AbsGeneralReg(r)   -> string_of_abs_x86_general_reg r
| AbsControlReg(r)   -> X86Disasm.string_of_x86_control_reg r
| AbsDebugReg(r)     -> X86Disasm.string_of_x86_debug_reg r
| AbsSegReg(r)       -> X86Disasm.string_of_x86_segreg r
| AbsFPUReg(r)       -> X86Disasm.string_of_x86_fpureg r
| AbsMMXReg(r)       -> X86Disasm.string_of_x86_mmxreg r
| AbsXMMReg(r)       -> X86Disasm.string_of_x86_xmmreg r
| AbsJccTarget(jt,_) -> Printf.sprintf "%08lX" jt
| AbsImmediate(i)    -> string_of_abs_x86_immediate i
| AbsMemexpr(m)      -> string_of_abs_x86_memexpr m
| AbsFarTarget(a)    -> X86Disasm.string_of_x86_far_target a

type abs_x86instr = X86.x86mnem * (abs_x86operand list)

type abs_x86instrpref = { abspref: X86.x86_group1_prefix list; absinstr: abs_x86instr; }

let string_of_abs_x86instr { absinstr = (m,l) } =
  let l = List.map string_of_abs_x86operand l in
  match l with
  | [] -> X86Disasm.string_of_x86mnem m
  | _  -> X86Disasm.string_of_x86mnem m^" "^(Util.opt_get (StringUtil.intersperse_string ", " l))

let print_abs_x86instrlist = List.iter (fun i -> IDA.msg "%s\n" (string_of_abs_x86instr i))


(*
List.iter 
 (function 
  | [x;y] -> 
      IDA.msg 
        "Sequence: %s %s\n" 
       (X86Disasm.string_of_x86instr x) 
       (X86Disasm.string_of_x86instr y) 
  | _ -> invalid_arg "sequences") 
  sequences;;
*)


(* First version of canonicalizer.  Will need to be improved.
   A sequence is only accepted right now if:
   * It contains at most two 32-bit registers and ESP
*)
let list_mapfilter f =
  let rec aux out = function
  | x::xs -> (match f x with
    | Some(y) -> aux (y::out) xs
    | None    -> aux     out  xs)
  | [] -> List.rev out
  in aux []

let reg_to_baseno x = let open X86 in match x with
| GeneralReg(Gd(x)) ->
 (match x with 
  | Eax -> 0
  | Ecx -> 1
  | Edx -> 2
  | Ebx -> 3
  | Esp -> 4
  | Ebp -> 5
  | Esi -> 6
  | Edi -> 7)
| GeneralReg(Gw(x)) ->
 (match x with 
  | Ax -> 0
  | Cx -> 1
  | Dx -> 2
  | Bx -> 3
  | Sp -> 4
  | Bp -> 5
  | Si -> 6
  | Di -> 7)
| GeneralReg(Gb(x)) ->
 (match x with 
  | Al | Ah -> 0
  | Cl | Ch -> 1
  | Dl | Dh -> 2
  | Bl | Bh -> 3)
| _ -> invalid_arg "reg_to_baseno"

let uniquify list =
  let h = Hashtbl.create 20 in
  let _ = List.iter (fun r -> Hashtbl.replace h r ()) list in
  Hashtbl.fold (fun r _ l -> r::l) h []
  
let uniquify list =
  let rec aux out = function
  | x::xs -> if List.exists (fun y -> y = x) xs then aux out xs else aux (x::out) xs
  | [] -> out
  in aux [] (List.rev list)

(* So what is it that I am actually doing here? I need to 

   A) canonicalize instructions as they appear in the disassembly listing.
   B) reject sequences that don't fit the canonicalization rules.

   For an example of A), a sequence of instructions such as:
   
     push ebx
     mov ebx, 01234567h
     add eax, 89ABCDEFh
     add eax, ebx
     sub eax, 89ABCDEFh
     pop ebx
   
   Needs to become:
   
     push AbsReg32(0)
     mov AbsReg32(0), AbsImmediate(AId(AbsImmArbitrary(0)))
     add AbsReg32(1), AbsImmediate(AId(AbsImmArbitrary(1)))
     add AbsReg32(1), AbsReg32(0)
     sub AbsReg32(1), AbsImmediate(AId(AbsImmArbitrary(1)))
     pop AbsReg32(0)
   
   I do the canonicalization in N phases.  
   
   Phase one:  Harvest.  I iterate over a list of instructions, iterating over 
   each operand for each instruction, and extract each register and each 
   immediate that is used throughout.  This is pretty straightforward.  Since 
   this is a deterministic process, a sequence of instructions that is the same
   up to register and constant renaming will produce the same register and 
   immediate lists (up to renaming) in the same order.
   
   Phase two:  Assign each register and constant to a distinct abstract 
   operand.  There are some subtleties here that have yet to be addressed.  
   Consider:
   
     (1)                             (2)
     push ax                         push eax
     mov ah, 12h   => add cl, 12h    not byte ptr [esp+1] => not ah
     add cl, ah                      pop eax
     pop ax
   
   There are two problems here that are currently not addressed.  For (1), we
   need to keep track of the fact that ah is a sub-register of ax and assign
   them the same symbolic number.  For (2), the pattern ONLY WORKS for the high
   8-bit subregister.  We need some way of distinguishing this.  More thinking
   is required.
   
   Phase three:  Replace each pertinent concrete operand with its abstract 
   version, as above.
*)
let canonicalize_opt sequence =
  let open X86 in
  let rec harvest (regs,imms) = function
  | [] -> (List.rev regs,List.rev imms)
  | { pref = _; instr = (_,l) }::xs -> 
    let regs,imms = 
      List.fold_left
       (fun (regs,imms) o -> match o with
        | GeneralReg(_) -> (o::regs,imms)
        | Memexpr(Mb (Mem32(_,br,sr,io)))
        | Memexpr(Mw (Mem32(_,br,sr,io)))
        | Memexpr(Md (Mem32(_,br,sr,io)))
        | Memexpr(Mf (Mem32(_,br,sr,io)))
        | Memexpr(Mq (Mem32(_,br,sr,io)))
        | Memexpr(Mt (Mem32(_,br,sr,io)))
        | Memexpr(Mdq(Mem32(_,br,sr,io))) ->
         (let imms = match io with
          | Some(imm) -> (Immediate(Id(imm)))::imms
          | None -> imms
          in
          match br,sr with
          | None,None           -> (regs,imms)
          | Some(br),None       -> ((GeneralReg(Gd(br)))::regs,imms)
          | None,Some(sr,_)     -> ((GeneralReg(Gd(sr)))::regs,imms)
          | Some(br),Some(sr,_) -> ((GeneralReg(Gd(br)))::(GeneralReg(Gd(sr)))::regs,imms))
        | Memexpr(Mb (Mem16(_,br,sr,io)))
        | Memexpr(Mw (Mem16(_,br,sr,io)))
        | Memexpr(Md (Mem16(_,br,sr,io)))
        | Memexpr(Mf (Mem16(_,br,sr,io)))
        | Memexpr(Mq (Mem16(_,br,sr,io)))
        | Memexpr(Mt (Mem16(_,br,sr,io)))
        | Memexpr(Mdq(Mem16(_,br,sr,io))) ->
         (let imms = match io with
          | Some(imm) -> (Immediate(Iw(imm)))::imms
          | None -> imms
          in
          match br,sr with
          | None,None         -> (regs,imms)
          | Some(br),None     -> ((GeneralReg(Gw(br)))::regs,imms)
          | None,Some(sr)     -> ((GeneralReg(Gw(sr)))::regs,imms)
          | Some(br),Some(sr) -> ((GeneralReg(Gw(br)))::(GeneralReg(Gw(sr)))::regs,imms))
        | Immediate(_)        -> (regs,o::imms)
        | _ -> (regs,imms))
       (regs,imms)
        l  
    in harvest (regs,imms) xs
  in
  let (regs,imms) = harvest ([],[]) sequence in
  let (regs,imms) = (uniquify regs,uniquify imms) in
  
  let c2a_regs = Hashtbl.create 20 in
  let c2a_imms = Hashtbl.create 20 in
  let num_regs_used = 
    List.fold_left (fun i r -> 
      let a_opt = try Some(Hashtbl.find c2a_regs r) with Not_found -> None in
      match a_opt with
      | Some(a) -> i
      | None    ->
        let a,i = match r with
        | GeneralReg(Gd(Esp)) -> AbsGeneralReg(AGd(AbsEsp)),i
        | GeneralReg(Gw(Sp))  -> AbsGeneralReg(AGw(AbsSp)),i
        | GeneralReg(Gd(x))   -> AbsGeneralReg(AGd(AbsReg32(i))),i+1
        | GeneralReg(Gw(x))   -> AbsGeneralReg(AGw(AbsReg16(i))),i+1
        | GeneralReg(Gb(x))   -> AbsGeneralReg(AGb(AbsReg8 (i))),i+1
        | _                   -> invalid_arg "canonicalizing registers"
        in
        Hashtbl.replace c2a_regs r a;
        i)
      0
      regs
  in
  let num_imms_used =
    List.fold_left (fun i r -> 
      let f r = try Some(Hashtbl.find c2a_imms r) with Not_found -> None in
      let negate_constant = function
      | Immediate(Id(x)) -> Immediate(Id(Int32.neg x))
      | Immediate(Iw(x)) -> Immediate(Iw(Int32.logand (Int32.neg x) 0xffffl))
      | Immediate(Ib(x)) -> Immediate(Ib(Int32.logand (Int32.neg x) 0xffl))
      | _ -> invalid_arg "negate_constant"
      in
      match f r, f (negate_constant r) with
      | Some(_),_ -> i
      | _,Some(_) -> i
      | None,None ->
        let a,inv_opt = match r with
        | Immediate(Id(x)) -> 
         (match x with
          | 0xffffffffl -> AId(AbsImmNeg1),None
          | 0l          -> AId(AbsImm0),None
          | 1l          -> AId(AbsImm1),None
          | 2l          -> AId(AbsImm2),None
          | 4l          -> AId(AbsImm4),None
          | y           -> AId(AbsImmArbitrary(i)),Some(AId(AbsImmNegArbitrary(i))))
        | Immediate(Iw(x)) ->
         (match x with
          | 0xffffl -> AIw(AbsImmNeg1),None
          | 0l      -> AIw(AbsImm0),None
          | 1l      -> AIw(AbsImm1),None
          | 2l      -> AIw(AbsImm2),None
          | 4l      -> AIw(AbsImm4),None
          | y       -> AIw(AbsImmArbitrary(i)),Some(AIw(AbsImmNegArbitrary(i))))
        | Immediate(Ib(x)) ->
         (match x with
          | 0xffl -> AIb(AbsImmNeg1),None
          | 0l    -> AIb(AbsImm0),None
          | 1l    -> AIb(AbsImm1),None
          | 2l    -> AIb(AbsImm2),None
          | 4l    -> AIb(AbsImm4),None
          | y     -> AIb(AbsImmArbitrary(i)),Some(AIb(AbsImmNegArbitrary(i))))
        | _                -> invalid_arg "canonicalizing immediates"
        in
        let _ = match inv_opt with
        | None -> Hashtbl.replace c2a_imms r a
        | Some(n) -> Hashtbl.replace c2a_imms r a; Hashtbl.replace c2a_imms (negate_constant r) n
        in
        i+1)
      0
      imms
  in
  let strip_tycon32      = function | AbsGeneralReg(AGd(r)) -> r | _ -> invalid_arg "strip_tycon32" in
  let strip_tycon16      = function | AbsGeneralReg(AGw(r)) -> r | _ -> invalid_arg "strip_tycon16" in
  let replace32_opt      = function | None -> None | Some(r)   -> Some(strip_tycon32 (Hashtbl.find c2a_regs (GeneralReg(Gd(r)))))   in
  let replace32_pair_opt = function | None -> None | Some(r,i) -> Some(strip_tycon32 (Hashtbl.find c2a_regs (GeneralReg(Gd(r)))),i) in
  let replace16_opt      = function | None -> None | Some(r)   -> Some(strip_tycon16 (Hashtbl.find c2a_regs (GeneralReg(Gw(r)))))   in
  let replace32_imm_opt  = function | None -> None | Some(imm) -> Some(Hashtbl.find c2a_imms (Immediate(Id(imm)))) in
  let replace16_imm_opt  = function | None -> None | Some(imm) -> Some(Hashtbl.find c2a_imms (Immediate(Iw(imm)))) in

  let canonicalize_regs_and_immediates i = match i with
  | ControlReg(c)                        -> AbsControlReg(c)
  | DebugReg(d)                          -> AbsDebugReg(d)
  | SegReg(s)                            -> AbsSegReg(s)
  | FPUReg(f)                            -> AbsFPUReg(f)
  | MMXReg(m)                            -> AbsMMXReg(m)
  | XMMReg(x)                            -> AbsXMMReg(x)
  | JccTarget(t,f)                       -> AbsJccTarget(t,f)
  | FarTarget(f)                         -> AbsFarTarget(f)
  | GeneralReg(_)                        -> Hashtbl.find c2a_regs i
  | Memexpr(Mb (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMb (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mw (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMw (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Md (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMd (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mf (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMf (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mq (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMq (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mt (Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMt (AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mdq(Mem32(seg,bro,sro,imm))) -> AbsMemexpr(AMdq(AbsMem32(seg,replace32_opt bro,replace32_pair_opt sro,replace32_imm_opt imm)))
  | Memexpr(Mb (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMb (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Mw (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMw (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Md (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMd (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Mf (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMf (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Mq (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMq (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Mt (Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMt (AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Memexpr(Mdq(Mem16(seg,bro,sro,imm))) -> AbsMemexpr(AMdq(AbsMem16(seg,replace16_opt bro,replace16_opt      sro,replace16_imm_opt imm)))
  | Immediate(_)                         -> AbsImmediate(Hashtbl.find c2a_imms i)
  in
  let canon_sequence = 
    List.map 
     (fun ({ pref = p; instr = (m,l) } as i) -> { abspref = p; absinstr = (m,List.map canonicalize_regs_and_immediates l) })
      sequence 
  in
  canon_sequence
(*let regs_used = Array.make 8 0 in*)


(*
 (match reg_list_opt with
  | None -> ()
  | Some(r) ->
    let h = Hashtbl.create 17 in
    let _ = List.fold_left (fun i x -> Hashtbl.replace h x i; i+1) 0 r in
    let replace = function 
    | GeneralReg(Gd(Esp)) -> AbsGeneralReg(AGd(AbsEsp)) 
    | GeneralReg(Gw(Sp))  -> AbsGeneralReg(AGw(AbsSp)) 
    | r -> Hashtbl.find h r 
    in
    let replace_opt = function | None -> None | Some(r) -> Some(replace r) in
    let replace_pair_opt = function | None -> None | Some(r,i) -> Some(replace r,i) in
    List.map 
    ());
  ()
*)
  
(* The general idea is to take sequences of code and automatically find equivalent
   sequences of code. *)
   
let source_instruction_sequence = 
  let open X86 in
  [{ pref = []; instr = (Dec,[GeneralReg(Gd(Eax))]) };
   { pref = []; instr = (Not,[GeneralReg(Gd(Eax))]) }]

let allowable_instructions = 
  [(X86.Neg, [AbsGeneralReg(AGd(AbsReg32(0)))]);
   (X86.Push,[AbsGeneralReg(AGd(AbsReg32(0)))]);
   (X86.Pop, [AbsGeneralReg(AGd(AbsReg32(0)))]);]

let cpdebug = ref false

let canonicalize sequences bprint = 
  let canonical_hashtbl = Hashtbl.create 1024 in
  let incr_ch e = let ex = try Hashtbl.find canonical_hashtbl e with Not_found -> 0 in Hashtbl.replace canonical_hashtbl e (ex+1) in
  let _ = List.iter (fun s -> 
    let c = try Some(canonicalize_opt s) with Not_found -> IDA.msg "Skipped sequence\n"; None in 
    if !cpdebug && c <> None
    then 
     (IDA.msg "Original\n";
      let _ = List.iter (fun i -> IDA.msg "%s\n" (X86Disasm.string_of_x86instr i)) s in
      IDA.msg "Canonical\n";
      let _ = print_abs_x86instrlist (Util.opt_get c) in
      ());
    match c with | Some(c) -> incr_ch c | None -> ())
    sequences
  in
  (*
  let _ = IDA.msg "%d\n" (Hashtbl.length canonical_hashtbl)
  let _ = Hashtbl.iter (fun s i -> IDA.msg "Canonical %d\n" i; List.iter print_abs_x86instrlist s) canonical_hashtbl
  *)
  
  let canonical_array = Array.make (Hashtbl.length canonical_hashtbl) ([],0) in
  let _ = Hashtbl.fold (fun s i index -> canonical_array.(index) <- (s,i); (index+1)) canonical_hashtbl 0 in
  let _ = Array.sort (fun (_,a) (_,b) -> Pervasives.compare b a) canonical_array in
  if bprint
  then begin
    Array.iter 
     (fun (s,i) -> 
        if i > sequence_occurrence_threshhold
        then IDA.msg "Canonical %d\n" i; print_abs_x86instrlist s)
      canonical_array 
  end;
  Array.to_list canonical_array

let get_subsequences_of_length_two list = 
  let rec aux list = function
  | x::y::z -> aux ([x;y]::list) (y::z)
  | _ -> list
  in aux [] list;;

let get_subsequences_of_length_three list = 
  let rec aux list = function
  | w::x::y::z -> aux ([w;x;y]::list) (x::y::z)
  | _ -> list
  in aux [] list;;

let get_subsequences_of_length_four list = 
  let rec aux list = function
  | v::w::x::y::z -> aux ([v;w;x;y]::list) (w::x::y::z)
  | _ -> list
  in aux [] list;;

let get_subsequences_of_length_five list = 
  let rec aux list = function
  | u::v::w::x::y::z -> aux ([u;v;w;x;y]::list) (v::w::x::y::z)
  | _ -> list
  in aux [] list;;

let get_subsequences_of_length_six list = 
  let rec aux list = function
  | t::u::v::w::x::y::z -> aux ([t;u;v;w;x;y]::list) (u::v::w::x::y::z)
  | _ -> list
  in aux [] list;;

let get_subsequences_of_length_seven list = 
  let rec aux list = function
  | s::t::u::v::w::x::y::z -> aux ([s;t;u;v;w;x;y]::list) (t::u::v::w::x::y::z)
  | _ -> list
  in aux [] list;;

let hashtbl_filter h f = 
  let h2 = Hashtbl.create (Hashtbl.length h * 2) in
  Hashtbl.iter (fun a b -> if (f a b) then Hashtbl.add h2 a b) h2;
  h2

let get_sequences f i =
  let s = C.G.fold_vertex (fun v l -> (f (C.get_ir cfg v))@l) cfg [] in
  match i with
  | Some(i) -> IDA.msg "Sequences of length %d\n" i; canonicalize s true
  | None    -> canonicalize s false
  
let sequences2 = get_sequences get_subsequences_of_length_two   (None);;
let sequences3 = get_sequences get_subsequences_of_length_three (None);;
let sequences4 = get_sequences get_subsequences_of_length_four  (None);;
let sequences5 = get_sequences get_subsequences_of_length_five  (None);;
let sequences6 = get_sequences get_subsequences_of_length_six   (None);;
let sequences7 = get_sequences get_subsequences_of_length_seven (None);;

let filter_sequence s =
  let open X86 in
  let filtered = ref true in
  let has_memexpr { abspref = _; absinstr = (m,l) } = 
    match m with 
    | Push | Pop -> filtered := false; ()
    | Lea -> ()
    | _ -> List.iter (function | AbsMemexpr(_) -> filtered := false | _ -> ()) l 
  in
  List.iter has_memexpr s;
  !filtered;;
  
type regalloc =
| RAArbitrary
| RABinding of (int * X86.x86_reg32) list
| RAFailed

let assign_concrete_registers sequence = 
  let has_shift_by_reg8 list { abspref = _; absinstr = (m,l) } =
    let open X86 in
    match m,l with
    | (Shl|Shr|Sar|Rol|Ror|Rcl|Rcr),[_;AbsGeneralReg(AGb(AbsReg8(i)))] -> i::list
    | _ -> list
  in
  let res = List.fold_left has_shift_by_reg8 [] sequence in
  match uniquify res with
  | []  -> (* IDA.msg "Free assignment\n"; *)RAArbitrary 
  | [x] -> IDA.msg "%d must be ecx/cl\n" x;  RABinding([x,X86.Ecx])
  | _   -> IDA.msg "Sequence has multiple registers that must be bound against ecx/cl!\n"; RAFailed;;

let sequences2_filtered = List.filter (fun (s,i) -> filter_sequence s) sequences2 in
let _ = List.iter (fun (s,i) -> IDA.msg "*%d*\n" i; print_abs_x86instrlist s) sequences2_filtered in
let _ = List.iter (fun (s,i) -> ignore(assign_concrete_registers s)) sequences2_filtered in
();;

let sequences3_filtered = List.filter (fun (s,i) -> filter_sequence s) sequences3 in
let _ = List.iter (fun (s,i) -> IDA.msg "*%d*\n" i; print_abs_x86instrlist s) sequences3_filtered in
();;

