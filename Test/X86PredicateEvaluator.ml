open X86Predicate

let truncate_to i32 = function
| S32 -> (i32,S32)
| S16 -> (Int32.logand 0xffffl i32,S16)
| S8  -> (Int32.logand 0xffl i32,S8)
| S1  -> (Int32.logand 1l i32,S1)

let sign_aux mand mor i32 =
  if Int32.logand i32 mand <> 0l
  then Int32.logor mor i32
  else i32

let sign_extend i32 = function
| S32 -> i32
| S16 -> sign_aux 0x8000l 0xffff0000l i32
| S8  -> sign_aux 0x80l   0xffffff00l i32
| S1  -> sign_aux 0x1l    0xfffffffel i32

let compare bsign beq size lhs32 rhs32 =
  let c = if beq then (<=) else (<) in
  let res =
    match bsign with
    (* sl? *) 
    | true   -> c (sign_extend lhs32 size) (sign_extend rhs32 size)
    (* ul? *) 
    | false  -> 
      let lhs_neg = lhs32 < 0l in
      let rhs_neg = rhs32 < 0l in
      match lhs_neg,rhs_neg with

      (* Both positive, compare directly *)
      | false,false -> c lhs32 rhs32

      (* LHS negative, RHS positive => LHS is unsigned bigger *)
      | false,true -> true

      (* RHS negative, LHS positive => RHS is unsigned bigger *)
      | true,false -> false
      
      (* Both negative, so ... 
         Let's say we have LHS = -1, RHS = -2

         0xffffffff, 0xfffffffe
         We need this comparison to return false.

         c lhs rhs = false => 

         0xfffffffe, 0xffffffff
         We need this comparison to return true.
         c lhs rhs = true
      
      *)
      | true,true -> c lhs32 rhs32
      
  in
  if res then 1l else 0l

let cmp_sle = compare true  true
let cmp_slt = compare true  false
let cmp_ule = compare false true  S32
let cmp_ult = compare false false S32

let eval_expr instate outstate aux2 expr =
  let open JITRegion in
  let (sf,zf,af,pf,cf,ofl,df) = X86Misc.eflags2fl (Int32.to_int instate.eflags) in
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
  | X86Reg(X86.Gw(X86.Si)) -> truncate_to instate.esi S16
  | X86Reg(X86.Gw(X86.Di)) -> truncate_to instate.edi S16

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
  | Binop(l,((Add|Sub|Xor|And|Or|Eq|Ne|Slt|Sle|Ult|Ule) as b),r)    ->
    let (lv,ls),(rv,rs) = aux2 l,aux2 r in
    if ls <> rs then failwith "eval_expr: binop size mismatch";
   (match b with
    | Add -> truncate_to (Int32.add lv rv) ls
    | Sub -> truncate_to (Int32.sub lv rv) ls
    | Xor -> truncate_to (Int32.logxor lv rv) ls
    | And -> truncate_to (Int32.logand lv rv) ls
    | Or  -> truncate_to (Int32.logor  lv rv) ls
    | Eq  -> (if lv = rv then 1l else 0l),S1
    | Ne  -> (if lv = rv then 0l else 1l),S1
    | Slt -> (cmp_slt ls lv rv,S1)
    | Sle -> (cmp_sle ls lv rv,S1)
    | Ult -> (cmp_ult lv rv,S1)
    | Ule -> (cmp_ule lv rv,S1)
    | _   -> failwith "Not possible by OCaml type system soundness 1.")
  | Binop(l,b,r)    ->
    let (lv,ls),(rv,rs) = aux2 l,aux2 r in
    if S8 <> rs then failwith "eval_expr: shift size mismatch";
   (match b with
    | Shl -> truncate_to (Int32.shift_left          lv (Int32.to_int rv)) ls
    | Shr -> truncate_to (Int32.shift_right_logical lv (Int32.to_int rv)) ls
    | Sar -> truncate_to (Int32.shift_right         (sign_extend lv ls) (Int32.to_int rv)) ls
(*
    | Shl -> truncate_to (Int32.shift_left          lv ((Int32.to_int rv) land (int_of_esize ls - 1))) ls
    | Shr -> truncate_to (Int32.shift_right_logical lv ((Int32.to_int rv) land (int_of_esize ls - 1))) ls
    | Sar -> truncate_to (Int32.shift_right         lv ((Int32.to_int rv) land (int_of_esize ls - 1))) ls

    | Shl -> truncate_to (Int32.shift_left          lv (Int32.to_int rv)) ls
    | Shr -> truncate_to (Int32.shift_right_logical lv (Int32.to_int rv)) ls
    | Sar -> truncate_to (Int32.shift_right         lv (Int32.to_int rv)) ls
*)
    | _   -> failwith "Not possible by OCaml type system soundness 2.")
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
        then p
        else aux (p lxor i) (i lsr 1) (n + 1)
      in 
      let p = aux 0 i 0 in
      Int32.of_int ((p land 1) lxor 1)
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
    | SecondBit -> (if (Int32.logand (get_second_const es) ev <> 0l) then 1l else 0l),S1
    | FifthBit  -> (if (Int32.logand 0x10l ev <> 0l) then 1l else 0l),S1)
  | Extend(m,s,e) ->
    let ev,es = aux2 e in
   (match m with
    (* Cast to a larger size *)
    | Unsigned -> if int_of_esize es <= int_of_esize s then ev,s else failwith "eval_expr: bad unsigned"
    | Signed   -> if int_of_esize es <= int_of_esize s then sign_extend ev s,s else failwith "eval_expr: bad signed"
    (* Cast to a smaller size *)
    | Low      -> if int_of_esize s <= int_of_esize es then truncate_to ev s else failwith "eval_expr: bad low" )
  | ITE(b,t,f) ->
    let bv,bs = aux2 b in
    if bs <> S1 then failwith "eval_expr: boolean term was non-boolean";
    if bv = 1l then aux2 t else aux2 f
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

let eval_stmt eval_expr instate outstate stmt = 
  let open JITRegion in
(*let _ = Printf.printf "Evaluating statement: %s\n" (string_of_stmt stmt) in*)
  let (sf,zf,af,pf,cf,ofl,df) = X86Misc.eflags2fl (Int32.to_int outstate.eflags) in
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
    | X86.Gw(X86.Si) -> fst (truncate_to outstate.esi S16) = ev
    | X86.Gw(X86.Di) -> fst (truncate_to outstate.edi S16) = ev
    
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
