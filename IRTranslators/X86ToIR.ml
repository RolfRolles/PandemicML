(* Todo: IRTranslator_[L].cpp:  Defines functions that translate opcodes starting with [L] into the IR. *)
(* 
IRTranslator_M:  mov (r32,id/r16,iw/r8,ib/ed,gd/eb,gb/gb,eb/gd,ed/sw,ew/ed,sw/ew,sw)
IRTranslator_P:  push (id/iw, arbitrary dword expression RHS)
                 pop (arbitrary dword/word expression LHS)
*)

open IR
open IRUtil
open X86
open X86Disasm
open X86ToIRUtil

(* These are duplicated from elsewhere; factor them out somewhere *)
let sign_extend_byte_word  b = if Int32.compare b 0x80l < 0 then b else Int32.logor 0xff00l b
let sign_extend_byte_dword b = if Int32.compare b 0x80l < 0 then b else Int32.logor 0xffffff00l b
let curry2 f (a,b) = f a b  

(* Can't put this in IRUtil due to a cyclic dependency with IRTypeCheck *)
(* If we put an ITE operator into the language itself, then replace this with
   a wrapper around the IR expression creator; for now, use this *)
let mk_ite eb ethen eelse =
  let open IRTypeCheck in
  (* Sizes must be equal, but we assume that we're given well-typed 
     expressions; if this is not the case, typechecking will detect
     this *)
  let sz e = type_of_integer_type (typecheck_expr e) in
  let ethen = mk_and (mk_signed_cast (sz ethen) eb) ethen in
  let eelse = mk_and (mk_signed_cast (sz eelse) (mk_not eb)) eelse in
  mk_or ethen eelse

(* State variables.  Throughout the course of translation, various functions 
   will call `gen instr` to store an IR instruction onto the top of the current
   translation list.  The list is created in reverse order, since appending an
   element to a linked list in OCaml is practically free.  After an instruction
   has been translated entirely, the client calls `get_translation`, which 
   returns the reverse of the list, and resets the translation buffer to the 
   empty list. *)
let translation_list = ref []
let gen instr = translation_list := instr::(!translation_list)
let clear () = translation_list := []
let get_translation () = let x = List.rev !translation_list in clear (); x

(* Creates a new variable of the given size, with the given expression *)
let assign_get_var s e =
  let var = new_var s in
  gen (mk_assign var e);
  var

(* Helpers for arithmetic operations (flags, decompositions of arithmetic into
   IR expressions) *)
let x_adc size x y = mk_add x (mk_add y (mk_unsigned_cast size eCF))
let x_sbb size x y = mk_sub x (mk_add y (mk_unsigned_cast size eCF))

let x_rol size orig ad = 
  let and_expr = (mk_and ad (mk_byte (Int64.of_int (IRUtil.bits size - 1)))) in
  let e1 = mk_shl orig and_expr in
  let e2 = mk_shr orig (mk_sub (mk_byte (Int64.of_int (IRUtil.bits size))) ad) in
  mk_or e1 e2
 
let x_rcl size orig ad =
  let and_expr = (mk_and ad (mk_byte (Int64.of_int (IRUtil.bits size - 1)))) in
  let and_expr = match size with
  | TypeReg_8  -> mk_umod and_expr (mk_byte 9L)
  | TypeReg_16 -> mk_umod and_expr (mk_byte 17L)
  | _ -> and_expr 
  in
  let one = (mk_byte 1L) in
  let eCF = (mk_unsigned_cast (TypeReg_8) eCF) in
  let plain = 
    let e1 = mk_or (mk_shl orig and_expr) (mk_shl eCF (mk_sub and_expr one)) in
    let e2 = mk_shr orig (mk_sub (mk_byte (Int64.of_int (IRUtil.bits size + 1))) ad) in
    mk_or e1 e2
  in
  mk_ite (mk_eq and_expr one)
   (mk_or (mk_shl orig one) eCF)
   (match size with
    | TypeReg_1 -> invalid_arg "mk_rcl"
    | TypeReg_8
    | TypeReg_32 
    | TypeReg_64 -> plain
    | TypeReg_16 -> 
      mk_ite (mk_eq and_expr (mk_byte 16L))
       (mk_or (mk_shl eCF (mk_byte 15L)) (mk_shr orig one))
       plain)

let x_ror size orig ad = 
  let and_expr = (mk_and ad (mk_byte (Int64.of_int (IRUtil.bits size - 1)))) in
  let e1 = mk_shr orig and_expr in
  let e2 = mk_shl orig (mk_sub (mk_byte (Int64.of_int (IRUtil.bits size))) ad) in
  mk_or e1 e2

let add_carry    orig ad nr _ = mk_ult  (mk_evar nr) (mk_evar orig)
let adc_carry    orig _  nr _ = mk_ule  (mk_evar nr) (mk_evar orig)

let add_aux      orig ad nr s = 
  let x = mk_xor (mk_evar orig) (mk_xor (mk_evar nr) (mk_evar ad)) in
  mk_ne (mk_and (mk_fixed_const 0x10L s) x) (mk_zero s)

let mk_get_ith_topmost_bit s i e = mk_shr e (mk_byte (Int64.of_int ((bits s)-i)))

let add_overflow orig ad nr s = 
  let orig = mk_evar orig in
  let e1 = mk_xor orig (mk_xor (mk_evar ad) (mk_max_const s)) in
  let e2 = mk_xor orig (mk_evar nr) in
  let e3 = mk_and e1 e2 in
  let e4 = mk_get_ith_topmost_bit s 1 e3 in
  cast_to_bit e4

let sbb_carry    orig ad nr s = mk_or (mk_ult (mk_evar orig) (mk_evar ad)) (mk_eq (mk_evar nr) (mk_max_const s))
let sub_carry    orig ad nr s = mk_ult (mk_evar orig) (mk_evar ad)
let sub_overflow orig ad nr s = 
  let orig = mk_evar orig in
  let e1 = mk_xor orig (mk_evar ad) in
  let e2 = mk_xor orig (mk_evar nr) in
  let e3 = mk_and (mk_and e1 e2) (mk_sign_const s) in
  mk_ne e3 (mk_zero s)

let rol_overflow _ _ nr s =
  let nr = mk_evar nr in cast_to_bit (mk_get_ith_topmost_bit s 1 nr)
  
let rol_carry _ _ nr _ = cast_to_bit (mk_evar nr)

let ror_overflow _ _ nr s =
  let nr = mk_evar nr in 
  let e1 = mk_get_ith_topmost_bit s 1 nr in
  let e2 = mk_get_ith_topmost_bit s 2 nr in
  cast_to_bit (mk_xor e1 e2)

let ror_carry  _ _ nr s = cast_to_bit (mk_get_ith_topmost_bit s 1 (mk_evar nr))

let shl_overflow orig ad nr s = 
  let orig,ad,nr = mk_evar orig,mk_evar ad,mk_evar nr in
  let e1 = mk_sub (mk_byte (Int64.of_int (bits s))) ad in
  let e2 = mk_shr orig e1 in
  let e3 = mk_get_ith_topmost_bit s 1 nr in
  cast_to_bit (mk_xor e2 e3)

let shl_carry orig ad _ s = 
  let e1 = mk_sub (mk_byte (Int64.of_int (bits s))) (mk_evar ad) in
  let e2 = mk_shr (mk_evar orig) e1 in
  cast_to_bit e2

let shr_overflow orig ad nr s = 
  let nr = mk_evar nr in
  let e1 = mk_shl nr (mk_byte 0x1L) in
  let e2 = mk_xor nr e1 in
  let e3 = mk_get_ith_topmost_bit s 1 e2 in
  cast_to_bit e3

let shr_carry orig ad _ s = 
  let e1 = mk_sub (mk_evar ad) (mk_byte 0x1L) in
  let e2 = mk_shr (mk_evar orig) e1 in
  cast_to_bit e2
  
let sar_overflow _ _ _ _ = mk_false

let neg_carry    orig _ nr s = mk_ne (mk_zero s) (mk_evar nr)
let neg_aux      orig _ nr s = mk_ne (mk_and (mk_const (0xFL) s) (mk_evar nr)) (mk_zero s)
let neg_overflow    _ _ nr s = mk_eq (mk_sign_const s) (mk_evar nr) 

let mk_zf_expr        s e  = mk_eq e (mk_zero s)
let mk_zf             s nr = mk_zf_expr s (mk_evar nr)
let mk_zf_assign_expr s e  = gen (mk_assign vZF (mk_zf_expr s e))
let mk_zf_assign      s nr = mk_zf_assign_expr s (mk_evar nr)

let mk_sf_expr        s e  = mk_ne (mk_and e (mk_sign_const s)) (mk_zero s)
let mk_sf             s nr = mk_sf_expr s (mk_evar nr)
let mk_sf_assign_expr s e  = gen (mk_assign vSF (mk_sf_expr s e))
let mk_sf_assign      s nr = mk_sf_assign_expr s (mk_evar nr)

let mk_pf_expr        _ e  =
  let shr e i = mk_shr e (mk_byte (Int64.of_int i)) in
  mk_low_cast 
    (TypeReg_1) 
    (mk_not (mk_xor (shr e 7) (mk_xor (shr e 6) (mk_xor (shr e 5) (mk_xor (shr e 4) (mk_xor (shr e 3) (mk_xor (shr e 2) (mk_xor (shr e 1) e))))))))

let mk_pf             _ nr = mk_pf_expr () (mk_evar nr)
let mk_pf_assign_expr _ e  = gen (mk_assign vPF (mk_pf_expr () e))
let mk_pf_assign      _ nr = mk_pf_assign_expr () (mk_evar nr)

(* Helpers for dealing with memory expressions *)
let sum l = 
  if l = [] then failwith "expr_of_memexpr: empty memory expression";
  List.fold_left 
    (fun expr part -> mk_add expr part)
    (List.hd l)
    (List.tl l)

(* Right now we ignore segmentation, which is clearly wrong, must fix *)
let expr_of_memexpr = function
| Mem16(_,bo,so,io) -> 
  let r_of_o l = function
  | Some(b) -> l@[(expr_of_reg16 b)]
  | None -> l
  in
  let b  = r_of_o [] bo in
  let bs = r_of_o b  so in
  let bsi = match io with
  | Some(i) -> bs@[(mk_word (Int64.of_int32 i))]
  | _ -> bs
  in sum bsi
| Mem32(_,bo,so,io) -> 
  let b = match bo with 
  | Some(b) -> [expr_of_reg32 b]
  | None -> []
  in
  let bs = match so with
  | Some(sr,sf) -> 
    let sr = expr_of_reg32 sr in
    let sr = if sf = 0 then sr else mk_shl sr (mk_byte (Int64.of_int sf)) in
    b@[sr]
  | None -> b
  in
  let bsi = match io with
  | Some(i) -> bs@[(mk_dword (Int64.of_int32 i))]
  | _ -> bs
  in sum bsi
  
let e_producer memexpr = mk_load eMem (expr_of_memexpr memexpr)

let mb_producer memexpr = e_producer memexpr (TypeReg_8 )
let md_producer memexpr = e_producer memexpr (TypeReg_32)
let mw_producer memexpr = e_producer memexpr (TypeReg_16)

let mem_writer size ewhere ewhat = mk_assign vMem (mk_store eMem (expr_of_memexpr ewhere) ewhat size)
let mb_writer = mem_writer (TypeReg_8 )
let md_writer = mem_writer (TypeReg_32)
let mw_writer = mem_writer (TypeReg_16)

let gb_writer whichreg ewhat = (instr_setter_of_r8 whichreg) whichreg ewhat
let gd_writer whichreg ewhat = mk_assign (var_of_reg32 whichreg) ewhat
let gw_writer = instr_set_r16

let ib_producer i32 = mk_byte  (Int64.of_int32 i32)
let id_producer i32 = mk_dword (Int64.of_int32 i32)
let iw_producer i32 = mk_word  (Int64.of_int32 i32)

let writer_of_x86opnd = function
| GeneralReg(Gb(glhs)) -> (instr_setter_of_r8 glhs) glhs
| GeneralReg(Gw(glhs)) -> instr_set_r16 glhs
| GeneralReg(Gd(glhs)) -> mk_assign (var_of_reg32 glhs)
| ControlReg(cr)       -> mk_assign (var_of_controlreg cr)
| DebugReg(dr)         -> mk_assign (var_of_debugreg dr)
| SegReg(sr)           -> mk_assign (var_of_segreg sr)
| FPUReg(_)            -> invalid_arg "writer_of_x86opnd: floating point currently unsupported"
| MMXReg(_)            -> invalid_arg "writer_of_x86opnd: MMX currently unsupported"           
| XMMReg(_)            -> invalid_arg "writer_of_x86opnd: XMM currently unsupported"           
| Immediate(_)         -> invalid_arg "writer_of_x86opnd: cannot write to an Immediate, caller is incorrect"
| Memexpr(Mb(elhs))    -> mb_writer elhs
| Memexpr(Mw(elhs))    -> mw_writer elhs
| Memexpr(Md(elhs))    -> md_writer elhs
| Memexpr(_)           -> invalid_arg "writer_of_x86opnd: > 32-bit memory locations currently unsupported"
| JccTarget(_)         -> invalid_arg "writer_of_x86opnd: cannot write to a JccTarget, caller is incorrect"
| FarTarget(_)         -> invalid_arg "writer_of_x86opnd: cannot write to a FarTarget, caller is incorrect"

let expr_of_x86opnd = function
| GeneralReg(Gb(glhs)) -> expr_of_reg8  glhs
| GeneralReg(Gw(glhs)) -> expr_of_reg16 glhs
| GeneralReg(Gd(glhs)) -> expr_of_reg32 glhs
| Immediate(Ib(irhs))  -> ib_producer irhs
| Immediate(Iw(irhs))  -> iw_producer irhs
| Immediate(Id(irhs))  -> id_producer irhs
| ControlReg(cr)       -> expr_of_controlreg cr
| DebugReg(dr)         -> expr_of_debugreg dr
| SegReg(sr)           -> expr_of_segreg sr
| FPUReg(_)            -> invalid_arg "expr_of_x86opnd: floating point currently unsupported"
| MMXReg(_)            -> invalid_arg "expr_of_x86opnd: MMX currently unsupported"
| XMMReg(_)            -> invalid_arg "expr_of_x86opnd: XMM currently unsupported"
| Memexpr(Mb(elhs))    -> mb_producer elhs
| Memexpr(Mw(elhs))    -> mw_producer elhs
| Memexpr(Md(elhs))    -> md_producer elhs
| Memexpr(_)           -> invalid_arg "expr_of_x86opnd: memory expressions other than 8/16/32 currently unsupported"
| JccTarget(_)         -> invalid_arg "expr_of_x86opnd: can't produce expr for JccTarget; caller is incorrect"
| FarTarget(_)         -> invalid_arg "expr_of_x86opnd: can't produce expr for FarTarget; caller is incorrect"

let size_of_x86opnd = function
| GeneralReg(Gb(_)) -> TypeReg_8
| GeneralReg(Gw(_)) -> TypeReg_16
| GeneralReg(Gd(_)) -> TypeReg_32
| ControlReg(_)     -> TypeReg_32
| DebugReg(_)       -> TypeReg_32
| SegReg(_)         -> TypeReg_16
| FPUReg(_)         -> invalid_arg "size_of_x86opnd: floating point currently unsupported"
| MMXReg(_)         -> invalid_arg "size_of_x86opnd: MMX currently unsupported"
| XMMReg(_)         -> invalid_arg "size_of_x86opnd: XMM currently unsupported"
| Immediate(Ib(_))  -> TypeReg_8
| Immediate(Iw(_))  -> TypeReg_16
| Immediate(Id(_))  -> TypeReg_32
| Memexpr(Mb(_))    -> TypeReg_8
| Memexpr(Mw(_))    -> TypeReg_16
| Memexpr(Md(_))    -> TypeReg_32
| Memexpr(_)        -> invalid_arg "size_of_x86opnd: memory expressions other than 8/16/32 currently unsupported"
| JccTarget(_)      -> invalid_arg "size_of_x86opnd: tried to take size of a JccTarget; caller is incorrect"
| FarTarget(_)      -> invalid_arg "size_of_x86opnd: tried to take size of a FarTarget; caller is incorrect"

let lhs_info_of_x86opnd opnd = (size_of_x86opnd opnd, expr_of_x86opnd opnd, writer_of_x86opnd opnd)

let translate_binop_rhs lhs_size op2 = match lhs_size,op2 with
| _,          GeneralReg(Gb(grhs)) -> expr_of_reg8  grhs
| _,          GeneralReg(Gd(grhs)) -> expr_of_reg32 grhs
| _,          GeneralReg(Gw(grhs)) -> expr_of_reg16 grhs
| _,          Memexpr(Mb(erhs))    -> mb_producer erhs
| _,          Memexpr(Md(erhs))    -> md_producer erhs
| _,          Memexpr(Mw(erhs))    -> mw_producer erhs
| TypeReg_8,  Immediate(Ib(irhs))  -> ib_producer irhs
| TypeReg_16, Immediate(Ib(irhs))  -> iw_producer (sign_extend_byte_word  irhs)
| TypeReg_32, Immediate(Ib(irhs))  -> id_producer (sign_extend_byte_dword irhs)
| _,          Immediate(Id(irhs))  -> id_producer irhs
| _,          Immediate(Iw(irhs))  -> iw_producer irhs
| _,_                              -> failwith "translate_binop_rhs: typechecking prevents this"

let sign_extended_expr_of_x86opnd s o =
  match o with
  | Immediate(Ib(irhs)) ->
   (match s with
    | TypeReg_1  -> invalid_arg "sign_extended_expr_of_x86opnd: 1"
    | TypeReg_8  -> ib_producer irhs
    | TypeReg_16 -> iw_producer (sign_extend_byte_word  irhs)
    | TypeReg_32 -> id_producer (sign_extend_byte_dword irhs)
    | TypeReg_64 -> invalid_arg "sign_extended_expr_of_x86opnd: 64")
  | _ -> expr_of_x86opnd o
    
(* Helpers for instructions that use the flags *)
let expr_of_jcc = function
| Js  | Sets  | Cmovs  -> eS 
| Jns | Setns | Cmovns -> eNS
| Jo  | Seto  | Cmovo  -> eO 
| Jno | Setno | Cmovno -> eNO
| Jz  | Setz  | Cmovz  -> eZ 
| Jnz | Setnz | Cmovnz -> eNZ
| Jp  | Setp  | Cmovp  -> eP 
| Jnp | Setnp | Cmovnp -> eNP
| Ja  | Seta  | Cmova  -> eA 
| Jae | Setae | Cmovae -> eAE
| Jb  | Setb  | Cmovb  -> eB 
| Jbe | Setbe | Cmovbe -> eBE
| Jl  | Setl  | Cmovl  -> eL 
| Jle | Setle | Cmovle -> eLE
| Jg  | Setg  | Cmovg  -> eG 
| Jge | Setge | Cmovge -> eGE
| _   -> invalid_arg "expr_of_jcc"

let translate_cmov cmcc elhs erhs =
  let lhs_expr,lhs_writer = expr_of_x86opnd elhs,writer_of_x86opnd elhs in
  gen (lhs_writer (mk_ite (expr_of_jcc cmcc) (expr_of_x86opnd erhs) lhs_expr))

let translate_set cmcc elhs =
  let lhs_writer = writer_of_x86opnd elhs in
  gen (lhs_writer (mk_ite (expr_of_jcc cmcc) (mk_byte 1L) (mk_byte 0L)))
  
(* Machinery for instructions that set the flags *)
type flag_producer = (IR.var -> IR.var -> IR.var -> IR.typereg -> IR.expr)
type flagstranslator = 
| FlagSet 
| FlagClear 
| FlagComplement
| FlagDefaultProducer
| FlagProducer of flag_producer
| FlagPredicated of IR.var * flag_producer * flag_producer

let zps_flags = [(X86F_Z,FlagDefaultProducer);(X86F_P,FlagDefaultProducer);(X86F_S,FlagDefaultProducer);]

let translate_flags lhs_size vor vad vnr flags =
  let do_flag which = function
  | FlagSet   -> gen (mk_assign (var_of_flagreg which) (mk_true ))
  | FlagClear -> gen (mk_assign (var_of_flagreg which) (mk_false))
  | FlagComplement -> 
    let v,e = var_of_flagreg which, expr_of_flagreg which in 
    gen (mk_assign v (mk_not (mk_evar v)))
  | FlagDefaultProducer -> (match which with
     | X86F_Z -> mk_zf_assign lhs_size vnr
     | X86F_P -> mk_pf_assign lhs_size vnr
     | X86F_S -> mk_sf_assign lhs_size vnr
     | _ -> failwith "translate_flags_binop/do_flag: invalid default flag specification")
  | FlagProducer(f) -> gen (mk_assign (var_of_flagreg which) (f vor vad vnr lhs_size))
  | FlagPredicated(v,t,e) -> gen (mk_assign (var_of_flagreg which) (mk_ite (mk_evar v) (t vor vad vnr lhs_size) (e vor vad vnr lhs_size)))
  in
  List.iter (fun (x,y) -> do_flag x y) flags

let x_add _ = mk_add let x_and _ = mk_and let x_sub _ = mk_sub let x_or  _ = mk_or 
let x_sar _ = mk_sar let x_shl _ = mk_shl let x_shr _ = mk_shr let x_xor _ = mk_xor

let translate_binop_op = function
| X86.Adc  -> (x_adc, true , zps_flags@[(X86F_C,FlagPredicated(vCF,adc_carry,add_carry));(X86F_O,FlagProducer(add_overflow));(X86F_A,FlagProducer(add_aux))])
| X86.Add  -> (x_add, true , zps_flags@[(X86F_C,FlagProducer(add_carry));(X86F_O,FlagProducer(add_overflow));(X86F_A,FlagProducer(add_aux))])
| X86.And  -> (x_and, true , zps_flags@[(X86F_C,FlagClear);(X86F_O,FlagClear);(X86F_A,FlagClear)]) (* clear AF, "undefined" *)
| X86.Cmp  -> (x_sub, false, zps_flags@[(X86F_C,FlagProducer(sub_carry));(X86F_O,FlagProducer(sub_overflow));(X86F_A,FlagProducer(add_aux))])
| X86.Or   -> (x_or , true , zps_flags@[(X86F_C,FlagClear);(X86F_O,FlagClear);(X86F_A,FlagClear)]) (* clear AF, "undefined" *)
| X86.Sbb  -> (x_sbb, true , zps_flags@[(X86F_C,FlagPredicated(vCF,sbb_carry,sub_carry));(X86F_O,FlagProducer(sub_overflow));(X86F_A,FlagProducer(add_aux))])
| X86.Sub  -> (x_sub, true , zps_flags@[(X86F_C,FlagProducer(sub_carry));(X86F_O,FlagProducer(sub_overflow));(X86F_A,FlagProducer(add_aux))])
| X86.Test -> (x_and, false, zps_flags@[(X86F_C,FlagClear);(X86F_O,FlagClear);(X86F_A,FlagClear)]) (* clear AF, "undefined" *)
| X86.Xor  -> (x_xor, true , zps_flags@[(X86F_C,FlagClear);(X86F_O,FlagClear);(X86F_A,FlagClear)]) (* clear AF, "undefined" *)
| mnem     -> invalid_arg ("translate_binop: "^string_of_x86mnem mnem)

(* Behaves the same way as add, except it takes the original LHS and stores it
   into the RHS afterwards *)
let translate_xadd lhs rhs =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd lhs in
  let _,rhs_expr,rhs_writer = lhs_info_of_x86opnd rhs in

  (* Original LHS *)
  let vtemp = assign_get_var lhs_size lhs_expr in

  (* Same code from above *)
  let binop_result_producer, wb, flags = translate_binop_op (X86.Add) in
  let vor = assign_get_var lhs_size lhs_expr in
  let vad = assign_get_var lhs_size rhs_expr in
  let vnr = assign_get_var lhs_size (binop_result_producer lhs_size (mk_evar vor) (mk_evar vad)) in
  translate_flags lhs_size vor vad vnr flags;
  gen (lhs_writer (mk_evar vnr));
  
  (* Write the LHS to the RHS *)
  gen (rhs_writer (mk_evar vtemp))

let translate_shift_op = function
| X86.Rol  -> (x_rol, rol_carry, rol_overflow)
| X86.Shr  -> (x_shr, shr_carry, shr_overflow)
| X86.Ror  -> (x_ror, ror_carry, ror_overflow)
| X86.Sar  -> (x_sar, shr_carry, sar_overflow)
| X86.Sal  -> (x_shl, shl_carry, shl_overflow)
| X86.Shl  -> (x_shl, shl_carry, shl_overflow)
| mnem     -> invalid_arg ("translate_binop: "^string_of_x86mnem mnem)

let translate_binop mnem op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = sign_extended_expr_of_x86opnd lhs_size op2 in
  let binop_result_producer, wb, flags = translate_binop_op mnem in
  let vor = assign_get_var lhs_size lhs_expr in
  let vad = assign_get_var lhs_size rhs_expr in
  let vnr = assign_get_var lhs_size (binop_result_producer lhs_size (mk_evar vor) (mk_evar vad)) in
  translate_flags lhs_size vor vad vnr flags;
  if wb then gen (lhs_writer (mk_evar vnr))
  
let translate_shift bis_rotate mnem op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = expr_of_x86opnd op2 in
  let res_prod, cf_prod, of_prod = translate_shift_op mnem in
  let vor = assign_get_var lhs_size    lhs_expr in
  let vad = assign_get_var (TypeReg_8) rhs_expr in
  let vad = assign_get_var (TypeReg_8) (mk_and (Const(Int64.of_int ((IRUtil.bits lhs_size) - 1),TypeReg_8)) (mk_evar vad)) in
  let vnr = assign_get_var lhs_size (res_prod lhs_size (mk_evar vor) (mk_evar vad)) in
  let zero = mk_zero (TypeReg_8) in
  let ad_0_predicator = mk_eq (mk_evar vad) zero in
  let ad_0_pred t e = mk_ite ad_0_predicator t e in
  if not bis_rotate then 
   (gen (mk_assign vZF (ad_0_pred eZF (mk_zf lhs_size vnr)));
    gen (mk_assign vSF (ad_0_pred eSF (mk_sf lhs_size vnr)));
    gen (mk_assign vPF (ad_0_pred ePF (mk_pf lhs_size vnr))));
  gen (mk_assign vCF (ad_0_pred eCF (cf_prod vor vad vnr lhs_size)));
  gen (mk_assign vOF (ad_0_pred eOF (of_prod vor vad vnr lhs_size)));
  gen (lhs_writer (mk_evar vnr))
  
let translate_rotate = translate_shift true
let translate_shift  = translate_shift false

let translate_inc_dec fakemnem op1 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = mk_const (1L) lhs_size in
  let binop_result_producer, _, flags = translate_binop_op fakemnem in
  let vor = assign_get_var lhs_size lhs_expr in
  let vad = assign_get_var lhs_size rhs_expr in
  let vnr = assign_get_var lhs_size (binop_result_producer lhs_size (mk_evar vor) (mk_evar vad)) in
  let flags = List.filter (fun (f,_) -> f <> (X86F_C)) flags in
  translate_flags lhs_size vor vad vnr flags;
  gen (lhs_writer (mk_evar vnr))
  
let translate_inc = translate_inc_dec (X86.Add)
let translate_dec = translate_inc_dec (X86.Sub)

let translate_not op1 = gen ((writer_of_x86opnd op1) (mk_not (expr_of_x86opnd op1)))

let translate_neg op1 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let vor = assign_get_var lhs_size lhs_expr in
  let vnr = assign_get_var lhs_size (mk_neg (mk_evar vor)) in
  translate_flags lhs_size vor vor vnr (zps_flags@[(X86F_C,FlagProducer(neg_carry));(X86F_O,FlagProducer(neg_overflow));(X86F_A,FlagProducer(neg_aux))]);
  gen (lhs_writer (mk_evar vnr))

let double = function
| TypeReg_1  -> failwith "double: TypeReg_1"
| TypeReg_8  -> TypeReg_16
| TypeReg_16 -> TypeReg_32
| TypeReg_32 -> TypeReg_64
| TypeReg_64 -> failwith "double: TypeReg_64: 64-bit not supported yet"

let halve = function
| TypeReg_1  -> failwith "halve: TypeReg_1"
| TypeReg_8  -> failwith "halve: TypeReg_8"
| TypeReg_16 -> TypeReg_8
| TypeReg_32 -> TypeReg_16
| TypeReg_64 -> TypeReg_32

let do_mul widen size lhs rhs = 
  let out_sz = double size in
  let expand e = widen out_sz (mk_low_cast size e) in
  let elhs = expand lhs in
  let erhs = expand rhs in
  let eres = mk_mul elhs erhs in
  (eres,out_sz)
  
let do_umul = do_mul mk_unsigned_cast
let do_smul = do_mul mk_signed_cast

let translate_imul_mul_onearg whichmul flfun op1 = 
  let rhs_size,rhs_expr = size_of_x86opnd op1,expr_of_x86opnd op1 in
  let vor = assign_get_var rhs_size 
   (match rhs_size with 
    | TypeReg_8 -> eAl 
    | TypeReg_16 -> eAx 
    | TypeReg_32 -> eEax 
    | _ -> invalid_arg "translate_imul_mul_onearg") 
  in
  let vad = assign_get_var rhs_size rhs_expr in
  let (eres,ressize) = whichmul rhs_size (mk_evar vor) (mk_evar vad) in
  let vnr = assign_get_var ressize eres in
  let enr = mk_evar vnr in
  let ehigh = mk_high_cast rhs_size enr in
 (match rhs_size with
  | TypeReg_8  -> gen (instr_set_r16 (Ax) (mk_evar vnr))
  | TypeReg_16 -> 
    gen (instr_set_r16 (Ax) (mk_low_cast (TypeReg_16) enr));
    gen (instr_set_r16 (Dx) ehigh)
  | TypeReg_32 -> 
    gen (mk_assign vEax (mk_low_cast (TypeReg_32) enr));
    gen (mk_assign vEdx ehigh)
  | TypeReg_64 -> invalid_arg "translate_imul_mul_onearg: rax unsupported"
  | TypeReg_1  -> invalid_arg "translate_imul_mul_onearg: TypeReg_1 absurd");
  flfun eres rhs_size
  
let umul_fl expr sz = 
  let efl  = mk_ne (mk_high_cast sz expr) (mk_zero sz) in
  gen (mk_assign vCF efl);
  gen (mk_assign vOF efl);
  (* These flags are "undefined" *)
  gen (mk_assign vZF mk_false);
  gen (mk_assign vSF mk_false);
  gen (mk_assign vAF mk_false);
  gen (mk_assign vPF mk_false)

let translate_mul = translate_imul_mul_onearg do_umul umul_fl

let smul_fl expr sz =
  let efl = mk_ne expr (mk_signed_cast (double sz) (mk_low_cast sz expr)) in
  gen (mk_assign vCF efl);
  gen (mk_assign vOF efl);
  (* These flags are "undefined" *)
  gen (mk_assign vZF mk_false);
  gen (mk_assign vSF mk_false);
  gen (mk_assign vAF mk_false);
  gen (mk_assign vPF mk_false)
  

let translate_imul_onearg = translate_imul_mul_onearg do_smul smul_fl

let translate_imul_twoargs op1 op2 = 
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = expr_of_x86opnd op2 in
  let vor = assign_get_var lhs_size lhs_expr in
  let vad = assign_get_var lhs_size rhs_expr in
  let (eres,ressize) = do_smul lhs_size (mk_evar vor) (mk_evar vad) in
  smul_fl eres lhs_size;
  gen (lhs_writer (mk_low_cast lhs_size eres))

let translate_imul_threeargs op1 op2 op3 =
  let lhs_size,_,lhs_writer = lhs_info_of_x86opnd op1 in
  let mhs_expr              = expr_of_x86opnd op2 in
  let rhs_expr              = sign_extended_expr_of_x86opnd lhs_size op3 in
  let vor                   = assign_get_var lhs_size mhs_expr in
  let vad                   = assign_get_var lhs_size rhs_expr in
  let eres,ressize          = do_smul lhs_size (mk_evar vor) (mk_evar vad) in
  smul_fl eres lhs_size;
  gen (lhs_writer (mk_low_cast lhs_size eres))

let translate_mov_extend fext op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = expr_of_x86opnd op2 in
  gen (lhs_writer (fext lhs_size rhs_expr))

let translate_movzx = translate_mov_extend mk_unsigned_cast
let translate_movsx = translate_mov_extend mk_signed_cast

let translate_div widen divfn modfn op1 =
  let rhs_size,rhs_expr = size_of_x86opnd op1,expr_of_x86opnd op1 in
  let ds = double rhs_size in
  let lhs = match rhs_size with
  | TypeReg_8  -> eAx
  | TypeReg_16 -> 
    mk_or
     (mk_shr (mk_unsigned_cast ds eDx) (mk_byte 16L))
     (mk_unsigned_cast ds eAx)
  | TypeReg_32 -> 
    mk_or
     (mk_shr (mk_unsigned_cast ds eEdx) (mk_byte 32L))
     (mk_unsigned_cast ds eEax)
  | TypeReg_1  -> invalid_arg "translate_div"
  | TypeReg_64 -> invalid_arg "translate_div: 64-bit unsupported"
  in
  let vad = assign_get_var ds (widen ds rhs_expr) in
  let vor = assign_get_var ds lhs in
  let vqt = assign_get_var ds (mk_low_cast rhs_size (divfn (mk_evar vor) (mk_evar vad))) in
  let vrm = assign_get_var ds (mk_low_cast rhs_size (modfn (mk_evar vor) (mk_evar vad))) in
  match rhs_size with
  | TypeReg_8 -> 
    gen (instr_set_r8_lower_in_r32 (Al) (mk_evar vqt));
    gen (instr_set_r8_upper_in_r32 (Ah) (mk_evar vrm))
  | TypeReg_16 -> 
    gen (instr_set_r16 (Ax) (mk_evar vqt));
    gen (instr_set_r16 (Dx) (mk_evar vrm))
  | TypeReg_32 -> 
    gen (mk_assign vEax (mk_evar vqt));
    gen (mk_assign vEdx (mk_evar vrm))
  | TypeReg_1  -> invalid_arg "translate_div"
  | TypeReg_64 -> invalid_arg "translate_div: 64-bit unsupported"
    
let translate_idiv = translate_div (mk_signed_cast  ) (mk_sdiv) (mk_smod)
let translate_div  = translate_div (mk_unsigned_cast) (mk_udiv) (mk_umod)

let translate_cbw  () = translate_movsx (GeneralReg(Gw(Ax)))  (GeneralReg(Gb(Al)))
let translate_cwde () = translate_movsx (GeneralReg(Gd(Eax))) (GeneralReg(Gw(Ax)))
let translate_cwd  () = gen (instr_set_r16 (Dx) (mk_high_cast (TypeReg_16) (mk_signed_cast (TypeReg_32) eAx)))
let translate_cdq  () = gen (mk_assign vEdx (mk_high_cast (TypeReg_32) (mk_signed_cast (TypeReg_64) eEax)))

let translate_lahf () =
  let m f b = mk_shl (mk_unsigned_cast (TypeReg_8) f) (mk_byte b) in
  let f acc (eF,i) = mk_or (m eF i) acc in
  let ah = List.fold_left f (mk_unsigned_cast (TypeReg_8) eCF) [ePF,2L;eAF,4L;eZF,6L;eSF,7L] in
  gen (instr_set_r8_upper_in_r32 (Ah) ah)
  
let translate_sahf () =
  let ah = mk_evar (assign_get_var (TypeReg_8) eAh) in
  let z  = mk_byte 0L in
  let assign vF amt = gen (mk_assign vF (mk_ne (mk_and (mk_byte amt) ah) z)) in
  List.iter (curry2 assign) [vCF,0x01L;vPF,0x04L;vAF,0x10L;vZF,0x40L;vSF,0x80L]
  
let translate_salc () = gen (instr_set_r8_lower_in_r32 (Al) (mk_ite eCF (mk_byte 0xFFL) (mk_byte 0L)))
let translate_cmc  () = gen (mk_assign vCF (mk_not eCF))
let translate_clc  () = gen (mk_assign vCF mk_false)
let translate_stc  () = gen (mk_assign vCF mk_true)
let translate_cld  () = gen (mk_assign vDF mk_false)
let translate_std  () = gen (mk_assign vDF mk_true)

(* Refactor aaa and aas together *)
let translate_aa_add_sub fn =
  let aland = (mk_and eAl (mk_byte 0xFL)) in
  let vcond = assign_get_var (TypeReg_1) (mk_or (mk_eq eAF (mk_true)) (mk_ult (mk_byte 9L) aland)) in
  let econd = mk_evar vcond in
  gen (instr_set_r16 (Ax) (mk_ite econd (fn eAx (mk_word 0x106L)) eAx));
  gen (mk_assign vAF econd);
  gen (mk_assign vCF econd);
  gen (instr_set_r8_lower_in_r32 (Al) aland)

let translate_aaa () = translate_aa_add_sub mk_add
let translate_aas () = translate_aa_add_sub mk_sub

let translate_aad op1 = 
  let imm = match op1 with | Immediate(Ib(i)) -> Int64.of_int32 i | _ -> failwith "translate_aad: typechecking prevents this" in
  let newal = mk_and (mk_add eAl (mk_mul eAh (mk_byte imm))) (mk_byte 0xFFL) in
  gen (instr_set_r16 (Ax) (mk_unsigned_cast (TypeReg_16) newal));
  mk_zf_assign_expr (TypeReg_8) newal;
  mk_sf_assign_expr (TypeReg_8) newal;
  mk_pf_assign_expr (TypeReg_8) newal
  
let translate_aam op1 =
  let imm = match op1 with | Immediate(Ib(i)) -> Int64.of_int32 i | _ -> failwith "translate_aad: typechecking prevents this" in
  let newal = mk_udiv eAl (mk_byte imm) in
  let newah = mk_umod eAh (mk_byte imm) in
  gen (instr_set_r8_lower_in_r32 (Al) newal);
  gen (instr_set_r8_upper_in_r32 (Ah) newah);
  mk_zf_assign_expr (TypeReg_8) newal;
  mk_sf_assign_expr (TypeReg_8) newal;
  mk_pf_assign_expr (TypeReg_8) newal

let translate_daa () = 
  let old_al = mk_evar (assign_get_var (TypeReg_8) eAl) in
  let old_cf = mk_evar (assign_get_var (TypeReg_1) eCF) in
  gen (mk_assign vCF mk_false);
  let cond1 = mk_or (mk_ult (mk_byte 9L) (mk_and old_al (mk_byte 0xFL))) eAF in
  gen (instr_set_r8_lower_in_r32 (Al) (mk_ite cond1 (mk_add eAl (mk_byte 6L)) eAl));
  gen (mk_assign vCF (mk_ite cond1 (mk_or old_cf (mk_ult (mk_byte 0xF9L) old_al)) eCF));
  gen (mk_assign vAF cond1);
  let cond2 = mk_or (mk_ult (mk_byte 0x99L) old_al) old_cf in
  gen (instr_set_r8_lower_in_r32 (Al) (mk_ite cond2 (mk_add eAl (mk_byte 0x60L)) eAl));
  gen (mk_assign vCF cond2);
  let new_al = eAl in
  mk_zf_assign_expr (TypeReg_8) new_al;
  mk_sf_assign_expr (TypeReg_8) new_al;
  mk_pf_assign_expr (TypeReg_8) new_al
  
let translate_das () = 
  let old_al = mk_evar (assign_get_var (TypeReg_8) eAl) in
  let old_cf = mk_evar (assign_get_var (TypeReg_1) eCF) in
  gen (mk_assign vCF mk_false);
  let cond1 = mk_or (mk_ult (mk_byte 9L) (mk_and old_al (mk_byte 0xFL))) eAF in
  gen (instr_set_r8_lower_in_r32 (Al) (mk_ite cond1 (mk_sub eAl (mk_byte 6L)) eAl));
  gen (mk_assign vCF (mk_ite cond1 (mk_or old_cf (mk_ult old_al (mk_byte 0x06L))) eCF));
  gen (mk_assign vAF cond1);
  let cond2 = mk_or (mk_ult (mk_byte 0x99L) old_al) old_cf in
  gen (instr_set_r8_lower_in_r32 (Al) (mk_ite cond2 (mk_sub eAl (mk_byte 0x60L)) eAl));
  gen (mk_assign vCF (mk_ite cond2 mk_true eCF));
  let new_al = eAl in
  mk_zf_assign_expr (TypeReg_8) new_al;
  mk_sf_assign_expr (TypeReg_8) new_al;
  mk_pf_assign_expr (TypeReg_8) new_al
  
let translate_bitscan m op1 op2 = 
  let revlist_of_range low hi = 
    let rec aux i l =
      if i < hi
      then aux (i+1) (i::l)
      else l
    in aux low []
  in
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr = expr_of_x86opnd op2 in
  let rhs_expr = mk_evar (assign_get_var lhs_size rhs_expr) in
  let nums     = revlist_of_range 0 (IRUtil.bits lhs_size) in
  let mapfunc  = match m with | Bsf -> List.map | Bsr -> List.rev_map | _ -> invalid_arg "translate_bitscan" in
  let shifts   = mapfunc (fun i -> (mk_const (Int64.shift_left Int64.one i) lhs_size,i)) nums in
  let zero     = mk_zero lhs_size in
  let expr     = 
    List.fold_left 
     (fun acc (mask,i) -> mk_ite (mk_ne (mk_and rhs_expr mask) zero) (mk_const (Int64.of_int i) lhs_size) acc) 
     (mk_const 0x1337L lhs_size)
      shifts 
  in
  let reqz = mk_eq rhs_expr zero in
  let expr = mk_ite reqz lhs_expr expr in
  gen (lhs_writer expr);
  gen (mk_assign vZF reqz)
  
let translate_bittest_register m op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_expr  = expr_of_x86opnd op2 in
  let rhs_expr  = mk_evar (assign_get_var lhs_size rhs_expr) in
  let rhs_expr  = mk_evar (assign_get_var (TypeReg_8) (mk_low_cast (TypeReg_8) rhs_expr)) in
  let rhs_anded = mk_evar (assign_get_var lhs_size (mk_and (mk_const (Int64.of_int (IRUtil.bits lhs_size - 1)) (TypeReg_8)) rhs_expr)) in
  let mask      = mk_shl (mk_const Int64.one lhs_size) rhs_anded in
  let zero      = mk_zero lhs_size in
  gen (mk_assign vCF (mk_ne zero (mk_and lhs_expr mask)));
  match m with
  | Bt  -> ()
  | Bts -> gen (lhs_writer (mk_or  lhs_expr mask))
  | Btr -> gen (lhs_writer (mk_and lhs_expr (mk_not mask)))
  | Btc -> gen (lhs_writer (mk_xor lhs_expr mask))
  | _ -> invalid_arg "translate_bt_insn_register"
  
let translate_bittest_memory m op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in  
  let lhs_bits    = IRUtil.bits lhs_size in
  let lhs_bitmask = Int64.of_int (lhs_bits - 1) in
  let rhs_expr    = expr_of_x86opnd op2 in

  let displacement,index = match op2 with

  (* Two cases: either the RHS is an immediate... *)
  | Immediate(Ib(i))   -> 
    
    (* Immediates can't encode byte displacements *)
    mk_dword 0L, 
    
    (* Immediates can only specify indices within the register size *)
    mk_byte (Int64.logand lhs_bitmask (Int64.of_int32 i))

  (* ... or it's a register. *)
  | GeneralReg(Gw(_))
  | GeneralReg(Gd(_)) -> 
    (* Cast the register to a 32-bit quantity (redundant for 32-bit registers *)
    let rhs_expr = mk_unsigned_cast (TypeReg_32) rhs_expr in

    (* Registers can specify displacements.  Whereas the Intel manuals and
       the Bochs source code treat these as being measured in the number of 
       words/dwords, and have some complications to go along with how many
       words/dwords are being specified, we follow VEX and instead treat 
       this as a number of bytes.  This allows for uniform handling of the 
       two cases.  The displacement (in number of bytes) is equal to 
       (rhs >> 3), where the shift is of the arithmetic variety. *)
    mk_sar rhs_expr (mk_byte 3L),
    
    (* Index is the low 3 bits *)
    mk_low_cast (TypeReg_8) (mk_and rhs_expr (mk_dword 7L))
   
  | _ -> invalid_arg "translate_bittest_memory: typechecking prevents this"
  
  in
  let memloc = mk_add lhs_expr displacement in
  let loaded = mk_load eMem memloc (TypeReg_8) in
  let zero   = mk_zero lhs_size in
  let mask   = mk_shl (mk_const Int64.one (TypeReg_8)) index in

  gen (mk_assign vCF (mk_ne zero (mk_and loaded mask)));
  match m with
  | Bt  -> ()
  | Bts -> gen (mk_assign vMem (mk_store eMem memloc (mk_or  loaded mask) (TypeReg_8)))
  | Btr -> gen (mk_assign vMem (mk_store eMem memloc (mk_and loaded (mk_not mask)) (TypeReg_8)))
  | Btc -> gen (mk_assign vMem (mk_store eMem memloc (mk_xor loaded mask) (TypeReg_8)))
  | _ -> invalid_arg "translate_bittest_memory"

let translate_bittest m op1 op2 =
  match op1 with
  | GeneralReg(_) -> translate_bittest_register m op1 op2
  | Memexpr(_)    -> translate_bittest_memory m op1 op2
  | _ -> invalid_arg "translate_bt:  lhs of illegal type (typechecking prevents this)"

let translate_popcnt lhs rhs = 
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd lhs in
  let rhs_expr,rhs_size = expr_of_x86opnd rhs,size_of_x86opnd rhs in
  let rhs_expr = mk_evar (assign_get_var rhs_size rhs_expr) in
  let rhs_bits = bits rhs_size in
  let rec aux i lastvar mask =
    if i = rhs_bits
    then lastvar
    else
      let newvar = 
        assign_get_var 
          lhs_size 
         (mk_ite 
           (mk_ne (mk_and (mk_const mask rhs_size) rhs_expr) (mk_const 0L rhs_size)) 
           (mk_add lastvar (mk_const 1L rhs_size)) 
            lastvar)
      in
      aux (i+1) (mk_evar newvar) (Int64.shift_left mask 1)
  in
  let terminalvar = aux 0 (mk_const 0L rhs_size) 1L in
  mk_zf_assign_expr lhs_size terminalvar;
  let zero = mk_const 0L (TypeReg_1) in
  gen (mk_assign vOF zero);
  gen (mk_assign vSF zero);
  gen (mk_assign vAF zero);
  gen (mk_assign vPF zero);
  gen (mk_assign vCF zero);
  gen (lhs_writer terminalvar)
  
let mk_esp_add s =
  let espadd = mk_add eEsp (mk_dword (Int64.of_int (IRUtil.bits s / 8))) in
  gen (mk_assign vEsp espadd)

let mk_esp_sub s =
  let espsub = mk_sub eEsp (mk_dword (Int64.of_int (IRUtil.bits s / 8))) in
  gen (mk_assign vEsp espsub)

let push_quantity expr s = match s with
| TypeReg_32
| TypeReg_16 -> 
  (* We must assign what we're pushing to a temporary variable before
     pushing it.  Consider push dword ptr [esp]:  we need to grab the
     contents of memory before we subtract from esp. *)
  let vWhat = assign_get_var s expr in
  mk_esp_sub s; 
  gen (mk_assign vMem (mk_store eMem eEsp (mk_evar vWhat) s))
| _ -> invalid_arg "push_quantity"

(* This only works for 32-bit registers, because 16-bit registers are not 
   variables, they're sub-expressions *)
let pop_quantity_to vTo s = match s with
| TypeReg_32
| TypeReg_16 -> 
  let vWhat = assign_get_var s (mk_load eMem eEsp s) in
  (* The addition to esp must come before the assignment to the variable:  
     consider "pop esp".  This instruction simply replaces esp with whatever
     is on the stack; it's equivalent to "mov esp, [esp]".  If we add after
     we've assigned, we will incorrectly increment esp.  This is why we 
     must inelegantly introduce the temporary variable above. *)
  mk_esp_add s;
  gen (mk_assign vTo (mk_evar vWhat))
| _ -> invalid_arg "pop_quantity"

let pop_quantity s = let v = new_var s in pop_quantity_to v s; v

let translate_push op1 = push_quantity (expr_of_x86opnd op1) (size_of_x86opnd op1)
let translate_pop  op1 = match op1 with
| SegReg(s) -> pop_quantity_to (var_of_segreg s) (TypeReg_16)
| GeneralReg(Gd(gd)) -> pop_quantity_to (var_of_reg32 gd) (TypeReg_32)
| GeneralReg(Gw(gw)) -> gen (instr_set_r16 gw (mk_evar (pop_quantity (TypeReg_16))))
| Memexpr(Mw(_))
| Memexpr(Md(_)) -> 
  let writer,size = writer_of_x86opnd op1,size_of_x86opnd op1 in
  let v = pop_quantity size in
  gen (writer (mk_evar v))
| _ -> failwith "translate_pop:  trying to pop into non-Sw/Gw/Gd/Mw/Md"

let translate_leave () =
  let _ = gen (mk_assign vEsp eEbp) in
  translate_pop (GeneralReg(Gd(Ebp)))

let translate_pushad () =
  let vOrigEsp = assign_get_var (TypeReg_32) eEsp in
  List.iter (fun e -> push_quantity e (TypeReg_32)) [eEax;eEcx;eEdx;eEbx;(mk_evar vOrigEsp);eEbp;eEsi;eEdi]
  
let translate_pushaw () =
  let vOrigEsp = assign_get_var (TypeReg_32) eEsp in
  List.iter (fun e -> push_quantity e (TypeReg_16)) [eAx;eCx;eDx;eBx;(mk_low_cast (TypeReg_16) (mk_evar vOrigEsp));eBp;eSi;eDi]

let translate_popad () =
  let do_pops list = List.iter (fun v -> pop_quantity_to v (TypeReg_32)) list in
  do_pops [vEdi;vEsi;vEbp];
  mk_esp_add (TypeReg_32);
  do_pops [vEbx;vEdx;vEcx;vEax]

let translate_popaw () =
  let do_pops list = List.iter (fun r16 -> let v = pop_quantity (TypeReg_16) in gen (instr_set_r16 r16 (mk_evar v))) list in
  do_pops [Di;Si;Bp];
  mk_esp_add (TypeReg_16);
  do_pops [Bx;Dx;Cx;Ax]

let translate_pushf s =
  let t e a = mk_shl (mk_unsigned_cast s e) (mk_byte a) in
  let fl   = List.map (curry2 t) [eSF,7L;eZF,6L;eAF,4L;ePF,2L;eOF,11L;eDF,10L] in
  let expr = List.fold_left mk_or (mk_unsigned_cast s eCF) fl in
  push_quantity expr s

let translate_pushfd () = translate_pushf (TypeReg_32)
let translate_pushfw () = translate_pushf (TypeReg_16)

let translate_popf s =
  let eFl = mk_evar (pop_quantity s) in
  let t v a = gen (mk_assign v (mk_low_cast (TypeReg_1) (mk_shr eFl (mk_byte a)))) in
  List.iter (curry2 t) [vSF,7L;vZF,6L;vAF,4L;vPF,2L;vOF,11L;vDF,10L];
  gen (mk_assign vCF (mk_low_cast (TypeReg_1) eFl))

let translate_popfd () = translate_popf (TypeReg_32)
let translate_popfw () = translate_popf (TypeReg_16)

(* FIXME:  DO COA FLAGS *)
let translate_shld_shlr shift_lhs shift_mhs op1 op2 op3 = 
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let ecount = mk_evar (assign_get_var (TypeReg_8) (mk_and (expr_of_x86opnd op3) (mk_byte 0x1FL))) in
  let lhs_bits = mk_byte (Int64.of_int (IRUtil.bits lhs_size)) in
  let ad_0_pred = mk_ite (mk_eq (mk_zero (TypeReg_8)) ecount) in
  let shift_in = shift_mhs (expr_of_x86opnd op2) (mk_sub lhs_bits ecount) in
  let shifted_lhs = shift_lhs lhs_expr ecount in
  let vresult = assign_get_var lhs_size (mk_or shift_in shifted_lhs) in
  let eresult = mk_evar vresult in
  gen (lhs_writer (ad_0_pred lhs_expr eresult));
  gen (mk_assign vZF (ad_0_pred eZF (mk_zf lhs_size vresult)));
  gen (mk_assign vSF (ad_0_pred eSF (mk_sf lhs_size vresult)));
  gen (mk_assign vPF (ad_0_pred ePF (mk_pf lhs_size vresult)))

let translate_shld = translate_shld_shlr mk_shl mk_shr
let translate_shrd = translate_shld_shlr mk_shr mk_shl 

let mk_bswap32 e =
  let ee,ve = expr_of_reg32 e,var_of_reg32 e in
  let e1 = mk_shr ee (mk_byte 0x18L) in
  let e2 = mk_and (mk_shr ee (mk_byte 0x08L)) (mk_dword 0xFF00L  ) in
  let e3 = mk_and (mk_shl ee (mk_byte 0x08L)) (mk_dword 0xFF0000L) in
  let e4 = mk_shl ee (mk_byte 0x18L) in
  let er = mk_or e1 (mk_or e2 (mk_or e3 e4)) in
  gen (mk_assign ve er)

let mk_bswap16 e = gen (instr_set_r16 e (mk_word 0x0L))

let translate_bswap = function
| GeneralReg(Gd(r)) -> mk_bswap32 r
| GeneralReg(Gw(r)) -> mk_bswap16 r
| _ -> failwith "translate_bswap: typechecking prevents this"

let mk_low_cast      = mk_cast (Low)

let translate_lea op1 op2 =
  let lhs_size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let rhs_memexpr = match op2 with
  | Memexpr(Mb (m))
  | Memexpr(Mw (m))
  | Memexpr(Md (m))
  | Memexpr(Mf (m))
  | Memexpr(Mq (m))
  | Memexpr(Mt (m))
  | Memexpr(Mdq(m)) -> m
  | _ -> failwith "lea: rhs not a memory expression"
  in
  let rhs_size = match rhs_memexpr with | Mem16(_) -> TypeReg_16 | Mem32(_) -> TypeReg_32 in
  let rhs_expr = expr_of_memexpr rhs_memexpr in
  match lhs_size,rhs_size with
  | TypeReg_16,TypeReg_16 -> gen (lhs_writer rhs_expr)
  | TypeReg_16,TypeReg_32 -> gen (lhs_writer (mk_low_cast lhs_size rhs_expr))
  | TypeReg_32,TypeReg_16 -> gen (lhs_writer (mk_unsigned_cast lhs_size rhs_expr))
  | TypeReg_32,TypeReg_32 -> gen (lhs_writer rhs_expr)
  | _,_ -> failwith "lea: {16,32}/{16,32} currently supported (no 64-bit or ill-formed expressions)"

let translate_mov op1 op2 =
  let lhs_writer = writer_of_x86opnd op1 in
  match op1,op2 with
  | GeneralReg(Gd(g)),SegReg(s) -> gen (lhs_writer (mk_unsigned_cast (TypeReg_32) (expr_of_segreg s)))
  | _,_ -> gen (lhs_writer (expr_of_x86opnd op2))

let translate_xchg op1 op2 =
  let size,lhs_expr,lhs_writer = lhs_info_of_x86opnd op1 in
  let _,   rhs_expr,rhs_writer = lhs_info_of_x86opnd op2 in
  let temp_evar = mk_evar (assign_get_var size lhs_expr) in
  gen (lhs_writer rhs_expr);
  gen (rhs_writer temp_evar)

(* FIXME:  Does not work for 16-bit addressing modes, need to inspect the argument *)
let translate_lodsb _ =
  gen (instr_set_r8_lower_in_r32 (Al) (mk_load eMem eEsi (TypeReg_8)));
  gen (mk_assign vEsi (mk_add eEsi (mk_dword 1L)))

(* FIXME:  Does not work for 16-bit addressing modes, need to inspect the argument *)
let translate_lodsw _ =
  gen (instr_set_r16 (Ax) (mk_load eMem eEsi (TypeReg_16)));
  gen (mk_assign vEsi (mk_add eEsi (mk_dword 2L)))

(* FIXME:  Does not work for 16-bit addressing modes, need to inspect the argument *)
let translate_lodsd _ =
  gen (mk_assign vEax (mk_load eMem eEsi (TypeReg_32)));
  gen (mk_assign vEsi (mk_add eEsi (mk_dword 4L)))

(* FIXME:  Does not work for 16-bit addressing modes, need to inspect the argument *)
let translate_stosb _ =
  gen (mk_assign vMem (mk_store eMem eEdi eAl (TypeReg_8)));
  gen (mk_assign vEdi (mk_add eEdi (mk_dword 1L)))

(* FIXME:  Need to modify for 16-bit code? *)
let translate_ret opnd =
  let ra = pop_quantity (TypeReg_32) in
 (match opnd with
  | None -> ()
  | Some(i) -> gen (mk_assign vEsp (mk_add eEsp (mk_dword (Int64.of_int32 i)))));
  gen (mk_jmp (mk_evar ra))

let get_one_arg = function
| op1::[] -> op1
| _ -> failwith "get_one_arg: typechecking prevents this"

let get_two_args = function
| op1::op2::[] -> (op1,op2)
| _ -> failwith "get_two_args: typechecking prevents this"

let get_three_args = function
| op1::op2::op3::[] -> (op1,op2,op3)
| _ -> failwith "get_three_args: typechecking prevents this"

let translate_instr ea ({ pref = pref; instr = (m,o) } as ip) =
  let curry2 f o = let op1,op2 = get_two_args o in f op1 op2 in
  let curry3 f o = let op1,op2,op3 = get_three_args o in f op1 op2 op3 in
  clear ();
  gen (mk_label ea);
  gen (mk_comment (string_of_x86instr ip));
 (match m with
  | Adc | Add | And | Cmp | Or | Sbb | Sub | Test | Xor -> curry2 (translate_binop m) o
  | Sar | Shl | Shr | Sal -> curry2 (translate_shift m) o
  | Rol | Ror -> curry2 (translate_rotate m) o

  (* OBVIOUSLY WRONG, NEED TO DO THESE TWO *)
  | Rcl -> curry2 (translate_rotate (Rol)) o
  | Rcr -> curry2 (translate_rotate (Ror)) o

  | Bswap -> translate_bswap (get_one_arg o)

  | Cmovs  | Cmovns | Cmovo  | Cmovno | Cmovz  | Cmovnz | Cmovp | Cmovnp | Cmova 
  | Cmovae | Cmovb  | Cmovbe | Cmovl  | Cmovle | Cmovg  | Cmovge -> 
    curry2 (translate_cmov m) o

  | Sets  | Setns | Seto  | Setno | Setz  | Setnz | Setp | Setnp | Seta 
  | Setae | Setb  | Setbe | Setl  | Setle | Setg  | Setge -> 
    translate_set m (get_one_arg o)

  | Ja  | Jae | Jb | Jbe | Jcxz | Jecxz | Jg | Jge | Jl | Jle | Jno | Jnp
  | Jns | Jnz | Jo | Jp  | Js   | Jz -> let ta,fa = X86Util.extract_jcctargets o in 
    gen (CJmp(expr_of_jcc m,mk_dword (Int64.of_int32 ta),mk_dword (Int64.of_int32 fa)))
  
  | Loop -> let ta,fa = X86Util.extract_jcctargets o in 
    gen (mk_assign vEcx (mk_sub eEcx (mk_dword 1L)));
    gen (CJmp(mk_ne eEcx (mk_dword 0L),mk_dword (Int64.of_int32 ta),mk_dword (Int64.of_int32 fa)))
  
  | Jmp ->
   (match o with
    | [JccTarget(ta,_)] -> gen (IR.Jmp(mk_dword (Int64.of_int32 ta)))
    | [x] -> gen (IR.Jmp(expr_of_x86opnd x))
    | _   -> failwith "translate_instr: jmp that does not have one argument; typechecking prevents this")

  | Movsx -> curry2 translate_movsx o
  | Movzx -> curry2 translate_movzx o
  
  | Cbw  -> translate_cbw  ()
  | Cwde -> translate_cwde ()
  | Cwd  -> translate_cwd  ()
  | Cdq  -> translate_cdq  ()
  | Lahf -> translate_lahf ()
  | Sahf -> translate_sahf ()
  | Salc -> translate_salc ()
  | Cmc  -> translate_cmc  ()
  | Clc  -> translate_clc  ()
  | Stc  -> translate_stc  ()
  | Cld  -> translate_cld  ()
  | Std  -> translate_std  ()
  | Aaa  -> translate_aaa  ()
  | Aad  ->    
   (match List.length o with
    | 0 -> translate_aad (Immediate(Ib(0xAl)))
    | 1 -> translate_aad (get_one_arg o)
    | _ -> failwith "translate_instr: aad: typechecking prevents this")
  | Aam  ->    
   (match List.length o with
    | 0 -> translate_aam (Immediate(Ib(0xAl)))
    | 1 -> translate_aam (get_one_arg o)
    | _ -> failwith "translate_instr: aam: typechecking prevents this")
  | Aas  -> translate_aas ()
  | Daa  -> translate_daa ()
  | Das  -> translate_das ()
  | Bsf
  | Bsr -> curry2 (translate_bitscan m) o
  | Bt
  | Bts
  | Btr
  | Btc -> curry2 (translate_bittest m) o
  | Popcnt -> curry2 translate_popcnt o
  | Mul -> translate_mul (get_one_arg o)
  | Imul -> 
   (match List.length o with
    | 1 -> translate_imul_onearg (get_one_arg o)
    | 2 -> curry2 translate_imul_twoargs o
    | 3 -> curry3 translate_imul_threeargs o
    | _ -> invalid_arg ("translate_instr: imul with >3 args"))
  | Div  -> translate_div  (get_one_arg o)
  | Idiv -> translate_idiv (get_one_arg o)
  | Push   -> translate_push (get_one_arg o)
  | Pushad -> translate_pushad ()
  | Pushaw -> translate_pushaw ()
  | Pushfd -> translate_pushfd ()
  | Pushfw -> translate_pushfw ()
  | Pop    -> translate_pop (get_one_arg o)
  | Popad  -> translate_popad ()
  | Popaw  -> translate_popaw ()
  | Popfd  -> translate_popfd ()
  | Popfw  -> translate_popfw ()
  | Inc    -> translate_inc (get_one_arg o)
  | Dec    -> translate_dec (get_one_arg o)
  | Not    -> translate_not (get_one_arg o)
  | Neg    -> translate_neg (get_one_arg o)
  | Shld   -> curry3 translate_shld o
  | Shrd   -> curry3 translate_shrd o
  | Wait   -> ()
  | Pause  -> ()
  | Nop    -> ()
  | Lea    -> curry2 translate_lea o
  | Mov    -> curry2 translate_mov o
  | Xchg   -> curry2 translate_xchg o
  | Lodsb when pref = [] -> translate_lodsb (get_one_arg o)
  | Lodsw when pref = [] -> translate_lodsw (get_one_arg o)
  | Lodsd when pref = [] -> translate_lodsd (get_one_arg o)
  | Stosb when pref = [] -> translate_stosb (get_one_arg o)
  | Ret    ->
   (match o with
    | [] -> translate_ret (None)
    | [Immediate(Iw(i))] -> translate_ret (Some(i))
    | _ -> failwith "translate_instr: ret: typechecking prevents this")
  | Xadd   -> curry2 translate_xadd o
  | Leave  -> translate_leave ()
  | mnem   -> invalid_arg ("translate_instr: "^string_of_x86mnem mnem));
  get_translation ()

let translate ea = 
  let instr,len,_ = X86Decode.decode ea in 
  match instr with
  | { pref = _; instr = (Call,[opnd])} ->
    gen (mk_label ea);
    gen (mk_comment (string_of_x86instr instr));
    push_quantity (Const(Int64.add (Int64.of_int32 ea) (Int64.of_int len),TypeReg_32)) (TypeReg_32);
   (match opnd with
    | JccTarget(ta,_) -> gen (mk_jmp (Const(Int64.of_int32 ta,TypeReg_32)))
    | _ -> invalid_arg "translate call: only JccTarget supported for now");
    get_translation ()
  | _ -> translate_instr ea instr
