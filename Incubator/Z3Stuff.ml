Functionality provided in this module:

* Making a new context -- no dependencies
* Making "sorts", given a context and an IR.typereg size -- needs a context

* Mapping IR.var terms into Z3.ast terms -- relies upon previous function
* Making a new variable, given a context, a name, and a type -- needs a context
* Making numerals from integers, and given a size -- needs a context
* Making integer booleans from Z3's internal representation -- needs a context

* Making Z3.ast terms from IR.expr terms -- needs a context and all of the above

* Asserting Z3 statements from IR.instr terms (assert a postcondition too)

* Checking the validity of the context
* Retrieving the result

open IR

let mk_context () = Z3.mk_context_x [|("MODEL", "true")|]
let mk_var ctx name ty = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) ty

let sort_of_typereg ctx = 
  let bv1sort  = Z3.mk_bv_sort ctx 1  in
  let bv8sort  = Z3.mk_bv_sort ctx 8  in
  let bv16sort = Z3.mk_bv_sort ctx 16 in
  let bv32sort = Z3.mk_bv_sort ctx 32 in
  let bv64sort = Z3.mk_bv_sort ctx 64 in
  function
  | TypeReg_1  -> bv1sort
  | TypeReg_8  -> bv8sort
  | TypeReg_16 -> bv16sort
  | TypeReg_32 -> bv32sort
  | TypeReg_64 -> bv64sort
  
let z3var_of_variable tr2sort ctx = 
  let htbl = Hashtbl.create 50 in
 (fun v ->
    let p = PpIR.ppVar v in
    let r = try Hashtbl.find htbl p with Not_found ->
     (let s = 
        match v with
        | Variable(_,s) -> tr2sort s
        | Mem(_,_,_)    -> Z3.mk_array_sort ctx (tr2sort (TypeReg_32)) (tr2sort (TypeReg_8))
      in 
      mk_var ctx p s) in
      Hashtbl.replace htbl p r; r)

let symbolic_execute instrs postcondition =
  let ctx = mk_context () in
  let tr2s = sort_of_typereg ctx in
  let z3var_of_variable = z3var_of_variable tr2s ctx in
  let typereg_of_intexpr e = IRTypeCheck.type_of_integer_type (IRTypeCheck.typecheck_expr e) in
  let bits_of_intexpr e = IRUtil.bits (typereg_of_intexpr e) in
  let z3num_of_int   i s = Z3.mk_numeral ctx (Printf.sprintf "%d"  i) (tr2s s) in
  let z3num_of_int64 i s = Z3.mk_numeral ctx (Printf.sprintf "%Ld" i) (tr2s s) in
  let z3boolify expr = Z3.mk_ite ctx expr (z3num_of_int 1 (TypeReg_1)) (z3num_of_int 0 (TypeReg_1)) in
  let rec z3ast_of_irexpr = function
  | Binop(l,o,r) ->
    let z3l = z3ast_of_irexpr l in
    let d f = f ctx z3l (z3ast_of_irexpr r) in
   (match o with
    | Add  -> d Z3.mk_bvadd
    | Sub  -> d Z3.mk_bvsub 
    | Mul  -> d Z3.mk_bvmul
    | SDiv -> d Z3.mk_bvsdiv
    | UDiv -> d Z3.mk_bvudiv
    | SMod -> d Z3.mk_bvsrem
    | UMod -> d Z3.mk_bvurem
    | Shl | Shr | Sar  -> 
      let open IRTypeCheck in 
      let z3r = z3ast_of_irexpr (IRUtil.mk_unsigned_cast (typereg_of_intexpr l) r) in
     (match o with
      | Shl -> Z3.mk_bvshl  ctx z3l z3r
      | Shr -> Z3.mk_bvlshr ctx z3l z3r
      | Sar -> Z3.mk_bvashr ctx z3l z3r
      | _   -> failwith "impossible")
    | And -> d Z3.mk_bvand
    | Or  -> d Z3.mk_bvor
    | Xor -> d Z3.mk_bvxor 
    | EQ  -> z3boolify (d Z3.mk_eq)
    | NE  -> Z3.mk_ite ctx (d Z3.mk_eq) (z3num_of_int 0 (TypeReg_1)) (z3num_of_int 1 (TypeReg_1))
    | ULT -> z3boolify (d Z3.mk_bvult)
    | ULE -> z3boolify (d Z3.mk_bvule)
    | SLT -> z3boolify (d Z3.mk_bvslt)
    | SLE -> z3boolify (d Z3.mk_bvsle))
  | Unop(Neg,e) -> Z3.mk_bvneg ctx (z3ast_of_irexpr e)
  | Unop(Not,e) -> Z3.mk_bvnot ctx (z3ast_of_irexpr e)
  | Cast(Unsigned,s,e) -> Z3.mk_zero_ext ctx (IRUtil.bits s - (bits_of_intexpr e)) (z3ast_of_irexpr e)
  | Cast(Signed,s,e)   -> Z3.mk_sign_ext ctx (IRUtil.bits s - (bits_of_intexpr e)) (z3ast_of_irexpr e)
  | Cast(High,s,e)     -> let h = IRUtil.bits s in Z3.mk_extract ctx (h-1) (h - bits_of_intexpr e) (z3ast_of_irexpr e)
  | Cast(Low,s,e)      -> let l = IRUtil.bits s in Z3.mk_extract ctx (l-1) 0 (z3ast_of_irexpr e)
  | Load(m,a,s)        -> if s = (TypeReg_1) then failwith "z3ast_of_irexpr: 1-bit load";
    let m  = z3ast_of_irexpr m in
    let a  = z3ast_of_irexpr a in
    let n1 = IRUtil.bits s in
    let n  = n1 / 8 in
    (* Read n bytes linearly (a[i] where 0 <= i < n) and store them into a list *)
    let rec aux l i =
      if i > (n-1)
      then List.rev l (* Remove List.rev for big-endian; TypeReg_32 index might break later assumptions *)
      else aux ((Z3.mk_select ctx m (Z3.mk_bvadd ctx (z3num_of_int i (TypeReg_32)) a))::l) (i+1)
    in 
    let l = aux [] 0 in
    (* Map these bytes into bit-vectors of size n1 (the size of the read) *)
    let l = List.map (fun z3e -> Z3.mk_zero_ext ctx (n1-8) z3e) l in
    (* Shift each byte (now byte/word/dword) left by a multiple of 8 and OR them together *)
    let res,_ = List.fold_left
     (fun (folded,sf) z3expr -> (Z3.mk_bvor ctx (Z3.mk_bvshl ctx z3expr (z3num_of_int sf (TypeReg_32))) folded,sf+8))
     (List.hd l,8)
     (List.tl l)
    in res
  | Store(m,a,t,s)     -> if s = (TypeReg_1) then failwith "z3ast_of_irexpr: 1-bit store";
    let m  = z3ast_of_irexpr m in
    let a  = z3ast_of_irexpr a in
    let t  = z3ast_of_irexpr t in
    let n1 = IRUtil.bits s in
    let n  = n1 / 8 in
    (* Extract n bytes (BV(8)s) from the bitvector specified in t *)
    let rec aux l i =
      if i > (n-1)
      then List.rev l (* Remove List.rev for big-endian; TypeReg_32 index might break later assumptions *)
      else aux ((Z3.mk_extract ctx 7 0 (Z3.mk_bvlshr ctx t (z3num_of_int (i*8) s)))::l) (i+1)
    in
    let l = aux [] 0 in
    (* Store byte #i to a[i], where 0 <= i < n. *)
    let res,_ = List.fold_left 
     (fun (folded,i) z3byte -> (Z3.mk_store ctx folded (Z3.mk_bvadd ctx (z3num_of_int i (TypeReg_32)) a) z3byte,i+1))
     (m,0)
      l
    in res
  | Var(v)             -> z3var_of_variable v
  | Const(i,s)         -> z3num_of_int64 i s
  | Let(v,eexpr,ein)   -> invalid_arg "z3ast_of_irexpr: Let"
  in
  let instr_to_z3 = function
  | Assign(v,e)    -> Z3.assert_cnstr ctx (Z3.mk_eq ctx (z3var_of_variable v) (z3ast_of_irexpr e))
  | Assert(e)      -> Z3.assert_cnstr ctx (z3ast_of_irexpr e)
  | Label(_)       -> ()
  | Comment(_)     -> ()
  | Special(_)     -> invalid_arg "instr_to_z3: Special"
  | Jmp(e)         -> invalid_arg "instr_to_z3: Jmp"
  | CJmp(eb,et,en) -> invalid_arg "instr_to_z3: CJmp"
  | Halt(e)        -> invalid_arg "instr_to_z3: Halt"
  in
  let ssa_instrs,ssactxt = IRSSA.bb_to_ssa 38 instrs in
  List.iter instr_to_z3 ssa_instrs;
  let postcondition = IRLocalOpt.replace_var_with_var ssactxt postcondition in
  Z3.assert_cnstr ctx (Z3.mk_eq ctx (z3num_of_int 1 (TypeReg_1)) (z3ast_of_irexpr postcondition));
  let (result,m) = Z3.check_and_get_model ctx in
  let rsyns = match result with
  | Z3.L_FALSE -> "unsat"
  | Z3.L_UNDEF -> "unknown"
  | Z3.L_TRUE  -> "sat"
  in
  let rmod = (Z3.model_to_string ctx m) in
  Z3.del_model ctx m;
  Z3.del_context ctx;
  (rsyns^"\n"^rmod)
  
let mk_dword ctx i = Z3.mk_numeral ctx (Printf.sprintf "%ld" i) (Z3.mk_bv_sort ctx 32)

let store_byte_at  mem (addr:int32) (b:int32) = (fun a -> if a = addr then b else mem a)
(* Assert that address `addr` in array `memvar` is equal to 8-bit (int32) value `b` in context `ctx` *)
let assert_byte_at ctx memvar addr b =
  Z3.assert_cnstr 
    ctx 
   (Z3.mk_eq 
     ctx 
     (Z3.mk_select ctx memvar (mk_dword ctx addr))
     (Z3.mk_numeral ctx (Printf.sprintf "%ld" b) (Z3.mk_bv_sort ctx 8)))
  
let assert_and_store_byte_at mem ctx memvar addr b = 
  assert_byte_at ctx memvar addr b;
  store_byte_at mem addr b

let gb d fac = Int32.logand (Int32.shift_right d fac) 0xffl
let gw d fac = Int32.logand (Int32.shift_right d fac) 0xffffl
let aa a i = Int32.add a i
let sbm mem a i s d   = store_byte_at mem (aa a i) (gb d s)
let abm ctx m a i s d = assert_byte_at ctx m (aa a i) (gb d s)

let store_word_at mem addr w =
  let mem0 = sbm mem addr 0l 00 w in
  let mem1 = sbm mem0 addr 1l 08 w in
  mem1

let assert_word_at ctx memvar addr w = 
  abm ctx memvar addr 0l 00 w;
  abm ctx memvar addr 1l 08 w

let assert_and_store_word_at mem ctx memvar addr w = 
  assert_word_at ctx memvar addr w;
  store_word_at mem addr w

let store_dword_at mem addr d = 
  let mem0 = store_word_at mem addr d in
  let mem1 = store_word_at mem0 (aa addr 2l) (gw d 16) in
  mem1

let assert_dword_at ctx memvar addr d =
  assert_word_at ctx memvar addr d;
  assert_word_at ctx memvar (aa addr 2l) (gw d 16)

let assert_and_store_dword_at mem ctx memvar addr d = 
  assert_dword_at ctx memvar addr d;
  store_dword_at mem addr d

let store_string_at mem addr str =
  let l = String.length str in
  let rec aux mem i =
    if i < l 
    then 
      aux (sbm mem addr (Int32.of_int i) 0 (Int32.of_int (Char.code str.[i]))) (i+1)
    else mem
  in 
  sbm (aux mem 0) addr (Int32.of_int l) 0 0l

let assert_string_at ctx memvar addr str =
  let l = String.length str in
  let rec aux i =
    if i < l 
    then 
     (abm ctx memvar addr (Int32.of_int i) 0 (Int32.of_int (Char.code str.[i]));
      aux (i+1))
    else ()
  in 
  aux 0;
  abm ctx memvar addr (Int32.of_int l) 0 0l
  
let assert_and_store_string_at mem ctx memvar addr str =
  assert_string_at ctx memvar addr str;
  store_string_at mem addr str

