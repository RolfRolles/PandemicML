open IR
module Z3 = Z3.V3

(* Create a default context. *)
let mk_context () = Z3.mk_context_x [|("MODEL", "true");("SOFT_TIMEOUT", "10000")|]

(* Given a context, make a variable with the given name and type. *)
let mk_var ctx name ty = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) ty

(* Given a context and an IR.TypeReg term, return a bitvector sort.  We 
   partially apply the context against this function to yield a function
   of type IR.TypeReg -> Z3.ast. *)
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
  
(* Given a context, the function returned from sort_of_typereg, and an IR
   variable, return a new variable of compatible type. *)
let get_z3var ctx tr2sort v = 
  match v with
  | Variable(_,s) -> tr2sort s
  | Mem(_,_,_)    -> Z3.mk_array_sort ctx (tr2sort (TypeReg_32)) (tr2sort (TypeReg_8))

(* Given a context and the function returned from sort_of_typereg, create a 
   *function* that maps each IR variable into an actual Z3 variable.  
   Internally, the function caches varibles in a hash table. *)
let z3var_of_variable tr2sort ctx = 
  let htbl = Hashtbl.create 50 in
 (fun v ->
    let p = PpIR.ppVar v in
    let r = 
      try Hashtbl.find htbl p 
      with Not_found -> 
       (let r = mk_var ctx p (get_z3var ctx tr2sort v) in 
        Hashtbl.replace htbl p r; r) 
    in r)

(* Given a context and an IR variable, create a Z3 variable *)
let mk_z3var ctx var = 
  let tr2s = sort_of_typereg ctx in 
  z3var_of_variable tr2s ctx var

(* Given a context and an IR expression, create a Z3 AST expression *)
let z3ast_of_irexpr ctx expr =

  (* Convenience definitions *)
  let tr2s = sort_of_typereg ctx in
  let z3var_of_variable = z3var_of_variable tr2s ctx in
  
  (* Size of a type as enumeration *)
  let typereg_of_intexpr e = IRTypeCheck.type_of_integer_type (IRTypeCheck.typecheck_expr e) in
  
  (* Size of a type in bits *)
  let bits_of_intexpr e = IRUtil.bits (typereg_of_intexpr e) in

  (* Z3 numerals from integers and int64s *)
  let z3num_of_int   i s = Z3.mk_numeral ctx (Printf.sprintf "%d"  i) (tr2s s) in
  let z3num_of_int64 i s = Z3.mk_numeral ctx (Printf.sprintf "%Ld" i) (tr2s s) in
  
  (* Z3 uses an internal representation of booleans that is not synonymous with
     the numbers 1 and 0.  Therefore we use this function to build an expression
     that converts Z3's booleans to numbers. *)
  let z3boolify expr = Z3.mk_ite ctx expr (z3num_of_int 1 (TypeReg_1)) (z3num_of_int 0 (TypeReg_1)) in
  
  (* The main function:  given an expression, create a Z3 AST term from it *)
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
  (*let _ = IDA.msg "1\n" in*)
    let a  = z3ast_of_irexpr a in
  (*let _ = IDA.msg "2\n" in*)
    let n1 = IRUtil.bits s in
  (*let _ = IDA.msg "3\n" in*)
    let n  = n1 / 8 in
    (* Read n bytes linearly (a[i] where 0 <= i < n) and store them into a list *)
    let rec aux l i =
      if i = n
      then List.rev l (* Remove List.rev for big-endian; TypeReg_32 index might break later assumptions *)
      else 
        let z3address = Z3.mk_bvadd ctx (z3num_of_int i (TypeReg_32)) a in 
        let select = Z3.mk_select ctx m z3address in
        aux (select::l) (i+1)
    in 
    let l = aux [] 0 in
  (*let _ = IDA.msg "4\n" in*)
    (* Map these bytes into bit-vectors of size n1 (the size of the read) *)
  (*let l = List.map (fun z3e -> Z3.mk_zero_ext ctx (n1-8) z3e) l in*)
  (*let _ = IDA.msg "5\n" in*)

    (* Shift each byte (now byte/word/dword) left by a multiple of 8 and OR them together *)
    let res = List.fold_left
     (fun folded z3expr -> 
        Z3.mk_concat ctx z3expr folded)
(*
     (Z3.mk_bvor ctx (Z3.mk_bvshl ctx z3expr (z3num_of_int sf s)) folded,sf+8))
 *)   
     (List.hd l)
     (List.tl l)
    in 
  (*let _ = IDA.msg "6\n" in*)
  (*let _ = IDA.msg "Load: %s\n" (Z3.ast_to_string ctx res) in*)
    res
  | Store(m,a,t,s)     -> if s = (TypeReg_1) then failwith "z3ast_of_irexpr: 1-bit store";
  (*let _ = IDA.msg "1\n" in*)
    let m  = z3ast_of_irexpr m in
  (*let _ = IDA.msg "2\n" in*)
    let a  = z3ast_of_irexpr a in
  (*let _ = IDA.msg "3\n" in*)
    let t  = z3ast_of_irexpr t in
  (*let _ = IDA.msg "4\n" in*)
    let n1 = IRUtil.bits s in
    let n  = n1 / 8 in
    (* Extract n bytes (BV(8)s) from the bitvector specified in t *)
    let rec aux l i =
      if i = n
      then List.rev l (* Remove List.rev for big-endian; TypeReg_32 index might break later assumptions *)
    (*else aux ((Z3.mk_extract ctx 7 0 (Z3.mk_bvlshr ctx t (z3num_of_int (i*8) s)))::l) (i+1)*)
      else 
      (*let _ = IDA.msg "5: %d\n" i in*)
        let extract = Z3.mk_extract ctx (i*8+7) (i*8) t in
      (*let _ = IDA.msg "5: %d: %s" i (Z3.ast_to_string ctx extract) in*)
        aux (extract::l) (i+1)
    in
    let l = aux [] 0 in
    (* Store byte #i to a[i], where 0 <= i < n. *)
  (*let _ = IDA.msg "6\n" in*)
    let res,_ = List.fold_left 
     (fun (folded_array,i) z3byte -> 
     (*let _ = IDA.msg "7: %d\n" i in*)
       let z3address = Z3.mk_bvadd ctx (z3num_of_int i (TypeReg_32)) a in
       let new_array = Z3.mk_store ctx folded_array z3address z3byte in
     (*IDA.msg "Iteration %d: %s\n" i (Z3.ast_to_string ctx new_array);*)
      (new_array,i+1))
     (m,0)
      l
    in res
  | Var(v)             -> z3var_of_variable v
  | Const(i,s)         -> z3num_of_int64 i s
  | Let(v,eexpr,ein)   -> invalid_arg "z3ast_of_irexpr: Let"
  in
  z3ast_of_irexpr expr

let instr_to_z3 ctx instr = 
  let tr2s = sort_of_typereg ctx in
  let z3var_of_variable = z3var_of_variable tr2s ctx in
(*IDA.msg "Processing %s\n" (PpIR.ppInstr instr);*)
  match instr with  
  | Assign(v,e)    -> Z3.assert_cnstr ctx (Z3.mk_eq ctx (z3var_of_variable v) (z3ast_of_irexpr ctx e))
  | Assert(e)      -> Z3.assert_cnstr ctx (z3ast_of_irexpr ctx e)
  | Label(_)       -> ()
  | Comment(_)     -> ()
  | Special(_)     -> invalid_arg "instr_to_z3: Special"
  | Jmp(e)         -> invalid_arg "instr_to_z3: Jmp"
  | CJmp(eb,et,en) -> invalid_arg "instr_to_z3: CJmp"
  | Halt(e)        -> invalid_arg "instr_to_z3: Halt"

let symbolic_execute ctx instrs postcondition =
(*let ssactxt,ssa_instrs = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars instrs in*)
  let ssa_instrs = instrs in
  let tr2s = sort_of_typereg ctx in
  let z3num_of_int   i s = Z3.mk_numeral ctx (Printf.sprintf "%d"  i) (tr2s s) in
  List.iter (instr_to_z3 ctx) ssa_instrs;
(*let postcondition = IRLocalOpt.replace_var_with_var ssactxt postcondition in*)
  Z3.assert_cnstr ctx (Z3.mk_eq ctx (z3num_of_int 1 (TypeReg_1)) (z3ast_of_irexpr ctx postcondition));
  let (result,m) = Z3.check_and_get_model ctx in
  let string_of_search_failure = function 
  |	Z3.NO_FAILURE       -> "NO_FAILURE"
  |	Z3.UNKNOWN          -> "UNKNOWN"
  |	Z3.TIMEOUT          -> "TIMEOUT"
  |	Z3.MEMOUT_WATERMARK -> "MEMOUT_WATERMARK"
  |	Z3.CANCELED         -> "CANCELED"
  |	Z3.NUM_CONFLICTS    -> "NUM_CONFLICTS"
  |	Z3.THEORY           -> "THEORY"
  |	Z3.QUANTIFIERS      -> "QUANTIFIERS"
  in
  let rsyns = match result with
  | Z3.L_FALSE -> "unsat"
  | Z3.L_UNDEF -> "unknown\n"^(string_of_search_failure (Z3.get_search_failure ctx))
  | Z3.L_TRUE  -> "sat\n"^(let rmod = Z3.model_to_string ctx m in Z3.del_model ctx m; rmod)
  in
(*IDA.msg "%s\n" rsyns;*)
(*Z3.del_context ctx;*)
  rsyns^"\n"
  
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

