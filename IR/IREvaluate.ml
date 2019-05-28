let concrete_evaluate ir regctx memctx = 
  let open IR in
  let open IRUtil in
  (* All this shit is duplicated from IRLocalOpt *)
  let bool2e = function | true  -> Const(0x1L,TypeReg_1) | false -> Const(0x0L,TypeReg_1) in
  let i64_of_bool = function | true  -> 1L | false -> 0L in
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
  let cc = function | Const(c,s) -> c | _ -> failwith "concrete_evaluate::cc: typechecking prevents this" in
  let cs = function | Const(c,s) -> s | _ -> failwith "concrete_evaluate::cs: typechecking prevents this" in
  
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
        let c = Int32.logor (Int32.shift_left addr (i*8)) c in
        aux c (uninit or uni) (i+1)
      else (Const(Int64.of_int32 c,s),uni)
    in
    let c,uni = aux 0l false 0 in
   (if uni
    then uninit_reads := (a32,c,s)::(!uninit_reads));
    c
  in
  let write_mem a32 v32 s =
    let n = (IRUtil.bits s)/8 in
    let rec aux v32 i =
      if i = n
      then ()
      else
        let addr = Int32.add a32 (Int32.of_int i) in
        Hashtbl.replace memctx addr (Int32.logand v32 0xffl);
        aux (Int32.shift_right_logical v32 8) (i+1)
    in aux v32 0
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
  | Binop(l,UDiv,r)    -> failwith "concrete_evaluate::expr: UDIV unimplemented"
  | Binop(l,SDiv,r)    -> failwith "concrete_evaluate::expr: SDIV unimplemented"
  | Binop(l,UMod,r)    -> failwith "concrete_evaluate::expr: UMOD unimplemented"
  | Binop(l,SMod,r)    -> failwith "concrete_evaluate::expr: SMOD unimplemented"

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
    let a = expr a in
    let v = read_mem (Int64.to_int32 (cc a)) s in
    reads := (a,v,s)::(!reads);
    v

  | Store(m,a,t,s)     -> 
    let a,t = expr a,expr t in
    write_mem (Int64.to_int32 (cc a)) (Int64.to_int32 (cc t)) s;
    writes := (a,t,s)::(!writes);
    m
  | Let(_,_,_)         -> failwith "concrete_evaluate::expr: LET unimplemented"
  in
  
  let stmt = function
  | Assign(Mem(_,_,_),e) -> let _ = expr e in ()
  | Assign(v,e) -> Hashtbl.replace regctx v (expr e)
  | Label(_) -> ()
  | Comment(_) -> ()
  | Assert(_) -> ()
  | Jmp(e) -> failwith "concrete_evaluate::stmt: JMP encountered"
  | CJmp(e,t,f) -> failwith "concrete_evaluate::stmt: CJMP encountered"
  | Halt(_) -> failwith "concrete_evaluate::stmt: HALT never used"
  | Special(_) -> failwith "concrete_evaluate::stmt: SPECIAL never used"
  in
  List.iter stmt ir;
  ((!reads,!uninit_reads,uninit),!writes)
  
