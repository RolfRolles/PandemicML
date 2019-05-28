exception UnboundVariable of string

let expr e regctx f_read f_write = 
  let open IR in
  let open IRUtil in
  let open NEWIREvalUtil in
  let cc = function | Const(c,s) -> c | _ -> failwith "expr::cc: typechecking prevents this" in
  let cs = function | Const(c,s) -> s | _ -> failwith "expr::cs: typechecking prevents this" in
  
  let rec expr = function
  | Var(v)             -> (try Hashtbl.find regctx v with Not_found -> raise (UnboundVariable(PpIR.ppVar v)))
  | Const(_,_) as c    -> c

  | Binop(l,Add,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.add (cc l) (cc r)),(cs l))
  | Binop(l,Mul,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.mul (cc l) (cc r)),(cs l))
  | Binop(l,And,r)     -> let l,r = expr l,expr r in Const(Int64.logand (cc l) (cc r),(cs l))
  | Binop(l,Xor,r)     -> let l,r = expr l,expr r in Const(Int64.logxor (cc l) (cc r),(cs l))
  | Binop(l, Or,r)     -> let l,r = expr l,expr r in Const(Int64.logor  (cc l) (cc r),(cs l))

  | Binop(l,Sub,r)     -> let l,r = expr l,expr r in Const(truncate_to (cs l) (Int64.sub (cc l) (cc r)),(cs l))
  | Binop(l,UDiv,r)    -> failwith "expr: UDIV unimplemented"
  | Binop(l,SDiv,r)    -> failwith "expr: SDIV unimplemented"
  | Binop(l,UMod,r)    -> failwith "expr: UMOD unimplemented"
  | Binop(l,SMod,r)    -> failwith "expr: SMOD unimplemented"

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
    let v = f_read a s in
    IRUtil.mk_fixed_const (Int64.of_int32 v) s

  | Store(m,a,t,s)     -> 
    let a,t = Int64.to_int32 (cc (expr a)), Int64.to_int32 (cc (expr t)) in
    f_write a t s;
    m
  | Let(_,_,_)         -> failwith "expr: LET unimplemented"
  in expr e
  
let stmt s regctx f_read f_write = let open IR in match s with
| Assign(Mem(_,_,_),Var(Mem(_,_,_))) -> ()
| Assign(Mem(_,_,_),e) -> let _ = expr e regctx f_read f_write in ()
| Assign(v,e) -> Hashtbl.replace regctx v (expr e regctx f_read f_write)
| Label(_) -> ()
| Comment(_) -> ()
| Assert(_) -> ()
| Jmp(e) -> failwith "stmt: JMP encountered"
| CJmp(e,t,f) -> failwith "stmt: CJMP encountered"
| Halt(_) -> failwith "stmt: HALT never used"
| Special(_) -> failwith "stmt: SPECIAL never used"

(* Printf.printf "%s\n%!" (PpIR.ppInstr s); *)
let stmts ir regctx f_read f_write = List.iter (fun s -> stmt s regctx f_read f_write) ir

let eval ht e = expr e ht (fun _ _ -> failwith "Memory read in eval") (fun _ _ _ -> failwith "Memory write in eval")
let eval_to_int32 ht e = match (eval ht e) with | IR.Const(i,_) -> Int64.to_int32 i | _ -> failwith "eval_to_int32"
