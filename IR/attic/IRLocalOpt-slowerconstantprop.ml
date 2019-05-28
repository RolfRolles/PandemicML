let rec fold_expr_constants e = 
  let fold1 ?(changed=false) e1 = 
    let everchanged = ref changed in
    let rec aux e1 = 
      let ef,b = fold_expr_constants e1 in
      if b then (everchanged := true; aux ef) else e1
    in
    let x = aux e1 in (x,!everchanged)
  in
  let refold1 ?(changed=false) expr fn =
    let x',b = fold1 expr in
    let x''  = fn x' in
    if (b || changed) then fold1 ~changed:true x'' else (x'',changed)
  in
  let refold2 ?(changed=false) e1 e2 fn =
    let e1',b1 = fold1 e1 in
    let e2',b2 = fold1 e2 in
    let x = fn e1' e2' in
    if (b1 || b2 || changed) then fold1 ~changed:true x else (x,changed)
  in
  let refold3 ?(changed=false) e1 e2 e3 fn =
    let e1',b1 = fold1 e1 in
    let e2',b2 = fold1 e2 in
    let e3',b3 = fold1 e3 in
    let x = fn e1' e2' e3' in
    if (b1 || b2 || b3 || changed) then fold1 ~changed:true x else (x,changed) (* ' *)
  in
  let bool2e = function
  | true  -> Const(0x1L,TypeReg_1)
  | false -> Const(0x0L,TypeReg_1)
  in
  let aux expr = match expr with
  | Binop(_,And,Const(0L,s))              -> (Const(0L,s),true)
  | Binop(x,And,Const(-1L,s))             -> fold1 ~changed:true x
  | Binop(Const(0L,s),Xor,x)              -> fold1 ~changed:true x
  | Binop(x,Sub,Const(0L,s))              -> fold1 ~changed:true x
  | Binop(Const(0L,s),And,_)              -> (Const(0L,s),true)
  | Binop(Const(0L,s),Add,x)              -> fold1 ~changed:true x
  | Binop(Const(0L,s),Sub,x)              -> refold1 ~changed:true x (fun x -> Unop(Neg,x))
  | Binop(Const(0L,s),Or ,x)              -> fold1 ~changed:true x
  | Binop(Const(i,s),Add,Unop(Not,x))     -> refold1 ~changed:true x (fun x -> Binop(Const(truncate_to s (Int64.sub i 1L),s),Add,Unop(Neg,x)))
  | Unop(Neg,Unop(Neg,x))                 -> fold1 ~changed:true x
  | Binop(Const(c1,s1),Add,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.add c1 c2),s1),true)
  | Binop(Const(c1,s1),Sub,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.sub c1 c2),s1),true)
  | Binop(Const(c1,s1),And,Const(c2,s2))  -> (Const(Int64.logand c1 c2,s1),true)
  | Binop(Const(c1,s1),Xor,Const(c2,s2))  -> (Const(Int64.logxor c1 c2,s1),true)
  | Binop(Const(c1,s1), Or,Const(c2,s2))  -> (Const(Int64.logor  c1 c2,s1),true)
  | Binop(Const(c1,s1), EQ,Const(c2,s2))  -> (bool2e (c1 =  c2),true)
  | Binop(Const(c1,s1), NE,Const(c2,s2))  -> (bool2e (c1 <> c2),true)
  | Binop(Const(c1,s1),ULT,Const(c2,s2))  -> (ucomp c1 c2 (ULT),true)
  | Binop(Const(c1,s1),ULE,Const(c2,s2))  -> (ucomp c1 c2 (ULE),true)
  | Binop(Const(c1,s1),SLT,Const(c2,s2))  -> (scomp c1 c2 (SLT) s1,true)
  | Binop(Const(c1,s1),SLE,Const(c2,s2))  -> (scomp c1 c2 (SLE) s1,true)
  | Binop(Const(c1,s1),Mul,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.mul c1 c2),s1),true)
  | Unop(Not,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.logxor (-1L) c1),s1),true)
  | Unop(Neg,Const(c1,s1))                -> (Const(truncate_to s1 (Int64.add 1L (Int64.logxor (-1L) c1)),s1),true)
  | Cast(_,s,e) when IRTypeCheck.type_of_integer_type (IRTypeCheck.typecheck_expr e) = s -> fold1 ~changed:true e
  | Cast(Low,s,Const(c1,s1))              -> (Const(truncate_to s c1,s),true)
  | Cast(Unsigned,s,Const(c1,s1))         -> (Const(c1,s),true)
  | Cast(Signed,s,Const(c1,s1))           -> (do_signed_cast c1 s1 s,true)
  | Cast(High,s,Const(c1,s1))             -> (do_high_cast c1 s1 s,true)
  | Binop(Const(c1,s1),Shl,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_left c1 (Int64.to_int c2)),s1),true)
  | Binop(Const(c1,s1),Shr,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right_logical c1 (Int64.to_int c2)),s1),true)
  | Binop(Const(c1,s1),Sar,Const(c2,s2))  -> (Const(truncate_to s1 (Int64.shift_right (sign_extend c1 s1) (Int64.to_int c2)),s1),true)
  | Binop(x,Add,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Add,x))
  | Binop(x,Or ,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Or, x))
  | Binop(x,Add,Binop(y,Add,z))           -> refold3 ~changed:true x y z (fun x y z -> Binop(Binop(x,Add,y),Add,z))
  | Binop(x,Or ,Binop(y,Or ,z))           -> refold3 ~changed:true x y z (fun x y z -> Binop(Binop(x,Or ,y),Or ,z))
  | Binop(Const(c1,s1),Xor,Binop(Const(c2,s2),Xor,z)) -> refold1 ~changed:true z (fun z -> Binop(Const(Int64.logxor c1 c2,s1),Xor,z))
  | Binop(x,Xor,Const(c,s))               -> refold1 ~changed:true x (fun x -> Binop(Const(c,s),Xor,x))
  (* These three cause an infinite loop, must resolve *)
  (*
  | Binop(x,Sub,Const(c1,s1))             -> Binop(Const(truncate_to s1 (Int64.logxor (-1L) c1),s1),Add,x)
  *)
  (* Have to AND the shiftand *)
  | Binop(x,o,y)                          -> refold2 x y (fun x y -> Binop(x, o, y))
  | Unop(o,x)                             -> refold1 x (fun x -> Unop(o, x))
  | Load(e1,e2,tr)                        -> refold2 e1 e2 (fun e1 e2 -> Load(e1, e2, tr))
  | Cast(a,b,e)                           -> refold1 e (fun e -> Cast(a, b, e))
  | Store(e1,e2,e3,r)                     -> refold3 e1 e2 e3 (fun e1 e2 e3 -> Store(e1, e2, e3, r))
  | Var(v) as e                           -> (e,false)
  | Const(_) as c                         -> (c,false)
  | Let(v,e1,e2)                          -> refold2 e1 e2 (fun e1 e2 -> Let(v, e1, e2))
  in
  aux e
  
let fold_expr_constants e = let e,_ = fold_expr_constants e in e

