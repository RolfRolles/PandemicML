open BV

exception BBUnimplemented of string

let bit_blast bvexpr = 
  let open BitVector in

  let conjuncts = ref [] in
  let conjoin sf = conjuncts := (sf::(!conjuncts)) in

  let make_implication size ibv = 
    let obv = create_fresh_anon size in
    iteri (fun b i -> conjoin (PL.Iff(bit obv i,b))) ibv; 
    obv
  in
  let bitwise_binop_anon lbv rbv f = make_implication (size_of_bv lbv) (map2_anon f lbv rbv)in
  let bitwise_unop_anon  ebv f = make_implication (size_of_bv ebv) (map_anon f ebv) in
  let bitwise_reduced_op ebv i f = 
    let obv = create_fresh_anon 1 in 
    conjoin (PL.Iff(bit obv 0, fold_left f i ebv));
    obv
  in
  
  let do_constant_shift lbv i bleft barith =
    let s = size_of_bv lbv in
    let shift_in = if (not barith) then (Existing(tzero)) else (Extract(lbv,s-1,s-1)) in
    if i = 0 then lbv 
    else create_custom_anon 
     (if i >= s then [Repeat(shift_in,s)]
      else 
       (if bleft
        then [Repeat(Existing(tzero),i);Extract(lbv,s-(i+1),0)]
        else [Extract(lbv,s-1,i);Repeat(shift_in,i)]))
  in
  let do_constant_rotate lbv i bleft =
    let s = size_of_bv lbv in
    let i = if bleft then i else s-i in
    let i = i mod s in
    if i = 0 
    then lbv
    else create_custom_anon [Extract(lbv,s-1,s-(i+1));Extract(lbv,s-i,0)]
  in
  
  let is_power_of_2 i = i land (i-1) == 0 in
  let log2_power_of_2 x = 
    let rec aux i mask =
      if (x land mask) <> 0
      then i
      else aux (i+1) (mask+mask)
    in aux 1 1
  in

  
  let do_barrel_shift lbv rbv = 
    let ls,rs = size_of_bv lbv,size_of_bv rbv in
    if not (is_power_of_2 ls) then raise (BBUnimplemented("Shifts/rotates must have LHS as a power of two"));
    if true then raise (BBUnimplemented("Code barrel shift/rotate")) else lbv
  in    
  
  let f_and l r = PL.And(l,r) in
  let f_or  l r = PL.Or (l,r) in
  let f_xor l r = PL.And(PL.Or(l,r),PL.Or(PL.Not(l),PL.Not(r))) in
  let f_not l   = PL.Not(l) in
  
  
  let rec aux = function
  | Constant(i64,i)   -> create_fresh_from_i64 i64 i
  | Variable(bv)      -> bv
  | BV.Extract(e,h,l) -> create_custom_anon [Extract(aux e,h,l)]
  | BV.Concat (l,r)   -> create_custom_anon [Existing(aux r);Existing(aux l)]
  | BV.Repeat (e,i)   -> create_custom_anon [Repeat(Existing(aux e),i)]
  | ZeroExt(e,i)      -> create_custom_anon [Existing(aux e);Repeat(Existing(tzero),i)]
  | SignExt(e,i)      -> let ebv = aux e in let el = (size_of_bv ebv)-1 in
    create_custom_anon [BitVector.Existing(ebv);BitVector.Repeat(BitVector.Extract(ebv,el,el),i)]
  | And(l,r)          -> bitwise_binop_anon (aux l) (aux r) f_and
  | Or (l,r)          -> bitwise_binop_anon (aux l) (aux r) f_or
  | Xor(l,r)          -> bitwise_binop_anon (aux l) (aux r) f_xor
  | Not(e)            -> bitwise_unop_anon  (aux e)         f_not
  | RedAnd(e)         -> bitwise_reduced_op (aux e) pone    f_and
  | RedOr (e)         -> bitwise_reduced_op (aux e) pzero   f_or
  | RedXor(e)         -> bitwise_reduced_op (aux e) pzero   f_xor
  | ShlConst(l,i)     -> do_constant_shift  (aux l) i true  false
  | ShrConst(l,i)     -> do_constant_shift  (aux l) i false false
  | SarConst(l,i)     -> do_constant_shift  (aux l) i false true
  | RolConst(l,i)     -> do_constant_rotate (aux l) i true
  | RorConst(l,i)     -> do_constant_rotate (aux l) i false
  
  (* Shifts and rotates by a bitvector *)
  | ShlBv(l,r)        -> do_barrel_shift (aux l) (aux r)
  | ShrBv(l,r)        -> do_barrel_shift (aux l) (aux r)
  | SarBv(l,r)        -> do_barrel_shift (aux l) (aux r)
  | RolBv(l,r)        -> do_barrel_shift (aux l) (aux r)
  | RorBv(l,r)        -> do_barrel_shift (aux l) (aux r)
  
  (* Arithmetic operations *)
  | Add(l,r)          -> raise (BBUnimplemented("Add"))
  | Sub(l,r)          -> raise (BBUnimplemented("Sub"))
  | Neg(e)            -> raise (BBUnimplemented("Neg"))
  (* XXX MUL, UDIV, SDIV, UREM, SREM, SMOD *)
  
  (* Relational operations *)
  | EQ (l,r)          -> bitwise_reduced_op (map2_anon (fun l r -> PL.Iff(l,r)) (aux l) (aux r)) pone f_and  
  | NE (l,r)          -> raise (BBUnimplemented("NE"))
  | SLT(l,r)          -> raise (BBUnimplemented("SLT"))
  | SLE(l,r)          -> raise (BBUnimplemented("SLE"))
  | ULT(l,r)          -> raise (BBUnimplemented("ULT"))
  | ULE(l,r)          -> raise (BBUnimplemented("ULE"))
  
  in 
  let _ = aux bvexpr in
  List.fold_left (fun a t -> PL.And(a,t)) (PL.Constant(PL.True)) (!conjuncts)
  

let bitblast_tests = [
  EQ(Xor(Constant(0x1L,1), Constant(0x0L,1)),Constant(0x1L,1));
(*EQ(And(Constant(0xCL,4), Constant(0xAL,4)),Constant(0x8L,4));
  EQ( Or(Constant(0xCL,4), Constant(0xAL,4)),Constant(0xEL,4));
  EQ(Xor(Constant(0xCL,4), Constant(0xAL,4)),Constant(0x6L,4));
  EQ(Not(Constant(0x2L,2)),Constant(0x1L,2));*)
  ]

let _ =
  List.iter 
   (fun e -> 
      let f = bit_blast e in
    (*let f,_ = PLTVLAI.bitwise_ai_prop f (Hashtbl.create 5) in*)
      Printf.printf "%s\n" (PL.string_of_prop f))
    bitblast_tests