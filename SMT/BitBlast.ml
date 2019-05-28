open BV

exception BBUnimplemented of string

let pltrue  = PL.pltrue
let plfalse = PL.plfalse

let f_and l r = PL.And(l,r)
let f_or  l r = PL.Or (l,r)
let f_xor l r = let open PL in And(Or(l,r),Or(Not(l),Not(r)))
let f_not l   = PL.Not(l)
let f_ite t1 t2 t3 = let open PL in And(Implies(t1,t2),Implies(Not(t1),t3))

let do_constant_shift lbv i bleft barith =
  let open BitVector in
  let s = size_of_bv lbv in
  let shift_in = if (not barith) then (Existing(tzero)) else (Extract(lbv,s-1,s-1)) in
  if i = 0 then lbv 
  else create_custom_anon 
   (if i >= s then [Repeat(shift_in,s)]
    else 
     (if bleft
      then [Repeat(Existing(tzero),i);Extract(lbv,s-(i+1),0)]
      else [Extract(lbv,s-1,i);Repeat(shift_in,i)]))

let do_constant_rotate lbv i bleft =
  let open BitVector in
  let s = size_of_bv lbv in
  let i = if bleft then i else s-i in
  let i = i mod s in
  if i = 0 
  then lbv
  else create_custom_anon [Extract(lbv,s-1,s-i);Extract(lbv,s-(i+1),0)]
  
let is_power_of_2 i = i land (i-1) == 0
let log2_power_of_2 x = 
  let rec aux i mask =
    if (x land mask) <> 0
    then i
    else aux (i+1) (mask+mask)
  in aux 1 1

let bit_blast bvexpr = 
  let open BitVector in

  (* We produce a set of formulae that are ANDed together; these are those *)
  let conjuncts = ref [] in
  let conjoin sf = conjuncts := (sf::(!conjuncts)) in

  (* Declares that a new bitvector is equal bit-wise to ibv *)
  let make_implication ibv = 
    let obv = create_fresh_anon (size_of_bv ibv) in
    iteri (fun b i -> match b with 
      | PL.Constant(_) -> set_bit obv i b
      | _ -> conjoin (PL.syntactic_simplify (PL.Iff(bit obv i,b))))
      ibv; 
    obv
  in
  let bitwise_binop_anon lbv rbv f = 
    make_implication (map2_anon (fun x y -> PL.syntactic_simplify (f x y)) lbv rbv)
  in
  let bitwise_unop_anon  ebv f = 
    make_implication (map_anon (fun x -> PL.syntactic_simplify (f x)) ebv) 
  in
  let bitwise_reduced_op ebv i f = 
    let obv = create_fresh_anon 1 in 
   (match PL.syntactic_simplify (fold_left f i ebv) with
    | PL.Constant(_) as b -> set_bit obv 0 b
    | b -> conjoin (PL.Iff(bit obv 0,b)));
    obv
  in
  let do_eq l r = bitwise_reduced_op (map2_anon (fun l r -> PL.Iff(l,r)) l r) pone f_and in
  let do_ne l r = bitwise_reduced_op (map2_anon (fun l r -> PL.Not(PL.Iff(l,r))) l r) pzero f_or in
  
  (* If shift count is greater than number of bits:
     * Shl,Shr,Sar:  
     * * Shl,Shr:  result is 0
     * * Sar:  result is ls copies of l[ls-1]
     * Ror,Rol:  can be safely ignored
  *)
  let shift_core f_shift_value f_badidx_opt lbv rbv =
    let ls,rs = size_of_bv lbv,size_of_bv rbv in
    if not (is_power_of_2 ls) then raise (BBUnimplemented("Shifts/rotates must have LHS as a power of two"));
    let rec round l k in_bv = 
      if k = 0
      then in_bv
      else 
        let mapped = mapi_anon (fun i b -> PL.syntactic_simplify (f_ite (bit rbv l) (f_shift_value ls i k in_bv) b)) in_bv in
        let res = make_implication mapped in
        round (l-1) (k lsr 1) res
    in
    let final = round (log2_power_of_2 ls-2) (ls lsr 1) (lbv) in
    match f_badidx_opt with
    | None -> final
    | Some(f) -> 
      let highbits = create_custom_anon [Extract(rbv,rs-1,log2_power_of_2 ls)] in
      let redand = bitwise_reduced_op highbits pone f_and in
      let defvec = f final in
      make_implication (map2_anon (fun x y -> PL.syntactic_simplify (f_ite (bit redand 0) x y)) defvec final)
  in
  let do_shr = shift_core 
    (fun ls i k in_bv -> if i < (ls-k) then bit in_bv (i+k) else plfalse )
    (Some(fun in_bv -> map_anon (fun _ -> plfalse) in_bv))
  in
  let do_sar lbv rbv = let ls = size_of_bv lbv in shift_core 
    (fun ls i k in_bv -> if i < (ls-k) then bit in_bv (i+k) else bit lbv (ls-1))
    (Some(fun in_bv -> map_anon (fun _ -> bit in_bv (ls-1)) in_bv))
    lbv
    rbv
  in
  let do_shl = shift_core
    (fun _ i k in_bv -> if i  > k then bit in_bv (i-k) else plfalse)
    (Some(fun in_bv -> map_anon (fun _ -> plfalse) in_bv))
  in
  let do_ror = shift_core 
    (fun ls i k in_bv -> if i < (ls-k) then bit in_bv (i+k) else bit in_bv ((i+k)-ls))
    None
  in
  let do_rol = shift_core
    (fun ls i k in_bv -> if i >= k then bit in_bv (i-k) else bit in_bv ((ls-k)+i))
    None
  in
  let add_core initial_acc lbv rbv = 
    let s = size_of_bv lbv in
    let obv,cbv = create_fresh_anon s,create_fresh_anon s in 
    let _,finalacc = fold_left2 (fun (i,acc) l r ->
      let newacc = PL.syntactic_simplify (PL.Or(PL.And(l,r),PL.Or(PL.And(l,acc),PL.And(r,acc)))) in
      let outbit = PL.syntactic_simplify (f_xor l (f_xor r acc)) in
      set_bit obv i outbit;
      set_bit cbv i newacc;
      (i+1,newacc))
      (0,initial_acc)
      lbv
      rbv
    in
    (obv,finalacc)
  in
  let do_add l r = fst(add_core plfalse l r) in
  let do_sub_core l r = add_core pltrue l (bitwise_unop_anon r f_not) in
  let do_sub l r = fst(do_sub_core l r) in
  let do_neg e   = do_sub (create_fresh_from_i64 0L (size_of_bv e)) e in
  let do_ult l r = 
    let obv = create_fresh_anon 1 in
    let acc = snd(do_sub_core l r) in
    set_bit obv 0 (PL.syntactic_simplify (f_not acc));
    obv
  in  
  let do_slt l r =
    let obv = create_fresh_anon 1 in
    let s = size_of_bv l - 1 in
    let acc = snd(do_sub_core l r) in
    set_bit obv 0 (PL.syntactic_simplify (f_xor (f_xor (bit l s) (bit r s)) (f_not acc)));
    obv
  in
  let do_mul lbv rbv = 
    let ls,rs = size_of_bv lbv,size_of_bv rbv in
    if ls <> rs then raise (BBUnimplemented("Mul:  size(lhs) != size(rhs)"));
    let rec aux i bv = 
      if i = ls
      then bv
      else
        let shifted = map_anon (fun x -> PL.syntactic_simplify (f_ite (bit rbv i) x plfalse)) lbv in
        let extended = create_custom_anon [Repeat(Existing(tzero),i);Existing(shifted);Repeat(Existing(tzero),ls-i)] in
        aux (i+1) (do_add bv extended)
    in
    let result = aux 0 (create_custom_anon [Repeat(Existing(tzero),ls+rs)]) in
    create_custom_anon [Extract(result,ls-1,0)]
  in
  let udivmod_core abv bbv = 
    let az,bs = size_of_bv abv,size_of_bv bbv in
    if az <> bs then raise (BBUnimplemented("UDiv:  size(lhs) != size(rhs)"));
    let qbv = create_fresh_anon az in 
    let rec aux i rbv = 
      if i = ~-1
      then rbv
      else
        let abit_bv = create_custom_anon [Extract(abv,i,i)] in
        let tbv = create_custom_anon [Existing(abit_bv);Extract(rbv,bs-2,0)] in
        let nbv = do_slt bbv tbv in
        let subtracted = do_sub tbv bbv in
        let ite_sub = mapi_anon (fun i x -> PL.syntactic_simplify (f_ite (bit nbv 0) (bit subtracted i) x)) tbv in
        set_bit qbv i (bit nbv 0);
        aux (i-1) ite_sub
    in
    let rbv = aux (az-1) (create_custom_anon [Repeat(Existing(tzero),bs)]) in
   (qbv,rbv)
  in
  let do_udiv l r = fst(udivmod_core l r) in
  let do_umod l r = snd(udivmod_core l r) in
  let sdivmod_core abv bbv =
    let absvalue bv = 
      let topbit = bit bv (size_of_bv bv - 1) in
      let negated = do_neg bv in
      mapi_anon (fun i x -> PL.syntactic_simplify (f_ite topbit (bit negated i) x)) bv
    in
    let a_sign = bit abv (size_of_bv abv - 1) in
    let b_sign = bit bbv (size_of_bv bbv - 1) in
    let uquotient,uremainder = udivmod_core (absvalue abv) (absvalue bbv) in
    let nquotient,nremainder = do_neg uquotient,do_neg uremainder in
    let sremainder = mapi_anon (fun i x -> PL.syntactic_simplify (f_ite a_sign (bit nremainder i) x)) uremainder in
    let signdiff = PL.syntactic_simplify (f_xor a_sign b_sign) in
    let squotient  = mapi_anon (fun i x -> PL.syntactic_simplify (f_ite signdiff (bit nquotient i) x)) uquotient in
   (squotient,sremainder)
  in
  
  let do_sdiv l r = fst(sdivmod_core l r) in
  let do_smod l r = snd(sdivmod_core l r) in

  let rec aux = function
  | Constant(i64,i)   -> create_fresh_from_i64 i64 i
  | Variable(bv)      -> bv
  | BV.Extract(e,h,l) -> create_custom_anon [Extract(aux e,h,l)]
  | BV.Concat (l,r)   -> create_custom_anon [Existing(aux r);Existing(aux l)]
  | BV.Repeat (e,i)   -> create_custom_anon [Repeat(Existing(aux e),i)]
  | ZeroExt(e,i)      -> create_custom_anon [Existing(aux e);Repeat(Existing(tzero),i)]
  | SignExt(e,i)      -> let ebv = aux e in let el = (size_of_bv ebv)-1 in 
    create_custom_anon [Existing(ebv);Repeat(Extract(ebv,el,el),i)]
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
  | ShlBv(l,r)        -> do_shl  (aux l) (aux r) 
  | ShrBv(l,r)        -> do_shr  (aux l) (aux r)
  | SarBv(l,r)        -> do_sar  (aux l) (aux r)
  | RolBv(l,r)        -> do_rol  (aux l) (aux r)
  | RorBv(l,r)        -> do_ror  (aux l) (aux r)
  
  (* Arithmetic operations *)
  | Add(l,r)          -> do_add  (aux l) (aux r)
  | Sub(l,r)          -> do_sub  (aux l) (aux r)
  | Neg(e)            -> do_neg  (aux e)
  | Mul(l,r)          -> do_mul  (aux l) (aux r)
  | UDiv(l,r)         -> do_udiv (aux l) (aux r)
  | UMod(l,r)         -> do_umod (aux l) (aux r)
  | SDiv(l,r)         -> do_sdiv (aux l) (aux r)
  | SMod(l,r)         -> do_smod (aux l) (aux r)

  (* Relational operations *)
  | EQ (l,r)          -> do_eq (aux l) (aux r)
  | NE (l,r)          -> do_ne (aux l) (aux r)
  | SLT(l,r)          -> do_slt (aux l) (aux r)
  | SLE(l,r)          -> let l,r = aux l,aux r in bitwise_binop_anon (do_eq l r) (do_slt l r) f_or
  | ULT(l,r)          -> do_ult (aux l) (aux r)
  | ULE(l,r)          -> let l,r = aux l,aux r in bitwise_binop_anon (do_eq l r) (do_ult l r) f_or
  
  in 
  let res = aux bvexpr in
  (res,PL.syntactic_simplify (List.fold_left (fun a t -> PL.And(a,t)) (PL.Constant(PL.True)) (!conjuncts)))
  
let bitblast_tests = [
  RedAnd(Constant(0xFL,4)),pltrue;
  RedAnd(Constant(0xEL,4)),plfalse;
  RedOr (Constant(0x8L,4)),pltrue;
  RedOr (Constant(0x0L,4)),plfalse;
  RedXor(Constant(0xAL,4)),plfalse;
  RedXor(Constant(0xBL,4)),pltrue;
  EQ(BV.Extract(Constant(0x55L,8),7,5),Constant(0x2L,3)),pltrue;
  EQ(BV.Extract(Constant(0x55L,8),7,5),Constant(0x5L,3)),plfalse;
  NE(BV.Extract(Constant(0x55L,8),7,5),Constant(0x5L,3)),pltrue;
  NE(BV.Extract(Constant(0x55L,8),7,5),Constant(0x2L,3)),plfalse;
  EQ(Xor(Constant(0x1L,1), Constant(0x0L,1)),Constant(0x1L,1)),pltrue;
  EQ(Xor(Constant(0x1L,1), Constant(0x0L,1)),Constant(0x0L,1)),plfalse;
  NE(Xor(Constant(0x1L,1), Constant(0x0L,1)),Constant(0x0L,1)),pltrue;
  EQ(And(Constant(0xCL,4), Constant(0xAL,4)),Constant(0x8L,4)),pltrue;
  EQ( Or(Constant(0xCL,4), Constant(0xAL,4)),Constant(0xEL,4)),pltrue;
  EQ(Xor(Constant(0xCL,4), Constant(0xAL,4)),Constant(0x6L,4)),pltrue;
  EQ(Not(Constant(0x2L,2)),Constant(0x1L,2)),pltrue;
  EQ(ShlConst(Constant(0x4L,8),1),Constant(0x8L,8)),pltrue;
  EQ(ShrConst(Constant(0x4L,8),1),Constant(0x2L,8)),pltrue;
  EQ(SarConst(Constant(0x4L,3),1),Constant(0x6L,3)),pltrue;
  EQ(SarConst(Constant(0x4L,3),1),Constant(0x2L,3)),plfalse;
  NE(SarConst(Constant(0x4L,3),1),Constant(0x6L,3)),plfalse;
  EQ(RolConst(Constant(0x55L,8),1),Constant(0xAAL,8)),pltrue;
  EQ(RorConst(Constant(0x55L,8),1),Constant(0xAAL,8)),pltrue;
  EQ(Add(Constant(0x5L,4),Constant(0x3L,4)),Constant(0x8L,4)),pltrue;
  EQ(Sub(Constant(0x5L,4),Constant(0x3L,4)),Constant(0x2L,4)),pltrue;
  EQ(Neg(Constant(0x5L,4)),Constant(0xBL,4)),pltrue;
  ULT(Constant(0x5L,4),Constant(0xBL,4)),pltrue;
  ULT(Constant(0xBL,4),Constant(0x5L,4)),plfalse;
  ULE(Constant(0x5L,4),Constant(0xBL,4)),pltrue;
  ULE(Constant(0xBL,4),Constant(0x5L,4)),plfalse;
  ULE(Constant(0xBL,4),Constant(0xBL,4)),pltrue;
  SLT(Constant(0x5L,4),Constant(0xBL,4)),plfalse;
  SLT(Constant(0xBL,4),Constant(0x5L,4)),pltrue;
  SLE(Constant(0x5L,4),Constant(0xBL,4)),plfalse;
  SLE(Constant(0xBL,4),Constant(0x5L,4)),pltrue;
  SLE(Constant(0xBL,4),Constant(0xBL,4)),pltrue;
  EQ(ShlBv(Constant(0x4L,8),Constant(0x1L,8)),Constant(0x8L,8)),pltrue;
  EQ(ShrBv(Constant(0x4L,8),Constant(0x1L,8)),Constant(0x2L,8)),pltrue;
  EQ(SarBv(Constant(0x8L,4),Constant(0x1L,4)),Constant(0xCL,4)),pltrue;
  EQ(SarBv(Constant(0x8L,4),Constant(0x1L,4)),Constant(0x2L,4)),plfalse;
  NE(SarBv(Constant(0x8L,4),Constant(0x1L,4)),Constant(0x4L,4)),pltrue;
  EQ(RolBv(Constant(0x55L,8),Constant(0x1L,8)),Constant(0xAAL,8)),pltrue;
  EQ(RorBv(Constant(0x55L,8),Constant(0x1L,8)),Constant(0xAAL,8)),pltrue;
  EQ(Mul(Constant(0x2L,4),Constant(0x5L,4)),Constant(0xAL,4)),pltrue;
  EQ(UDiv(Constant(0xE0L,8),Constant(0x19L,8)),Constant(0x8L,8)),pltrue;
  EQ(UMod(Constant(0xE0L,8),Constant(0x19L,8)),Constant(0x18L,8)),pltrue;
  EQ(SDiv(Constant(0xE0L,8),Constant(0x19L,8)),Constant(0xFFL,8)),pltrue;
  EQ(SMod(Constant(0xE0L,8),Constant(0x19L,8)),Constant(0xF9L,8)),pltrue;
  ]

(*
let _ =
  List.iter 
   (fun (e,r) -> 
      let resbv,respl = bit_blast e in
      let resconst = 
        let s = BitVector.size_of_bv resbv in
        if s == 1 
        then BitVector.bit resbv 0
        else invalid_arg (Printf.sprintf "Invalid bitvector size %d, expecting 1" s)
      in
    (*let f,_ = PLTVLAI.bitwise_ai_prop f (Hashtbl.create 5) in*)
      Printf.printf "%s: expecting %s, got %s: %s\n" 
       (BVPrettyPrint.string_of_bvexpr e) 
       (PL.string_of_prop r) 
       (PL.string_of_prop resconst) 
       (if resconst = r then "PASSED" else "FAILED"))
    bitblast_tests
*)