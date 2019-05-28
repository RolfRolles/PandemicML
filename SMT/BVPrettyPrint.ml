let shallow_string_of_bvexpr_op e = let open BV in match e with 
| Constant(c,i)  -> Printf.sprintf "BVConstant(0x%Lx,%d)" c i
| Variable(bv)   -> Printf.sprintf "%s[%d]" (BitVector.name_of_bv bv) (BitVector.size_of_bv bv)
| Extract(e,_,_) -> "BVExtract"
| Repeat(_,_)    -> "BVRepeat"
| Concat(_,_)    -> "BVConcat"
| ZeroExt(_,_)   -> "BVZeroExt"
| SignExt(_,_)   -> "BVSignExt"
| And(_,_)       -> "BVAnd"
| Or (_,_)       -> "BVOr"
| Xor(_,_)       -> "BVXor"
| Not(_)         -> "BVNot"
| RedAnd(_)      -> "BVRedAnd"
| RedOr (_)      -> "BVRedOr"
| RedXor(_)      -> "BVRedXor"
| ShlConst(_,_)  -> "BVShlConst"
| ShrConst(_,_)  -> "BVShrConst"
| SarConst(_,_)  -> "BVSarConst"
| RolConst(_,_)  -> "BVRolConst"
| RorConst(_,_)  -> "BVRorConst"
| ShlBv(_,_)     -> "BVShlBv"
| ShrBv(_,_)     -> "BVShrBv"
| SarBv(_,_)     -> "BVSarBv"
| RolBv(_,_)     -> "BVRolBv"
| RorBv(_,_)     -> "BVRorBv"
| Add(_,_)       -> "BVAdd"
| Sub(_,_)       -> "BVSub"
| Neg(_)         -> "BVNeg"
| Mul(_,_)       -> "BVMul"
| UDiv(_,_)      -> "BVUDiv"
| UMod(_,_)      -> "BVUMod"
| SDiv(_,_)      -> "BVSDiv"
| SMod(_,_)      -> "BVSMod"
| EQ (_,_)       -> "BVEQ"
| NE (_,_)       -> "BVNE"
| SLT(_,_)       -> "BVSLT"
| SLE(_,_)       -> "BVSLE"
| ULT(_,_)       -> "BVULT"
| ULE(_,_)       -> "BVULE"

let string_of_bvexpr e = let open BV in 
  let rec aux = function
  | Constant(c,i)  -> Printf.sprintf "BVConstant(0x%Lx,%d)" c i
  | Variable(bv)   -> Printf.sprintf "%s[%d]" (BitVector.name_of_bv bv) (BitVector.size_of_bv bv)
  | Extract(e,i,j) -> Printf.sprintf "BVExtract(%s,%d,%d)" (aux e) i j
  | Repeat(e,i)    -> Printf.sprintf "BVRepeat(%s,%d)"     (aux e) i
  | Concat(l,r)    -> Printf.sprintf "BVConcat(%s,%s)"     (aux l) (aux r)
  | ZeroExt(e,i)   -> Printf.sprintf "BVZeroExt(%s,%d)"    (aux e) i
  | SignExt(e,i)   -> Printf.sprintf "BVSignExt(%s,%d)"    (aux e) i
  | And(l,r)       -> Printf.sprintf "BVAnd(%s,%s)"        (aux l) (aux r)
  | Or (l,r)       -> Printf.sprintf "BVOr(%s,%s)"         (aux l) (aux r)
  | Xor(l,r)       -> Printf.sprintf "BVXor(%s,%s)"        (aux l) (aux r)
  | Not(e)         -> Printf.sprintf "BVNot(%s)"           (aux e)
  | RedAnd(e)      -> Printf.sprintf "BVRedAnd(%s)"        (aux e)
  | RedOr (e)      -> Printf.sprintf "BVRedOr(%s)"         (aux e)
  | RedXor(e)      -> Printf.sprintf "BVRedXor(%s)"        (aux e)
  | ShlConst(e,i)  -> Printf.sprintf "BVShlConst(%s,%d)"   (aux e) i
  | ShrConst(e,i)  -> Printf.sprintf "BVShrConst(%s,%d)"   (aux e) i
  | SarConst(e,i)  -> Printf.sprintf "BVSarConst(%s,%d)"   (aux e) i
  | RolConst(e,i)  -> Printf.sprintf "BVRolConst(%s,%d)"   (aux e) i
  | RorConst(e,i)  -> Printf.sprintf "BVRorConst(%s,%d)"   (aux e) i
  | ShlBv(l,r)     -> Printf.sprintf "BVShlBv(%s,%s)"      (aux l) (aux r)
  | ShrBv(l,r)     -> Printf.sprintf "BVShrBv(%s,%s)"      (aux l) (aux r)
  | SarBv(l,r)     -> Printf.sprintf "BVSarBv(%s,%s)"      (aux l) (aux r)
  | RolBv(l,r)     -> Printf.sprintf "BVRolBv(%s,%s)"      (aux l) (aux r)
  | RorBv(l,r)     -> Printf.sprintf "BVRorBv(%s,%s)"      (aux l) (aux r)
  | Add(l,r)       -> Printf.sprintf "BVAdd(%s,%s)"        (aux l) (aux r)
  | Sub(l,r)       -> Printf.sprintf "BVSub(%s,%s)"        (aux l) (aux r)
  | Neg(e)         -> Printf.sprintf "BVNeg(%s)"           (aux e)
  | Mul(l,r)       -> Printf.sprintf "BVMul(%s,%s)"        (aux l) (aux r)
  | UDiv(l,r)      -> Printf.sprintf "BVUDiv(%s,%s)"       (aux l) (aux r)
  | UMod(l,r)      -> Printf.sprintf "BVUMod(%s,%s)"       (aux l) (aux r)
  | SDiv(l,r)      -> Printf.sprintf "BVSDiv(%s,%s)"       (aux l) (aux r)
  | SMod(l,r)      -> Printf.sprintf "BVSMod(%s,%s)"       (aux l) (aux r)
  | EQ (l,r)       -> Printf.sprintf "BVEQ(%s,%s)"         (aux l) (aux r)
  | NE (l,r)       -> Printf.sprintf "BVNE(%s,%s)"         (aux l) (aux r)
  | SLT(l,r)       -> Printf.sprintf "BVSLT(%s,%s)"        (aux l) (aux r)
  | SLE(l,r)       -> Printf.sprintf "BVSLE(%s,%s)"        (aux l) (aux r)
  | ULT(l,r)       -> Printf.sprintf "BVULT(%s,%s)"        (aux l) (aux r)
  | ULE(l,r)       -> Printf.sprintf "BVULE(%s,%s)"        (aux l) (aux r)
  in aux e

