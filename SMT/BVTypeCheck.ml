exception TypeException of string

let size_of_bvexpr expr =
  let open BV in
  let rec aux e = match e with
  | Constant(_,i) -> i
  | Variable(bv) -> BitVector.size_of_bv bv
  | Extract(e,h,l) ->
    let size = aux e in
   (if l < 0 || h < 0 || l > h || h > size || l > size || (l-h+1) > size 
    then 
      let sexn = 
        Printf.sprintf "extract(%s[%d],%d,%d): bad bounds"
         (BVPrettyPrint.shallow_string_of_bvexpr_op e)
          size
          l
          h
      in
      raise (TypeException(sexn)));
    size
  | Repeat(e,n) ->
    let es = aux e in
    n*es
  | Concat(l,r) ->
    let ls = aux l in
    let rs = aux r in
    ls+rs
  | ZeroExt(e,i)
  | SignExt(e,i) ->
    let es = aux e in
    es+i
  | RedAnd(e)
  | RedOr (e)
  | RedXor(e) ->
    let _ = aux e in
    (* Here I am choosing to represent booleans as bitvectors of size 1 *)
    1
  
  | ShlConst(e,i)
  | ShrConst(e,i)
  | SarConst(e,i)
  | RolConst(e,i)
  | RorConst(e,i) ->
    let es = aux e in
    es

  | ShlBv(l,r)
  | ShrBv(l,r)
  | SarBv(l,r)
  | RolBv(l,r)
  | RorBv(l,r) ->
    let ls = aux l in
    let _  = aux r in
    ls

  | And(l,r) 
  | Or (l,r)
  | Xor(l,r)
  | Add(l,r)
  | Sub(l,r) 
  | Mul(l,r) 
  | UDiv(l,r)
  | UMod(l,r)
  | SDiv(l,r)
  | SMod(l,r) ->
    let ls = aux l in
    let rs = aux r in
   (if ls <> rs then 
      let sexn = 
        Printf.sprintf "%s(%s,%s): lhs size %d != rhs size %d"
         (BVPrettyPrint.shallow_string_of_bvexpr_op e)
         (BVPrettyPrint.shallow_string_of_bvexpr_op l)
         (BVPrettyPrint.shallow_string_of_bvexpr_op r)
          ls
          rs
      in raise (TypeException(sexn)));
    ls

  | Not(e)
  | Neg(e) -> aux e
    
  | EQ (l,r) 
  | NE (l,r) 
  | SLT(l,r) 
  | SLE(l,r) 
  | ULT(l,r) 
  | ULE(l,r) ->
    let ls = aux l in
    let rs = aux r in
   (if ls <> rs then 
      let sexn = 
        Printf.sprintf "%s(%s,%s): lhs size %d != rhs size %d"
         (BVPrettyPrint.shallow_string_of_bvexpr_op e)
         (BVPrettyPrint.shallow_string_of_bvexpr_op l)
         (BVPrettyPrint.shallow_string_of_bvexpr_op r)
          ls
          rs
      in raise (TypeException(sexn)));
    1
  in
  aux expr

let typecheck expr = ignore(size_of_bvexpr expr)