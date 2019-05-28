type castkind = | Unsigned | Signed | High | Low     
let convert_castkind = function 
| Unsigned -> IR.Unsigned 
| Signed   -> IR.Signed 
| High     -> IR.High
| Low      -> IR.Low

let string_of_castkind = function 
| Unsigned -> "Unsigned"
| Signed   -> "Signed"
| High     -> "High"
| Low      -> "Low"

type typereg  = | TypeReg_1 | TypeReg_8 | TypeReg_16 | TypeReg_32 | TypeReg_64 
let convert_typereg = function
| TypeReg_1  -> IR.TypeReg_1 
| TypeReg_8  -> IR.TypeReg_8 
| TypeReg_16 -> IR.TypeReg_16
| TypeReg_32 -> IR.TypeReg_32
| TypeReg_64 -> IR.TypeReg_64

type var = IR.var
type const = IR.const
type binop = | Add | Sub | Mul | SDiv | UDiv | SMod | UMod | Shl | Shr | Sar 
             | And | Or | Xor | EQ | NE | ULT | ULE | SLT | SLE
let convert_binop = function 
| Add  -> IR.Add 
| Sub  -> IR.Sub 
| Mul  -> IR.Mul 
| SDiv -> IR.SDiv 
| UDiv -> IR.UDiv 
| SMod -> IR.SMod 
| UMod -> IR.UMod 
| Shl  -> IR.Shl  
| Shr  -> IR.Shr 
| Sar  -> IR.Sar 
| And  -> IR.And 
| Or   -> IR.Or  
| Xor  -> IR.Xor 
| EQ   -> IR.EQ  
| NE   -> IR.NE   
| ULT  -> IR.ULT 
| ULE  -> IR.ULE 
| SLT  -> IR.SLT 
| SLE  -> IR.SLE 

let string_of_binop = function 
| Add  -> "Add"
| Sub  -> "Sub"
| Mul  -> "Mul"
| SDiv -> "SDiv" 
| UDiv -> "UDiv" 
| SMod -> "SMod" 
| UMod -> "UMod" 
| Shl  -> "Shl" 
| Shr  -> "Shr"
| Sar  -> "Sar"
| And  -> "And"
| Or   -> "Or"
| Xor  -> "Xor"
| EQ   -> "EQ"
| NE   -> "NE" 
| ULT  -> "ULT"
| ULE  -> "ULE"
| SLT  -> "SLT"
| SLE  -> "SLE"

type unop  = | Neg | Not
let convert_unop = function 
| Neg -> IR.Neg 
| Not -> IR.Not

let string_of_unop = function 
| Neg -> "Neg"
| Not -> "Not"

let sz e = let open IRTypeCheck in 
  try type_of_integer_type (typecheck_expr e) 
  with IRTypeCheck.TypeException(s) -> (Printf.printf "Typecheck error! %s\n" s; failwith s)

type bitunop = Parity | SignBit | SecondBit | FifthBit
let string_of_bitunop = function
| Parity    -> "Parity"
| SignBit   -> "SignBit"
| SecondBit -> "SecondBit"
| FifthBit  -> "FifthBit"

type bitbinop = SignDiff
let string_of_bitbinop = function | SignDiff -> "SignDiff"

let parity e = let open IRUtil in
  let shr e i = mk_shr e (mk_byte (Int64.of_int i)) in
  mk_low_cast 
    (IR.TypeReg_1) 
    (mk_not (mk_xor (shr e 7) (mk_xor (shr e 6) (mk_xor (shr e 5) (mk_xor (shr e 4) (mk_xor (shr e 3) (mk_xor (shr e 2) (mk_xor (shr e 1) e))))))))

let signbit e   = IRUtil.mk_slt e (IR.Const(0L,sz e))
let secondbit e = IRUtil.mk_slt (IRUtil.mk_shl e (IR.Const(1L,sz e))) (IR.Const(0L,sz e))
let fifthbit e  = IRUtil.mk_ne (IR.Const(0L,sz e)) (IRUtil.mk_and (IR.Const(16L,sz e)) e)

let convert_signdiff x y = IRUtil.(mk_xor (signbit x) (signbit y))

type expr = 
| Binop of expr * binop * expr 
| Unop of unop * expr 
| Cast of castkind * typereg * expr
| Var of var
| Const of const
| BitUnop of bitunop * expr
| BitBinop of expr * bitbinop * expr
| ITE of expr * expr * expr

(* Can't put this in IRUtil due to a cyclic dependency with IRTypeCheck *)
(* If we put an ITE operator into the language itself, then replace this with
   a wrapper around the IR expression creator; for now, use this *)
let mk_ite eb ethen eelse = let open IRUtil in
  (* Sizes must be equal, but we assume that we're given well-typed 
     expressions; if this is not the case, typechecking will detect
     this *)  
  let ethen = mk_and (mk_signed_cast (sz ethen) eb) ethen in
  let eelse = mk_and (mk_signed_cast (sz eelse) (mk_not eb)) eelse in
  mk_or ethen eelse

let rec convert_expr = function
| Binop(l,b,r)           -> IR.Binop(convert_expr l,convert_binop b,convert_expr r)
| Unop(o,e)              -> IR.Unop(convert_unop o,convert_expr e)
| Var(v)                 -> IR.Var(v)
| Cast(k,s,e)            -> IR.Cast(convert_castkind k,convert_typereg s,convert_expr e)
| Const(c)               -> IR.Const(c)
| ITE(b,t,f)             -> mk_ite (convert_expr b) (convert_expr t) (convert_expr f)
| BitUnop(Parity,e)      -> parity    (convert_expr e)
| BitUnop(SignBit,e)     -> signbit   (convert_expr e)
| BitUnop(SecondBit,e)   -> secondbit (convert_expr e)
| BitUnop(FifthBit,e)    -> fifthbit  (convert_expr e)
| BitBinop(l,SignDiff,r) -> convert_signdiff (convert_expr l) (convert_expr r)

let rec score = function
| Var(_)                 -> 1
| Const(i64,_)           -> if i64 = 0L then 1 else 3
| Binop(l,(EQ|NE),r)     -> 1 + (score l) + (score r)
| Binop(l,(And|Or|Xor),r)      -> 3 + (score l) + (score r)
| Binop(l,(SLT|SLE|ULT|ULE),r) -> 7 + (score l) + (score r)
| Binop(l,_,r)           -> 15 + (score l) + (score r)
| Unop(_,e)              -> 1 + (score e)
| BitUnop(FifthBit,e)    -> 1 + (score e)
| BitUnop(Parity,e)      -> 1 + (score e)
| BitUnop(SignBit,e)     -> 50 + (score e)
| BitUnop(SecondBit,e)   -> 50 + (score e)
| BitBinop(l,SignDiff,r) -> 50 + (score l) + (score r)
| Cast(Low,_,e)          -> 1000 + (score e)
| Cast(_,_,e)            -> 20 + (score e)
| ITE(b,t,f)             -> 1000 + (score b) + (score t) + (score f)

let generator_of_expr expr ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast = let open Generator in
  let lookup ht e = mk_generator (try Hashtbl.find ht e with Not_found -> [e]) in
  let rec aux expr = match expr with
  | Binop(l,b,r)    -> triple_generator_flatten  (fun ((l,r),b) -> Binop(l,b,r))    (aux l) (aux r) (lookup ht_binop b) 
  | Unop(o,e)       -> generator_combine_flatten (fun (e,o)     -> Unop(o,e))       (aux e)         (lookup ht_unop  o) 
  | BitUnop(o,e)    -> generator_combine_flatten (fun (e,o)     -> BitUnop(o,e))    (aux e)         (lookup ht_bitunop o)
  | BitBinop(l,o,r) -> triple_generator_flatten  (fun ((l,r),o) -> BitBinop(l,o,r)) (aux l) (aux r) (lookup ht_bitbinop o)
  | Cast(o,z,e)     -> generator_combine_flatten (fun (e,o)     -> Cast(o,z,e))     (aux e)         (lookup ht_cast  o)
  | ITE(c,t,f)      -> triple_generator_flatten  (fun ((t,f),c) -> ITE(c,t,f))      (aux t) (aux f) (aux c)
  | Var(_) 
  | Const(_)      -> lookup ht_atom expr
  in 
(*mk_transform_generator (fun x -> let ir = SynthesisIR.convert_expr x in Printf.printf "%s\n" (PpIR.ppExpr false ir); x,ir) (aux expr)*)
  aux expr
(*
let ctr = ref 0
let generator_of_expr expr ht_atom ht_binop ht_unop ht_bitop ht_cast = let open Generator in
  let depth = ref (-1) in
  let mk_prefix () = String.make ((!depth + 1)*4) ' ' in
  let string_of_sym s = PpIR.ppExpr false (convert_expr s) in
  let lookup f str m ht e = 
    let g   = mk_generator (try Hashtbl.find ht e with Not_found -> [e]) in
    incr depth;
    let pfx = mk_prefix () in
    decr depth;
   (fun b -> 
      Printf.printf "%s%s %s: enter %b\n" pfx str (f m) b; 
      match Generator.generate_opt g b with
      | Some(x) -> Printf.printf "%s%s %s: yielded %b (%s)\n" pfx str (f m) b (f x); x
      | None    -> Printf.printf "%s%s %s: RAISED %b\n" pfx str (f m) b; raise Generator.GenerationComplete)      
  in
  let tgen t expr g = 
    let c = !ctr in 
    incr ctr;
    let pfx = mk_prefix () in
    let se = string_of_sym expr in
   (fun b -> 
      Printf.printf "%s%s %s %d: enter %b\n" pfx t se c b; 
      match Generator.generate_opt g b with
      | Some(x) -> Printf.printf "%s%s %s %d: yielded %b (%s)\n" pfx t se c b (string_of_sym x); x
      | None    -> Printf.printf "%s%s %s %d: RAISED %b\n" pfx t se c b ; raise Generator.GenerationComplete)
  in
  let rec a g = 
    let _ = Printf.printf "ROLF " in 
    let o = 
      try Some(ignore(g true)) 
      with Generator.GenerationComplete -> None
    in match o with
    | Some(_) -> a g
    | None -> g
  in
  let rec aux expr = 
    let auxr expr = 
      let _ = incr depth in
      let res = aux expr in
      let _ = decr depth in
      res
    in
    match expr with
  | Binop(l,b,r) -> a (tgen "BinopE" expr (triple_generator_flatten  (fun ((l,r),b) -> Binop(l,b,r)) (auxr l) (auxr r) (lookup string_of_binop    "BinopV" b ht_binop b) ))
  | Bitop(o,e)   -> a (tgen "BitopE" expr (generator_combine_flatten (fun (e,o)     -> Bitop(o,e))   (auxr e)          (lookup string_of_bitop    "BitopV" o ht_bitop o) ))
  | Unop(o,e)    -> a (tgen "UnopE"  expr (generator_combine_flatten (fun (e,o)     -> Unop(o,e))    (auxr e)          (lookup string_of_unop     "UnopV"  o ht_unop  o) ))
  | Cast(o,z,e)  -> a (tgen "CastE"  expr (generator_combine_flatten (fun (e,o)     -> Cast(o,z,e))  (auxr e)          (lookup string_of_castkind "CastV"  o ht_cast  o) ))
  | ITE(c,t,f)   -> a (tgen "ITE"    expr (triple_generator_flatten  (fun ((t,f),c) -> ITE(c,t,f))   (auxr t) (auxr f) (auxr c)            )                              )
  | Var(_)       -> a (tgen "VarE"   expr (lookup string_of_sym "VarA" expr ht_atom expr)                                                                                 )
  | Const(_)     -> a (tgen "ConstE" expr (lookup string_of_sym "ConstA" expr ht_atom expr)                                                                               )
  in 
(*mk_transform_generator (fun x -> let ir = SynthesisIR.convert_expr x in Printf.printf "%s\n" (PpIR.ppExpr false ir); x,ir) (aux expr)*)
  aux expr
*)
let generator_of_expr_highest l_subst expr ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast =
  let open RegUtil in 
  let gen = generator_of_expr expr ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast in
  match Generator.generate_opt gen false with
  | None -> Generator.mk_generator []
  | Some(e) -> let open X86 in let f = (match sz (convert_expr e) with
  | IR.TypeReg_64 -> (fun _ -> false)
  | IR.TypeReg_32 -> (function Reg(Gd(_)) -> true | _ -> false)
  | IR.TypeReg_16 -> (function Reg(Gw(_)) -> true | _ -> false)
  | IR.TypeReg_8  -> (function Reg(Gb(_)) -> true | _ -> false)
  | IR.TypeReg_1  -> (function Flag(_)    -> true | _ -> false))
  in
  let vsubst = List.map (fun r -> r,Hashtbl.find ht_reg2aftervar r) (List.filter f l_subst) in
  let g = Generator.(map_multiply 
   (fun e -> mk_transform_generator (fun (r,v) -> (r,e,IRUtil.(mk_eq (mk_evar v) (convert_expr e)))) (mk_generator vsubst))
   gen)
  in
  g
(*(fun b -> let ((_,_,ir) as x) = g b in Printf.printf "2 %s%!\n" (PpIR.ppExpr false ir); x)*)

