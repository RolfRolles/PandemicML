open IR

let bits = function
| TypeReg_1  -> 1
| TypeReg_8  -> 8
| TypeReg_16 -> 16
| TypeReg_32 -> 32
| TypeReg_64 -> 64

let typereg_of_int = function
| 1 ->  TypeReg_1
| 8 ->  TypeReg_8
| 16 -> TypeReg_16
| 32 -> TypeReg_32
| 64 -> TypeReg_64
| _ -> invalid_arg "typereg_of_int"

let truncate_to s x = Int64.logand (Int64.shift_right_logical (-1L) (64-(bits s))) x

let var_comparator i j = match i,j with
| Mem(_,_,_), Variable(_,_) -> -1
| Mem(_,_,_), Mem(_,_,_)    -> 0
| Variable(_,_), Mem(_,_,_) -> 1
| Variable(i,_), Variable(j,_) -> compare i j

module VarComparator = struct
  type t = IR.var
  let compare = var_comparator
end

module VarSet = Set.Make(VarComparator)
module VarMap = Map.Make(VarComparator)

(* Make a set/map from a list of variables/variable,value pairs *)
let var_setify l = List.fold_left (fun a v -> VarSet.add v a) VarSet.empty l
let var_mapify l = List.fold_left (fun a (v,x) -> VarMap.add v x a) VarMap.empty l

let mk_binop o x y    = Binop(x,o,y)
let mk_unop  o x      = Unop(o,x)
let mk_load   m a s   = Load(m,a,s)
let mk_store  m a t s = Store(m,a,t,s)
let mk_evar   v       = Var(v)
let mk_let    v r ie  = Let(v,r,ie)
let mk_cast   c t e   = Cast(c,t,e)
let mk_const  i s     = Const(i,s)

let mk_unsigned_cast t e   = Cast(Unsigned,t,e)
let mk_signed_cast   t e   = Cast(Signed,t,e)
let mk_high_cast     t e   = Cast(High,t,e)
let mk_low_cast      t e   = Cast(Low,t,e)

let mk_add  = mk_binop (Add)
let mk_sub  = mk_binop (Sub)
let mk_shl  = mk_binop (Shl)
let mk_shr  = mk_binop (Shr)
let mk_sar  = mk_binop (Sar)
let mk_and  = mk_binop (And)
let mk_or   = mk_binop (Or)
let mk_xor  = mk_binop (Xor)
let mk_eq   = mk_binop (EQ)
let mk_ne   = mk_binop (NE)
let mk_ult  = mk_binop (ULT)
let mk_ule  = mk_binop (ULE)
let mk_slt  = mk_binop (SLT)
let mk_sle  = mk_binop (SLE)
let mk_mul  = mk_binop (Mul)
let mk_sdiv = mk_binop (SDiv)
let mk_udiv = mk_binop (UDiv)
let mk_smod = mk_binop (SMod)
let mk_umod = mk_binop (UMod)

let mk_fn_of_binop = function
| Add  -> mk_add 
| Sub  -> mk_sub 
| Shl  -> mk_shl 
| Shr  -> mk_shr 
| Sar  -> mk_sar 
| And  -> mk_and 
| Or   -> mk_or  
| Xor  -> mk_xor 
| EQ   -> mk_eq  
| NE   -> mk_ne  
| ULT  -> mk_ult 
| ULE  -> mk_ule 
| SLT  -> mk_slt 
| SLE  -> mk_sle 
| Mul  -> mk_mul 
| SDiv -> mk_sdiv
| UDiv -> mk_udiv
| SMod -> mk_smod
| UMod -> mk_umod

let mk_neg = mk_unop (Neg)
let mk_not = mk_unop (Not)

let mk_fn_of_unop = function
| Neg -> mk_neg
| Not -> mk_not

let mk_assign v e     = Assign(v,e)
let mk_jmp    e       = Jmp(e)
let mk_cjmp   e t nt  = CJmp(e,t,nt)
let mk_halt   e       = Halt(e)
let mk_assert e       = Assert(e)
let mk_label  i       = Label(i)
let mk_special s      = Special(s)
let mk_comment c      = Comment(c)

let cast_to_bit      = mk_cast (Low) (TypeReg_1)
let cast_to_dword    = mk_cast (Unsigned) (TypeReg_32)
let cast_bit_to_byte = mk_cast (Unsigned) (TypeReg_8)

let mk_implies p q = mk_or (mk_not p) q

let mk_bit   i = mk_const (truncate_to (TypeReg_1)  i) (TypeReg_1)
let mk_byte  i = mk_const (truncate_to (TypeReg_8)  i) (TypeReg_8)
let mk_word  i = mk_const (truncate_to (TypeReg_16) i) (TypeReg_16)
let mk_dword i = mk_const (truncate_to (TypeReg_32) i) (TypeReg_32)
let mk_qword i = mk_const i (TypeReg_64)

let mk_true  = mk_bit 0x1L
let mk_false = mk_bit 0x0L

let mk_fixed_const i = function
| TypeReg_1  -> mk_bit   i
| TypeReg_8  -> mk_byte  i
| TypeReg_16 -> mk_word  i
| TypeReg_32 -> mk_dword i
| TypeReg_64 -> mk_qword i

let mk_fixed_const_of_i32 i32 = mk_fixed_const (Int64.of_int32 i32)

let mk_bit_of_int32   i32 = mk_fixed_const_of_i32 i32 TypeReg_1
let mk_byte_of_int32  i32 = mk_fixed_const_of_i32 i32 TypeReg_8
let mk_word_of_int32  i32 = mk_fixed_const_of_i32 i32 TypeReg_16
let mk_dword_of_int32 i32 = mk_fixed_const_of_i32 i32 TypeReg_32
let mk_qword_of_int32 i32 = mk_fixed_const_of_i32 i32 TypeReg_64

let mk_sign_const_i64 = function
| TypeReg_1  -> 0x1L
| TypeReg_8  -> 0x80L
| TypeReg_16 -> 0x8000L
| TypeReg_32 -> 0x80000000L
| TypeReg_64 -> 0x8000000000000000L

let mk_sign_const = function
| TypeReg_1  -> mk_bit 0x1L
| TypeReg_8  -> mk_byte 0x80L
| TypeReg_16 -> mk_word 0x8000L
| TypeReg_32 -> mk_dword 0x80000000L
| TypeReg_64 -> mk_qword 0x8000000000000000L

let mk_max_const_i64 = function
| TypeReg_1  -> 0x1L
| TypeReg_8  -> 0xFFL
| TypeReg_16 -> 0xFFFFL
| TypeReg_32 -> 0xFFFFFFFFL
| TypeReg_64 -> 0xFFFFFFFFFFFFFFFFL

let mk_max_const = function
| TypeReg_1  -> mk_bit 0x1L
| TypeReg_8  -> mk_byte 0xFFL
| TypeReg_16 -> mk_word 0xFFFFL
| TypeReg_32 -> mk_dword 0xFFFFFFFFL
| TypeReg_64 -> mk_qword 0xFFFFFFFFFFFFFFFFL

let mk_zero = mk_const 0x0L

let var_base = 1000
let var_ctr = ref var_base
let set_var_base i = var_ctr := i

let new_var s = let i = !var_ctr in incr var_ctr; Variable(i,s)
let new_mem e s = let i = !var_ctr in incr var_ctr; Mem(i,e,s)

let dup_var v = match v with
| Variable(_,s) -> new_var s
| Mem(_,e,s) -> new_mem e s

let varno = function
| Mem(i,_,_)
| Variable(i,_) -> i

let size_of_var = function
| Variable(_,s) -> s
| Mem(_,_,_) -> invalid_arg "size_of_var"