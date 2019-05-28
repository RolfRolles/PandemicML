open IR
open IRUtil

(* Put this in an IREvalUtil.ml, then have IRLocalOpt also reference it *)
(* All this shit is duplicated from IRLocalOpt *)

let i64_of_bool = function | true  -> 1L | false -> 0L
let bool2e = function | true  -> Const(0x1L,TypeReg_1) | false -> Const(0x0L,TypeReg_1)
let sign_extend_byte = function
| x when x <= 0x7FL -> x
| x -> Int64.logor  x 0xFFFFFFFFFFFFFF00L

let sign_extend_word = function
| x when x <= 0x7FFFL -> x
| x -> Int64.logor  x 0xFFFFFFFFFFFF0000L

let sign_extend_dword = function
| x when x <= 0x7FFFFFFFL -> x
| x -> Int64.logor  x 0xFFFFFFFF00000000L

let sign_extend c = function
| TypeReg_1  -> invalid_arg "sign_extend: TypeReg_1"
| TypeReg_8  -> sign_extend_byte c
| TypeReg_16 -> sign_extend_word c
| TypeReg_32 -> sign_extend_dword c
| TypeReg_64 -> c

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

let do_signed_cast c1 s1 s =
  let c =
    if (Int64.logand (mk_sign_const_i64 s1) c1) = 0L 
    then c1
    else Int64.logor c1 (Int64.logxor (mk_max_const_i64 s) (mk_max_const_i64 s1))
  in Const(c,s)

let do_high_cast c1 s1 s =
  let s1 = IRUtil.bits s1 in
  let s' = IRUtil.bits s  in
  if  s'> s1 then failwith "do_high_cast: typechecking prevents this";
  Const(Int64.shift_right_logical c1 (s1-s'),s) (* ' *)

