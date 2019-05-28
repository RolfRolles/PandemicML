type bvexpr =
| Constant of int64 * int
| Variable of BitVector.t

(* Resizing operations *)
| Extract  of bvexpr * int * int
| Repeat   of bvexpr * int
| Concat   of bvexpr * bvexpr
| ZeroExt  of bvexpr * int
| SignExt  of bvexpr * int

(* Bitwise operations *)
| And      of bvexpr * bvexpr
| Or       of bvexpr * bvexpr
| Xor      of bvexpr * bvexpr
| Not      of bvexpr
(* XXX NAND, NOR, XNOR *)

(* Reduced bitwise operations *)
| RedAnd   of bvexpr
| RedOr    of bvexpr
| RedXor   of bvexpr

(* Shifts and rotates by constants *)
| ShlConst of bvexpr * int
| ShrConst of bvexpr * int
| SarConst of bvexpr * int
| RolConst of bvexpr * int
| RorConst of bvexpr * int

(* Shifts and rotates by a bitvector *)
| ShlBv    of bvexpr * bvexpr
| ShrBv    of bvexpr * bvexpr
| SarBv    of bvexpr * bvexpr
| RolBv    of bvexpr * bvexpr
| RorBv    of bvexpr * bvexpr

(* Arithmetic operations *)
| Add      of bvexpr * bvexpr
| Sub      of bvexpr * bvexpr
| Neg      of bvexpr
| Mul      of bvexpr * bvexpr
| UDiv     of bvexpr * bvexpr
| UMod     of bvexpr * bvexpr
| SDiv     of bvexpr * bvexpr
| SMod     of bvexpr * bvexpr

(* Relational operations *)
| EQ       of bvexpr * bvexpr
| NE       of bvexpr * bvexpr
| SLT      of bvexpr * bvexpr
| SLE      of bvexpr * bvexpr
| ULT      of bvexpr * bvexpr
| ULE      of bvexpr * bvexpr
