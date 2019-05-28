(** Currently unused *)
exception Unimplemented

(** Strided interval main definition *)
type strided_interval =
{ lb: int64;
  ub: int64;
  s:  int64;
}

(** Convenience debugging function *)
val string_of_strided_interval : strided_interval -> string

(** Useful external functions *)
val is_canonical_singleton : strided_interval -> bool

(** Convenience function for constructing a top bitvector given a bit size *)
val singleton : int64 -> strided_interval
val make_top :  int   -> strided_interval

(** Unary and then binary arithmetic operations on SIs *)
val neg :    int -> strided_interval -> strided_interval
val add :    int -> strided_interval -> strided_interval -> strided_interval
val sub :    int -> strided_interval -> strided_interval -> strided_interval

(** Unary and then binary bitwise operations on SIs *)
val lognot : int -> strided_interval -> strided_interval
val logor :  int -> strided_interval -> strided_interval -> strided_interval
val logand : int -> strided_interval -> strided_interval -> strided_interval
val logxor : int -> strided_interval -> strided_interval -> strided_interval

(** Some constants *)
val zero      : strided_interval
val one       : strided_interval
val minus_one : strided_interval
