exception BitwiseAIExn of string

(* A bit is either known to be zero or one, or it is unknown (half) *)
type abstract_bit =
| AbsZero
| AbsOne
| AbsHalf

(* This data type mimics a variable in the main IR definition; it's either a
   bitvector term or a memory. *)
type absvalue = 
| AbsValue of abstract_bit array 
| AbsMem of (int64 -> abstract_bit array)

val make_constant_opt : abstract_bit array -> IR.expr option

val bitwise_abstract_interpret_ir_state_out : (IR.var * IR.expr) list -> IR.instr list -> (IR.var, absvalue) Hashtbl.t * IR.instr list
val bitwise_abstract_interpret_ir : (IR.var * IR.expr) list -> IR.instr list -> IR.instr list
val test : int -> unit
