(** Typechecking of operands.  *)

(** {6 Variant type describing an operand's operand or address size} *)

(** Sizes of operands.  Registers {b and} memory locations are considered to 
    have sizes.  I.e. [movzx eax, byte ptr \[esi\]] corresponds to an [OPSZ_32]
    and an [OPSZ_8]. *)
type opndsize = 
| OPSZ_8
| OPSZ_16 
| OPSZ_32 
| OPSZ_48 
| OPSZ_64 
| OPSZ_80 
| OPSZ_128

(** {6 Type-checking functions} *)

(** Determine whether the second argument is an instance of the type prescribed
    by the first argument.  Returns bool. *)
val typecheck_operand : X86InternalOperand.x86_abstract_operand -> X86.x86operand -> bool

(** Retrieves the size of an operand. *)
val size_of_operand : X86.x86operand -> opndsize

(** Retrieves the address size of an operand. *)
val addr_of_operand : X86.x86operand -> opndsize