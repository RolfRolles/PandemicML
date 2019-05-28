(** {6 Types} *)

type bytes = int32 list

(** The stem of how the instruction should be encoded.  ModRMs need to be 
    reified with the proper mod,gpart,epart,sib?,sdisp8/32?. *)
type x86enc = 
| Literal of bytes
| Native32 of bytes
| Native16 of bytes
| ModRM of bytes
| ModRMGroup of int * bytes

(** Given an mnemonic, return a list of encodings together with operand / 
    address size constraints and the x86enc variant *)
val mnem_to_encodings_full :
  X86.x86mnem ->
  ((X86InternalOperand.x86_abstract_operand list *
    (X86Constraints.opnd_size_constraint *
     X86Constraints.opnd_addr_constraint)) *
    x86enc)
  list

(** Given an mnemonic, return a list of encodings together with operand / 
    address size constraints, but no x86enc variant.  TODO:  Refactor rest
    of codebase to use merely the above function; map out the encoding with 
    fst if need be (or ignore it). *)
val mnem_to_encodings :
  X86.x86mnem ->
   (X86InternalOperand.x86_abstract_operand list *
    (X86Constraints.opnd_size_constraint *
     X86Constraints.opnd_addr_constraint))
  list