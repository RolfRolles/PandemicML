(** The x86 instruction encoder.  Provides one function to encode an instruction:  
    you give it an {!X86.x86instr}, it gives you a list of int32s describing
    the encoding.  Since I haven't had to encode jumps, this is clearly 
    lacking the address parameter to the encoding function.  Also, I use
    {!X86.x86instr} instead of {!X86.x86instrpref}, which means I don't encode
    instruction prefixes.  These changes are trivial, but they have not yet 
    been implemented. *)

(** {6 Exceptions} *)

(** Instruction didn't type-check *)
exception InvalidOperands of X86.x86instrpref

(** {6 Encoding functions} *)

(** Give it an instruction (no prefix-wrapper) and currently not an address, 
    and it gives you a list of int32 values corresponding to the encoding. *)
val encode_instruction : X86.x86instrpref -> int32 list

(** Given an mnemonic, return a list of encodings together with operand / 
    address size constraints *)
val mnem_to_encodings :
  X86.x86mnem ->
   (X86InternalOperand.x86_abstract_operand list *
    (X86Constraints.opnd_size_constraint *
     X86Constraints.opnd_addr_constraint))
  list