(** Module for decoding X86 instructions.  First, the module must be initialized
    by calling {!init}, whose parameter get_byte is a function that returns a byte,
    given an address.  After doing so, the user is free to call {!decode}, which
    either raises one of the exceptions listed below, or returns an x86 
    instruction object, i.e. an {!X86.x86mnem} and a list of {!X86.x86operand}s,
    the length of the instruction, and an {!ASMUtil.cfsuccessors} variant type
    describing the successor(s). *)

(** {6 Exceptions} *)

(** Instruction unimplemented.  Should not occur in practice, as I support all
    of x86/32. *)
exception UnimplementedInstruction of string

(** Instruction was too long to be decoded.  Could happen in practice if there
    are too many prefixes on the instruction. *)
exception TooLongInstruction

(** Instruction (the exception parameter) had an invalid lock prefix. Could 
    easily happen in practice. *)
exception InvalidLockPrefix of X86.x86instrpref

(** Instruction was invalid.  Could happen in practice. *)
exception InvalidInstruction

(** Encountered an ambiguous situation:  multiple segment prefixes, or multiple
    prefixes on an SSE instruction.  The string describes which fault occurred. *)
exception DoMoreResearch of string

(** {6 Functions} *)

(** Initialize the module.  Argument is a routine that takes an address and 
    returns an int32 (the byte at that address).  There are probably smarter 
    ways to do this, e.g. by declaring this as a module, and parameterizing the 
    get_byte function. *)
val init : (int32 -> int32) -> unit

(** Given an address, return an instruction (a mnemonic, its list of operands,
    and its group 1 prefixes, i.e. [lock], [rep]), the length of the instruction,
    and a variant type describing its successors. *)
val decode : int32 -> X86.x86instrpref * int * ASMUtil.cfsuccessors * X86InternalOperand.x86_abstract_operand list

(** Generate C source code corresponding to the decode table. *)
val make_c_disassembler : unit -> unit