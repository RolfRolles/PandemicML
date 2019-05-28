(** Functions responsible for translating x86 instructions into {!IR.instr} types.
    There exist two functions.  Both take as their first parameter the address at 
    which the instruction is situated.  This is used to generate labels within the
    IR.  One function also takes an x86 instruction as input; the other one uses
    {!X86Decode.decode} to generate the {!X86.x86instr} instructions on the fly. *)

(** {6 IR translator functions} *)

(** Produce IR translation, given an address and an {!X86.x86instrpref}. *)
val translate_instr : int32 -> X86.x86instrpref -> IR.instr list

(** Produce IR translation.  Use {!X86Decode.decode} to produce the instruction 
    from the given address. *)
val translate       : int32 -> IR.instr list
