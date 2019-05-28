(** Random generation of X86 instructions, used for testing the assembler and 
    disassembler.  Generate random instruction, assemble it, disassemble it, and
    compare whether the OCaml objects match.  Also used briefly in the random
    testing of the IR translation. *)

(** {6 Functions} *)

(** First argument is a function which tells which mnemonics to exclude (it 
    returns true if we should exclude the mnemonic).  Return value is a 
    complete {!X86.x86instr} object. *)
val generate_random_instruction : (X86.x86mnem -> bool) -> X86.x86instr

(** Generates a segment randomly.  Used during IR testing. *)
val rnd_seg : unit -> X86.x86_segreg
