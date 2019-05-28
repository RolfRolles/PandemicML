(** Miscellaneous stuff relating to X86.  I should put everything that is needed
    to create a "minimal" disassembly string here.  I.e., ignore default segments
    and soforth.  For now, it's a bit of a junk pile. *)

(** {6 Useful functions} *)

(** Analyze the registers used in a memory expression to determine the default
    segment that would normally be used by a segment-less instruction encoding.
    This allows us to not display segment prefixes for instructions that don't
    require them. *)
val default_seg  : X86.x86_addr_expr -> X86.x86_segreg

(** Get the segment from a given x86 memory expression. *)
val get_seg      : X86.x86_addr_expr -> X86.x86_segreg

(**/**)

val x86_is_cjmp  : X86.x86mnem -> bool
val x86_is_call  : X86.x86mnem -> bool
val x86_is_jmp   : X86.x86mnem -> bool
val x86_is_ret   : X86.x86mnem -> bool
val x86_is_retf  : X86.x86mnem -> bool
val x86_is_callf : X86.x86mnem -> bool
val x86_is_jmpf  : X86.x86mnem -> bool
val extract_jcctargets_opt : X86.x86operand list -> (int32 * int32) option
val extract_jcctargets     : X86.x86operand list -> int32 * int32
val reg16_covers_reg8 : X86.x86_reg16 -> X86.x86_reg8 -> bool
val reg32_covers_reg8 : X86.x86_reg32 -> X86.x86_reg8 -> bool
val reg32_covers_reg16 : X86.x86_reg32 -> X86.x86_reg16 -> bool
val reg32_low8 : X86.x86_reg32 -> X86.x86_reg8
val reg32_high8 : X86.x86_reg32 -> X86.x86_reg8
val reg32_low16 : X86.x86_reg32 -> X86.x86_reg16
val reg16_low8 : X86.x86_reg16 -> X86.x86_reg8
val reg16_high8 : X86.x86_reg16 -> X86.x86_reg8
val reg32_super_of_reg16 : X86.x86_reg16 -> X86.x86_reg32
val reg32_super_of_reg8 : X86.x86_reg8 -> X86.x86_reg32