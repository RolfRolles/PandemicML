(** Functions for converting X86 instruction parts into strings.  These 
    functions are pure, and their names are self-explanatory. *)

(** {6 X86-parts-to-strings functions} *)

val string_of_x86mnem : X86.x86mnem -> string
val string_of_x86_reg32 : X86.x86_reg32 -> string
val string_of_x86_debug_reg : X86.x86_debug_reg -> string
val string_of_x86_control_reg : X86.x86_control_reg -> string
val string_of_x86_reg16 : X86.x86_reg16 -> string
val string_of_x86_reg8 : X86.x86_reg8 -> string
val string_of_x86_segreg : X86.x86_segreg -> string
val string_of_x86_fpureg : X86.x86_fpureg -> string
val string_of_x86_mmxreg : X86.x86_mmxreg -> string
val string_of_x86_xmmreg : X86.x86_xmmreg -> string
val string_of_x86_immediate : X86.x86_immediate -> string
val string_of_x86_far_target : X86.x86_far_target -> string
val string_of_displ : int32 -> string
val string_of_x86_general_reg : X86.x86_general_reg -> string
val string_of_scalefac : int -> string
val string_of_x86_memexpr : X86.x86_mem_expr -> string
val string_of_x86operand : X86.x86operand -> string
val string_of_x86instr : X86.x86instrpref -> string
val string_of_x86_flags : X86.x86_flags -> string

