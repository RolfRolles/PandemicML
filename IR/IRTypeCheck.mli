(** Support for interrogating the type of IR terms, and ensuring the semantic 
    consistency of instructions. *)

(** {6 Exceptions} *)

(** Raised by most of the functions in this module upon failure. *)
exception TypeException of string

(** {6 Type-checking functions} *)

(** Typecheck an expressions.  Returns a variable "archetype", i.e., if we 
    were to replace the expression with a variable, what would the type of the
    variable need to be?  Raise a {!TypeException} upon failure. *)
val typecheck_expr : IR.expr -> IR.var

(** Typecheck an instruction.  Returns nothing; a {!TypeException} is raised
    upon failure. *)
val typecheck_instr : IR.instr -> unit

(** {6 Type-related helper functions} *)

(** Is the variable an integer? *)
val is_integer_type : IR.var -> bool

(** Is the variable a memory? *)
val is_memory_type  : IR.var -> bool

(** Returns the {!IR.typereg} type of a given integer, or raises a 
    {!TypeException}. *)
val type_of_integer_type : IR.var -> IR.typereg

(** Returns the integer size (in bits) of a given integer, or raises a 
    {!TypeException}. *)
val size_of_integer_type : IR.var -> int

(** Memories match memories always (revisit for endianness?), integers match
    integers if their sizes match.  Raise a {!TypeException} upon comparing an
    integer with a memory. *)
val compare_types : IR.var -> IR.var -> bool

(** Returns the endianness of a given memory, or raises a {!TypeException}. *)
val endianness_of_memory_type : IR.var -> IR.typeendian
