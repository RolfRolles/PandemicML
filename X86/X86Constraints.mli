(** This module shouldn't be used outside of the assembler and random 
    instruction generator.  It is used to ensure that things that ought to have
    the same size, do.  For instance, consider the instruction encoding 
    [mov Ev, Gv], and consider the instruction [mov eax, bx].  [eax] is in fact 
    of type [Ev], and [bx] is of type [Gv], but their sizes must match, and 
    hence this is an invalid instruction.  Something similar can be said for 
    instructions that take multiple memory expressions (e.g. [movsb]); both the
    source and the destination must have the same address size. *)

(** {6 Union types} *)

(** Size-related constraints for a list of operands. *)
type opnd_size_constraint =
| OpNone (** No constraints. *)
| Op12SizeEq  (** Size of operands one and two must match. *)
| Op123SizeEq (** Size of operands one, two, and three must match. *)
| Op12GvMa (** LHS is 16 or 32; RHS is double. *)
| Op12GvMp (** LHS is 16 or 32; RHS is 32 or 48, respectively. *)
| Op12GzMp (** LHS is 16 or 32; RHS is 32 or 48, respectively. *)

(** Address size-related constraints for a list of operands. *)
type opnd_addr_constraint =
| AddrNone (** No constraints. *)
| Addr12SizeEq (** Address sizes of operands one and two must match. *)

(** {6 Functions} *)

(** Does the list of operands conform to the given size constraints? *)
val satisfies_size_constraints : X86.x86operand list -> opnd_size_constraint -> bool

(** Does the list of operands conform to the given address size constraints? *)
val satisfies_addr_constraints : X86.x86operand list -> opnd_addr_constraint -> bool

(** Returns a list of conformant size configurations for two operands, given
    which constraint is supposed to be enforced. *)
val satisfying_size_configurations2 : 
  opnd_size_constraint -> 
  (X86TypeCheck.opndsize * X86TypeCheck.opndsize) list

(** Returns a list of conformant size configurations for three operands, given
    which constraint is supposed to be enforced. *)
val satisfying_size_configurations3 : 
  opnd_size_constraint -> 
  (X86TypeCheck.opndsize * X86TypeCheck.opndsize * X86TypeCheck.opndsize) list

(** Returns a list of conformant address size configurations for two operands, 
    given which constraint is supposed to be enforced. *)
val satisfying_addr_configurations2 : 
  opnd_addr_constraint -> 
  (X86TypeCheck.opndsize * X86TypeCheck.opndsize) list

(** Given a list of operands, an operand size constraint, and an address size
    constraint, return true if the operands satisfy the constraints. *)
val satisfies_constraints : X86.x86operand list -> opnd_size_constraint -> opnd_addr_constraint -> bool
