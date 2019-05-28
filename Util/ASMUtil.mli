(** Common types and functions for dealing with 32-bit assembly languages. *)

(** {6 Types} *)

(** Abstraction of the address type; this should be moved or at least thought
    about more carefully. *)
(*type int32 = int32*)

(** Control flow successor; aids in generic disassembly. *)
type cfsuccessors =
| Flow of int32 (** Instruction passes control to next instruction. *)
| Jmp of int32  (** Unconditional jump to successor instruction. *)
| Call of int32 * int32 (** Calls an address, returns to successor (potentially). *)
| Jcc of int32 * int32  (** Jumps to one of two addresses. *)
| ICall of int32 (** Calls indirectly; returns to successor (potentially). *)
| IJmp (** Jumps indirectly *)
| Return (** Returns to the address on the stack. *)