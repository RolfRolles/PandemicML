(** The main IR definition.  So far, it's taken more or less directly from David
    Brumley's Ph.D. thesis, although big changes are planned soon. *)

(** Casts in the IR. *)
type castkind = 
| Unsigned (** Cast a smaller value to a larger one with zero-extension. *)
| Signed   (** Cast a smaller value to a larger one with sign-extension. *)
| High     (** Cast the high part of a value to a smaller sub-value. *)
| Low      (** Cast the high part of a value to a smaller sub-value. *)

(** Sizes of register quantities.  Stupidly-named; it's a transliteration of
    tau_reg from David Brumley's thesis.  Needs a 128-bit quantity if we 
    intend to support 64-bit assembly languages; will also need to code an 
    Int128 module. *)
type typereg = 
| TypeReg_1  (** 1-bit *)
| TypeReg_8  (** 8-bit *)
| TypeReg_16 (** 16-bit *)
| TypeReg_32 (** 32-bit *)
| TypeReg_64 (** 64-bit *)

(** Endianness of a memory. *)
type typeendian = 
| Little 
| Big 
| Norm (** Normalized.  Not using this currently; may go in a different direction. *)

(** Variable type. *)
type var =  
| Mem of int * typeendian * typereg (** Identifier, endianness, size of elements.  Is the latter necessary? *)
| Variable of int * typereg (** Identifier, size. *)

(** A constant is an [int64] and associated size. *)
type const = int64 * typereg

(** Binops in the IR.  Support ROR/ROL directly? *)
type binop = 
| Add 
| Sub 
| Mul 
| SDiv 
| UDiv 
| SMod 
| UMod 
| Shl 
| Shr 
| Sar 
| And 
| Or 
| Xor 
| EQ 
| NE 
| ULT 
| ULE 
| SLT 
| SLE

(** Unops in the IR. *)
type unop  = 
| Neg 
| Not

type expr = 
| Binop of expr * binop * expr 
| Unop of unop * expr 
| Load of expr * expr * typereg (** Memory, address, size *)
| Store of expr * expr * expr * typereg (** Memory, address, expr to write, size *)
| Var of var
| Let of var * expr * expr (** Let var = expr1 in expr2 *)
| Cast of castkind * typereg * expr (** Cast({!castkind},size,expr) *)
| Const of const

type instr = 
| Assign of var * expr (** var = expr (no side-effects) *)
| Jmp of expr (** Jump to an arbitrary location *)
| CJmp of expr * expr * expr (** if(expr1) then goto expr2 else goto expr3 *)
| Halt of expr (** Die *)
| Assert of expr (** Assert an arbitrary logical formula *)
| Label of int32  (** Where the instruction originally lived *)
| Special of int (** From David Brumley's thesis; might go differently *)
| Comment of string (** Comment associated with the instruction; usually its disassembly *)
