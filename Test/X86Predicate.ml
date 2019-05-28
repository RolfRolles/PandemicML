type esize =
| S32
| S16
| S8
| S1

let int_of_esize = function
| S32 -> 32
| S16 -> 16
| S8  -> 8
| S1  -> 1

let string_of_esize sz = string_of_int (int_of_esize sz)

type binop =
| Add
| Sub
| Xor
| And
| Or
| Shl
| Shr
| Sar
| Eq
| Ne
| Slt
| Sle
| Ult
| Ule

let l_binop = [Add;Sub;(*Adc;Sbb;*)Xor;And;Or] (*;Eq;Ne;Slt;Sle;Ult;Ule]*)
let log_binops = [Xor;And;Or]
let bit_binops = [Eq;Ne;Slt;Sle;Ult;Ule]
let shift_binops = [Shl;Shr;Sar]

type unop =
| Inc
| Dec
| Not
| Neg

let l_unop = [Inc;Dec;Not;Neg]
let log_unops = [Not]

type bitop =
| Parity
| SignBit
| SecondBit (* From the top, i.e. y in xyzzzzzz *)
| FifthBit (* From the bottom, i.e. y in xxxyxxxx *)

let l_bitop = [Parity;SignBit;SecondBit;FifthBit]

type ext = Signed | Unsigned | Low

let l_ext_larger  = [Signed;Unsigned]
let l_ext_smaller = [Low]

type expr =
| X86Flag of X86.x86_flags
| X86Reg  of X86.x86_general_reg
| BitImm  of bool
| Imm     of X86.x86_immediate
| Binop   of expr * binop * expr
| Unop    of unop * expr
| Bitop   of bitop * expr
| Extend  of ext * esize * expr
| ITE     of expr * expr * expr

let string_of_ext = function
| Signed   -> "Signed"
| Unsigned -> "Unsigned"
| Low      -> "Low"

let string_of_binop = function
| Add -> "Add"
| Sub -> "Sub"
| Xor -> "Xor"
| And -> "And"
| Or  -> "Or"
| Shl -> "Shl"
| Shr -> "Shr"
| Sar -> "Sar"
| Eq  -> "Eq"
| Ne  -> "Ne"
| Slt -> "Slt"
| Sle -> "Sle"
| Ult -> "Ult"
| Ule -> "Ule"

let is_commutative = function
| Add
| Xor
| And
| Or 
| Eq 
| Ne  -> true
| Sub
| Shl
| Shr
| Sar
| Slt
| Sle
| Ult
| Ule -> false

let string_of_unop = function
| Inc -> "Inc"
| Dec -> "Dec"
| Not -> "Not"
| Neg -> "Neg"

let string_of_bitop = function
| Parity    -> "Parity"
| SignBit   -> "SignBit"
| SecondBit -> "SecondBit"
| FifthBit  -> "FifthBit"

let rec string_of_expr = function
| X86Flag(f)        -> "X86Flag("^X86Disasm.string_of_x86_flags f^")"
| X86Reg(X86.Gd(r)) -> "X86Reg(Gd("^X86Disasm.string_of_x86_reg32 r^"))"
| X86Reg(X86.Gw(r)) -> "X86Reg(Gw("^X86Disasm.string_of_x86_reg16 r^"))"
| X86Reg(X86.Gb(r)) -> "X86Reg(Gb("^X86Disasm.string_of_x86_reg8  r^"))"
| BitImm(b)         -> "BitImm("^string_of_bool b^")"
| Imm(X86.Id(i))    -> Printf.sprintf "Imm(Id(0x%lx))" i
| Imm(X86.Iw(i))    -> Printf.sprintf "Imm(Iw(0x%lx))" i
| Imm(X86.Ib(i))    -> Printf.sprintf "Imm(Ib(0x%lx))" i
| Binop(l,o,r)      -> Printf.sprintf "Binop(%s,%s,%s)" (string_of_expr l) (string_of_binop o) (string_of_expr r)
| Unop(o,e)         -> Printf.sprintf "Unop(%s,%s)" (string_of_unop o) (string_of_expr e)
| Bitop(o,e)        -> Printf.sprintf "Bitop(%s,%s)" (string_of_bitop o) (string_of_expr e)
| Extend(m,s,e)     -> Printf.sprintf "Extend(%s,%s,%s)" (string_of_ext m) (string_of_esize s) (string_of_expr e)
| ITE(b,t,f)        -> Printf.sprintf "ITE(%s,%s,%s)" (string_of_expr b) (string_of_expr t) (string_of_expr f)

type stmt = 
| FlagEquals of X86.x86_flags * expr
| RegEquals of X86.x86_general_reg * expr
| ImmEquals of X86.x86_immediate * expr

let string_of_stmt = function
| FlagEquals(f,e)        -> "FlagEquals("^X86Disasm.string_of_x86_flags f^","^string_of_expr e^")"
| RegEquals(X86.Gd(r),e) -> "RegEquals(Gd("^X86Disasm.string_of_x86_reg32 r^","^string_of_expr e^"))"
| RegEquals(X86.Gw(r),e) -> "RegEquals(Gw("^X86Disasm.string_of_x86_reg16 r^","^string_of_expr e^"))"
| RegEquals(X86.Gb(r),e) -> "RegEquals(Gb("^X86Disasm.string_of_x86_reg8  r^","^string_of_expr e^"))"
| ImmEquals(X86.Id(i),e) -> Printf.sprintf "ImmEquals(Id(0x%lx),%s)" i (string_of_expr e)
| ImmEquals(X86.Iw(i),e) -> Printf.sprintf "ImmEquals(Iw(0x%lx),%s)" i (string_of_expr e)
| ImmEquals(X86.Ib(i),e) -> Printf.sprintf "ImmEquals(Ib(0x%lx),%s)" i (string_of_expr e)

type ('a,'b,'c,'d) quadruple =
{
  val32: 'a;
  val16: 'b;
  val8: 'c;
  val1: 'd;
}

let select_quad q = function
| S32 -> q.val32
| S16 -> q.val16
| S8  -> q.val8
| S1  -> q.val1

let update_quad q v = function
| S32 -> { q with val32 = v; }
| S16 -> { q with val16 = v; }
| S8  -> { q with val8  = v; }
| S1  -> { q with val1  = v; }

