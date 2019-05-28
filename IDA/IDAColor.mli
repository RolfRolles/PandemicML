(** Module for coloring strings for displaying them elsewhere in IDA. *)

(** {6 Types} *)

(** The color pallette that IDA supports as literals. *)
type idacolor =
    COLOR_DEFAULT
  | COLOR_REGCMT
  | COLOR_RPTCMT
  | COLOR_AUTOCMT
  | COLOR_INSN
  | COLOR_DATNAME
  | COLOR_DNAME
  | COLOR_DEMNAME
  | COLOR_SYMBOL
  | COLOR_CHAR
  | COLOR_STRING
  | COLOR_NUMBER
  | COLOR_VOIDOP
  | COLOR_CREF
  | COLOR_DREF
  | COLOR_CREFTAIL
  | COLOR_DREFTAIL
  | COLOR_ERROR
  | COLOR_PREFIX
  | COLOR_BINPREF
  | COLOR_EXTRA
  | COLOR_ALTOP
  | COLOR_HIDNAME
  | COLOR_LIBNAME
  | COLOR_LOCNAME
  | COLOR_CODNAME
  | COLOR_ASMDIR
  | COLOR_MACRO
  | COLOR_DSTR
  | COLOR_DCHAR
  | COLOR_DNUM
  | COLOR_KEYWORD
  | COLOR_REG
  | COLOR_IMPNAME
  | COLOR_SEGNAME
  | COLOR_UNKNAME
  | COLOR_CNAME
  | COLOR_UNAME
  | COLOR_COLLAPSED
  | COLOR_ADDR
  | COLOR_OPND1
  | COLOR_OPND2
  | COLOR_OPND3
  | COLOR_OPND4
  | COLOR_OPND5
  | COLOR_OPND6

(** {6 Functions} *)

(** Make a string of the given color *)
val idacolorstr : idacolor -> string -> string

(** Shortcut for COLOR_INSN *)
val col_insn : string -> string

(** Shortcut for COLOR_KEYWORD *)
val col_keyword : string -> string

(** Shortcut for COLOR_SYMBOL *)
val col_symbol : string -> string

(** Shortcut for COLOR_REG *)
val col_reg : string -> string

(** Shortcut for COLOR_VOIDOP *)
val col_memimm : string -> string

(** Shortcut for COLOR_NUMBER *)
val col_imm : string -> string

(** Shortcut for COLOR_ADDR *)
val col_addr : string -> string


