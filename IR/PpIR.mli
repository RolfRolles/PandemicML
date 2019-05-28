(** Pretty printing for terms in the IR. *)

(** {6 Pretty-printing functions} *)

(** Returns a string for a given binop. *)
val ppBinop : IR.binop    -> string

(** Returns a string for a given unop. *)
val ppUnop : IR.unop     -> string

(** Returns a string for a given castkind. *)
val ppCastkind : IR.castkind -> string

(** Returns the size of a bit-vector quantity. *)
val ppTypereg : IR.typereg -> string

(** Returns a textual representation of an expression, as well as whether to 
    parenthesize the outermost expression.  Since my parser does not have a 
    notion of operator precedence, there are waaaaay too many parentheses in
    the output for the time being. *)
val ppExpr : bool -> IR.expr -> string

(** Returns the textual representation of the instruction, along with its 
    subexpressions. *)
val ppInstr : IR.instr -> string

(** Describes a variable.  Uses hard-coded variable numbers for x86 registers
    currently.  Think about better ways to do this. *)
val ppVar : IR.var -> string