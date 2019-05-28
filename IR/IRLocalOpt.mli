(** Local optimizations (and miscellaneous support functions) upon the IR. *)

(** {6 Local optimization functions} *)

(** Extract a list of variables from an expression. *)
val extract_uses : IR.expr -> IR.var list

(** Given a hash table containing a {!IR.var} -> {!IR.expr} mapping, and an
    expression, return an expression where all variables which have entries in
    the map are substituted with the expression from the map.  Used for 
    forward substitution. *)
val replace_var_with_expr : (IR.var, IR.expr) Hashtbl.t -> IR.expr -> IR.expr

(** Same as previous function, except that it works on an instruction rather 
    than an expression.  Used for forward substitution. *)
val replace_instr_var_with_expr : (IR.var, IR.expr) Hashtbl.t -> IR.instr -> IR.instr

(** Same as previous function, except the map is from {!IR.var}s to {!IR.var}s,
    and variables are replaced with their mapped associates.  Used for SSA. *)
val replace_var_with_var :  (IR.var, IR.var) Hashtbl.t -> IR.expr -> IR.expr

(** Same as previous function, except that it works on an instruction rather 
    than an expression.  Used for SSA. *)
val replace_instr_var_with_var : bool -> (IR.var, IR.var) Hashtbl.t -> IR.instr -> IR.instr

(** Performs constant folding and algebraic reassociation. *)
val fold_expr_constants : IR.expr -> IR.expr

(** Performs dead store elimination.  Currently makes the x86-specific 
    assumption that the x86 registers, flags, and memory are live at the end
    of the instruction list.  Does so in a particularly unsafe way with 
    hard-coded (and somewhat obsolete) values. *)
val local_dse : IR.instr list -> IR.var list -> IR.instr list

(** Performs copy and constant propagation, and folding, on a list of 
    instructions. *)
val local_opt : IR.instr list -> IR.instr list

(** Performs copy and constant propagation, and folding, on a list of 
    instructions.  Returns the state (a hash table mapping all values
    to constant expressions).  Might need to revist this for the other 
    maps that contain variable copy information (l2rmap, r2lmap). *)
val local_opt_state_out : IR.instr list -> (IR.var, IR.expr) Hashtbl.t * IR.instr list

(** Performs copy and constant propagation, and folding, on a list of 
    instructions.  Allows for iterative computation:  takes in the state 
    (variable -> constant expr hash table) as input, and it is modified 
    by the analysis imperatively.  Might need to revist this for the other 
    maps that contain variable copy information (l2rmap, r2lmap). *)
val local_opt_state_in : (IR.var, IR.expr) Hashtbl.t -> IR.instr list -> IR.instr list

(** Substitutes expressions in for variables iteratively. *)
val forward_substitute : IR.instr list -> IR.instr list
