open IR

exception TypeException of string

let is_integer_type = function
| Mem(_,_,_)    -> false
| Variable(_,_) -> true

let is_memory_type = function
| Mem(_,_,_)    -> true
| Variable(_,_) -> false

let size_of_integer_type = function
| Variable(_,s) -> IRUtil.bits s
| Mem(_,_,_)    -> raise (TypeException("tried to extract size of integer type from a memory"))

let type_of_integer_type = function
| Variable(_,s) -> s
| Mem(_,_,_)    -> raise (TypeException("tried to extract type of integer type from a memory"))

let compare_types v1 v2 = match(v1,v2) with
| Variable(_,s1), Variable(_,s2) -> IRUtil.bits s1 == IRUtil.bits s2
| Mem(_,_,_), Mem(_,_,_)         -> true
| _,_                            -> raise (TypeException("comparing a variable against a memory"))

let endianness_of_memory_type = function
| Mem(_,e,_) -> e
| Variable(_,_) -> raise (TypeException("tried to extract endianness of memory type from an integer"))

(* I essentially want two pieces of information here:  
   1) Whether the type is correct:
   
   * Casts, Unops and Binops take integral expressions.
   * Casts are between valid sizes.
   * Both sides of a binop have the same size, except for shifts, and the
     shift size is always less than or equal to the integer size.
   * Loads and Stores take expressions evaluating to memories as their first
     parameter, integral expressions as the second ones, and Stores take 
     integral expressions for the third ones.
   * Lets are always well-typed.
   
   2) The type of the expression (as in, if we could replace the expression 
      with a variable, what would the type of that variable be?).
      
   We accomplish this by using exceptions:  if any of the above rules are
   violated, then we throw an exception.  Otherwise, we return the type of 
   the expression. *)
let rec typecheck_expr expr =
  let throw str = raise (TypeException(PpIR.ppExpr false expr^":  "^str)) in
  match expr with
| Cast(k,s,e)       -> let typ = typecheck_expr e in
                      (match k with
                       (* These casts take a smaller type (e) and turn it into a bigger type (s) *)
                       | Unsigned -> if not (size_of_integer_type typ <= IRUtil.bits s)
                                     then throw "Casting unsigned to a smaller type"
                       | Signed   -> if not (size_of_integer_type typ <= IRUtil.bits s)
                                     then throw "Casting signedto a smaller type"
                       (* This cast takes a bigger type (e) and turns it into a smaller type (s) *)
                       | High     -> if not (size_of_integer_type typ >= IRUtil.bits s)
                                     then throw "Casting high part to a bigger type"
                       (* This cast takes a bigger type (e) and turns it into a smaller type (s) *)
                       | Low      -> if not (size_of_integer_type typ >= IRUtil.bits s)
                                     then throw "Casting low part to a bigger type");
                       Variable(0,s)
| Unop(_,e)         -> let typ = typecheck_expr e in
                       if not (is_integer_type typ) then throw "Not an integer type";
                       typ
| Binop(e1,Shl,e2)
| Binop(e1,Sar,e2)
| Binop(e1,Shr,e2)  -> let ltyp = typecheck_expr e1 in
                       let rtyp = typecheck_expr e2 in
                       if not (is_integer_type ltyp) then throw "LHS is not an integer";
                       if not (is_integer_type rtyp) then throw "RHS is not an integer";
                       if not (size_of_integer_type ltyp >= size_of_integer_type rtyp)
                       then throw "RHS is bigger than LHS";
                       ltyp
| Binop(e1,EQ,e2)
| Binop(e1,NE,e2)
| Binop(e1,SLT,e2)
| Binop(e1,SLE,e2)
| Binop(e1,ULT,e2)
| Binop(e1,ULE,e2)  -> let ltyp = typecheck_expr e1 in
                       let rtyp = typecheck_expr e2 in
                       if not (is_integer_type ltyp) then throw "LHS is not an integer";
                       if not (is_integer_type rtyp) then throw "RHS is not an integer";
                       if not (size_of_integer_type ltyp == size_of_integer_type rtyp)
                       then throw "RHS is not the same size as LHS";
                       Variable(0,TypeReg_1)
| Binop(e1,_,e2)    -> let ltyp = typecheck_expr e1 in
                       let rtyp = typecheck_expr e2 in
                       if not (is_integer_type ltyp) then throw "LHS is not an integer";
                       if not (is_integer_type rtyp) then throw "RHS is not an integer";
                       if not (size_of_integer_type ltyp == size_of_integer_type rtyp)
                       then throw "RHS is not the same size as LHS";
                       ltyp
| Load(e1,e2,r)     -> let ltyp = typecheck_expr e1 in
                       let rtyp = typecheck_expr e2 in
                       if not (is_memory_type ltyp) then throw "LHS is not a memory";
                       if not (is_integer_type rtyp && size_of_integer_type rtyp = 32)
                       then throw "address is not a 32-bit integer";
                       (* && if normalized, then check r against rtyp *)
                       Variable(0,r)
| Store(e1,e2,e3,r) -> let ltyp = typecheck_expr e1 in
                       let rtyp = typecheck_expr e2 in
                       let typ3 = typecheck_expr e3 in
                       if not (is_memory_type ltyp) then throw "LHS is not a memory";
                       if not (is_integer_type rtyp && size_of_integer_type rtyp = 32)
                       then throw "address is not a 32-bit integer";
                       if not (is_integer_type typ3)
                       then throw "expression to write is not an integer";
                       (* && if normalized, then check r against rtyp and e3 *)
                       ltyp
| Var(v)            -> v
| Const(_,s)        -> Variable(0,s)
| Let(v,e1,e2)      -> let t1 = typecheck_expr e2 in
                       if not (compare_types v t1) then throw "LHS and RHS in Let incompatible";
                       typecheck_expr e2

let typecheck_instr i =
  let throw s = raise (TypeException(PpIR.ppInstr i^":  "^s)) in
  match i with
| Assign(v,e) ->  let et = typecheck_expr e in
                  if not (compare_types et v)
                  then throw "LHS incompatible with RHS"
| Jmp(e)      ->  let et = typecheck_expr e in
                  if not (is_integer_type et)
                  then throw "destination does not evaluate to an integer"
| CJmp(c,t,nt) -> let ct = typecheck_expr c in
                  if not (is_integer_type ct && size_of_integer_type ct = 1)
                  then throw "condition does not evaluate to a boolean";
                  let tt = typecheck_expr t in 
                  if not (is_integer_type tt)
                  then throw "taken expression is not an integer";
                  let ntt = typecheck_expr nt in 
                  if not (is_integer_type ntt)
                  then throw "not taken expression is not an integer"
| Halt(e)      -> let et = typecheck_expr e in
                  if not (is_integer_type et)
                  then throw "halting expression is not an integer"
| Assert(e)    -> let et = typecheck_expr e in
                  if not (is_integer_type et && size_of_integer_type et = 1)
                  then throw "assertion is not a boolean"
| Label(_) 
| Special(_)
| Comment(_)   -> ()
