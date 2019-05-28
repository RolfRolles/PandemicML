(* 7/4/2012 6:53:02 PM Implementation of Automated Synthesis of Symbolic Instruction Encodings from I/O Samples *)

(* High-level goals:
   1)  Integrate with the existing x86 JIT assembler to generate I/O pairs
   2)  Create a data type describing everything we need to describe templates
   3)  Integrate that data type with Z3
   4)  Implement the solution procedures DInputVal and SmartVal
   5)  Implement the "smart" input synthesis procedures
   6)  Solve, extract models 
 
*)

(* 1)  x86 JIT -- done already, look into this later *)

(* 2)  Template data type *)

(* I might need to gerrymander this a bit *)
(* Separate it out by type? E.g. ITE has one component that is boolean and another that is BV *)
type expr = 
(* Trinary *)
| ITE of expr * expr * expr

(* Binary *)
(* Binary, commutative *)
| AND of expr * expr
| OR  of expr * expr
| XOR of expr * expr
| EQUALS of expr * expr
(* Binary, noncommutative *)
| IMPLIES of expr * expr

(* Unary *)
| NOT of expr

(* Nullary *)
| CONST of int32
| FALSE
| TRUE

| BITAT of expr * int

| VAR of string

(* How should we implement the BW# functions?  Uninterpreted functions with axioms specifying their behavior? *)

let bw_main_template =
  let rec aux expr i = 
    if i = 16 then expr else
    aux (OR(expr,AND(EQUALS(VAR("c"),i),EQUALS(VAR("omain"),???)))) (i+1)
  in aux ??? 0

