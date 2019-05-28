(* Exceptions that may be thrown *)
exception Unimplemented of string
exception Unvaluated of string

(* Type declarations *)
type tval =
| False
| True

type prop = 
| Constant of tval
| Variable of string
| And of prop * prop
| Or  of prop * prop
| Not of prop
| Implies of prop * prop
| Iff of prop * prop

(* Pretty printers *)
let string_of_tval = function
| False -> "false"
| True  -> "true"

let rec string_of_prop = function
| Constant(c)  -> string_of_tval c
| Variable(v)  -> v
| And(l,r)     -> string_of_prop l^" /\\ "^string_of_prop r
| Or(l,r)      -> string_of_prop l^" \\/ "^string_of_prop r
| Not(e)       -> "!"^string_of_prop e
| Implies(l,r) -> string_of_prop l^" ==> "^string_of_prop r
| Iff(l,r)     -> string_of_prop l^" <=> "^string_of_prop r

(* Implementations of logical functions *)
let logand_table =          
[|
         (*False*)(*True*)
(*False*)[|False;  False;|];
(*True*) [|False;   True;|];
|]

let logor_table =          
[|
         (*False*)(*True*)
(*False*)[|False;   True;|];
(*True*) [|True;    True;|];
|]

let lognot_table = [|True; False;|]

let logimplies_table =          
[|
         (*False*)(*True*)
(*False*)[|True;   True;|];
(*True*) [|False;  True;|];
|]

let logiff_table =          
[|
         (*False*)(*True*)
(*False*)[|True;   False;|];
(*True*) [|False;   True;|];
|]

(* Used to implement the logical functions in terms of the tables above *)
let int_of_tval = function
| False -> 0
| True  -> 1

(* Wrappers for implementations of logical functions *)
let logical_and     t1 t2 =     logand_table.(int_of_tval t1).(int_of_tval t2)
let logical_or      t1 t2 =      logor_table.(int_of_tval t1).(int_of_tval t2)
let logical_implies t1 t2 = logimplies_table.(int_of_tval t1).(int_of_tval t2)
let logical_iff     t1 t2 =     logiff_table.(int_of_tval t1).(int_of_tval t2)
let logical_not     t1    =     lognot_table.(int_of_tval t1)

(* Valuation functions:  lookup, create from list, print *)
let lookup_value valuation pname =
  let res =
    try  Hashtbl.find valuation pname
    with Not_found -> raise (Unvaluated(pname))
  in res

let mk_valuation list =
  let ht = Hashtbl.create ((List.length list) * 2 + 1) in
  let rec aux = function
  | (s,tv)::xs -> Hashtbl.replace ht s tv; aux xs
  | [] -> ht
  in aux list

let print_valuation =
  Hashtbl.iter (fun s v -> Printf.printf "\t%s: %s\n" s (string_of_tval v))
    
(* Evaluator *)
let rec eval valuation = function
| Constant(p)  -> p
| Variable(s)  -> lookup_value valuation s
| And(l,r)     -> logical_and     (eval valuation l) (eval valuation r)
| Or(l,r)      -> logical_or      (eval valuation l) (eval valuation r)
| Not(e)       -> logical_not     (eval valuation e)
| Implies(l,r) -> logical_implies (eval valuation l) (eval valuation r)
| Iff(l,r)     -> logical_iff     (eval valuation l) (eval valuation r)

(* Test cases *)
let logical_and_testcases =
  [(And(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",False], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",True ], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",False], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",True],  True )]

let logical_or_testcases =
  [(Or(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",False], False);
   (Or(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",True ], True);
   (Or(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",False], True);
   (Or(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",True],  True)]

let logical_not_testcases =
  [(Not(Variable("a")),mk_valuation ["a",False], True);
   (Not(Variable("a")),mk_valuation ["a",True ], False)]

let logical_and_testcases =
  [(And(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",False], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",True ], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",False], False);
   (And(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",True],  True )]

let logical_implies_testcases =
  [(Implies(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",False],  True);
   (Implies(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",True ],  True);
   (Implies(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",False], False);
   (Implies(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",True],   True)]

let logical_iff_testcases =
  [(Iff(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",False],  True);
   (Iff(Variable("a"),Variable("b")),mk_valuation ["a",False;"b",True ], False);
   (Iff(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",False], False);
   (Iff(Variable("a"),Variable("b")),mk_valuation ["a",True; "b",True],   True)]

(* Test engine *)
let do_tests =
  let rec aux = function
  | (expr,valuation,res)::xs -> 
    if (eval valuation expr) <> res
    then
     (let _ = 
        Printf.printf "[E] Test failed: %s\nUnder valuation:\n" (string_of_prop expr);
        print_valuation valuation;
        Printf.printf "Failed to return expected value %s\n" (string_of_tval res)
      in
      failwith "failed test")
    else
      aux xs
  | [] -> Printf.printf "[I] All tests passed\n"
  in aux

(* Perform all test cases *)
let _ = 
  do_tests 
   (logical_and_testcases     @
    logical_or_testcases      @
    logical_not_testcases     @
    logical_implies_testcases @
    logical_iff_testcases)
