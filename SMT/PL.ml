exception Unimplemented of string
exception Unvaluated of string

type tval =
| False
| True

let string_of_tval = function
| False -> "false"
| True  -> "true"

type prop = 
| Constant of tval
| Variable of string
| And of prop * prop
| Or  of prop * prop
| Not of prop
| Implies of prop * prop
| Iff of prop * prop

let pltrue  = Constant(True)
let plfalse = Constant(False)

let rec string_of_prop = function
| Constant(c)  -> string_of_tval c
| Variable(v)  -> v
| And(l,r)     -> "("^string_of_prop l^" /\\ "^string_of_prop r^")"
| Or(l,r)      -> "("^string_of_prop l^" \\/ "^string_of_prop r^")"
| Not(e)       -> "!"^string_of_prop e
| Implies(l,r) -> "("^string_of_prop l^" ==> "^string_of_prop r^")"
| Iff(l,r)     -> "("^string_of_prop l^" <=> "^string_of_prop r^")"

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

let int_of_tval = function
| False -> 0
| True  -> 1

let logical_ite     t1 t2 t3 = And(Implies(t1,t2),Implies(Not(t1),t3))

let logical_and     t1 t2 =     logand_table.(int_of_tval t1).(int_of_tval t2)
let logical_or      t1 t2 =      logor_table.(int_of_tval t1).(int_of_tval t2)
let logical_implies t1 t2 = logimplies_table.(int_of_tval t1).(int_of_tval t2)
let logical_iff     t1 t2 =     logiff_table.(int_of_tval t1).(int_of_tval t2)
let logical_not     t1    =     lognot_table.(int_of_tval t1)

let lookup_value valuation pname =
  let res =
    try 
      Hashtbl.find valuation pname
    with
      Not_found -> raise (Unvaluated(pname))
  in res

let rec eval valuation = function
| Constant(p)  -> p
| Variable(s)  -> lookup_value valuation s
| And(l,r)     -> logical_and     (eval valuation l) (eval valuation r)
| Or(l,r)      -> logical_or      (eval valuation l) (eval valuation r)
| Not(e)       -> logical_not     (eval valuation e)
| Implies(l,r) -> logical_implies (eval valuation l) (eval valuation r)
| Iff(l,r)     -> logical_iff     (eval valuation l) (eval valuation r)

let mk_valuation list =
  let ht = Hashtbl.create ((List.length list) * 2 + 1) in
  let rec aux = function
  | (s,tv)::xs -> Hashtbl.replace ht s tv; aux xs
  | [] -> ht
  in aux list

let print_valuation =
  Hashtbl.iter (fun s v -> Printf.printf "\t%s: %s\n" s (string_of_tval v))
    
let logical_and_testcases =
  let f = And(Variable("a"),Variable("b")) in
  [(f,mk_valuation ["a",False;"b",False], False);
   (f,mk_valuation ["a",False;"b",True ], False);
   (f,mk_valuation ["a",True; "b",False], False);
   (f,mk_valuation ["a",True; "b",True],  True )]

let logical_or_testcases =
  let f = Or(Variable("a"),Variable("b")) in
  [(f,mk_valuation ["a",False;"b",False], False);
   (f,mk_valuation ["a",False;"b",True ], True);
   (f,mk_valuation ["a",True; "b",False], True);
   (f,mk_valuation ["a",True; "b",True],  True)]

let logical_not_testcases =
  let f = Not(Variable("a")) in
  [(f,mk_valuation ["a",False], True);
   (f,mk_valuation ["a",True ], False)]

let logical_implies_testcases =
  let f = Implies(Variable("a"),Variable("b")) in
  [(f,mk_valuation ["a",False;"b",False],  True);
   (f,mk_valuation ["a",False;"b",True ],  True);
   (f,mk_valuation ["a",True; "b",False], False);
   (f,mk_valuation ["a",True; "b",True],   True)]

let logical_iff_testcases =
  let f = Iff(Variable("a"),Variable("b")) in
  [(f,mk_valuation ["a",False;"b",False],  True);
   (f,mk_valuation ["a",False;"b",True ], False);
   (f,mk_valuation ["a",True; "b",False], False);
   (f,mk_valuation ["a",True; "b",True],   True)]

let logical_ite_testcases =
  let f = logical_ite (Variable("a")) (Variable("b")) (Variable("c")) in
  [(f,mk_valuation ["a",False;"b",False;"c",False], False);
   (f,mk_valuation ["a",False;"b",False;"c", True],  True);
   (f,mk_valuation ["a",False;"b", True;"c",False], False);
   (f,mk_valuation ["a",False;"b", True;"c", True],  True);
   (f,mk_valuation ["a", True;"b",False;"c",False], False);
   (f,mk_valuation ["a", True;"b",False;"c", True], False);
   (f,mk_valuation ["a", True;"b", True;"c",False],  True);
   (f,mk_valuation ["a", True;"b", True;"c", True],  True);]

let do_tests =
  let rec aux = function
  | (expr,valuation,res)::xs -> 
    if (eval valuation expr) <> res
    then
     (let _ = 
        Printf.printf "[PL] [E] Test failed: %s\nUnder valuation:\n" (string_of_prop expr);
        print_valuation valuation;
        Printf.printf "Failed to return expected value %s\n" (string_of_tval res)
      in
      failwith "failed test")
    else
      aux xs
  | [] -> Printf.printf "[PL] [I] All tests passed\n"
  in aux

let _ = 
  do_tests 
   (logical_and_testcases     @
    logical_or_testcases      @
    logical_not_testcases     @
    logical_implies_testcases @
    logical_iff_testcases     @
    logical_ite_testcases)

let rec syntactic_simplify e = match e with
| Constant(_)
| Variable(_) -> e
| And(Constant(False),_)
| And(_,Constant(False)) -> Constant(False)
| And(Constant(True),o)
| And(o,Constant(True))  -> syntactic_simplify o
| And(l,r) -> 
  let ls,rs = syntactic_simplify l,syntactic_simplify r in
  if ls <> l || rs <> r
  then syntactic_simplify(And(ls,rs))
  else e
| Or(Constant(True),o)
| Or(o,Constant(True))   -> Constant(True)
| Or(Constant(False),o)
| Or(o,Constant(False))  -> syntactic_simplify o
| Or(l,r) -> 
  let ls,rs = syntactic_simplify l,syntactic_simplify r in
  if ls <> l || rs <> r
  then syntactic_simplify(Or(ls,rs))
  else e
| Not(o) -> (match syntactic_simplify o with
  | Constant(False) -> Constant(True)
  | Constant(True)  -> Constant(False)
  | os -> Not(os))
| Implies(a,c) ->
  let az = syntactic_simplify a in
 (match az with
  | Constant(False) -> Constant(True)
  | Constant(True)  -> syntactic_simplify c
  | _ -> (match syntactic_simplify c with
    | Constant(True)  -> Constant(True)
    | Constant(False) -> Not(az)
    | cs -> if az <> a || cs <> c 
      then syntactic_simplify(Implies(az,cs))
      else e))
| Iff(l,r) -> let ls,rs = syntactic_simplify l,syntactic_simplify r in
  match ls,rs with
  | Constant(True), Constant(True)
  | Constant(False),Constant(False) -> Constant(True)
  | Constant(True), Constant(False)
  | Constant(False),Constant(True)  -> Constant(False)
  | Constant(True) ,x
  | x,Constant(True)                -> x
  | Constant(False),x
  | x,Constant(False)               -> Not(x)
  | x,y                             -> 
    if x <> l || y <> r 
    then syntactic_simplify(Iff(x,y))
    else e
