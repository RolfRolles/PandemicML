let rec pf_to_nnf = function
| Not(Not(p))          -> pf_to_nnf p
| Not(Constant(True))  -> False
| Not(Constant(False)) -> True
| Not(And(f1,f2))      -> Or (pf_to_nnf (Not(f1)), pf_to_nnf (Not(f2)))
| Not(Or(f1,f2))       -> And(pf_to_nnf (Not(f1)), pf_to_nnf (Not(f2)))
| Implies(f1,f2)       -> Or (pf_to_nnf (Not(f1)), pf_to_nnf f2)
| Iff(f1,f2)           -> And(pf_to_nnf (Implies(f1,f2)), pf_to_nnf (Implies(f2,f1)))

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

