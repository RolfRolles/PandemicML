(* PLTVLAI.ml *)

type abstract_bit = 
| AbsZero
| AbsHalf
| AbsOne

(* Debugging functions *)
let string_of_abstract_bit = function
| AbsZero -> "0"
| AbsOne  -> "1"
| AbsHalf -> "1/2"

(* Convert an abstract bit into an integer, so we can index them into tables *)
let int_of_abstract_bit = function
| AbsZero -> 0
| AbsOne  -> 2
| AbsHalf -> 1

(* Abstract version of and.  Works like you'd expect. *)
let abstract_and_table = 

            (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsZero; AbsZero; AbsZero |];
(*1/2*)  [| AbsZero; AbsHalf; AbsHalf |];
(* 1 *)  [| AbsZero; AbsHalf; AbsOne  |]|]

let abstract_and l r = abstract_and_table.(int_of_abstract_bit l).(int_of_abstract_bit r)

(* Abstract version of or.  Works like you'd expect. *)
let abstract_or_table = 

            (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsZero; AbsHalf; AbsOne  |];
(*1/2*)  [| AbsHalf; AbsHalf; AbsOne  |];
(* 1 *)  [| AbsOne;  AbsOne ; AbsOne  |]|]

let abstract_or l r = abstract_or_table.(int_of_abstract_bit l).(int_of_abstract_bit r)

(* Abstract version of not.  Works like you'd expect. *)
let abstract_not_table =
   (* 0 *)  (*1/2*)  (* 1 *)
[| AbsOne ; AbsHalf; AbsZero |]

let abstract_not e = abstract_not_table.(int_of_abstract_bit e)

(* Abstract version of and.  Works like you'd expect. *)
let abstract_implies_table = 

            (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsOne;  AbsOne;  AbsOne |];
(*1/2*)  [| AbsHalf; AbsHalf; AbsOne |];
(* 1 *)  [| AbsZero; AbsHalf; AbsOne |]|]

let abstract_implies l r = abstract_implies_table.(int_of_abstract_bit l).(int_of_abstract_bit r)


(* Abstract version of and.  Works like you'd expect. *)
let abstract_iff_table = 

            (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsOne;  AbsHalf; AbsZero |];
(*1/2*)  [| AbsHalf; AbsHalf; AbsHalf  |];
(* 1 *)  [| AbsZero; AbsHalf; AbsOne  |]|]

let abstract_iff l r = abstract_iff_table.(int_of_abstract_bit l).(int_of_abstract_bit r)

let abstract_lookup valuation n =
  let res =
    try  Hashtbl.find valuation n
    with Not_found -> AbsHalf
  in res

let bitwise_ai_prop form valuation =
  let open PL in
  let t = Constant(True) and f = Constant(False) in
  let const_of_abstract_bit g l r = function
  | AbsOne  -> t
  | AbsZero -> f
  | AbsHalf -> g l r
  in
  let rewrite_and l r = match (l,r) with
  | Constant( True),e | e,Constant( True) -> e
  | Constant(False),e | e,Constant(False) -> f
  | _,_                                   -> And(l,r)
  in
  let rewrite_or  l r = match (l,r) with
  | Constant( True),e | e,Constant( True) -> t
  | Constant(False),e | e,Constant(False) -> e
  | _,_                                   -> Or(l,r)
  in
  let rewrite_iff l r = match (l,r) with
  | Constant( True),e | e,Constant( True) -> e
  | Constant(False),e | e,Constant(False) -> Not(e)
  | _,_                                   -> Iff(l,r)
  in
  let rec aux e = match e with
  | Constant(True)  -> (e,AbsOne)
  | Constant(False) -> (e,AbsZero)
  | Variable(n)     -> (e,abstract_lookup valuation n)
  | And(l,r)        -> abstract_binop abstract_and     l r rewrite_and
  | Or(l,r)         -> abstract_binop abstract_or      l r rewrite_or
  | Implies(l,r)    -> abstract_binop abstract_implies l r (fun l r -> Implies(l,r))
  | Iff(l,r)        -> abstract_binop abstract_iff     l r rewrite_iff
  | Not(e)          -> 
    let tl,vl = aux e in
    let vres = abstract_not vl in
    let tres = match vres with
    | AbsOne  -> t
    | AbsZero -> f
    | AbsHalf -> Not(tl)
    in (tres,vres)
    
  and abstract_binop f l r g =
    let tl,vl = aux l in
    let tr,vr = aux r in
    let vres = f vl vr in
    (const_of_abstract_bit g tl tr vres,vres)
  
  in aux form
