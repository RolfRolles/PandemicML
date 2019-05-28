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

(* Abstract version of or.  Works like you'd expect. *)
let abstract_or_table = 

            (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsZero; AbsHalf; AbsOne  |];
(*1/2*)  [| AbsHalf; AbsHalf; AbsOne  |];
(* 1 *)  [| AbsOne;  AbsOne ; AbsOne  |]|]

(* Abstract version of not.  Works like you'd expect. *)
let abstract_not_table =
   (* 0 *)  (*1/2*)  (* 1 *)
[| AbsOne ; AbsHalf; AbsZero |]

(* Abstract version of implies.  Works like you'd expect. *)
let abstract_implies_table = 

                     (*RHS*)
(*LHS*)     (* 0 *)  (*1/2*)  (* 1 *)
(* 0 *)[|[| AbsOne;  AbsOne;  AbsOne |];
(*1/2*)  [| AbsHalf; AbsHalf; AbsOne |];
(* 1 *)  [| AbsZero; AbsHalf; AbsOne |]|]

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
  let const_of_abstract_bit e = function
  | AbsOne  -> Constant(True)
  | AbsZero -> Constant(False)
  | AbsHalf -> e
  in
  let replace e a = (const_of_abstract_bit e a,a) in
  
  let rec aux e = match e with
  | Constant(True)  -> (e,AbsOne)
  | Constant(False) -> (e,AbsZero)
  | Variable(n)     -> (e,abstract_lookup valuation n)
  | And(l,r)        -> 
    let (cl,al),(cr,ar) = aux l,aux r in
    let ares = 
    replace e (abstract_binop abstract_and     (aux l) (aux r))
  | Or(l,r)         -> replace e (abstract_binop abstract_or      (aux l) (aux r))
  | Implies(l,r)    -> replace e (abstract_binop abstract_implies (aux l) (aux r))
  | Iff(l,r)        -> replace e (abstract_binop abstract_iff     (aux l) (aux r))
  | Not(e)          -> replace e (abstract_not (aux e))

    let tl,vl = aux e in
    let vres = abstract_not vl in
    match vres with
    | AbsOne  -> (const_of_abstract_bit e vres)
    | AbsZero -> 
    | AbsHalf -> 
    
  and abstract_binop f l r =
    let tl,vl = aux l in
    let tr,vr = aux r in
    let tres,vres = f vl vr in
    f 
    
        
    match f with
    | And(_,_) -> abstract_and vl vr
    | Or(_,_)  -> abstract_or  vl vr
    | Implies(_,_) -> 
    let abs_binop_result = match e with
    
    match 
  | And(l,r)        -> 
    let tres,vres = 
      match (abstract_and vl vr) with
    | AbsOne as  x
    | AbsZero as x ->  
    | AbsHalf      -> (e,const_of_abstract_bit 