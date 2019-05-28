(* 
#use "c:\\paframework\\Incubator\\StridedInterval.ml";;
*)

exception Unimplemented

type strided_interval =
{ lb: int64;
  ub: int64;
  s:  int64;
}

let singleton i = { s = 0L; lb = i; ub = i }
let is_canonical_singleton si = match si.s with | 0L when si.lb = si.ub -> true | _ -> false

let string_of_strided_interval si = Printf.sprintf "%Lu[%LX,%LX]" si.s si.lb si.ub

(* Sign extend a smaller value to a signed int64 *)
let sign_extend k x =
  if k = 64 
  then x
  else let y = 64-k in Int64.shift_right (Int64.shift_left x y) y

let high_bit k = 
  let _ = assert(k <> 0) in (* 0-bit strided interval?  No thanks *)
  Int64.shift_left 1L (k-1)

let make_top k = 
  let highbit = high_bit k in
{ lb = sign_extend k highbit; ub = Int64.pred highbit; s = 1L; }

(* Cannibalized from IRLocalOpt.ml *)
let int64_ucomp c clhs crhs = 
  if clhs >= 0L && crhs >= 0L then c clhs crhs else
  if clhs <  0L && crhs >= 0L then true else
  if crhs <  0L && clhs >= 0L then false else
  not (c clhs crhs)

let int64_ult = int64_ucomp ( <  )
let int64_ule = int64_ucomp ( <= )

let min a b = if a < b then a else b
let max a b = if a > b then a else b

(* OCaml implements signed division.  We treat the stride as an unsigned 
   quantity, hence we need unsigned divisions while performing the GCD 
   computation.  The GCD computation actually requires an unsigned modulus
   operator, which we simulate with division, multiplication, and subtraction
   in the obvious way. 
   
   The "signed short division" algorithm comes from Hacker's Delight 9.3. *)
let int64_udiv x y = 
  if y < 0L 
  then
    if int64_ult x y
    then 0L
    else 1L
  else
    let q = Int64.shift_left (Int64.div (Int64.shift_right_logical x 2) y) 1 in
    let r = Int64.sub x (Int64.mul q y) in
    if int64_ule y r
    then Int64.succ q
    else q

let int64_umod x y =
  let quotient = int64_udiv x y in
  Int64.sub x (Int64.mul quotient y)

(*
  The Euclidean algorithm, from wikipedia:
  function gcd(a, b)
      if b = 0
         return a
      else
         return gcd(b, a mod b)
*)
let rec int64_gcd x y = 
  if y = 0L
  then x
  else int64_gcd y (int64_umod x y)

let add k si1 si2 = 
  let a = si1.lb and b = si1.ub and c = si2.lb and d = si2.ub in
  let lbound = Int64.add a c and ubound = Int64.add b d in
  let highbit = high_bit k in
  let is_negative value = Int64.logand highbit value <> 0L in
  let is_positive value = Int64.logand highbit value == 0L in
  let neg2check a b c = is_negative a && is_negative b && is_positive c in
  let pos2check a b c = is_positive a && is_positive b && is_negative c in
  let u = (neg2check a c lbound) && not (neg2check b d ubound) in
  let v = (not ((pos2check a c lbound) || (neg2check a c lbound))) && (pos2check b d ubound) in
  if u || v
  then make_top k
  else { lb = lbound; ub = ubound; s = int64_gcd si1.s si2.s; }

let neg k si =
  let highbit = high_bit k in
  match si.lb,si.ub with
  | h1,h2 when h1 = h2 && h1 = highbit -> { lb = highbit; ub = highbit; s = 0L }
  | c,_ when c <> highbit -> { lb = Int64.neg si.ub; ub = Int64.neg si.lb; s = si.s }
  | _,_ -> make_top k
  
let sub k si1 si2 = add k si1 (neg k si2)
  
(*
let s1 = { lb = 2L; ub = 8L; s = 3L } in
let s2 = { lb = 10L; ub = 15L; s = 5L } in
add_SIs 32 s1 s2;;
*)

let (-%) = Int64.sub;;
let (|%) = Int64.logor;;
let (&%) = Int64.logand;;
let bneg = Int64.neg;;
let bnot = Int64.lognot;;

let ntz x =
  let y = bneg x &% (x -% 1L) in
  let rec bits n y = if y = 0L then n else bits (n+1) (Int64.shift_right y 1) in
  bits 0 y

let minOR k a b c d = 
  let rec loop m =
    let cont () = loop (Int64.shift_right_logical m 1) in
    if m = 0L then a else
    if (bnot a) &% c &% m <> 0L
    then
      let temp = (a |% m) &% (bneg m) in
      if int64_ule temp b
      then temp
      else cont ()
    else if a &% (bnot c) &% m <> 0L
    then
      let temp = (c |% m) &% (bneg m) in
      if int64_ule temp d 
      then temp
      else cont ()
    else
      cont ()
  in
  let a = loop (high_bit k) in
  a |% c

let maxOR k a b c d = 
  let rec loop m =
    let cont () = loop (Int64.shift_right_logical m 1) in
    if m = 0L then b |% d else
    if b &% d &% m <> 0L
    then
      let temp = (b -% m) &% (m -% 1L) in
      if int64_ult a temp
      then temp |% d
      else 
        let temp = (d -% m) &% (m -% 1L) in
        if int64_ult c temp
        then b |% temp
        else cont ()
    else cont ()
  in 
  loop (high_bit k)

let logor k si1 si2 =
  (* This two-line special case is critical. *)
  if is_canonical_singleton si1 && is_canonical_singleton si2
  then singleton (Int64.logor si1.ub si2.ub)
  
  (* Else perform regular logor *)
  else
  let t = min (ntz si1.s) (ntz (si2.s)) in
  let s = Int64.shift_left 1L t in
  let mask = Int64.pred s in
  let a = si1.lb and b = si1.ub and c = si2.lb and d = si2.ub in
  let r = (a &% mask) |% (c &% mask) in
  let lb,ub = match a < 0L, b < 0L, c < 0L, d < 0L with
  | true,  true,  true,  true  -> (minOR k a b c d, maxOR k a b c d)
  | true,  true,  true,  false -> (a, -1L)
  | true,  true,  false, false -> (minOR k a b c d, maxOR k a b c d)
  | true,  false, true,  true  -> (c, -1L)
  | true,  false, true,  false -> (min a c, maxOR k 0L b 0L d)
  | true,  false, false, false -> (minOR k a (-1L) c d, maxOR k 0L b c d)
  | false, false, true,  true  -> (minOR k a b c d, maxOR k a b c d)
  | false, false, true,  false -> (minOR k a b c (-1L), maxOR k a b 0L d)
  | false, false, false, false -> (minOR k a b c d, maxOR k a b c d)
  | b1,b2,b3,b4 -> 
    let b2i = function | false -> 0 | true -> 1 in
    let ri = ((b2i b1) lsl 3) lor ((b2i b2) lsl 2) lor ((b2i b3) lsl 1) lor ((b2i b4) lsl 0) in
    failwith ("logor:  supposedly impossible case "^(string_of_int ri))
  in
  let nmask = bnot mask in
  { lb = (lb &% nmask) |% r; ub = (ub &% nmask) |% r; s = s; }
  
let lognot k si = { si with lb = bnot si.ub; ub = bnot si.lb }

let logand k si1 si2 = lognot k (logor k (lognot k si1) (lognot k si2))
let logxor k si1 si2 = 
  let lhs = lognot k (logor k (lognot k si1)  si2          ) in
  let rhs = lognot k (logor k  si1           (lognot k si2)) in
  logor k lhs rhs
  
let zero      = singleton 0L
let one       = singleton 1L
let minus_one = singleton (-1L)

let test n print_granularity =
  let ri64 () = Random.int64 Int64.max_int in
  let si i = singleton i in
  let k = 64 in
  let get_single_result_opt si = match si.s with | 0L -> Some(si.lb,si.lb=si.ub) | _ -> None in

  let test_abstract_binop absbinop concbinop =
    let lhs,rhs = ri64(),ri64() in
    let abslhs,absrhs = si lhs, si rhs in
    let absresult  = absbinop k abslhs absrhs in
    let concresult = concbinop lhs rhs in
    match get_single_result_opt absresult with
    | Some(i,true)  -> i = concresult
    | Some(_,false) -> IDA.msg "Abstract binop result was not singleton\n"; false
    | None -> 
      IDA.msg 
        "Abstract binop stride result was not zero:  %s op %s = %s (conc. %Lu)" 
       (string_of_strided_interval abslhs)
       (string_of_strided_interval absrhs)
       (string_of_strided_interval absresult)
        concresult;
      false
  in
  let test_abstract_unop absunop concunop =
    let lhs = ri64() in
    match get_single_result_opt (absunop k (si lhs)) with
    | Some(i,true)  -> i = (concunop lhs)
    | Some(_,false) -> false
    | None -> false
  in
  for i = 0 to (n-1) do
    if i mod print_granularity = 0 then IDA.msg "%s\n" ("Done "^string_of_int i^" tests");
    if not (test_abstract_binop logxor  Int64.logxor             ) then failwith "abstract xor failed"; (* 10,000,000 passed *)
    if not (test_abstract_binop logand  Int64.logand             ) then failwith "abstract and failed"; (* 10,000,000 passed *)
    if not (test_abstract_binop logor   Int64.logor              ) then failwith "abstract or failed" ; (* 10,000,000 passed *)
    if not (test_abstract_binop add     Int64.add                ) then failwith "abstract add failed"; (* 10,000,000 passed *)
    if not (test_abstract_binop sub     Int64.sub                ) then failwith "abstract sub failed"; (* 10,000,000 passed *)
    if not (test_abstract_unop  neg     Int64.neg                ) then failwith "abstract neg failed"; (* 10,000,000 passed *)
    if not (test_abstract_unop  lognot  Int64.lognot             ) then failwith "abstract not failed"; (* 10,000,000 passed *)
  done;
  IDA.msg "[I] All tests passed!"