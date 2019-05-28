open PL

(* New intermediate variable producer *)
let f_newvar_with_prefix prefix =
  let counter = ref 0 in
 (fun () -> 
    let i = !counter in
    incr counter;
    Variable(prefix^string_of_int i))

let random_newvar = f_newvar_with_prefix "_rnd#"

let generate_random_prop n_depth =
  let rt () = Random.int 8 in
  let rec aux n = function
  | 0 -> (Constant(True),[])
  | 1 -> (Constant(False),[])
  | 2 -> let v = random_newvar () in (v,[v])
  | 3 -> 
    let le,lv = aux (n-1) (rt ()) in
    let re,rv = aux (n-1) (rt ()) in
   (Or(le,re),cat lv rv)
  | 4 ->
    let le,lv = aux (n-1) (rt ()) in
    let re,rv = aux (n-1) (rt ()) in
   (And(le,re),cat lv rv)
  | 5 ->
    let ce,cv = aux (n-1) (rt ()) in
   (Not(ce),cv)
  | 6 ->
    let le,lv = aux (n-1) (rt ()) in
    let re,rv = aux (n-1) (rt ()) in
   (Implies(le,re),cat lv rv)
  | 7 ->
    let le,lv = aux (n-1) (rt ()) in
    let re,rv = aux (n-1) (rt ()) in
   (Iff(le,re),cat lv rv)
  | _ -> failwith "impossible lkjtkjsagt"
  in
  aux n_depth (rt ())

