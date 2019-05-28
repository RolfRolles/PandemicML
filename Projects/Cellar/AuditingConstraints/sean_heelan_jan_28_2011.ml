(* 
   (7:57:21 PM) Sean Heelan: Basically I have 3 variables, x, y and z. I 
   control x (it's the length of a string), y is x*4 and z must be >= 256KB. If 
   (y % 4096) is not 0 then x must be at least (Y - y) bytes bigger than y, 
   where Y, is the next multiple of 4096 bigger than y. Otherwise if (y % 4096) 
   is 0 then x must simply be greater than y. Finally, x <= y + z must hold. 
   (8:10:53 PM) Sean Heelan: Oh, the requirement is to minimise x + y + z. 
   Forgot to mention that bit I think.
*)
let check_sean_heelan_jan_28_2011 () = 
  let ctx = Z3.mk_context_x [|("MODEL", "true")|] in
  let bv32sort = Z3.mk_bv_sort ctx 32 in
  let mk_bv32 name = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) bv32sort in
  
  let ascnames = [|"xp";"yp";"zp"|] in
  let z3names  = Array.map (Z3.mk_string_symbol ctx) ascnames in
  let z3sorts  = Array.map (fun _ -> bv32sort) z3names in
  (* Bind the universally-quantified variables in the ordering induced by the de Bruijn indices *)
  let z3vars   = Array.init 3 (fun j->Z3.mk_bound ctx j (z3sorts.(j))) in
  
  let x, y, z  = mk_bv32 "x", mk_bv32 "y", mk_bv32 "z" in
  let xp,yp,zp = z3vars.(0),  z3vars.(1),  z3vars.(2)  in
  
  let mk_basic_constraints x y z = 
    let z3num_of_int   i = Z3.mk_numeral ctx (Printf.sprintf "%d"  i) bv32sort in
    let z3num_of_int32 i = Z3.mk_numeral ctx (Printf.sprintf "%ld" i) bv32sort in
    let yeq = Z3.mk_eq ctx y (Z3.mk_bvadd ctx (Z3.mk_bvmul ctx x (z3num_of_int 4)) (z3num_of_int 1024)) in
    let zlt = Z3.mk_bvsle ctx (z3num_of_int 0x40000) z in
    let xlt = Z3.mk_bvsle ctx x (Z3.mk_bvadd ctx y z) in
    let ylt = Z3.mk_bvsle ctx (Z3.mk_bvand ctx (Z3.mk_bvadd ctx y (z3num_of_int 0xfff)) (z3num_of_int32 0xFFFFF000l)) x in
    Z3.mk_and ctx [|yeq;zlt;xlt;ylt|]
  in
  let existential_cnstr = mk_basic_constraints x  y  z  in
  let   universal_cnstr = mk_basic_constraints xp yp zp in
  let mk_sum3 a b c = Z3.mk_bvadd ctx a (Z3.mk_bvadd ctx b c) in
  let implies = Z3.mk_implies ctx universal_cnstr (Z3.mk_bvsle ctx (mk_sum3 x y z) (mk_sum3 xp yp zp)) in
  let universally_quantified_implication = Z3.mk_forall ctx 0 [||] z3sorts z3names implies in

  Z3.assert_cnstr ctx existential_cnstr;
  Z3.assert_cnstr ctx universally_quantified_implication;
  let (lb,m) = Z3.check_and_get_model ctx in
 (match lb with
  | Z3.L_FALSE -> Ida.msg "Constraint system is inconsistent\n"
  | Z3.L_UNDEF -> 
   (let s = 
    match Z3.get_search_failure ctx with
    | Z3.NO_FAILURE -> "NO_FAILURE"
    | Z3.UNKNOWN -> "UNKNOWN"
    | Z3.TIMEOUT -> "TIMEOUT"
    | Z3.MEMOUT_WATERMARK -> "MEMOUT_WATERMARK"
    | Z3.CANCELED -> "CANCELED"
    | Z3.NUM_CONFLICTS -> "NUM_CONFLICTS"
    | Z3.THEORY -> "THEORY"
    | Z3.QUANTIFIERS -> "QUANTIFIERS"
    in
    Ida.msg "Search failed because %s\n" s)
  | Z3.L_TRUE  -> 
   (Ida.msg "There exists a satisfying assignment which minimizes the sum of x,y,z:\n";
    Ida.msg "%s\n" (Z3.model_to_string ctx m)));
  Z3.del_model ctx m;
  Z3.del_context ctx


