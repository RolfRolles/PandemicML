let f_tptime = ref 0.0

let verify ir_ssa pc = 
  let before = Sys.time () in
  let z3ctx = Z3.V3.mk_context_x [|("MODEL","true");("SOFT_TIMEOUT","5000")|] in
  let _ = Z3.V3.set_logic z3ctx "QF_ABV" in
  let str = Z3SymbolicExecute.symbolic_execute z3ctx ir_ssa pc in
  let res = if str = "unsat\n" 
    then false
    else (if Z3.V3.get_search_failure z3ctx = Z3.V3.TIMEOUT 
      then Printf.printf "Timed-out query: %s%!\n" (Z3.V3.context_to_string z3ctx);
      true)
  in
  Z3.V3.del_context z3ctx;
  let delta = (Sys.time () -. before) in
  f_tptime := !f_tptime +. delta;
  Printf.printf "Verified in %f: %s was %b\n" delta (PpIR.ppExpr false pc) res;
  Printf.printf "Query verified in %f\n" delta;
(*Printf.printf "Model: %s\n" str;*)
  res