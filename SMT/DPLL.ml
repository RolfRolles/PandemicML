type variable = int

type blah = 
| Positive of variable 
| Negative of variable

let polarity_of_blah = function | Positive(_) -> 1 | Negative(_) -> 0
let variable_of_blah = function | Positive(x) | Negative(x) -> x

type blee =
| Unassigned
| True
| False

type bloo =
| Zero
| One of variable
| Multiple

let top prop_formula = 
  let a_cnf_clauses,a_var_clausenum,m_new_old,l_single_elements = CNF.convert prop_formula in
  let num_satisfied_clauses = ref 0 in
  
  let current_assignment = Array.make (fun _ -> Unassigned) num_vars in
  let set first_literal l_worklist clauseno =
    let rec aux bloo = function
    | lit::lits
      (* If the literal is present in its same polarity, the clause is satisfied *)
      if first_literal = lit
      then (true,bloo)
      (* If the literal is present in the opposite polarity, ignore it *)
      else (if variable_of_blah lit = clauseno        
        then aux bloo lits
        (* Otherwise, the literal in the clause is not the same variable as the
           last assertion. *)
        else 
         (match current_assignment.(variable_of_blah lit) with
          (* If the literal is unassigned, it may be implied by BCP (if only one
             such unassigned literal remains). *)
          | Unassigned -> let bloo = match bloo with
            | Zero   -> One(lit)
            | One(_)
            | Multiple -> None
            in aux bloo 
          (* If the literal is assigned to true, and it's a positive literal, the clause is satisfied *)
          | True  -> if polarity_of_literal lit == 1 then (true,bloo) else aux bloo lits
          (* If the literal is assigned to false, and it's a negative literal, the clause is satisfied *)
          | False -> if polarity_of_literal lit == 0 then (true,bloo) else aux bloo lits))
    | [] -> (false,bloo)
    in
    let satisfied,res = aux Zero a_cnf_clauses.(clauseno) in
    if satisfied
    then l_single
    else match res with
    | None -> raise BCPConflict
    | Multiple -> l_single
    | One(l) -> l::l_single)
  in
  let rec bcp l_out = function
  | x::xs ->
    let clauses = 
      try IntMap.find m_var_clausenum x
      with Not_found -> failwith "bcp:  variable not in clause map"
    in
    let new_xs = List.fold_left (set x) xs clauses in
    bcp (x::l_out) new_xs
  | [] -> ()
  in
  let bcp = bcp [] in
  let _ = 
    try bcp l_single_elements
    with BCPConflict -> raise Unsatisfiable
  in
  let decide () = raise Unimplemented in
  let analyze_conflict () = raise Unimplemented in
  let backtrack bl = 
    let _ = ignore(dl := bl) in
    raise Unimplemented
  in
    
  let rec loop decisions_and_implications = 
    let lit = 
      try decide ()
      with NoMoreUnassignedLiterals -> raise Satisfiable(current_assignment)
    in
    let implied_variables = 
      try bcp [lit]
      with BCPConflict -> 
        if !dl == 0
        then raise Unsatisfiable
        else 
          let backtrack_level = analyze_conflict () in
          if backtrack_level < 0 
          then raise Unsatisfiable
          else (
            let reset_decisions = backtrack backtrack_level in
            loop reset_decisions)
    in
    incr dl;
    loop ((dl,(lit,implied_variables))::decisions_and_implications)
  in
  loop []