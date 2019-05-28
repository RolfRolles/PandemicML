(*
use_list -- those things which are used in any instruction in the function; in the present case, this is trivial: it's the registers, memory, and flags.
num_uses -- length of the above
use_to_num_hash -- hashtable mapping each use to the number that represents it
num_to_use_hash -- hashtable mapping each use from the use list to an integer, starting at 0
var_to_uses_hash -- hashtable mapping each variable to the locations at which it is used

join -- and two BVs together
meet -- or  two BVs together

make_top_bitvector    -- make a BV with num_uses bits, initialized to false
make_bottom_bitvector -- make a BV with num_uses bits, initialized to true

let do_lv_transfer (i_blockno,rchin) (x,y) =
  extract the variables used and defined in the block
  
  gen  = make_top_bitvector ()
  prsv = make_bottom_bitvector ()
  
  set all bit positions b in prsv to false, where b is:
    for all variables defined
      get the list of uses of the variable from var_to_uses_hash
      for each use, map it into the use number

  set all bit positions b in gen to true, where b is:
    for all variables used, map it into its number
  
  meet (join rchin prsv) gen
  
initial_worklist is a quasi-topological ordering of the CFG
outsets is a mapping from a vertex to a bitvector (initialized to top)
insets is a mapping from a vertex to a bitvector (initialized to top)

let rec iterate outsets insets worklst = match worklist with
| [] -> (insets,outsets)
| x::xs ->
  oldout,oldin = lookup x in the maps
  out (new outset) = union of the insets of the successors
  inp (new inset)  = the result of applying the instruction-level transfer function to each instruction in the basic block in reverse order, starting with the outset we just calculated
  
  let newworklist,outsets,insets = 
    if out <> oldout || inp <> oldin
    then (preds@xs, outsets w/ x->out, insets w/ x->inp)
    else (xs,outsets,insets)
  
  in iterate outsets insets newworklist

  let insets,outsets = iterate outsets insets initial_worklist in
*)
  (* Transform, now that we have analyzed *)
  let optutil = Optutil.create f.cfg in
  (* Given a list of operands defined by this instruction, see whether they are 
     used later on *)
  let are_all_definitions_dead deflist lvbv =
    let retvalue = ref true in
      List.iter 
        (fun (_,_,o) ->
          (* All uses of the particular variable that is defined *)
          let uselist = try Hashtbl.find var_to_uses_hash o with Not_found -> [] in
          (* Remove all uses that are not live at this point *)
          let subsequent_uses = List.filter 
            (fun x -> Bitv.get lvbv (Hashtbl.find use_to_num_hash x))
            uselist
          in 
          if subsequent_uses <> [] then retvalue := false
        )
      deflist;
    !retvalue

  in
  let determine_scr v =
    let retvalue = ref ~-1 in
    for i = 0 to (Array.length f.scr - 1) do
      if Util.IntSet.mem (C.G.V.label v) f.scr.(i)
      then retvalue := i
    done;
    !retvalue
  
  in
  let dead_statement_elimination_via_live_variables () = 
    let nse_scr = ref 0 in
    let nse     = ref 0 in
    let incr_nse v =
      if (determine_scr v) <> ~-1 
        then incr nse_scr
        else incr nse

    in
    C.G.iter_vertex 
      (fun v ->
        (* Extract the label for convenience *)
        let label = C.G.V.label v in
        (* Get the definitions that reach this basic block *)
        let outset = VertexMap.find v outsets in
        let _ = List.fold_left
          (fun set (x,y) ->
            let defs = Irutil.extract_def label (x,y) in
            let is_store = match y with Store(_,_) -> true | _ -> false in
            if (not is_store) && defs <> [] && are_all_definitions_dead defs set 
            then
              (incr_nse v;
               Optutil.set_stmt optutil label x (x,Nop));
            do_lv_transfer (label,set) (x,y)
          )
          (outset)
          (List.rev (Optutil.get_stmts optutil label))
        in ()
      )
      f.cfg;
    (!nse,!nse_scr)
  
  in 
  let eliminate_and_print () = 
    let nse, nse_scr = dead_statement_elimination_via_live_variables () in
    let new_function = { f with cfg = Optutil.update_cfg optutil } in
    (nse,nse_scr,new_function)
  
  in eliminate_and_print ()
    
let dse do_rep f =
  let rec aux f = 
    let nse,nse_scr,new_f = dse_ do_rep f in
    if nse <> 0 || nse_scr <> 0 
    then
      let nse2,nse_scr2,new_f2 = aux new_f in
      (nse+nse2,nse_scr+nse_scr2,new_f2)
    else
      (nse,nse_scr,new_f)
  
  in
  let nse,nse_scr,new_f = aux f in
    
  if do_rep
  then
    (print_endline ("Functions: "^string_of_int f.start);
    print_endline ("Number of statements eliminated in SCR: "^string_of_int nse_scr);
    print_endline ("Number of statements eliminated not in SCR: "^string_of_int nse));
  new_f

