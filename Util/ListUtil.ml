(* Funny probability trick here, do the math and you'll see why this algorithm
   chooses an element from the list uniformly at random, without knowing a 
   priori the size of the list:  on iteration i, the current element is chosen
   with 1/i probability.  It is mathematically correct (assuming the PRNG is 
   uniform, blah blah).  I learned this trick at a bar in Berkeley from a
   machine learning specialist. *)
let pick_random_element list =
  let rec aux i res = function
  | x::xs -> aux (i+1) (if Random.int i = 0 then Some(x) else res) xs 
  | [] -> res
  in aux 1 (None) list

(* Probably a better way to code this *)
let map_filter f_opt list =
  List.fold_left (fun acc el -> match (f_opt el) with | Some(x) -> (x::acc) | None -> acc) [] (List.rev list)

(* Given a list of lists, we produce a list of lists such that, in each of the
   resulting lists, there is one element from each of the lists in sequence.
   So if we have three lists a,b,c, and the list-of-lists [a;b;c], then we
   will have a list containing lists where there is first an element of a,
   then one from b, and finally one from c.  It covers all such combinations
   with no duplication (except whatever duplication might occur as a result
   of the input lists containing duplication themselves). 
   
   Also weird issue with the type system here.  I had originally declared
   this function as being partially-applied (i.e., with no list parameter,
   with that being an implicit parameter to the pass-through to aux), and
   I received the following compiler error:
   
   Error: The type of this expression, '_a list list -> '_a list list,
          contains type variables that cannot be generalized
          
   Investigate that.
   
   Also this 10-line function took more than four hours to code. :(
   *)
let cartesian_combinations list = 
  let rec aux combinations prefix = function
  (* If we have reached the final list from the list of lists, take the 
     elements from this final list, append each of them separately to the 
     prefix list, and reverse those lists. *)
  | last_list::[] -> List.rev_map (fun el -> List.rev (el::prefix)) last_list

  (* If there are multiple lists left, for each element in the current list,
     we call aux where:
     * the combinations list is empty (otherwise we would re-accumulate)
     * the prefix is prepended by that element
     * the list of lists has advanced forward by one
     Then we concatenate all of those lists together. *)
  | list::xs ->
    List.fold_left 
     (fun combinations list -> (aux [] (list::prefix) xs)@combinations)
      combinations 
      list
  (* This case can only happen when an empty list is passed in at the 
     top-level *)
  | [] -> []
  in aux [] [] list
