(* Compiles *)
(* Can be optimized:  when a set of new vertices is added to an interval, only 
   their successors need to be checked for admission to the interval; the whole
   interval need not be re-checked. *)
let intervals graph startv =
  let nb = C.G.nb_vertex graph * 2 in
  let get_preds s = C.G.fold_pred (fun v l -> v::l) graph s [] in
  let get_succs p = C.G.fold_succ (fun v l -> v::l) graph p [] in

  (* All pairs (h,l) where h is the header *)
  let all_intervals = ref [] in
  
  (* Currently-forming interval and functions for manipulating it *)
  let current_interval = ref [] and current_interval_hash = Hashtbl.create nb in
  let add_single_to_current   v  = 
    current_interval := v::(!current_interval);
    Hashtbl.replace current_interval_hash v ()
  in
  let add_multiple_to_current vl = 
    current_interval := vl@(!current_interval);
    List.iter (fun v -> Hashtbl.replace current_interval_hash v ()) vl
  in
  let is_in_current v = 
    let res =
      try ignore(Hashtbl.find current_interval_hash v); true
      with Not_found -> false
    in res
  in
  let reset_interval () =  
    current_interval := [];
    Hashtbl.clear current_interval_hash
  in
  (* Function to retrieve all successors of the current interval, where those
     successors are not already in the interval *)
  let get_noninterval_successors_of_interval () = 
    Hashtbl.fold 
     (* We only want the successors of the current vertex that are not in the
        interval already *)
     (fun v _ acc ->
       (List.filter (fun v -> not (is_in_current v)) (get_succs v))@acc)
        current_interval_hash
        []
  in

  (* Headers list and hash table (list for worklist, hashtable for lookup *)
  let headers = ref [startv] 
  and all_headers = Hashtbl.create nb in
  Hashtbl.replace all_headers startv ();

  let in_headers v = 
    let res = 
      try ignore(Hashtbl.find all_headers v); true
      with Not_found -> false
    in res
  in
  let consume_header () = 
    let x = List.hd !headers 
    and y = List.tl !headers 
    in headers := y; 
    x 
  in
  let add_headers hl = headers := hl@(!headers) in
  
  (* Main algorithm *)
  while (!headers <> [])
  do
    let n = consume_header () in
    reset_interval ();
    add_single_to_current n;

    (* Add all vertices all of whose immediate predecessors are in the set 
       already: { m \in N | \forall p \in pred(m), p \in I(n) } *)
       
    (* In other words:  for all successors of the interval, all of whose 
       immediate predecessors are in the interval already, we add those 
       vertices to the interval *)
    let rec aux () = 
      let new_interval_vertices = 
        List.filter
         (fun s -> List.for_all is_in_current (get_preds s))
         (get_noninterval_successors_of_interval ())
      in  
      add_multiple_to_current new_interval_vertices;
      match new_interval_vertices with
      | [] -> ()
      | _ -> aux ()
    in aux ();

    (* Add as headers all successor vertices of the interval, which are not
       already headers:
       already: { m \in N | m \notin H \sqcap m \notin I(n) \sqcap
       \exists p \in pred(m) s.t. p \in I(n) } *)
       
    (* In other words:  all successors of the interval which are not already
       headers get added as headers.  The exists check is for termination. *)
    let new_headers = 
      let succs = get_noninterval_successors_of_interval () in
      List.filter (fun v -> not (in_headers v)) succs
    in add_headers new_headers;
    all_intervals := (n,!current_interval)::(!all_intervals)
  done;
  !all_intervals
