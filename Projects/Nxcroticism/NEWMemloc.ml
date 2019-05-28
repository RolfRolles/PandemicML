(* Returns Some(x) if ht[key] exists, None otherwise *)
let ht_find_opt  ht key = let res = try Some(Hashtbl.find ht key) with Not_found -> None in res

let mk_memsub r32 addr = let open IRUtil in mk_sub (mk_dword_of_int32 addr) (mk_evar r32)
let mk_memloc ht v32 addr = (v32,NEWIREval.eval_to_int32 ht (mk_memsub v32 addr))
let mk_memlocs ht addr = List.map (fun x -> mk_memloc ht x addr)

let predict_memlocs_opt l_io e_jt =
  let l_m h a = mk_memlocs h a X86ToIRUtil.l_v_general_registers32 in

  (* Return location for e_jt (None = empty list, Some(None) = conflictory, Some(m) = loc) *)
  let mout = ref None in

  (* Main loop, over I/O pairs, to convert memory addresses to locations. *)
  let rec aux b ptr ptw tr tw = function

  (* #1: collect the memory locations+size accessed by all I/O pairs. *)
  | (h,r,w)::xs ->

    (* Add (memloc,size) pairs to t.  If it's not the first time through the
       loop, then check to see whether they are in pt before adding them. *)
    let inner pt t = List.fold_left (fun first_time (a,v,s) ->
      List.iter (fun m -> if first_time || (ht_find_opt pt (m,s) <> None) then Hashtbl.replace t (m,s) ()) (l_m h a);
      first_time)
      b
    in 

    (* Process the reads and writes into their respective hash tables *)
    let _,_ = inner ptr tr r,inner ptw tw w in

    (* Rotate the hash tables, progressively filtering the memory locations. *)
    aux false tr tw (Hashtbl.create 10) (Hashtbl.create 10) xs

  (* #2: affix every memory access with its unique memory location (or raise Not_found). *)
  | [] ->
    (* Create variables for the common locations; make new hash tables with these mappings *)
    let map ht = Hashtbl.fold (fun (m,s) _ (h,l) -> Hashtbl.replace h (m,s) (IRUtil.new_var s); (h,(m,s)::l)) 
      ht 
     (Hashtbl.create 10,[]) 
    in
    let (tr,lrm),(tw,lwm) = map ptr,map ptw in
    
    (* lrw:  locations that were both read from and written to (same size). *)
    let lrwm = Hashtbl.fold (fun p r l -> match ht_find_opt tw p with | None -> l | Some(_) -> p::l) tr [] in
    
    (* Transform the I/O pairs. *)
    let rec outer outlist = function
    | (h,r,w)::xs ->
      (* Locate the unique memory location for each memory access, insert it 
         into the transformed output list.  Raise Not_found if such a location
         does not exist uniquely. *)
      let rec inner k outlist t = function
      | [] -> outlist
      | (a,v,s)::avs -> 
        let ht_expand ht k = match ht_find_opt ht k with | Some(x) -> Some(k,x) | _ -> None in

        (* Iterate through the potential memory locations, keeping only one. *)
        let rec innermost r l = match r,l with
        | None,[]       -> raise Not_found
        | Some(x),[]    -> x
        | None,m::ms    -> innermost (ht_expand t (m,s)) ms
        | Some(_),m::ms -> match ht_expand t (m,s) with | None -> innermost r ms | Some(_) -> raise Not_found 
        in
        
        (* Get the single value.  We don't get here if an exception was thrown. *)
        let (m,s),vm = innermost (None) (l_m h a) in
        k m (a,v,s);
        
        (* Insert the variable->value mappings into the main I/O pair hash table *)
        Hashtbl.replace h vm (IRUtil.mk_fixed_const_of_i32 v s);
        
        (* Transform the memory address lists. *)
        inner k ((vm,m,s)::outlist) t avs
      in

      let jt = NEWIREval.eval_to_int32 h e_jt in
      let mo = ref None in
      let keep m (a,v,s) = if s = IR.TypeReg_32 && v = jt then match !mo with 
      | None -> mo := Some(Some(m))
      | Some(None) -> () 
      | Some(Some(_)) -> mo := Some(None)
      in

      let inn = inner keep [] tr r in
     (match !mout,!mo with
      | None,x -> mout := x
      | Some(None),_ -> ()
      | Some(Some(m1)),Some(Some(m2)) when m1 = m2 -> ()
      | _,_ -> mout := Some(None));
      
      (* Transform next I/O pair. *)
      outer ((h,inn,inner (fun _ _ -> ()) [] tw w)::outlist) xs

    (* Return the new output *)
    | [] -> NEWIOStruct.({ l_io = outlist; l_reads = lrm; h_reads = tr; l_writes = lwm; h_writes = tw; l_common = lrwm; })
    in
    outer [] l_io
  in
  try 
   (let io = aux true (Hashtbl.create 10) (Hashtbl.create 10) (Hashtbl.create 10) (Hashtbl.create 10) l_io in
    match !mout with
    | None    -> failwith "predict_memlocs_opt: empty l_io"
    | Some(x) -> Some(io,x))
  with Not_found -> None
 
