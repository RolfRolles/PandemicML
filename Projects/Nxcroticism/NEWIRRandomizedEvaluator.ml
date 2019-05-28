(* Evaluator functionality.  This function is factored out so it can be used
   elsewhere. *)
let write_mem memctx a32 v32 s =
  let n = (IRUtil.bits s)/8 in
  let rec aux v32 i =
    if i = n
    then ()
    else
      let addr = Int32.add a32 (Int32.of_int i) in
      Hashtbl.replace memctx addr (Int32.logand v32 0xffl);
      aux (Int32.shift_right_logical v32 8) (i+1)
  in aux v32 0

let read_mem memctx a32 s fgen =
  let n = (IRUtil.bits s)/8 in
  let rec aux c i =
    if i < n
    then 
      let addr = Int32.add a32 (Int32.of_int i) in
      let byte = 
        try Hashtbl.find memctx addr with Not_found ->
          let new_byte = fgen addr in 
          Hashtbl.replace memctx addr new_byte;
          new_byte
      in
      let c = Int32.logor (Int32.shift_left byte (i*8)) c in
      aux c (i+1)
    else c
  in
  aux 0l 0

let mk_mem_funs_ext memctx rndgen = 
  let reads  = ref [] in
  let writes = ref [] in

  let mem_read a s =
    let v = read_mem memctx a s rndgen in
    reads := (a,v,s)::(!reads);
    v
  in

  let mem_write a t s =
    write_mem memctx a t s;
    writes := (a,t,s)::(!writes);
    ()
  in
  (mem_read,reads,mem_write,writes,memctx)

let rndgen _ = Int32.succ (Random.int32 0xFFl)
let mk_mem_funs () = mk_mem_funs_ext (Hashtbl.create 30) rndgen
  
(* Given:

   ir: a list of IR statements.  Not in SSA form, and no jumps.
   regctx: (IR.var,IR.expr) Hashtbl.t, the register context
   memctx: (int32,int32) Hashtbl.t, the memory context
   
   Evaluate the statements in the given contexts, computing information about 
   the memory behavior of the sequence, as described below:
   
   Returns a pair (and modifies regctx/memctx): 
   reads:  (int32 address,int32 value, int32 size) list
   writes:  (int32 address,int32 value, int32 size) list
*)
let eval_stmts ir regctx = 
  let fr,r,fw,w,_ = mk_mem_funs () in
  NEWIREval.stmts ir regctx fr fw;
 (!r,!w)
