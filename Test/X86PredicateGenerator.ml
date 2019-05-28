open X86Predicate

exception Lapsed

class virtual ['a] generator (name:string) default_list = (* ' *)
  object(self)
    (* Probably needs to be initialized in the constructor *)

    val name = name
    method get_name () = name

    val mutable current_list = ( default_list : 'a list ) (*'*)
    val default_list = ( default_list : 'a list ) (*'*)

    val mutable cached_yield = ( None : expr option )

    method virtual increase_children : unit -> unit
    method virtual refresh_yield : unit -> unit
    
    (* Need to think about exception behavior *)
    method increase () = 
      cached_yield <- None; 
      match current_list with
      | []  ->  (try self#increase_children () with Lapsed -> raise Lapsed)
      | [x] ->   current_list <- default_list; 
       (try self#increase_children () with Lapsed -> raise Lapsed)
      | x::xs -> current_list <- xs
    
    method yield () =
      match cached_yield with
      | Some(y) -> y
      | None -> self#refresh_yield (); self#yield ()
  end;;

class atom_generator name default_list = (* ' *)
  object(self)
    constraint 'a = expr (* ' *)
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = raise Lapsed
    method refresh_yield () = 
      cached_yield <- Some(List.hd current_list)
  end;;

class virtual ['a] unop_generator name default_list child = (* ' *)
  object(self)
    val child = child
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = 
      try child#increase ()
      with Lapsed ->
        raise Lapsed
  end;;

class flag_unop_generator name default_list child = 
  object(self) 
    constraint 'a = bitop
    inherit ['a] unop_generator name default_list child  

    method refresh_yield () = 
      cached_yield <- Some(Bitop(List.hd current_list,child#yield ()))
  end;;

class  reg_unop_generator name default_list child = 
  object(self) 
    constraint 'a = unop  
    inherit ['a] unop_generator name default_list child 

    method refresh_yield () = 
      cached_yield <- Some(Unop(List.hd current_list,child#yield ()))
  end;;

class binop_generator name default_list lchild rchild = (* ' *)
  object(self)
    val lchild = lchild (*( lchild : ['b] generator ) (* ' *)*)
    val rchild = rchild (*( rchild : ['c] generator ) (* ' *)*)
    constraint 'a = binop
    inherit ['a] generator name default_list (* ' *)
    method increase_children () = 
      try lchild#increase ()
      with Lapsed ->
        try rchild#increase ()
        with Lapsed ->
          raise Lapsed

    method refresh_yield () = 
      cached_yield <- Some(Binop(lchild#yield (),List.hd current_list,rchild#yield ()))
  end;;

class ite_generator name bchild tchild fchild = (* ' *)
  object(self)
    val bchild = bchild (*( lchild : ['b] generator ) (* ' *)*)
    val tchild = tchild (*( rchild : ['c] generator ) (* ' *)*)
    val fchild = fchild (*( rchild : ['c] generator ) (* ' *)*)
    constraint 'a = binop
    inherit ['a] generator name [] (* ' *)
    method increase_children () = 
      try bchild#increase ()
      with Lapsed ->
        try tchild#increase ()
        with Lapsed ->
          try fchild#increase ()
          with Lapsed ->
            raise Lapsed

    method refresh_yield () = 
      cached_yield <- Some(ITE(bchild#yield (),tchild#yield (),fchild#yield ()))
  end;;

class cast_generator name default_list size child = 
  object(self) 
    constraint 'a = ext
    inherit ['a] unop_generator name default_list child  

    method refresh_yield () = 
      cached_yield <- Some(Extend(List.hd current_list,size,child#yield ()))
  end;;

class sequential_generator l_gens =
  object(self)
    val mutable list = (l_gens :> (esize * 'a generator) list) (* ' *)

    method increase_current () =
      match list with
      | [_,x]     -> (try (x#increase ();false) with Lapsed -> raise Lapsed)
      | (_,x)::((_,y)::_ as xs) -> 
        (try (x#increase ();false)
         with Lapsed -> 
           list <- xs; 
         (*Printf.printf "Increased generator to %s\n%!" (y#get_name ());*)
           true)
      | []        -> failwith "impossible 2"

    method yield_current () =
      match list with
      | (s,x)::xs -> (s,x#yield ())
      | []    -> failwith "impossible 3"
  end;;

let generator_of_expr expr regs imms =
  let rec aux expr = 
    let s = string_of_expr expr in 
    match expr with
    | X86Reg(X86.Gd(_)) -> ( new atom_generator s regs.val32 :> 'a generator ) (* ' *)
    | X86Reg(X86.Gw(_)) -> new atom_generator s regs.val16
    | X86Reg(X86.Gb(_)) -> new atom_generator s regs.val8
    | X86Flag(_)        -> new atom_generator s regs.val1

    | Imm(X86.Id(_)) -> new atom_generator s imms.val32
    | Imm(X86.Iw(_)) -> new atom_generator s imms.val16
    | Imm(X86.Ib(_)) -> new atom_generator s imms.val8
    | BitImm(_)      -> new atom_generator s imms.val1
    
    | Binop(l,Add,r)  -> ( new binop_generator s l_binop (aux l) (aux r) :> 'a generator ) (* ' *)
    | Binop(l,Xor,r)  -> ( new binop_generator s log_binops (aux l) (aux r) :> 'a generator ) (* ' *)
    | Binop(l,Shl,r)  -> ( new binop_generator s shift_binops (aux l) (aux r) :> 'a generator ) (* ' *)
    | Binop(l,Eq,r)   -> ( new binop_generator s bit_binops (aux l) (aux r) :> 'a generator ) (* ' *)
    | Unop(Inc,e)     -> ( new reg_unop_generator s l_unop (aux e) :> 'a generator ) (* ' *)
    | Unop(Not,e)     -> ( new reg_unop_generator s log_unops (aux e) :> 'a generator ) (* ' *)
    | Bitop(_,e)      -> ( new flag_unop_generator s l_bitop (aux e) :> 'a generator ) (* ' *)
    | ITE(b,t,f)      -> ( new       ite_generator s (aux b) (aux t) (aux f) :> 'a generator ) (* ' *)
    | Extend(Unsigned,z,e)  -> ( new cast_generator s l_ext_larger z (aux e) :> 'a generator ) (* ' *)
    | Extend(Low,z,e) -> ( new cast_generator s l_ext_smaller z (aux e) :> 'a generator ) (* ' *)
    | _ -> failwith ("generator_of_expr: error in %s"^(string_of_expr expr))
    
  in aux expr
  
let generators list q_regs q_imms =
  List.map (fun (s,e) -> (s,generator_of_expr e q_regs q_imms)) list
  
let seq_generator list q_regs q_imms = 
  new sequential_generator (generators list q_regs q_imms)

