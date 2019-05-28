open FrameworkUtil

let make_contrived_example gc write_size write_val write_binop addr32 = 
  let debug enc set = 
    let _ = f_printf "Testing:\n" in
    Encodable.print_encodable enc;
    f_printf "\n{";
    IRUtil.VarSet.iter (fun v -> f_printf "%s " (PpIR.ppVar v)) set;
    f_printf "}\n"
  in
  let blah = GadgetCache.get_write_binops gc write_size write_binop in
  let gadgets = 
    List.fold_left 
     (fun list (vr,vm,d32,enc,s) -> 
      let blah1 = GadgetCache.get_set_reg gc write_size vr write_val in
      let blah2 = GadgetCache.get_set_reg gc IR.TypeReg_32 vm (Int32.sub addr32 d32) in
      
      let rec aux list = function
      | (enc8,s8)::lcr8s ->
        let rec aux2 list = function
        | (enc32,s32)::lcr32s ->
          let list = 
            if IRUtil.VarSet.mem vr s32
            then list
            else ([enc8;enc32;enc])
          in
          let list = 
            if IRUtil.VarSet.mem vm s8
            then list
            else ([enc32;enc8;enc])
          in
          aux2 list lcr32s
        | [] -> list
        in 
        let list = aux2 list blah2 in
        aux list lcr8s
      | [] -> list
      in 
      let newlist = aux [] blah1 in
      match newlist with
      | [] -> list
      | _ -> newlist::list)
      []
      blah
  in
(*List.iter Encodable.print_encodable gadgets;*)
  gadgets

let make_contrived_example gc = 
  let addr32 = 0x12345678l in
  let set_negone = make_contrived_example gc IR.TypeReg_8 0xffl IR.Or addr32 in
  let add_9b = make_contrived_example gc IR.TypeReg_8 0x9Bl IR.Add addr32 in
  let not_9a_32 = Int32.logand 0xffl (Int32.lognot 0x9Al) in
  let xor_not_9a = make_contrived_example gc IR.TypeReg_8 not_9a_32 IR.Xor addr32 in
  let and_9a = make_contrived_example gc IR.TypeReg_8 0x9Al IR.Xor addr32 in

  let append_lists ll1 ll2 = 
    let rec aux list = function
    | x::xs ->
      let rec aux2 list = function
      | y::ys -> aux2 ((x@y)::list) ys
      | [] -> list
      in 
      let list = aux2 list ll2 in
      aux list xs
    | [] -> list
    in aux [] ll1
  in
  let negone_add = append_lists set_negone add_9b in
  let negone_xor = append_lists set_negone xor_not_9a in
  let negone_and = append_lists set_negone and_9a in
  let reify enclist = 
    let rec aux list cumsize next_ra = function
    | enc::encs ->
      let x86,size,arr = Encodable.encode enc next_ra in
      let open Encodable in
      let next_ra = Hashtbl.fold (fun i32 _ _ -> i32) enc.this_code_address 0l in
      aux ((next_ra,x86,arr)::list) (cumsize + size) next_ra encs
    | [] -> (cumsize,list)
    in aux [] 0 0l (List.rev enclist)
  in
  let reify = List.map reify in
  let enc_negone_add = reify negone_add in
  let enc_negone_xor = reify negone_xor in
  let enc_negone_and = reify negone_and in
  let sorted = 
    List.sort 
     (fun (s1,_) (s2,_) -> Pervasives.compare s1 s2) 
     (enc_negone_add@enc_negone_xor@enc_negone_and) 
  in
  let encl = List.map snd sorted in
  let print_individual (a32,x86,arr) =
    f_printf "\n; %08lx:\n" a32;
    List.iter (fun i -> f_printf "; %s\n" (X86Disasm.string_of_x86instr i)) x86;
    f_printf "\n";
    Array.iter (f_printf "%08lx\n") arr
  in  
  List.iter (fun i ->
    let _ = f_printf "\nPayload option:\n\n" in    
    List.iter print_individual i) 
    encl
    
  
