(* f_pred: len:int -> intarr:int array -> pos:int -> bool: should process at this location
   backwards_depth: number of bytes to traverse backwards
   i64_base: int64 base address of contents of fname int array
   fname: filename of raw byte contents, ASSUMES THAT RELOCATIONS HAVE BEEN APPLIED *)
let bruteforce_backwards f_pred n_tree backwards_depth base32 f_get_byte len =
  (* Set up the disassembler to custom decode *)
  let _ = X86Decode.init f_get_byte in

  (* Iterate over the entire section *)
  let rec iter i = 

    (* Stop if done *)
    if i = len then ()
    else
     ((* Is this byte interesting? *)
      if not (f_pred len (f_get_byte (Int32.of_int i)))

      (* No, go to next iteration *)
      then iter (i+1)

      (* Yes, we found a return instruction *)
      else
        (* Create an array to store the instructions found at backwards positions *)
        let a_instr = Array.make (backwards_depth+1) (None) in
        (*f_printf "%d: had return\n%!" i;*)
        let instr,len,_ = X86Decode.decode (Int32.of_int i) in
        a_instr.(0) <- Some(instr,len);
        
        (* Iterate backwards starting from the return address. Some finicking with
           the j argument here: it starts at 0, but we ... XXX *)
        let rec inner_iter j =

          let jp1 = j + 1 in

          (* Only iterate up to the backwards_depth argument *)
          if (j = backwards_depth || jp1 > i) then ()

          else
            (* Try to decode (might fail due to invalid instructions or a few 
               framework limitations) *)
            let res_opt = 
              try       Some(X86Decode.decode (Int32.of_int (i-jp1)))
              with _ -> None
            in

            (* Check the supposed instruction *)
            match res_opt with
            
            (* Instruction decoded successfully, and was of a type that transfers 
               control to the next instruction (i.e., not a jump, call, etc.) *)
            | Some(instr,len,ASMUtil.Flow(_)) ->
              
              (* j represents the current distance from the return instruction; it
                 starts at 0 as we iterate backwards. *)
              let _ = match () with
              
              (* If the length of the instruction is greater than our current j,
                 this means that the instruction that we decoded at position 
                 base32+(i-j) would have subsumed the return address too.  Hence
                 we should simply ignore this decoded instruction, since control
                 won't lead to the return address should we point EIP here. *)
              | _ when len > jp1 -> ()
              
              (* If the length of the instruction is less than the current j,
                 we have to check and see whether the subsequent instruction
                 (i.e. the one at base32+(i-j)+len) was valid. For this purpose,
                 we keep a hashtable mapping j -> (instr,len).  Then, when we
                 encounter a valid instruction at position j with length len,
                 we just check to see whether (j+len) decoded properly.  If it
                 did, we add the current entry to the hash table, so that it may
                 be a foothold for further backwards decodes. *)
              | _ when len < jp1 -> 
               (match a_instr.(jp1-len) with
                | Some(_,_) -> a_instr.(jp1) <- Some(instr,len)
                | None -> ())

              (* The base case of the whole process. For instructions whose lengths
                 equal j, i.e., they fall immediately before a return, we don't need
                 to check to see whether the "next" instruction was valid, since 
                 there is no next instruction.  Just put them into the table and
                 move on, so that prior instructions situated before this one might
                 precede it and be caught by the case above. *)
              | _ when len = jp1 -> 
                (a_instr.(jp1) <- Some(instr,len))
              
              (* Unreachable case, but it squelches compiler warning 25 *)
              | () -> () 
              in
              inner_iter jp1
              
            (* This case corresponds to where the instruction did not have a "flow"
               type, i.e., it was something like a jump, or a call, or another 
               return etc. Ignore it. *)
            | Some(_,_,_)
            (* This case corresponds to the situation wherein the instruction 
               couldn't be decoded (it might've been invalid, or fell through one of
               very few cracks in the disassembler. *)
            | None -> inner_iter jp1
        in
        
        (* Fill the a_instr array with the preceeding instructions *)
        let _ = inner_iter 0 in
        
        (* Prints one entry from the array, and then recurses backwards *)
        let rec print_at list k = 
          if k < 0 then ((*f_printf "\n%!";*) list) else
          match a_instr.(k) with
          | Some(instr,len) -> 
            let addr = Int32.add base32 (Int32.of_int (i-k)) in
          (*f_printf 
              "%08lx: %s%!\n" 
              addr
              (X86Disasm.string_of_x86instr instr); *)
            let ir = 
              try Some(X86ToIR.translate_instr addr instr)
              with _ -> None
            in
            print_at ((addr,instr,ir)::list) (k-len)
          | None -> failwith "bruteforce_backwards: error in this component%!\n"
        in
        
        (* Prints all entries from the array and their backwards chains *)
        let rec aux i = 
          if i = backwards_depth
          then ()
          else
            let _ = 
            match a_instr.(i) with
            | None -> ()
            | Some(_,_) -> 
              let list = print_at [] i in
              NaryTree.insert n_tree list
            in aux (i+1)
        in 
        (* Trigger the print over the entire array *)
        let _ = aux 0 in
        
        (* Jump back to top-level iteration at the next array index *)
        iter (i+1))
  in
  iter 0