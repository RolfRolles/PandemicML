exception Found of int

module C = X86CFG.X86CFGBuilder.C

let int32_of_jcc j = let open X86 in match j with
| Jo  -> (0x70l,0x80l)
| Jno -> (0x71l,0x81l)
| Jb  -> (0x72l,0x82l)
| Jae -> (0x73l,0x83l)
| Jz  -> (0x74l,0x84l)
| Jnz -> (0x75l,0x85l)
| Jbe -> (0x76l,0x86l)
| Ja  -> (0x77l,0x87l)
| Js  -> (0x78l,0x88l)
| Jns -> (0x79l,0x89l)
| Jp  -> (0x7Al,0x8Al)
| Jnp -> (0x7Bl,0x8Bl)
| Jl  -> (0x7Cl,0x8Cl)
| Jge -> (0x7Dl,0x8Dl)
| Jle -> (0x7El,0x8El)
| Jg  -> (0x7Fl,0x8Fl)
| _ -> invalid_arg "int32_of_jcc"

let invert_jcc = 
  let open X86 in 
  function
  | Ja  -> Jbe
  | Jae -> Jb
  | Jb  -> Jae
  | Jbe -> Ja
  | Jg  -> Jle
  | Jge -> Jl
  | Jl  -> Jge
  | Jle -> Jg
  | Jno -> Jo
  | Jnp -> Jp
  | Jns -> Js
  | Jnz -> Jz
  | Jo -> Jno
  | Jp -> Jnp
  | Js -> Jns
  | Jz -> Jnz
  | Jcxz  -> failwith  "Inverting jcxz, shouldn't have accepted this in the first place..."
  | Jecxz -> failwith "Inverting jecxz, shouldn't have accepted this in the first place..." 
  | _ -> failwith "invert_jcc: impossible"

type size =
| Short
| Long

let string_of_size = function
| Short -> "short"
| Long  -> "long"

type grout =
(* Destination vertex address * size of jump *)
| Uncond of int32 * size
(* Mnemonic * destination vertex address * size of jump *)
| Cond of X86.x86mnem * int32 * size
| Call of int32 * size

type masonry =
| Brick of int32 * int32 array
| Grout of grout

let split_jcc =
  let open X86 in
  let rec aux outlist = function
  | []    -> ([],None)
  | [{ pref = []; instr = ((x,_) as i)}]   -> 
    let cj = if X86Util.x86_is_cjmp x then Some(i) else None in
    (List.rev outlist,cj)
  | x::xs -> aux (x::outlist) xs
  in aux []

let assemble_channel cfg startea ob =
  let startv = 
    try  C.find_vertex cfg startea
    with Not_found -> failwith (Printf.sprintf "%08lx: assembler:  can't find staring vertex" startea)
  in
  let ordered_vertex_list = 
    try  X86CFG.X86CFGBuilder.get_order cfg startv
    with Not_found -> failwith "assembler: get_order threw Not_found"
  in
  let assemble_list list =
    let open X86 in
    Array.of_list (List.concat (List.map (fun i -> X86Encode.encode_instruction i) list))
  in
  let make_bricks label x86l = 
    (* Have to split at call boundaries *)
    let open X86 in
    let rec aux curr_list total_list = function
    (* The final result:  a list of all bricks and grout *)
    | [] -> List.rev total_list
    (* If there's a call, split around the boundary *)
    | { pref = _; instr = (Call,[JccTarget(ta,_)]) }::xs ->
      let total_list = 
        match curr_list with
        | [] -> Grout(Call(ta,Long))::total_list
        | _  -> Grout(Call(ta,Long))::Brick(assemble_list (List.rev curr_list))::total_list
      in aux [] total_list xs
    | x::xs -> aux (x::curr_list) total_list xs
    in aux [] [] x86l
  in
  let process_one_vertex x vno = 
    let sl,sn = C.G.fold_succ (fun v (l,i) -> (v::l,i+1)) cfg x ([],0) in
    match sn,sl with 
    | 0,[]  -> (* Had no children, couldn't end with a jmp *)
      (make_brick (C.G.V.label x) (C.get_ir cfg x),[])
    | 1,[s] -> 
      (* Had one child -- might require a ujmp, might not *)
      let grout = match vno with
      | Some(y) when s = y -> []
      | None
      | Some(_) -> [Uncond(C.G.V.label s,Short)]
      in
      (make_brick (C.G.V.label x) (C.get_ir cfg x),grout)
    | 2,_ -> 
      (* Had two children, had a cjmp or an N-way conditional with N=2 *)
      let open X86 in
      let ir,jco = split_jcc (C.get_ir cfg x) in
      let (m,ol) = match jco with
      | None -> failwith "Vertex had two children but did not end in a jcc; N-way conditional unsupported"
      | Some(jc) -> jc
      in 
      let ta,fa = match ol with
      | [JccTarget(ta,fa)] -> ta,fa
      | _ -> failwith "Jcc did not have a JccTarget operand, typechecking prevents this"
      in
      let grout = match vno with
      (* If the next vertex in the sequence corresponds to the fallthrough address 
         of the jcc, we only need one bit of grout describing where the true branch
         goes, and we do not need to invert the conditional. 
         
         If the next vertex in the sequence corresponds to the "taken" address of
         the jcc, we can invert the conditional, and then we only need one bit of
         grout.
         
         If there is no next vertex in the sequence, or neither of the above remarks
         apply, we need two bits of grout:  a conditional jump corresponding to the
         conditional in the basic block, and an unconditional jump to the fallthrough
         address immediately afterwards. *)
         
      | Some(y) when fa = C.G.V.label y -> [Cond(m,ta,Short)]
      | Some(y) when ta = C.G.V.label y -> [Cond(invert_jcc m,fa,Short)]
      | _ -> [Cond(m,ta,Short);Uncond(fa,Short)]
      in
      (make_brick (C.G.V.label x) ir,grout)
    | n,_ ->
      (* Multiple jump targets, fail for now *)
      failwith "N-way (N>2) conditional, currently unsupported"

  in
  let rec aux masonry = function
  | []  -> List.rev masonry
  | [x] -> aux ((process_one_vertex x (None))::masonry) []
  | x::(y::ys as yss) ->
    aux ((process_one_vertex x (Some(y))::masonry)) yss
  
  in 
  let masonry = aux [] ordered_vertex_list in
  let rec len l = function
  | [] -> l
  | (_,gl)::xs -> len (l+1+(List.length gl)) xs
  in 
  let mosaic = Array.make (len 0 masonry) (Grout(Uncond(0xffffffffl,Short))) in
  let rec place idx = function
  | [] -> ()
  | (b,gl)::ml ->
    mosaic.(idx) <- b;
    let idx = idx + 1 in
    let rec aux j = function
    | [] -> idx + j
    | g::gl -> mosaic.(idx + j) <- Grout(g); aux (j+1) gl
    in
    place (aux 0 gl) ml
  in
  place 0 masonry;
(*Array.iter
   (function
    | Brick(l,bytes)     -> IDA.msg "%08lx: " l; Array.iter (IDA.msg "%02lx ") bytes; IDA.msg "\n"
    | Grout(Uncond(l,s)) -> IDA.msg "jmp %s %08lx\n" (string_of_size s) l
    | Grout(Cond(m,l,s)) -> IDA.msg "%s %s %08lx\n" (X86Disasm.string_of_x86mnem m) (string_of_size s) l)
    mosaic;*)

  (* Create hash table once *)
  let posmap = Hashtbl.create (Array.length mosaic * 2) in
  let find_pos label =
    let result =
      try
        Hashtbl.find posmap label
      with Not_found -> (let m = Printf.sprintf "Lookup of %08lx failed\n" label in failwith m)
    in result
  
  in
  (* Generate a position map, given the mosaic *)
  let gen_posmap () =
    Hashtbl.clear posmap;
    Array.fold_left 
     (fun i be -> match be with
      | Brick(label,arr) -> 
        Hashtbl.replace posmap label i;
        i + Array.length arr
      | Grout(Uncond(_,Short))
      | Grout(Cond(_,_,Short)) -> i + 2
      | Grout(Uncond(_, Long)) -> i + 5
      | Grout(Cond(_,_, Long)) -> i + 6)
      0
      mosaic
  in

  (* Widen *)
  let rec widen () = 
    let _ = gen_posmap () in
  
    let to_widen = 
      try
        let _ = Array.fold_left 
         (fun (i,j) be ->
            let throw () = raise (Found(j)) in
            let j = j+1 in 
            match be with
            | Brick(_,arr) -> (i + Array.length arr,j)
            | Grout(Uncond(label,Short))
            | Grout(Cond(_,label,Short)) ->
              (* Point after branch *)
              let i = i + 2 in
              let destpos = find_pos label in
              let delta = destpos - i in
             (if (not ((delta >= -0x80) && (delta <= 0x7f)))
              then (throw ()));
              (i,j)
            | Grout(Uncond(label,Long)) -> (i + 5,j)
            | Grout(Cond(_,label,Long)) -> (i + 6,j))
         (0,0)
          mosaic 
        in
        None
      with
        Found(j) -> Some(j)
    in
    match to_widen with
    | None -> ()
    | Some(j) -> 
     (match mosaic.(j) with
      | Brick(_,_) -> failwith "brick impossible; only short grouts should be returned"
      | Grout(Uncond(label,Short)) -> mosaic.(j) <- Grout(Uncond(label,Long)); widen ()
      | Grout(Cond(m,label,Short)) -> mosaic.(j) <- Grout(Cond(m,label,Long)); widen ()
      | Grout(Uncond(_, Long)) -> failwith "long uncond impossible; only short cond/uncond should be returned"
      | Grout(Cond(_,_, Long)) -> failwith "long cond impossible; only short cond/uncond should be returned")
  
  in 
  let _ = widen () in

  (* Dry *)
  let _ = Array.fold_left 
   (fun (i,j) be -> 
      let j = j+1 in 
      let list_of_dword dw = 
       [Int32.logand dw 0xffl;
        Int32.logand (Int32.shift_right_logical dw 08) 0xffl;
        Int32.logand (Int32.shift_right_logical dw 16) 0xffl;
        Int32.logand (Int32.shift_right_logical dw 24) 0xffl]
      in
      let brickify_short label prefix =
        let i'' = i + 2 in
        let destpos = find_pos label in
        let delta = destpos - i'' in
        mosaic.(j-1) <- Brick(0l,Array.of_list (prefix::[Int32.logand 0xffl (Int32.of_int delta)]));
        (i'',j)
      in
      let brickify_long label jsz prefix =
        let i'' = i + jsz in
        let destpos = find_pos label in
        let delta = destpos - i'' in
        mosaic.(j-1) <- Brick(0l,Array.of_list (prefix@(list_of_dword (Int32.of_int delta))));
        (i'',j)
      in
      match be with
      | Brick(_,arr) -> (i + Array.length arr,j)
      | Grout(Uncond(label,Short)) -> brickify_short label 0xebl 
      | Grout(Cond(m,label,Short)) -> brickify_short label (fst(int32_of_jcc m))
      | Grout(Uncond(label, Long)) -> brickify_long  label 5 [0xe9l]
      | Grout(Cond(m,label, Long)) -> brickify_long  label 6 [0x0fl;snd(int32_of_jcc m)])
   (0,0)
    mosaic

  in  
  Array.iter
   (function
    | Brick(_,arr) -> Array.iter ob arr
    | Grout(_) -> failwith "grout should have dried by now")
    mosaic

let assemble_file cfg startea fname = 
  (* Save *)
  let out = open_out_bin fname in
  let ob i = output_byte out (Int32.to_int i) in
  assemble_channel cfg startea ob;
  close_out out
