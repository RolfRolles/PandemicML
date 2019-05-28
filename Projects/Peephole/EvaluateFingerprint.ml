(*
#use "c:\\paframework\\Projects\\Peephole\\EvaluateFingerprint.ml";;
*)
#use "c:\\paframework\\framework.ml";;

let num_random_tests = 16
let initial_hash_value = 0xdefec8edl

let ri32 () = Int64.to_int32 (Random.int64 0x100000000L)

let make_random_state espval =
  let open X86TestUtil in
  { 
    eax = ri32 ();
    ecx = ri32 ();
    edx = ri32 ();
    ebx = ri32 ();
    esp = espval;
    ebp = ri32 ();
    esi = ri32 ();
    edi = ri32 ();    
    eflags = Random.int 0x10000000;
    memory = [||];
  }
    
let make_random_states espval n =
  let rec aux list i = 
    if i = n
    then list
    else aux ((make_random_state espval)::list) (i+1)
  in aux [] 0

let zero_state espval =
  let open X86TestUtil in
  { 
    eax = 0l;
    ecx = 0l;
    edx = 0l;
    ebx = 0l;
    esp = espval;
    ebp = 0l;
    esi = 0l;
    edi = 0l;
    eflags = 0;
    memory = [||];
  }

let one_state espval =
  let open X86TestUtil in
  { 
    eax = 0xffffffffl;
    ecx = 0xffffffffl;
    edx = 0xffffffffl;
    ebx = 0xffffffffl;
    esp = espval;
    ebp = 0xffffffffl;
    esi = 0xffffffffl;
    edi = 0xffffffffl;
    eflags = ~-1;
    memory = [||];
  }

let make_states_vector espval n = 
  (zero_state espval)::
  (one_state espval)::
  (make_random_states espval n)

let states_hashtbls espval n = List.map X86TestUtil.mk_initial_state (make_states_vector espval n)

let evaluate states x86 = 
  let irtrans = List.concat (List.map (X86ToIR.translate_instr 0l) x86) in
  List.map (fun h -> let h = Hashtbl.copy h in ignore(IRLocalOpt.local_opt_state_in h irtrans); h) states

(* Duplicated from LowLevel.ml *)
let ror_dword d s = let s = Int32.to_int s in Int32.logor (Int32.shift_right_logical d s) (Int32.shift_left d (32-s))
let rol_dword d s = let s = Int32.to_int s in Int32.logor (Int32.shift_left d s) (Int32.shift_right_logical d (32-s))

let fingerprint_one inhash outstate =
  let open X86TestUtil in
  let step2 h v1 v2 =
    let h1 = Int32.add h v1 in
    let h2 = ror_dword h1 (Int32.logand v2 0x1fl) in
    let h3 = Int32.logxor h2 v2 in
    h3
  in
  let h = step2 inhash outstate.eax outstate.ecx in
  let h = step2 h      outstate.edx outstate.ebx in
(*let h = Int32.add h outstate.esp in*)
  let h = Int32.logxor h outstate.ebp in
  let h = step2 h      outstate.esi outstate.edi in
(*some shit with flags*)
  h
  
let state_opt_of_hashtbl hashtbl =
  let extract var = try Some(Hashtbl.find hashtbl var) with Not_found -> None in
  let extract var = match extract var with | Some(IR.Const(i,IR.TypeReg_32)) -> Some(Int64.to_int32 i) | Some(_) -> failwith "state_opt_of_hashtbl" | None -> None in
(*let extractfl v = None in*)
  let open X86TestUtil in
  let open X86ToIRUtil in
  match extract vEax, extract vEcx, extract vEdx, extract vEbx, (*extract vEsp, *)extract vEbp, extract vEsi, extract vEdi with
  | Some(a),Some(c),Some(d),Some(b),Some(bp),Some(si),Some(di) -> Some(
    { eax = a;
      ecx = c;
      edx = d;
      ebx = b;
      esp = 0l;
      ebp = bp;
      esi = si;
      edi = di;
      eflags = 0;
      memory = [||];
    })
  | _ -> None

type fingerprint_results = 
| Successful of X86TestUtil.x86_complete_state list
| NotConstant of int list

let fingerprint_hashtbl_list hashtbl_list = 
  let state_opt_list = List.map state_opt_of_hashtbl hashtbl_list in
  let rec aux list i = function
  | [] -> List.rev list
  | None::xs -> aux (i::list) (i+1) xs
  | Some(_)::xs -> aux list (i+1) xs
  in
  match aux [] 1 state_opt_list with
  | [] -> Successful(List.map (function | Some(x) -> x | None -> failwith "impossible") state_opt_list)
  | x  -> NotConstant(x)

let states_vector = ref None

type fingerprint =
| Hash of int32
| FailedTestsNotConstant of int list

(* Needs to take the liveness context into account *)
let fingerprint x86l espval =
  let states_vector = match !states_vector with
  | None -> let x = states_hashtbls espval num_random_tests in states_vector := Some(x); x
  | Some(x) -> x
  in
  let evaluated_list = evaluate states_vector x86l in
  match (fingerprint_hashtbl_list evaluated_list) with
  | NotConstant(il)    -> FailedTestsNotConstant il
  | Successful(states) -> Hash(List.fold_left fingerprint_one initial_hash_value states)

(*
let xchg_sequence     = let open X86 in
                        [{ pref = []; instr = (Xchg,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) }]
let xor_swap_sequence = let open X86 in
                        [{ pref = []; instr = (Xor, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };
                         { pref = []; instr = (Xor, [GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };
                         { pref = []; instr = (Xor, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) }]

let xchg_not_sequence = let open X86 in
                        [{ pref = []; instr = (Xchg,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };
                         { pref = []; instr = (Not, [GeneralReg(Gd(Ebx))]) };
                         { pref = []; instr = (Xchg,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) }]

let not_sequence      = let open X86 in
                        [{ pref = []; instr = (Not, [GeneralReg(Gd(Eax))]) }]


fingerprint xchg_sequence 0x1234l;;
fingerprint xor_swap_sequence 0x1234l;;
fingerprint xchg_not_sequence 0x1234l;;
fingerprint not_sequence 0x1234l;;
*)