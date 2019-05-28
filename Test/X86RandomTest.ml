open X86

exception Internal
let ei () = raise Internal

let marshall_out (i: X86.x86instrpref) s =
  let oc = open_out_bin "c:\\paframework\\instr.bin" in
  Marshal.to_channel oc i [Marshal.No_sharing];
  close_out oc;
  print_endline ("Marshalled an instruction "^s)

let get_disasm i s = 
  try X86Disasm.string_of_x86instr i 
  with Invalid_argument(_) -> (marshall_out i s; exit 0)

let enc i =
  let i_disasm () = get_disasm i "init" in
  let enclist = 
    try
      X86Encode.encode_instruction i
    with
    | Invalid_argument(s) -> (print_endline ("Invalid arg while assembling "^i_disasm ()^": "^s); ei ())
    | Failure(s) -> (print_endline ("Failure while assembling "^i_disasm ()^": "^s); ei ())
    | X86Encode.InvalidOperands(_) -> (print_endline ("Invalid operands while assembling "^i_disasm ()); ei ())
  in
  let encarray = Array.of_list enclist in
  encarray

let enc_simple i =
  let i_disasm () = get_disasm i "init" in
  let enclist = 
    try
      X86EncodeSimpler.encode_instruction i
    with
    | Invalid_argument(s) -> (print_endline ("Invalid arg while assembling "^i_disasm ()^": "^s); ei ())
    | Failure(s) -> (print_endline ("Failure while assembling "^i_disasm ()^": "^s); ei ())
    | X86Encode.InvalidOperands(_) -> (print_endline ("Invalid operands while assembling "^i_disasm ()); ei ())
  in
  let encarray = Array.of_list enclist in
  encarray

let decode encarray = 
  let encbytes () = (Array.fold_left (fun s i32 -> s^(Printf.sprintf "%02lx" i32)) "[" encarray)^"]" in
  let i_disasm () = "can't show" in
  let _ = X86Decode.init (fun i -> encarray.(Int32.to_int i)) in
  let d,_,_ = 
    try
      X86Decode.decode 0l 
    with
    | X86Decode.InvalidLockPrefix(i) -> (print_endline ("Invalid lock prefix while disassembling "^encbytes ()^": "^get_disasm i "lock"); ei ())
    | X86Decode.TooLongInstruction   -> (print_endline ("Too long instruction while disassembling "^encbytes ()); ei ())
    | X86Decode.InvalidInstruction   -> (print_endline ("Invalid instruction ("^i_disasm ()^") while disassembling "^encbytes ()); ei ())
    | X86Decode.DoMoreResearch(s)    -> (print_endline ("Do more research while disassembling "^encbytes ()^": "^s); ei ())
  in 
  d

let encode_and_decode i = 
  let i_disasm () = get_disasm i "init" in
  let encarray = enc i in
  let d = decode encarray in
  let encbytes () = (Array.fold_left (fun s i32 -> s^(Printf.sprintf "%02lx" i32)) "[" encarray)^"]" in
  if i <> d
  then 
    print_endline ("Failed: pre-encode = "^i_disasm ()^" "^encbytes ()^" post-decode = "^(get_disasm d "disasm"))

let encode2_and_decode2 i = 
  let i_disasm () = get_disasm i "init" in
(*print_endline (i_disasm ());*)
  let encarray1 = enc        i in
  let encarray2 = enc_simple i in
  let d1 = decode encarray1 in
  let d2 = decode encarray2 in
  let encbytes encarray = (Array.fold_left (fun s i32 -> s^(Printf.sprintf "%02lx" i32)) "[" encarray)^"]" in
  if d1 <> d2
  then 
    print_endline 
     ("Failed: pre-encode = "^
      i_disasm ()^
      " encode1= "^
      encbytes encarray1^
      " encode2= "^
      encbytes encarray2)

let exclude = function
| Xlat
| Lea
| Loopz
| Loopnz
| Loop
| Jz
| Js
| Jp
| Jo
| Jnz
| Jns
| Jnp
| Jno
| Jle
| Jl
| Jge
| Jg
| Jecxz
| Jcxz
| Jbe
| Jb
| Jae
| Ja  -> true
| _ -> false

let main () = 
  Random.self_init ();
  let rec aux i j = 
    if j = 5000 then exit 0;
    let i,j = 
      if i = 1000000
      then (Printf.eprintf "%d million\n%!" (j+1); 0,j+1)
      else (i+1),j
    in
    let instr = X86Random.generate_random_instruction exclude in 
   (try
      encode2_and_decode2 ({pref = []; instr = instr})
    with | Internal -> ());
    aux i j
  in aux 0 0
  
let _ = main ()

