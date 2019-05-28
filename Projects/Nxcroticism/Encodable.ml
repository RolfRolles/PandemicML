open FrameworkUtil

(* This construction is not exactly what I need.  The problem is that I end up
   with several of these things in a list, but I need to specialize individual
   elements by filling in the constant_value field.  I lose track of which
   encodable object needs to have that field reified.
   
   Or do I?  Can I just pass that in to the GadgetCache module when I specify
   a load constant behavior?
   
   Yeah, so that's what I think I'm going to do.  I'll incorporate the constant
   value into the call into GadgetCache, and while I'm at it, I'll return any
   gadgets that set the value to that constant specifically. *)
type encodable =
{
  (* X86 instructions *)
  x86: X86.x86instrpref list;

  (* The address corresponding to this gadget *)
  this_code_address: (int32, unit) Hashtbl.t;
  
  (* Address of the next gadget *)
  next_return_address: int32;
  
  (* Number of bytes in the function's stack frame *)
  size_of_frame: int;
  
  (* The displacement within the frame of the return address *)
  return_address_position: int;
  
  (* For stack load gadgets, position and size of constant, plus reifiable 
     value *)
  constant_position: (int * IR.typereg) option;
  constant_value: int32 option;
}

let print_encodable enc =
  let _ = Hashtbl.iter (fun i32 _ -> f_printf "%08lx " i32) enc.this_code_address in
  f_printf "\n";
  List.iter (fun i -> f_printf "%s\n" (X86Disasm.string_of_x86instr i)) enc.x86;
  f_printf "Frame size %d, return address %d\n" enc.size_of_frame enc.return_address_position;
 (match enc.constant_position with
  | Some(i,s) -> f_printf "Constant(%d) gets its value from %d\n" (IRUtil.bits s) i
  | None -> ());
  match enc.constant_value with
  | Some(i32) -> f_printf "Constant value is %lx\n" i32
  | None -> ()

external make : int -> 'a -> 'a array = "caml_make_vect"

let encode enc next_ra =
  let arr = Array.init (enc.size_of_frame / 4) (fun _ -> Random.int32 Int32.max_int) in
  let write_byte p b32 = 
    let pm = p mod 4 in
    let pq = p / 4 in
    let mask,shifted_val32 = 
      match pm with 
      | 0 -> 0xFFFFFF00l,b32
      | 1 -> 0xFFFF00FFl,Int32.shift_left b32 8
      | 2 -> 0xFF00FFFFl,Int32.shift_left b32 16
      | 3 -> 0x00FFFFFFl,Int32.shift_left b32 24
      | _ -> failwith "impossible" 
    in
    let i32 = arr.(pq) in
    let i32 = Int32.logor (Int32.logand i32 mask) shifted_val32 in
    arr.(pq) <- i32
  in
  let write_word p w32 =
    write_byte (p + 0) (Int32.logand (Int32.shift_right_logical w32 0) 0xffl);
    write_byte (p + 1) (Int32.logand (Int32.shift_right_logical w32 8) 0xffl)
  in
  let write_dword p d32 =
    write_word (p + 0) (Int32.logand (Int32.shift_right_logical d32 0) 0xffffl);
    write_word (p + 2) (Int32.logand (Int32.shift_right_logical d32 16) 0xffffl)
  in
 (match enc.constant_position,enc.constant_value with
  | Some(p,IR.TypeReg_8 ),Some(v) -> write_byte  p v
  | Some(p,IR.TypeReg_16),Some(v) -> write_word  p v
  | Some(p,IR.TypeReg_32),Some(v) -> write_dword p v
  | _,_ -> ());
  write_dword enc.return_address_position next_ra;
  (enc.x86,enc.size_of_frame,arr)

let mk_encodable x86l addr_ht framesize retpos =
{
  x86 = x86l;
  this_code_address = addr_ht;
  next_return_address = 0l;
  size_of_frame = framesize;
  return_address_position = retpos;
  constant_position = None;
  constant_value = None;
}

let add_constant enc constpos constsize =
{ enc with constant_position = Some(constpos,constsize); }

let mk_encodable_with_constant x86l eip32 framesize retpos constpos constsize =
  add_constant (mk_encodable x86l eip32 framesize retpos) constpos constsize

