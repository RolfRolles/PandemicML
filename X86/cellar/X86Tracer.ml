open X86
open X86Util

type x86traceentry = x86instr * (x86operand list)
type x86trace      = x86traceentry list

class type x86tracer =
  object
    method ret     : x86instr -> int32 option
    method retf    : x86instr -> int32 option
    method call    : x86instr -> int32 option
    method callf   : x86instr -> int32 option
    method cjmp    : x86instr -> int32 option
    method jmp     : x86instr -> int32 option
    method jmpf    : x86instr -> int32 option
    method stop    : int32 -> bool
    method clean   : x86trace -> x86trace
    method process : int32 * x86instr -> unit
end

let generate_trace start_ea tracer =
  let get_instr_and_follow ea =
    let ((mnem,oplist) as i) = X86Decode.decode ea in
    let next_ea = match () with
    | _ when x86_is_ret   mnem -> tracer#ret   i
    | _ when x86_is_cjmp  mnem -> tracer#cjmp  i
    | _ when x86_is_jmp   mnem -> tracer#jmp   i
    | _ when x86_is_call  mnem -> tracer#call  i
    | _ when x86_is_retf  mnem -> tracer#retf  i
    | _ when x86_is_jmpf  mnem -> tracer#jmpf  i
    | _ when x86_is_callf mnem -> tracer#callf i
    | _ -> Some(X86Decode.get_cursor ())
    in ((ea,mnem,oplist),next_ea)
  in
  let rec aux ea list = 
  (*Ida.msg "%08lx\n" ea; -- the client can just put this in their stop function *)
    if tracer#stop ea
    then List.rev list
    else
      let (b,n) = get_instr_and_follow ea in
      tracer#process b;
      match n with
      | Some(a) -> aux a (b::list)
      | None -> List.rev (b::list)
  in aux ea [] 0
  
