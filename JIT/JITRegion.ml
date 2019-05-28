(* JITRegion.ml *)

type t = int32 * int

type instate =  { 
  lhs_in: int32;
  rhs_in: int32;
  input3: int32;
  flags_in: int; }
   
type outstate = {
  lhs_out: int32;
  output2: int32;
  flags_out: int; }

external allocate   : int -> t option = "OCaml_JITRegion_allocate"
external deallocate : t -> unit = "OCaml_JITRegion_deallocate"
external blit_at    : t -> int -> int32 array -> bool = "OCaml_JITRegion_blit_at"
external execute_at : t -> int -> instate -> outstate = "OCaml_JITRegion_execute_at"

let blit t    = blit_at    t 0
let execute t = execute_at t 0

(** All registers and flags. *)
type x86ctx = 
{
  eax: int32;
  ecx: int32;
  edx: int32;
  ebx: int32;
  esp: int32;
  ebp: int32;
  esi: int32;
  edi: int32;
  eflags: int32;
}

(** I/O pair for some execution. *)
type x86_ctx_pair =
{
  input: x86ctx;
  outut: x86ctx;
}

external read_output_ctx : t -> x86ctx         = "OCaml_JITRegion_read_output_ctx"   
external write_input_ctx : t -> x86ctx -> unit = "OCaml_JITRegion_write_input_ctx"   
external internal_execute_at_complex : t -> int -> unit = "OCaml_JITRegion_execute_at_complex"

let execute_at_complex xt i mt instate =
  let _ = write_input_ctx mt instate in
  internal_execute_at_complex xt i;
  let outstate = read_output_ctx mt in
  outstate

let execute_complex xt = execute_at_complex xt 0
