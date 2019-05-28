(** Module for allocating and deallocating RWX memory regions, blitting bytes
    into them, and executing them. *)

(** {6 Module opaque type} *)

(** Opaque type of a JITtable memory region.  Internally, this is actually a 
    memory address returned by VirtualAlloc. 

    I made the type not opaque for the complex execution function.  I don't
    think this will break anything, but it has engineering impacts.  Revisit. *)
type t = int32 * int

(** {6 Basic operations on t} *)

(** Returns a region at least as big as the integer argument, or [None]. *)
val allocate   : int -> t option

(** Deallocates a previously-allocated region.  Hooray for opaque types here,
    as we don't have to worry about the user passing in a random value. *)
val deallocate : t -> unit

(** Write an array of bytes into the memory region, starting at the first
    address. *)
val blit       : t -> int32 array -> bool

(** Write an array of bytes into the memory region, starting at the address
    supplied as the second parameter. *)
val blit_at    : t -> int -> int32 array -> bool

(** {6 Types used in random testing, simpler interface} *)

(** That which comes in:  three input values and the flags. *)
type instate =  { 
  lhs_in: int32;
  rhs_in: int32;
  input3: int32;
  flags_in: int; }
   
(** That which goes out:  two output values and the flags. *)
type outstate = {
  lhs_out: int32;
  output2: int32;
  flags_out: int; }

(** {6 Random testing functions, simpler interface} *)

(** Execute the code on the region, starting at address zero.  Given an 
    {!instate}, the JITted code is responsible for producing an {!outstate}. *)
val execute    : t -> instate -> outstate

(** Execute the code on the region, starting at the address provided by the
    second parameter.  Given an {!instate}, the JITted code is responsible 
    for producing an {!outstate}. *)
val execute_at : t -> int -> instate -> outstate

(** {6 Types used in random testing, complex interface} *)

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

(** {6 Random testing functions, complex interface} *)

(** {6 Types used in more extensive texting } *)

val read_output_ctx : t -> x86ctx
val write_input_ctx : t -> x86ctx -> unit

(** Execute the code on the region, starting at the address provided by the
    second parameter.  Implicitly it produces  *)
val execute_complex : t -> t -> x86ctx -> x86ctx
val execute_at_complex : t -> int -> t -> x86ctx -> x86ctx
