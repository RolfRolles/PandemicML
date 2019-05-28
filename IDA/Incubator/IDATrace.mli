(** Module for tracing in IDA.  Call {!begin_trace} with a tracer to begin 
    tracing.  The tracer can be of any data type.  The tracer function, 
    "{!process}" (horribly named, process_step maybe?), is called upon every 
    instruction executed.  If it returns false, no further steps are taken.  
    Otherwise, the debugger continues executing.
    
    Still haven't decided exactly what this module should look like; should there
    be an "{!initialize}" function?  Maybe objects works better here?  (Maybe not;
    can we interface with them in C?) *)

(** {6 Types} *)

(** The register state of an X86 process *)
type x86state =
{ eax: int32;
  ecx: int32;
  edx: int32;
  ebx: int32;
  esp: int32;
  ebp: int32;
  esi: int32;
  edi: int32;
  cs: int32;
  ds: int32;
  es: int32;
  fs: int32;
  gs: int32;
  ss: int32;
  sf: bool;
  cf: bool;
  pf: bool;
  zf: bool;
  af: bool;
  ofl:bool;
  df: bool;
}

(** Process tracing object.  See introductory remarks. *)
type 'a tracer = { obj: 'a; process: ('a -> int32 -> bool); initialize: ('a -> x86state -> unit) }

(** Given a tracer object, tries to begin tracing.  Returns success if tracing
    began. *)
val begin_trace : 'a tracer -> bool
