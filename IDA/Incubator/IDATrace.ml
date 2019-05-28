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

type 'a tracer = { obj: 'a; process: ('a -> int32 -> bool); initialize: ('a -> x86state -> unit) }

external trace : 'a tracer -> bool = "OCamlTrace" (* ' *)

let begin_trace = trace


