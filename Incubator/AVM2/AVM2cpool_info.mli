open AVM2Pervasives

type t =
{ int_count: u30;
  integer: s32 array;
  uint_count: u30;
  uinteger: u32 array;
  double_count: u30;
  double: d64 array; (* Should use floats here? *)
  string_count: u30;
  string_info: AVM2string_info.t array;
  namespace_count: u30;
  namespace: AVM2namespace_info.t array;
  ns_set_count: u30;
  ns_set: AVM2ns_set_info.t array
  multiname_count: u30;
  multiname: AVM2multiname_info.t array;
}

val parse : prim_parser -> t