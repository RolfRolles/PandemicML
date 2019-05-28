open AVM2Pervasives

type t =
{
  method_: u30;
  max_stack: u30;
  local_count: u30;
  init_scope_depth: u30;
  max_scope_depth: u30;
  code_length: u30;
  code: u8 array;
  exception_count: u30;
  exception_: AVM2exception_info.t array;
  trait_count: u30;
  trait: AVM2traits_info.t array;
}

val parse : prim_parser -> t