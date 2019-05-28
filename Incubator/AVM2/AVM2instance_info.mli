open AVM2Pervasives

type t =
{ name: u30;
  super_name: u30;
  flags: u8;
  protectedNs: u30 option;
  intrf_count: u30;
  interface: u30 array; 
  iinit: u30;
  trait_count: u30;
  trait: AVM2traits_info.t array;
}

val parse : prim_parser -> t