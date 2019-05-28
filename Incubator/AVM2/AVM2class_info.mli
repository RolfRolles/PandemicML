open AVM2Pervasives

type t = 
{
  cinit: u30;
  trait_count: u30;
  traits: AVM2traits_info.t array;
}

val parse : prim_parser -> t