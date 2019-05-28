open AVM2Pervasives

type t =
{
  init: u30;
  trait_count: u30;
  trait: AVM2traits_info.t array;
}

val parse : prim_parser -> t