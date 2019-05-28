open AVM2Pervasives

type t =
{
  param_name: u30 array;
}

val parse : prim_parser -> t