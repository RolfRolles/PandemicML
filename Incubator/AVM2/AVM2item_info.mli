open AVM2Pervasives

type t =
{ key: u30;
  value: u30;
}

val parse : prim_parser -> t