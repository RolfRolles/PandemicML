open AVM2Pervasives

type t =
{ val_: u30;
  kind: u8;
}

val parse : prim_parser -> t