open AVM2Pervasives

type t =
{ name: u30;
  kind: u8;
  data: u8 array;
  metadata_count: u30 option;
  metadata: (u30 array) option;
}

val parse : prim_parser -> t