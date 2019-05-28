open AVM2Pervasives

type t =
{ name: u30;
  item_count: u30;
  items: AVM2item_info.t array;
}

val parse : prim_parser -> t