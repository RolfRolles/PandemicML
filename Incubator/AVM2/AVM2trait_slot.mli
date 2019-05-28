open AVM2Pervasives

type t =
{
  slot_id: u30;
  type_name: u30;
  vindex: u30;
  vkind: u8;
}

val parse : prim_parser -> t