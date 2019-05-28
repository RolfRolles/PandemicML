open AVM2Pervasives

type t =
{ option_count: u30;
  option: AVM2option_detail.t array;
}

val parse : prim_parser -> t