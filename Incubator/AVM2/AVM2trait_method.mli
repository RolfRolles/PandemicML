open AVM2Pervasives

type t = 
{
  disp_id: u30;
  method_: u30;
}

val parse : prim_parser -> t