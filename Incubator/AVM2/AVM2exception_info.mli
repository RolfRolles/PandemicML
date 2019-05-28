open AVM2Pervasives

type t = 
{
  from: u30;
  to_: u30;
  target: u30 array;
  exc_type: u30;
  var_name: u30;
}

val parse : prim_parser -> t