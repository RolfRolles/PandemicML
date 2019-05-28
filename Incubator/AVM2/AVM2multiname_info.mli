open AVM2Pervasives

(* *)
type t =
{ kind: u8;
  data: u8 array;
}

val parse : prim_parser -> t