open AVM2Pervasives

(** A string of 16-bit characters *)
type t = 
{ (* Size of UTF-8 encoded field below *)
  size: u30;
  (* UTF-8 encoded string *)
  utf8: u8 array;
}

val parse : prim_parser -> t