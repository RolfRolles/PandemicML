open AVM2Pervasives

type t = 
{ (* CONSTANT_ type describing the namespace *)
  kind: u8;
  name: u30; (** Depending upon the kind, is either a reference into the 
                 string_info in the cpool_info structure, or zero. *)
}

val parse : prim_parser -> t