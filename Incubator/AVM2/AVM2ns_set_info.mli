open AVM2Pervasives

(** "Defines a set of namespaces, allowing the set to be used as a unit in the 
    definition of multinames" *)
type t = 
{ (* Number of namespaces *)
  count: u30;
  ns: u30 array;
}

val parse : prim_parser -> t