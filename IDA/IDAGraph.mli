(** Module for visualizing graphs in IDA's interactive graph viewer. *)

(** {6 Types} *)

(** A type for visualizing the graph layout on IDA's graph viewer.  The [graph]
    itself is an arbitrary object.  The [sizer] function produces the number of
    vertices, given the object.  The [text] function takes an object and an 
    integer (a vertex number), and returns the text for that vertex.  The 
    [edges] function takes an object and returns a list of pairs between 
    numbered vertices.  The uniq field is deprecated and should be removed. *)
type 'a graph_viewer =
{ graph: 'a;
  sizer: 'a -> int;
  text:  'a -> int -> string;
  edges: 'a -> (int * int) list;
}

(** {6 Functions} *)

(** Display a graph viewer with a given title in IDA. *)
val show_graph : 'a graph_viewer -> string -> bool