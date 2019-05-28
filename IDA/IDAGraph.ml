type 'a graph_viewer =
{ graph: 'a;
  sizer: 'a -> int;
  text:  'a -> int -> string;
  edges: 'a -> (int * int) list;
}

external display_graph : 'a graph_viewer -> string -> bool = "OCamlShowGraph"

let show_graph = display_graph