let open IDAGraph in
let test_graph_viewer = {
  graph = ();
  sizer = (fun _ -> 4);
  text  = (fun _ i -> IDAColor.col_addr (string_of_int i));
  edges = (fun _ -> [(0,1);(0,2);(1,3);(2,3)]);
}
in show_graph test_graph_viewer "Testing";;