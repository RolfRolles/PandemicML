let rec print_generator p g = 
  match try Some(g true) with Generator.GenerationComplete -> None with
  | Some(v) -> p v; print_generator p g
  | None -> ()
  
let gen_t = Generator.mk_triple_generator [1;2;3] [4;5;6] [7;8;9]
let gen_q = Generator.mk_quad_generator   [1;2] [3;4] [5;6] [7;8]
let gen_f = Generator.mk_filtered_generator (fun (_,d,_) -> d land 1 = 0) gen_t
let gen_s = Generator.mk_sequential_generator [gen_t;gen_f]

let print_triple (a,b,c) = Printf.printf "(%d,%d,%d)\n"    a b c
let print_quad (a,b,c,d) = Printf.printf "(%d,%d,%d,%d)\n" a b c d

let _ = print_generator print_triple gen_t
let _ = print_generator print_quad   gen_q
let _ = print_generator print_triple gen_f
let _ = print_generator print_triple gen_s

let rec test_generator p g = function
| l::ls -> let v = g true in
  if v = l 
  then test_generator p g ls 
  else (failwith (Printf.sprintf "test_generator printer: expected %s, got %s\n" (p l) (p v)))
| [] -> ()
