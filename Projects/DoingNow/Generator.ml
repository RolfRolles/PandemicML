exception GenerationComplete

let mk_generator l = 
  let _ = if l = [] then invalid_arg "mk_generator:  empty list" in
  let lr = ref l in
 (fun b -> match b,!lr with 
  | _,[] -> (if b then lr := l); raise GenerationComplete
  | true,[x] -> lr := l; raise GenerationComplete
  | true,x::(y::xs as tail) -> lr := tail; y
  | false,x::xs -> x)
  
let generator_combine g1 g2 = (fun b -> let v1,b = try (g1 b,false) with GenerationComplete -> (g1 b,true) in (v1,g2 b))
let generator_combine_flatten f g1 g2 = let g = generator_combine g1 g2 in (fun b -> f (g b))

let mk_pair_generator_g g1 g2 = generator_combine g1 g2
let mk_pair_generator l1 l2 = mk_pair_generator_g (mk_generator l1) (mk_generator l2) 

let flatten_triple ((a,b),c) = (a,b,c)
let flatten_quad   ((a,b,c),d) = (a,b,c,d)

let triple_generator_flatten f g1 g2 g3 = generator_combine_flatten f (generator_combine g1 g2) g3
let mk_triple_generator_g g1 g2 g3 = triple_generator_flatten flatten_triple g1 g2 g3 
let mk_triple_generator l1 l2 l3 = mk_triple_generator_g (mk_generator l1) (mk_generator l2) (mk_generator l3) 

let mk_quad_generator_g g1 g2 g3 g4 = generator_combine_flatten flatten_quad (mk_triple_generator_g g1 g2 g3) g4
let mk_quad_generator l1 l2 l3 l4 = mk_quad_generator_g (mk_generator l1) (mk_generator l2) (mk_generator l3) (mk_generator l4) 

let mk_filtered_generator f g = (fun b -> let rec aux b = let r = g b in if f r then aux true else r in aux b)

let mk_sequential_generator l = 
  let _ = if l = [] then invalid_arg "mk_sequential_generator:  empty list" in
  let lr = ref l in
  let rec gen b = 
    match b,!lr with
    | _,[] -> (if b then lr := l); raise GenerationComplete
    | true,f::[]  -> (try f b with GenerationComplete -> (lr := l; raise GenerationComplete))
    | true,f::fs  -> (try f b with GenerationComplete -> (lr := fs; gen true))
    | false,f::fs -> f b
  in gen
