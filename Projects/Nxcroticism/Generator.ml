(* This module implements "Generators".  This is an extremely elegant and powerful 
   design technique for programs that can be viewed as a series of transformations
   upon data.  Generators eliminate a lot of tedious boilerplate while programming.
   
   The basic idea of a generator is of a function that you can call to generate
   objects.  For example, let's say we start with a list [1;2;3] and we create a
   generator from it.  Now we have a function g : (bool -> int) that we can call
   to generate the values of the list successively.  The bool tells whether we 
   want to peek at the value without updating the generator state internally (if
   the bool is false), or if we want to return the value and also increment the 
   state (if true).
   
   An example run, starting from a generator created from the list [1;2;3].  Call
   that generator g123, as we will use it again later.
   
   g123 true  yields 1
   g123 true  yields 2
   g123 true  yields 3
   g123 true  throws GenerationComplete and resets to the beginning of the list
   g123 true  yields 1
   g123 false yields 2
   g123 true  yields 2

   Note that, although the example above involved a list, the concept of generators
   is more general and doesn't necessarily have anything to do with lists.
   
   The strength of generators lies in their composability.  Let's say I have two
   lists and I want to generate all pairs of elements drawn from either list.
   This is as easy as calling mk_pair_generator_g with the two generators.
   mk_triple_generator_g and mk_quad_generator_g do what you expect.
   
   Let's say I want to throw away some values of a generator.  This is the concept
   of "filtration".  mk_filtered_generator (fun i -> not(is_odd i)) g123 would
   generate the sequence `1,3,throw GenerationComplete.`

   Let's say I only want some particular number of results from a given generator.
   mk_counting_generator 2 g123 would generate the sequence `1,2,throw GenerationComplete.`
   
*)

exception GenerationComplete

let mk_generator l = 
  if l = [] then (fun _ -> raise GenerationComplete) else
  let lr = ref l in
 (fun b -> match b,!lr with 
  | _,[]          -> (if b then lr := l); raise GenerationComplete
  | true, x::tail -> lr := tail; x
  | false,x::xs   -> x)
  
let generate_opt g b = try Some(g b) with GenerationComplete -> None

let generator_combine g1 g2 = (fun b -> 
  let increment g = try (g true; ()) with GenerationComplete -> () in
  match b,generate_opt g1 b,generate_opt g2 false with

  (* g1 not empty, g2 empty => something went wrong; let's stop the generator *)
  | _,Some(_),None    -> raise GenerationComplete 
  (* failwith "generator_combine:  illegal generator configuration" *)

  (* Had a pair, so return it *)
  | _,Some(x),Some(y) -> (x,y)

  (* Both generators were empty, and an update was not called for *)
  | false,None,None   -> raise GenerationComplete

  (* Wrinkly case:  first generator is empty, second one is not, but an update
     wasn't called for.  This is a "half-incremented" state, where the first
     generator has lapsed, but the second one hasn't been updated yet.  This
     happens as soon as the first generator lapses:  the call to generate_opt
     will return Some, yet the internal state of the generator will be lapsed.
     However, since it returned Some, there was no indication that we needed
     to increment that generator.  This case is that behavior coming home to
     roost.  We need to increment the second generator, completing the second
     half of the update.  If the second generator lapses in the process, then
     we have exhausted both of them.  If it does not, then we update the first
     generator back to the beginning of the list. *)
  | false,None,Some(y)-> increment g2; 
   (match generate_opt g2 false with
    | Some(y) -> (let _ = increment g1 in g1 false,y)
    | None    -> raise GenerationComplete)

  (* First generator was empty, and an update was called for.
     In this case, g2's value is stale, so we increment it.
     It may now be the case that g2 is empty.
     If it is, this is the end of both generators, so throw.
     Otherwise, return the pair.  *)
  | true,None,_       -> increment g2;
   (match generate_opt g2 false with
    | Some(y) -> (g1 true,y)
    | None -> increment g2; raise GenerationComplete))

let generator_combine_flatten f g1 g2 = 
  let g = generator_combine g1 g2 in 
 (fun b -> f (g b))

let mk_pair_generator_g g1 g2 = 
  generator_combine g1 g2

let mk_pair_generator l1 l2 = 
  mk_pair_generator_g (mk_generator l1) (mk_generator l2) 

let flatten_triple ((a,b),c)     = (a,b,c)
let flatten_quad   ((a,b),(c,d)) = (a,b,c,d)

let triple_generator_flatten f g1 g2 g3 = 
  generator_combine_flatten f (generator_combine g1 g2) g3

let mk_triple_generator_g g1 g2 g3 = 
  triple_generator_flatten flatten_triple g1 g2 g3 

let mk_triple_generator l1 l2 l3 = 
  mk_triple_generator_g 
    (mk_generator l1) 
    (mk_generator l2) 
    (mk_generator l3) 

let mk_quad_generator_g g1 g2 g3 g4 = 
  generator_combine_flatten 
    flatten_quad 
   (mk_pair_generator_g g1 g2) 
   (mk_pair_generator_g g3 g4)

let mk_quad_generator l1 l2 l3 l4 = 
  mk_quad_generator_g 
   (mk_generator l1) (mk_generator l2) 
   (mk_generator l3) (mk_generator l4) 

let mk_filtered_generator f g = 
 (fun b -> 
    let rec aux b = 
      let r = g b in 
      if f r 
      then aux true 
      else r 
    in 
    aux b)

let mk_counting_generator n g =
  let rn = ref 0 in
 (fun b -> if !rn = n then raise GenerationComplete else (incr rn; g b))

(* Given a generator that generates generators, create the equivalent of a 
   sequential generator.  I.e., a generator that exhausts the first generator,
   then moves on to the next, until the last generator. *)
let generate_all_generators ggen =
  let g1 = ref (ggen true) in
  let rec aux b = try !g1 b with GenerationComplete -> (g1 := ggen true; aux b) in
  aux

let mk_exn_of_opt_generator g = 
 (fun b -> match g b with | None -> raise GenerationComplete | Some(x) -> x)

let list_generator_prepend    l g = generator_combine_flatten (fun (e,l) -> e::l) (mk_generator l) g
let mk_list_gen_of_lists        l = List.fold_right list_generator_prepend l (mk_generator [[]]) 
let list_generators_prepend g1 g2 = generator_combine_flatten (fun (e,l) -> e::l) g1 g2
let mk_list_gen_of_generators   l = List.fold_right list_generators_prepend l (mk_generator [[]]) 

let mk_transform_generator f g = (fun b -> f (g b))
let mk_transform_filter_generator f g = 
 (fun b -> 
    let rec aux _ = 
      match f (g b) with
      | Some(x) -> x
      | None -> aux ()
    in aux ())

let mk_sequential_generator l = 
  if l = [] then (fun _ -> raise GenerationComplete) else
  let lr = ref l in
  let rec gen b = 
    match b,!lr with
    | _,[] -> (if b then lr := l); raise GenerationComplete
    | true,f::[]  -> (try f b with GenerationComplete -> (lr := l; raise GenerationComplete))
    | true,f::fs  -> (try f b with GenerationComplete -> (lr := fs; gen true))
    | false,f::fs -> f b
  in gen

let rec iter f g   = match generate_opt g true with | Some(x) -> f x; iter f g    | None -> ()
let rec fold f g a = match generate_opt g true with | Some(x) -> fold f g (f x a) | None -> a

let map_multiply f g =
  let tgen = ref None in
  let rec gen b = 
    match !tgen with
    | Some(tg) -> (match generate_opt tg b with 
      | Some(x) -> x
      | None -> tgen := None; gen b)
    | None -> let x = g b in (tgen := Some(f x); gen b)
  in gen
