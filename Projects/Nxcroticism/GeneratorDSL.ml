type ('a, 'b, 'c, 'd, 'e) gen_dsl =
| G1 of 'a list * ('a -> bool) option * ('a -> 'e)
| G2 of 'a list * 'b list * (('a * 'b) -> bool) option * (('a * 'b) -> 'e)
| G3 of 'a list * 'b list * 'c list * (('a * 'b * 'c) -> bool) option * (('a * 'b * 'c)-> 'e) 
| G4 of 'a list * 'b list * 'c list * 'd list * (('a * 'b * 'c * 'd) -> bool) option * (('a * 'b * 'c * 'd) -> 'e)
(* ' *)

let generator_dsl x = let open Generator in match x with
| G1(l1,None,m)             -> let g = mk_generator l1                                         in (fun b -> m (g b))
| G1(l1,Some(f),m)          -> let g = mk_filtered_generator f (mk_generator l1)               in (fun b -> m (g b))
| G2(l1,l2,None,m)          -> let g = mk_pair_generator l1 l2                                 in (fun b -> m (g b))
| G2(l1,l2,Some(f),m)       -> let g = mk_filtered_generator f (mk_pair_generator l1 l2)       in (fun b -> m (g b))
| G3(l1,l2,l3,None,m)       -> let g = mk_triple_generator l1 l2 l3                            in (fun b -> m (g b))
| G3(l1,l2,l3,Some(f),m)    -> let g = mk_filtered_generator f (mk_triple_generator l1 l2 l3)  in (fun b -> m (g b))
| G4(l1,l2,l3,l4,None,m)    -> let g = mk_quad_generator l1 l2 l3 l4                           in (fun b -> m (g b))
| G4(l1,l2,l3,l4,Some(f),m) -> let g = mk_filtered_generator f (mk_quad_generator l1 l2 l3 l4) in (fun b -> m (g b))
