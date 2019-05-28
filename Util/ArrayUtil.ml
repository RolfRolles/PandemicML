(* Tail-recursive *)
let map2 init f x_arr y_arr =
  let len1 = Array.length x_arr in
  let len2 = Array.length y_arr in
  if len1 <> len2 then (invalid_arg "array_map2:  array sizes did not match");
  let outarr = Array.make len1 init in
  let rec aux i =
    if i = len1 then ()
    else (outarr.(i) <- f x_arr.(i) y_arr.(i); aux (i+1))
  in aux 0;
  outarr

(*val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a*)

let fold_lefti f acc arr =
  snd(Array.fold_left  (fun (i,acc) arrel -> (i+1,f i acc arrel)) (0,acc) arr)

let fold_righti f arr acc =
  snd(Array.fold_right (fun arrel (i,acc) -> (i-1,f i arrel acc)) arr (Array.length arr - 1,acc))

(*
let entropy_of_histogram_array histogram sample_size = 
  let d1log2 = 1.4426950408889634073599246810023 in
  Array.fold_left (fun entropy byte_dist_ent -> 
    let proportion = (float_of_int byte_dist_ent) /. (float_of_int sample_size) in
 	  if proportion > 0.0 then entropy -. (proportion *. (log proportion) *. d1log2) else entropy)
    0.0
    histogram
*)