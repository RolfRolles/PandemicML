type bitwise_interval = 
| BWBottom
| BWZeroZero
| BWOneOne
| BWTop

type bitwise_interval_vector = bitwise_interval array

let meet_bitwise_interval bw1 bw2 = 
  match (bw1,bw2) with
  | BWBottom,_
  | _,BWBottom            -> BWBottom
  | BWZeroZero,BWOneOne   -> BWBottom
  | BWOneOne,BWZeroZero   -> BWBottom
  | BWZeroZero,BWZeroZero -> BWZeroZero
  | BWOneOne,BWOneOne     -> BWOneOne
  | BWTop,x
  | x,BWTop               -> x
  
let meet_bitwise_interval_vector = (*ArrayUtil.*)map2 (BWBottom) meet_bitwise_interval

let concretize_two_bitwise (bw1,bw0) =
  match bw1,bw0 with
  | BWBottom,_
  | _,BWBottom -> 0
  | BWZeroZero,BWZeroZero -> (1 lsl 0) 
  | BWZeroZero,BWOneOne   -> (1 lsl 1)
  | BWOneOne,  BWZeroZero -> (1 lsl 2)
  | BWOneOne,  BWOneOne   -> (1 lsl 3)
  | BWTop,     BWZeroZero -> (1 lsl 2) lor (1 lsl 0)
  | BWTop,     BWOneOne   -> (1 lsl 3) lor (1 lsl 1)
  | BWZeroZero,BWTop      -> (1 lsl 1) lor (1 lsl 0)
  | BWOneOne  ,BWTop      -> (1 lsl 3) lor (1 lsl 2)
  | BWTop     ,BWTop      -> 15

type interval = int * int

let string_of_interval (l,u) = Printf.sprintf "[%d,%d]" l u

let concretize_interval (beg,ending) = 
  let mk_mask i = 1 lsl i in
  let rec aux value j =
    if j <= ending
    then aux (value lor (mk_mask j)) (j+1)
    else value
  in aux 0 beg

let concrete_meet ps1 ps2 = ps1 land ps2
let concrete_join ps2 ps2 = ps1 lor  ps2

let string_of_concrete ps = 
  "{"^
  (if ps land 0x1 then "0 " else "")^
  (if ps land 0x2 then "1 " else "")^
  (if ps land 0x4 then "2 " else "")^
  (if ps land 0x8 then "3 " else "")^
  "}"

let meet_concretizations interval two_bitwise = 
  concrete_meet 
   (concretize_interval interval) 
   (concretize_two_bitwise two_bitwise)

let rec all_abstract_intervals beg ending = 
  let beg_pairs =
    let rec aux list i =
      if i <= ending
      then aux ((beg,i)::list) (i+1)
      else List.rev list
    in aux [] beg
  in
  if beg <= ending
  then beg_pairs@(all_abstract_intervals (beg+1) ending)
  else []

let all_abstract_intervals 

let all_abstract_bitwise_intervals = 