(*
#use "C:\\paframework\\Incubator\\IntervalBitwiseIntervalDirectProduct.ml";;
*)

type bitwise_interval = 
| BWBottom
| BWZeroZero
| BWOneOne
| BWTop

let string_of_bitwise_interval = function
| BWBottom   -> "{}"
| BWZeroZero -> "[0,0]"
| BWOneOne   -> "[1,1]"
| BWTop      -> "[0,1]"

let string_of_two_bitwise (bw1,bw0) =
  string_of_bitwise_interval bw1^string_of_bitwise_interval bw0

type two_bitwise = bitwise_interval * bitwise_interval

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
  
let meet_two_bitwise (bw11,bw12) (bw21,bw22) = 
 (meet_bitwise_interval bw11 bw21,
  meet_bitwise_interval bw12 bw22)
  
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

let string_of_interval (l,u) = 
  if u < l
  then "[]"
  else Printf.sprintf "[%d,%d]" l u

let concretize_interval (beg,ending) = 
  if ending < beg
  then 0
  else
    let mk_mask i = 1 lsl i in
    let rec aux value j =
      if j <= ending
      then aux (value lor (mk_mask j)) (j+1)
      else value
    in aux 0 beg
  
let max i j = if i > j then i else j
let min i j = if i < j then i else j

let meet_interval (l1,u1) (l2,u2) = (max l1 l2,min u1 u2)

let meet_pair (i1,tb1) (i2,tb2) = (meet_interval i1 i2,meet_two_bitwise tb1 tb2)

let concrete_meet ps1 ps2 = ps1 land ps2

let string_of_concrete ps = 
  "{"^
  (if (ps land 0x1) <> 0 then "0 " else "")^
  (if (ps land 0x2) <> 0 then "1 " else "")^
  (if (ps land 0x4) <> 0 then "2 " else "")^
  (if (ps land 0x8) <> 0 then "3 " else "")^
  "}"

let meet_concretizations interval two_bitwise = 
  concrete_meet 
   (concretize_interval interval) 
   (concretize_two_bitwise two_bitwise)

let rec all_intervals beg ending = 
  let beg_pairs =
    let rec aux list i =
      if i <= ending
      then aux ((beg,i)::list) (i+1)
      else List.rev list
    in aux [] beg
  in
  if beg <= ending
  then beg_pairs@(all_intervals (beg+1) ending)
  else []
  
let all_intervals = (1,0)::(all_intervals 0 3)

let all_two_bitwise =
  let list = [BWBottom;BWZeroZero;BWOneOne;BWTop] in
  let rec cartesian_list_product sum list1 list2 =
    match list1 with
    | x::xs -> cartesian_list_product ((List.map (fun y -> (x,y)) list2)@sum) xs list2
    | [] -> sum
  in cartesian_list_product [] list list

module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let add_to_map map key pair =
  let existing_list = try IntMap.find key map with Not_found -> [] in
  IntMap.add key (pair::existing_list) map 

let meet_all_concretizations () =
  let meet i tb = 
    let meet = meet_concretizations i tb in
    let _ = IDA.msg "y1(%s) /\\ y2(%s) = %s\n" (string_of_interval i) (string_of_two_bitwise tb) (string_of_concrete meet) in
    meet
  in
  let rec print_all map list1 list2 =
    match list1 with
    | x::xs -> let map = List.fold_left (fun map y -> let meet = meet x y in add_to_map map meet (x,y)) map list2 in print_all map xs list2
    | [] -> map
  in print_all IntMap.empty all_intervals all_two_bitwise
  
let print_all_same_concretizations () =
  let map = meet_all_concretizations () in
  IntMap.iter 
   (fun k list -> 
      IDA.msg "%s -> {" (string_of_concrete k); 
      List.iter (fun (i,tb) -> IDA.msg "(%s,%s) " (string_of_interval i) (string_of_two_bitwise tb)) list;
      IDA.msg "\n")
    map    

let meet_all_same_concretizations () = 
  let map = meet_all_concretizations () in
  let meetmap = IntMap.map (fun list -> List.fold_left (fun e l -> meet_pair e l) (List.hd list) (List.tl list)) map in
  IntMap.iter 
   (fun k (i,tb) -> IDA.msg "%s -> (%s,%s)\n" (string_of_concrete k) (string_of_interval i) (string_of_two_bitwise tb))
    meetmap      
