(* (int -> bool) -> 'a list -> 'a list
   pred : int -> bool, whether we should keep the element #int (counts from 0). *)
let filteri pred = 
  let rec aux i outlist = function
  | [] -> List.rev outlist
  | x::xs -> aux (i+1) (if pred i then (x::outlist) else outlist) xs
  in aux 0 []

(* (int -> bool) -> int -> 'a list -> 'a list list
   pred: as above 
   n: number of subsequences to generate
   inlist: arbitrary *)
let generate_subsequences pred n inlist =
  if n < 0 then invalid_arg "generate_subsequences";
  let rec aux i outlist = 
    if i = n
    then outlist
    else aux (i+1) ((filteri pred inlist)::outlist)
  in aux 0 []

(* int -> 'a list -> 'a list list
   n: n as in N-grams
   list (implicit): obvious *)
let compute_n_grams_compounded inlist n =
  let rec take_n_opt i acc = function
  | [] -> None
  | _ when i = 0 -> Some(List.rev acc)
  | x::xs -> take_n_opt (i-1) (x::acc) xs
  in
  let cons_some list = function | Some(x) -> x::list | None -> list in
  let rec aux n_grams list = match list with
  | [] -> n_grams
  | x::xs -> aux (cons_some n_grams (take_n_opt n [] list)) xs
  in aux inlist

let compute_n_grams = compute_n_grams_compounded []
  
(* 'a list list -> ('a list, int) Hashtbl.t
   list: obvious *)
let count_occurences list =
  (* Could pass in the number of n-grams ahead of time for efficiency *)
  let hashtbl = Hashtbl.create 1023 in
  let update el = 
    let existing =
      try
        Hashtbl.find hashtbl el
      with
        Not_found -> 0
    in
    Hashtbl.replace hashtbl (existing+1)
  in
  List.iter update list;
  hashtbl

(* (int -> bool) -> int -> 'a list -> ('a list, int) Hashtbl.t
   pred: distribution dropping function
   n: n-gram length
   inlist: initial list *)
let ngram_occurences pred n inlist = 
  count_occurences
   (List.fold_left 
     (fun acc el -> compute_n_grams_compounded acc n el) 
     (generate_subsequences pred n inlist))

(* (int -> bool) -> int -> 'a list -> ('a list, int) Hashtbl.t
   pred: distribution dropping function
   n: n-gram length to compute up to (e.g. we compute 1-grams, 2-grams, ..., n-grams)
   inlist: initial list *)
let ngram_occurences_upto_n pred n inlist = 
  if n < 1 then invalid_arg "ngram_occurences_upto_n";
  let rec aux i outlist = 
    if i < (n+1)
    then aux (i+1) ((ngram_occurences pred i inlist)::outlist)
    else List.rev outlist
  in aux 1 []

(* Same as before, but for a list of sequences *)
let ngram_occurences_upto_n_multi pred n =
  List.map (ngram_occurences_upto_n pred n) 

(* int -> int -> (int -> bool)
   numerator, denominator: obvious *)
let make_uniform_distribution numerator denominator = 
  let float = (float_of_int numerator) /. (float_of_int denominator) in
  (fun _ -> Random.float 1.0l <= float)

let els = Hashtbl.fold (fun el count acc -> (el,count)::acc) ht [] in
let sorted_els = List.sort (fun (_,lc) (_,rc) -> rc >= lc) els in
