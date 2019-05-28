(* Either *)
let ( ||| ) p1 p2 s = try p1 s with Not_found -> p2 s

(* Pipe forward *)
let ( >| ) p k i = let e, s = p i in k e, s

(* Sequential composition *)
let ( ++ ) p1 p2 s = let e1, s = p1 s in let e2, s = p2 s in (e1, e2), s

(* Matches the front of the input against some parser p *)
let some p = function | h::t when p h -> h, t | _ -> raise Not_found;;

(* Matches a specific character *)
let a x = some (( = ) x)

(* Matches zero or more repetitions of p *)
let rec many p s =
  try let e, s = p s in let es, s = many p s in e::es, s
  with Not_found -> [], s;;

(* Matches *)
let several p = many (some p);;

module Abstr : sig
    type t
    val x : t
  end = struct
    type t = int
    let x = 0
  end

let collect (h,t) = String.concat "" (List.map (String.make 1) (h::t));;

let fin = function | [] as t -> Abstr.x, t | _ -> raise Not_found

let space = function
| ' ' | '\t' | '\n' | '\r' -> true
| _ -> false

type token = Hex of int32

let hexdigit = function
| '0'..'9'
| 'a'..'f'
| 'A'..'F' -> true
| _ -> false

let int32_of_hexdigit x = match x with 
| '0'..'9' -> Int32.of_int (int_of_char x - (int_of_char '0'))
| 'a'..'f' -> Int32.of_int (0xa + (int_of_char x - (int_of_char 'a')))
| 'A'..'F' -> Int32.of_int (0xa + (int_of_char x - (int_of_char 'A')))
| _ -> failwith (Printf.sprintf "Why was %c passed to int32_of_hexdigit?" x)

let int32_of_hexchars c1 c2 = Int32.(logor (shift_left (int32_of_hexdigit c1) 4) (int32_of_hexdigit c2))

let list_of_string string =
  let list = ref [] in
  String.iter (fun c -> list := c :: !list) string;
  List.rev !list

