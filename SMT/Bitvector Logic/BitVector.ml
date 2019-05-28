type specified = int
type maximum = int

exception BVIndexOutOfBounds of string * specified * maximum
exception BVZeroSized of string

let pzero = PL.Constant(PL.False)
let pone  = PL.Constant(PL.True)

type t = 
{
  name: string;
  bits: PL.prop array;
  real: bool;
}

let tzero = { name: "@zero"; bits: [|pzero|]; real: false; }
let tone  = { name: "@one";  bits: [|pone |]; real: false; }

let name_of_bv t = t.name
let size_of_bv t = Array.length t.bits
let bit t i = 
  let s = size_of_bv t in
  if i >= s then raise (BVIndexOutOfBounds(t.name,i,s))
  else t.bits.(i)

(* breal: true if it corresponds to a user-declared bitvector, false if it is
   the result of evaluating an anonymous subexpression *)
let create_inner breal name n =
  let prefix = 
    if breal
    then name^"#"
    else "@"^name^"#"
  in
  check_bv_size name n;
  { 
    name = name; 
    bits = Array.init n (fun i -> PL.Variable(prefix^string_of_int i));
    real = real;
  }  

let create_fresh_named = create_inner true
  
let anon_ctr = ref 0

let make_anon_name () = 
  let i = !anon_ctr in
  incr anon_ctr;
  "a"^string_of_int i

let create_fresh_anon = create_inner false (make_anon_name ())

let const_ctr = ref 0

let create_fresh_from_i64 i64 n =
  let i = !const_ctr in
  incr const_ctr;
  let name = "@c"^string_of_int i in
  let arr = Array.make n pzero in
  let rec aux i mask =
    if i = n then ()
    else 
     (if (Int64.logand i64 mask) <> 0L
      then arr.(i) <- (PL.Constant(PL.True));
      aux (i+1) (Int64.add mask mask))
  in aux 0 1L;
  { 
    name = name; 
    bits = arr;
    real = false;
  }

let write arr bv i len =
  let rec loop j = 
    if j <> len
    then 
      arr.(i+j) <- bit bv (l+j);
      loop (j+1)
    else 
  in
  loop 0;
  i+len

type bvsubconstructor =           
| Extract of t * int * int
| Repeat of bvsubconstructor * int
| Existing of t

let rec write_piece arr pos = function
| Extract(bv,h,l) -> write arr bv pos (h-(l-1))
| Repeat(x,n) -> 
  let rec aux pos i = 
    if i = n
    then pos
    else new_pos = aux (write_piece arr pos x) (i+1)
  in aux pos 0           
| Existing(bv) -> write arr bv pos (BitVector.size_of_bv bv)

let create_custom list = 
  let rec length_of_composed_bitvector = function
  | Extract(_,h,l) -> h-(l-1)
  | Repeat(x,n)    -> (length_of_composed_bitvector x)*n
  | Existing(bv)   -> size_of_bv bv
  in
  let len = List.fold_left (fun acc s = acc + (length_of_composed_bitvector s)) 0 list in
  let arr = Array.make len pzero in
  let _ = List.fold_left (write_piece arr) 0 list in
  arr

let create_custom_named name list =
{ 
  name = name; 
  bits = create_custom list;
  real = true;
}

let create_custom_anon list = 
{ 
  name = make_anon_name ();
  bits = create_custom list;
  real = false;
}

let map_core f bv1 =
  let l1 = size_of_bv bv1 in
  let obv = Array.make l1 pzero in
  let rec aux i =
    if i = l1
    then obv
    else 
      obv.(i) <- f (bit bv1 i);
      aux (i+1)
  in aux 0

let map_anon f bv1 = 
{
  name = make_anon_name ();
  bits = map_core f bv1;
  real = false;
}
      
let map_named name f bv1 = 
{
  name = name;
  bits = map_core f bv1;
  real = true;
}

let map2_core f bv1 bv2 =
  let l1,l2 = size_of_bv bv1,size_of_bv bv2 in
  if l1<>l2 
  then invalid_arg (Printf.sprintf "BitVector.map2_core: size %d != %d" l1 l2)
  else
    let obv = Array.make l1 pzero in
    let rec aux i =
      if i = l1
      then obv
      else 
        obv.(i) <- f (bit bv1 i) (bit bv2 i);
        aux (i+1)
    in aux 0

let map2_anon f bv1 bv2 = 
{
  name = make_anon_name ();
  bits = map2_core f bv1 bv2;
  real = false;
}
      
let map2_named name f bv1 bv2 = 
{
  name = name;
  bits = map2_core f bv1 bv2;
  real = true;
}

let iteri f bv =
  let len = size_of_bv bv in
  let rec aux i = 
    if i = len
    then ()
    else f (bit bv i) i; aux (i+1)
  in aux 0
  
let fold_left f acc bv =
  let a = ref acc in
  iteri (fun b _ -> a := f (!a) b);
  !a