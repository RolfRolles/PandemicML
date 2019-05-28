module Z3 = Z3.V3

let z3TextualParsedModel_from_lexbuf = Z3ModelParser.model Z3ModelLexer.token

let f_printf fmt_etc = IDA.msg fmt_etc

type size  = (*| BIT *)| BYTE | WORD | DWORD

let bits_of_size = function
(*| BIT   -> 1*)
| BYTE  -> 8
| WORD  -> 16
| DWORD -> 32

exception Z3O_VARIABLE_NAME_DOES_NOT_EXIST of string
exception Z3O_VARIABLE_DOES_NOT_EXIST
exception Z3O_UNIMPLEMENTED of string
let z3o_default_ht_size = 1023

(* Move this stuff out to a Z3Util class *)
let string_of_search_failure = function 
|	Z3.NO_FAILURE       -> "NO_FAILURE"
|	Z3.UNKNOWN          -> "UNKNOWN"
|	Z3.TIMEOUT          -> "TIMEOUT"
|	Z3.MEMOUT_WATERMARK -> "MEMOUT_WATERMARK"
|	Z3.CANCELED         -> "CANCELED"
|	Z3.NUM_CONFLICTS    -> "NUM_CONFLICTS"
|	Z3.THEORY           -> "THEORY"
|	Z3.QUANTIFIERS      -> "QUANTIFIERS"

(* Change this *)
type parsed_model = string
let string_of_parsed_model pm = pm

type querymodel = 
| UNSAT
| UNKNOWN of string
| SAT of parsed_model

let querymodel_of_model z3 m = function
| Z3.L_FALSE -> UNSAT
| Z3.L_UNDEF -> UNKNOWN(string_of_search_failure (Z3.get_search_failure z3))
| Z3.L_TRUE  -> SAT(let rmod = Z3.model_to_string z3 m in Z3.del_model z3 m; rmod)

let string_of_querymodel = function
| UNSAT -> "unsat"
| UNKNOWN(s) -> "unknown: "^s
| SAT(pm) -> "sat\n"^string_of_parsed_model pm

module Int32Comparator = struct
  type t = int32
  let compare i j = Int32.compare i j
end

module Int32Set = Set.Make(Int32Comparator)

(* Make pretty type constructors from the parsed model *)
let tyconify_z3TextualParsedModel n =
  let open Z3TextualParsedModel in 
  (* Make a byte array from the parsed model *)
  let make_array_with_default_value n list =
    let default_value,tl = match list with
    | [] -> let _ = invalid_arg "make_array_with_default_value: empty list" in (0l,[])
    | DefaultEntry(sz,value)::xs -> Int32.of_string value, xs
    | _ -> 0l, list
    in
    let arr = 
      List.fold_left (fun acc -> function
        | DefaultEntry(_) -> let _ = failwith "make_array_with_default_value: should have consumed the DefaultEntry already? check input format" in acc
        | RealEntry((isz,ival),(esz,eval)) -> Int32Set.add (Int32.of_string eval) acc)
      (Int32Set.empty)
      tl
    in
    (arr,default_value)
  in
  List.fold_left 
   (fun (constants,arrays) x -> match x with
    | n,BitVector( "8",value) -> ((n, BYTE,Int32.of_string value)::constants,arrays)
    | n,BitVector("16",value) -> ((n, WORD,Int32.of_string value)::constants,arrays)
    | n,BitVector("32",value) -> ((n,DWORD,Int32.of_string value)::constants,arrays)
    | n,BitVector(x,_) -> failwith ("tyconify_z3TextualParsedModel: unknown bitvector size "^x)
    | s,Array(list) -> (constants,(s,make_array_with_default_value n list)::arrays))
   ([],[])

(* Make a string from the z3textualparsedmodel.  This is superior to just 
   printing the model that Z3 gives us, because it has reference arrays and
   array contents strewn about in random order. *)
let string_of_z3textualparsedmodel (name,ty) = 
  let open Z3TextualParsedModel in 
  let string_of_bvpair (sz,value) = Printf.sprintf "BV(%s): %s" sz value in
  let str = match ty with
  | BitVector(bvpair) -> string_of_bvpair bvpair
  | Array(list) -> 
    let string_of_array_el = function
    | DefaultEntry(bvpair) -> Printf.sprintf "Default entry: %s" (string_of_bvpair bvpair)
    | RealEntry(bv1,bv2) -> Printf.sprintf "%s -> %s" (string_of_bvpair bv1) (string_of_bvpair bv2)
    in
    List.fold_left (fun acc el -> acc^((string_of_array_el el)^"\n")) "" list
  in Printf.sprintf "%s: %s" name str;;

let uncurry3 f (a,b,c) = f a b c

class z3o =
  object(self)
    val mutable z3              = Z3.mk_context_x [|("MODEL", "true");("SOFT_TIMEOUT", "1000")|]
    method get_z3 ()            = z3

    val mutable var_counter     = 0
    method get_ctr ()           = let v = var_counter in var_counter <- v + 1; v

    val mutable hash_name2var   = Hashtbl.create z3o_default_ht_size
    method get_var name         = try Hashtbl.find hash_name2var name with Not_found -> raise (Z3O_VARIABLE_NAME_DOES_NOT_EXIST(name))
    
    (* CAN'T DO THIS.  CAN'T USE A Z3.ast AS A HASHTABLE KEY *)
  (*val mutable hash_var2name   = Hashtbl.create z3o_default_ht_size
    method get_name var         = try Hashtbl.find hash_var2name var  with Not_found -> raise (Z3O_VARIABLE_DOES_NOT_EXIST)*)

    method set_var name var     = Hashtbl.replace hash_name2var name var (*; Hashtbl.replace hash_var2name var name*)

    (* Hack to get around the type system (can't initialize defaults due to 'z3' also being a mutable val). Not too ugly. *)
    val mutable bv_sorts        = Hashtbl.create 10
    method private mk_bv_sort i = Z3.mk_bv_sort z3 i
    method bv_sort i            = 
      let res = 
        try 
          Hashtbl.find bv_sorts i
        with Not_found -> 
          let s = self#mk_bv_sort i in Hashtbl.replace bv_sorts i s; s 
      in res
    method private new_bv_var n name = let v = Z3.mk_const z3 (Z3.mk_string_symbol z3 name) (self#bv_sort  n) in self#set_var name v; v
    method  new_byte name       = self#new_bv_var  8 name
    method  new_word name       = self#new_bv_var 16 name
    method new_dword name       = self#new_bv_var 32 name

    method new_const n str      = Z3.mk_numeral z3 str (self#bv_sort  n)

    method private new_const_of_int64 n i64 = Z3.mk_numeral z3 (Printf.sprintf "%Ld" i64) (self#bv_sort  n)
    method  byte_of_int64 i64   = self#new_const_of_int64  8 i64
    method  word_of_int64 i64   = self#new_const_of_int64 16 i64
    method dword_of_int64 i64   = self#new_const_of_int64 32 i64
    method qword_of_int64 i64   = self#new_const_of_int64 64 i64

    method new_const_of_int32 n i32 = Z3.mk_numeral z3 (Printf.sprintf "%ld" i32) (self#bv_sort  n)
    method  byte_of_int32 i32   = self#new_const_of_int32  8 i32
    method  word_of_int32 i32   = self#new_const_of_int32 16 i32
    method dword_of_int32 i32   = self#new_const_of_int32 32 i32

    method private new_const_of_int n i = Z3.mk_numeral z3 (Printf.sprintf "%d" i) (self#bv_sort  n)
    method  byte_of_int i       = self#new_const_of_int  8 i
    method  word_of_int i       = self#new_const_of_int 16 i
    method dword_of_int i       = self#new_const_of_int 32 i

    method mk_ne e1 e2          = Z3.mk_not z3 (Z3.mk_eq z3 e1 e2)
    
    val mutable array_sorts     = Hashtbl.create 10

    method array_sort iidx iel  = 
      let res = 
        try 
          Hashtbl.find array_sorts (iidx,iel)
        with Not_found -> 
          let sidx = self#bv_sort iidx in 
          let sel  = self#bv_sort iel  in 
          let aso  = Z3.mk_array_sort z3 sidx sel in
          Hashtbl.replace array_sorts (iidx,iel) aso; aso
      in res

    method new_array name iidx iel = 
      Z3.mk_const 
        z3 
       (Z3.mk_string_symbol z3 name) 
       (self#array_sort iidx iel)
    
    method mk_select_conc_dword_idx_int aarr idx = 
      Z3.mk_select 
        z3 
        aarr
       (self#dword_of_int idx)

    method establish_checkpoint () = Z3.push z3
    method return_to_checkpoint () = Z3.pop z3 1; Z3.push z3
  end;;

(*
let mk_sized_var   (z3o:z3o) varname = function
(*| BIT   -> z3o#new_bit   varname*)
| BYTE  -> z3o#new_byte  varname
| WORD  -> z3o#new_word  varname
| DWORD -> z3o#new_dword varname

let mk_sized_const (z3o:z3o) i32 = function
(*| BIT   -> z3o#  bit_of_int32 i32*)
| BYTE  -> z3o# byte_of_int32 i32
| WORD  -> z3o# word_of_int32 i32
| DWORD -> z3o#dword_of_int32 i32
*)