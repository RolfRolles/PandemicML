type specified = int
type maximum = int

exception BVIndexOutOfBounds of string * specified * maximum

type t

val name_of_bv : t -> string
val size_of_bv : t -> int 
val bit : t -> int -> PL.prop

val create_fresh_named : string -> int -> t
val create_fresh_anon : int -> t
val create_fresh_from_i64 : int64 -> int -> t

type bvsubconstructor = 
| Extract of t * int * int
| Repeat of bvsubconstructor * int
| Existing of t

val create_custom_named : string -> bvsubconstructor list -> t
val create_custom_anon : bvsubconstructor list -> t
