type specified = int
type maximum = int

exception BVIndexOutOfBounds of string * specified * maximum

val pzero : PL.prop
val pone : PL.prop

type t

val tzero : t
val tone : t

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


val map_anon : (PL.prop -> PL.prop) -> t -> t
val map_named : string -> (PL.prop -> PL.prop) -> t -> t
val map2_anon : (PL.prop -> PL.prop -> PL.prop) -> t -> t -> t
val map2_named : string -> (PL.prop -> PL.prop -> PL.prop) -> t -> t -> t
val iteri : (PL.prop -> int -> unit) -> t -> unit
val fold_left : ('a -> PL.prop -> 'a) -> 'a -> t -> 'a