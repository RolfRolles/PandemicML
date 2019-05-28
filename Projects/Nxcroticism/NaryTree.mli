type ('k,'d,'v) nary_tree = | Node of 'k * 'd * ('v,unit) Hashtbl.t * ('k,('k,'d,'v) nary_tree) Hashtbl.t
type ('k,'d,'v) nary_tree_roots = ('k,('k,'d,'v) nary_tree) Hashtbl.t
type ('k,'d,'v) t = { roots: ('k,'d,'v) nary_tree_roots; } (* ' *)


val insert : ('a,'b,'c) t -> ('c*'a*'b) list -> unit 
val make   : unit -> ('a,'b,'c) t
val make_n : int  -> ('a,'b,'c) t

(* For N-ary trees where the data is an option type, thin the tree at every None *)
val filter : ('a, ('a, 'b option, 'c) nary_tree) Hashtbl.t -> ('a, ('a, 'b, 'c) nary_tree) Hashtbl.t -> unit
(* Filter a whole tree starting from the roots *)
val filter_roots : ('a, 'b option, 'c) t -> ('a, 'b, 'c) t