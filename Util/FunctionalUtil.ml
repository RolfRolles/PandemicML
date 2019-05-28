(* Belongs in a FunctionalUtil module *)
let id x = x
let uncurry2 f (x,y)     = f x y
let uncurry3 f (x,y,z)   = f x y z
let uncurry4 f (x,y,z,w) = f x y z w