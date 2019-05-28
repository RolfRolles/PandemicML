type bvpair = string * string

type arrayt =
| RealEntry of bvpair * bvpair
| DefaultEntry of bvpair

type t =
| BitVector of bvpair
| Array of arrayt list