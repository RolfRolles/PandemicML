type kind_of_value_set =
| VSglob 
| VSsingle 
| VSarb 
| VSTop

type region =
| Global
| Local of int32
| Heap of int32
| Special of string

module ValueSet = struct
  module RegionMap = struct
    type t = region
    let compare i j = match i,j with
    | Global,Global         -> 0
    | Local(i),Local(j)     -> Int32.compare i j
    | Heap(i),Heap(j)       -> Int32.compare i j
    | Special(s),Special(t) -> String.compare s t
    | Global,_              -> -1
    | Local(_),Global       -> 1
    | Local(_),_            -> -1
    | Heap(_),Global        -> 1
    | Heap(_),Local(_)      -> 1
    | Heap(_),_             -> -1
    | Special(_),Global     -> 1
    | Special(_),Local(_)   -> 1
    | Special(_),Heap(_)    -> 1
  end
  module ValueSetMap = Map.Make(RegionMap)

  type t = StridedInterval.strided_interval ValueSetMap.t
  
  let sorted_list_of_value_sets = ValueSetMap.bindings
  
  let kind t =
    match sorted_list_of_value_sets t with
    | Global::[] -> VSglob
    | x::[] -> VSsingle
    | x::y::ys -> VSarb
    | [] -> VStop

  let add k si1 si2 =
    





Terminology

MemRgn = {Global} \cup Procs \cup AllocMemRgn
ValueSet = MemRgn -> StridedInterval
AlocEnv[R] = alocs[R] -> ValueSet

Components of AbsEnv:

register -> ValueSet
Flag -> Bool3
{Global} -> AlocEnv[Global]
Proc -> AlocEnv[Proc]
AllocMemRgn -> AlocEnv[AllocMemRgn]
