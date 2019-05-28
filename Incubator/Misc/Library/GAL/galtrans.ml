type mregion = int

type lhs     = Ignore | LRegister of reg | LMem of mregion * rhs
type rhs     = Bitvector of mword | RRegister of reg | RMem of mregion * rhs | Restrict of rhs * int * int
type jump    = Goto of addr | Cgoto of rhs
type control = Jump of jump | Ite of cond * jump * jump
type data    = Assign of lhs list * f * rhs list
type instr   = data list * control

let gentest1 node_init =
  let res = (* empty Set (???) *) in
  let rec aux node phi = 
    match node with
    | ??? end node ??? ->
      try
        let sp = solve phi in
        (* Set Add ??? *) sp
      with Unsat -> ()
    | Assign(
    