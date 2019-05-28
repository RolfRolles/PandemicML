type sz = n
type var = id * sz * sz option
type flag = id

type condition = 
| Flag of flag
| C of c

type lval = var
type rval = 
| Var of v
| Interval of z * z


type rreil = 
| Load of lval * rval
| Store of lval * rval
| Brc of condition * rval
| Call of rval
| Return of rval
| Add  of lval * rval * rval
| Sub  of lval * rval * rval
| Mul  of lval * rval * rval
| Div  of lval * rval * rval
| Divs of lval * rval * rval
| Shl  of lval * rval * rval
| Shr  of lval * rval * rval
| Shrs of lval * rval * rval
| Mod  of lval * rval * rval
| And  of lval * rval * rval
| Or   of lval * rval * rval
| Xor  of lval * rval * rval
| Cmpeq  of flag * rval * rval
| Cmpleu of flag * rval * rval
| Cmples of flag * rval * rval
| Cmpltu of flag * rval * rval
| Cmplts of flag * rval * rval
| SignExtend of lval * rval
| Mov of lval * rval