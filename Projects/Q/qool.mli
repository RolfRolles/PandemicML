type exp = 
| LoadMem of exp * typ
| BinOp of binop * exp * exp
| Const of int32 * typ

type stmt =
| StoreMem of exp * exp * typ
| Assign of var * exp
| CallExternal of func * exp list
| Syscall