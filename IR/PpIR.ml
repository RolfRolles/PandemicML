open IR

let ppBinop = function
| Add  -> "+"
| Sub  -> "-"
| Mul  -> "*"
| SDiv -> "/s"
| UDiv -> "/u"
| SMod -> "%s"
| UMod -> "%u"
| Shl  -> "<<"
| Shr  -> ">>"
| Sar  -> ">>a"
| And  -> "&"
| Or   -> "|"
| Xor  -> "^"
| EQ   -> "=="
| NE   -> "!="
| ULT  -> "<u"
| ULE  -> "<=u"
| SLT  -> "<s"
| SLE  -> "<=s"

let ppUnop = function
| Neg -> "-"
| Not -> "!"

let ppTypereg = function
| TypeReg_1  -> "TypeReg_1"
| TypeReg_8  -> "TypeReg_8"
| TypeReg_16 -> "TypeReg_16"
| TypeReg_32 -> "TypeReg_32"
| TypeReg_64 -> "TypeReg_64"

let ppCastkind = function
| Unsigned ->  "unsigned"
| Signed   ->  "signed"
| High     ->  "high"
| Low      ->  "low"

let ppVar = function
| Mem(i,_,_) -> "mem"^string_of_int i
| Variable(i,s) ->
  match i with
  |  0 -> "EAX"
  |  1 -> "ECX"
  |  2 -> "EDX"
  |  3 -> "EBX"
  |  4 -> "ESP"
  |  5 -> "EBP"
  |  6 -> "ESI"
  |  7 -> "EDI"
  |  8 -> "ES"
  |  9 -> "CS"
  | 10 -> "SS"
  | 11 -> "DS"
  | 12 -> "FS"
  | 13 -> "GS"
  | 14 -> "CF"
  | 15 -> "PF"
  | 16 -> "AF"
  | 17 -> "ZF"
  | 18 -> "SF"
  | 19 -> "OF"
  | 20 -> "DF" 
  | 21 -> "CR0"
  | 22 -> "CR1"
  | 23 -> "CR2"
  | 24 -> "CR3"
  | 25 -> "CR4"
  | 26 -> "CR5"
  | 27 -> "CR6"
  | 28 -> "CR7"
  | 29 -> "DR0"
  | 30 -> "DR1"
  | 31 -> "DR2"
  | 32 -> "DR3"
  | 33 -> "DR4"
  | 34 -> "DR5"
  | 35 -> "DR6"
  | 36 -> "DR7"
  (* No 37, that's the memory -- change that? *)
  | 38 -> "AL"
  | 39 -> "AH"
  | 40 -> "CL"
  | 41 -> "CH"
  | 42 -> "DL"
  | 43 -> "DH"
  | 44 -> "BL"
  | 45 -> "BH"
  | 46 -> "AX"
  | 47 -> "CX"
  | 48 -> "DX"
  | 49 -> "BX"
  | 50 -> "SP"
  | 51 -> "BP"
  | 52 -> "SI"
  | 53 -> "DI"
  | _  -> "T"^string_of_int i^(
    match s with
    | TypeReg_1 -> "bit"
    | TypeReg_8 -> "b"
    | TypeReg_16 -> "w"
    | TypeReg_32 -> "d"
    | TypeReg_64 -> "q")

let f = false
let rec ppExpr b o = 
  let op,cl = if b then ("(",")") else ("","") in
  match o with
| Load(m,a,s) -> "load("^ppExpr f m^","^ppExpr f a^","^ppTypereg s^")"
| Store(m,a,t,s) -> "store("^ppExpr f m^","^ppExpr f a^","^ppExpr f t^","^ppTypereg s^")"
| Binop(l,o,r) -> op^ppExpr true l^ppBinop o^ppExpr true r^cl
| Unop(o,e) -> ppUnop o^ppExpr true e
| Const(i,s) -> Printf.sprintf "const(%s,0x%LX)" (ppTypereg s) i
| Var(v)     -> ppVar v
| Let(v,r,i)  -> "let "^ppVar v^" = "^ppExpr f r^" in "^ppExpr f i
| Cast(k,t,e) -> "cast("^ppCastkind k^","^ppTypereg t^","^ppExpr f e^")"

let ppInstr = function
| Assign(v,e) -> ppVar v^" = "^ppExpr f e
| Jmp(e) -> "jmp "^ppExpr f e
| CJmp(c,t,nt) -> "cjmp("^ppExpr f c^","^ppExpr f t^","^ppExpr f nt^")"
| Halt(e) -> "halt "^ppExpr f e
| Assert(e) -> "assert "^ppExpr f e
| Label(i) -> Printf.sprintf "label_%08lX:" i
| Comment(s) -> "; "^s
| Special(s) -> "???"