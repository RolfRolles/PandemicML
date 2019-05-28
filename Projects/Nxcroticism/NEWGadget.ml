type memloc = IR.var * int32

type reg = IR.var
type  gadget =
| BinopReg of reg * reg * IR.binop * reg
| BinopRead of reg * IR.binop * memloc
| BinopWrite of memloc * IR.binop * reg
| Constant of reg * int32
| Copy of reg * reg
| Read of reg * memloc
| Write of memloc * reg
| WriteConst of memloc * int32 * IR.typereg

let string_of_memloc (v,d) = Printf.sprintf "[%s+0x%lx]" (PpIR.ppVar v) d
let string_of_gadget g = 
  let pb = PpIR.ppBinop in let pv = PpIR.ppVar in let pm = string_of_memloc in
  match g with
  | BinopReg(vres,vlhs,b,vrhs) -> Printf.sprintf "BinopReg(%s,%s,%s,%s)" (pv vres) (pv vlhs) (pb b) (pv vrhs)
  | Constant(v,i)              -> Printf.sprintf "Constant(%s,0x%lx)" (pv v) i
  | Copy(vdst,vsrc)            -> Printf.sprintf "Copy(%s,%s)" (pv vdst) (pv vsrc) 
  | Read(vdst,m)               -> Printf.sprintf "Read(%s,%s)" (pv vdst) (pm m)
  | BinopRead(vres,b,m)        -> Printf.sprintf "BinopRead(%s,%s,%s)" (pv vres) (pb b) (pm m)
  | Write(m,vdst)              -> Printf.sprintf "Write(%s,%s)" (pm m) (pv vdst)
  | WriteConst(m,c,s)          -> Printf.sprintf "WriteConst(%s,0x%lx,%d)" (pm m) c (IRUtil.bits s)
  | BinopWrite(m,b,v)          -> Printf.sprintf "BinopWrite(%s,%s,%s)" (pm m) (pb b) (pv v)

