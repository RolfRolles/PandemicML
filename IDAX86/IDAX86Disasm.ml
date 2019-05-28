open X86
open X86Disasm
open IDAColor

(* Code duplicated from X86Disasm *)
let colstr_of_memexpr = function
| Mem16(seg,bo,so,dop) -> 
  let bs = match bo  with Some(b) -> [col_reg (string_of_x86_reg16 b)]    | None -> [] in
  let ss = match so  with Some(s) -> bs@[col_reg (string_of_x86_reg16 s)] | None -> bs in
  let ds = match dop with Some(d) -> ss@[col_memimm (string_of_displ d)]     | None -> ss in
  let s  = StringUtil.intersperse_string (col_symbol "+") ds in
  (col_reg (string_of_x86_segreg seg))^(col_symbol ":[")^Util.opt_get s^(col_symbol "]")
| Mem32(seg,bo,so,dop) -> 
  let bs = match bo with Some(b) -> [col_reg (string_of_x86_reg32 b)]    | None -> [] in
  let ss = match so with 
  | Some(sr,sf) -> 
    let l = [col_reg (string_of_x86_reg32 sr)] in
    let l = if sf <> 0 then l@[col_symbol (string_of_scalefac sf)] else l in  
    bs@[Util.opt_get (StringUtil.intersperse_string (col_symbol "*") l)] 
  | None -> bs in
  let ds = match dop with Some(d) -> ss@[col_memimm (string_of_displ d)]     | None -> ss in
  let s  = StringUtil.intersperse_string (col_symbol "+") ds in
  (col_reg (string_of_x86_segreg seg))^(col_symbol ":[")^Util.opt_get s^(col_symbol "]")

let colstr_of_memexpr sizestr m = (col_keyword sizestr)^" "^(colstr_of_memexpr m)

let colstr_of_x86memexpr = function
| Mb(m)  -> colstr_of_memexpr    "byte ptr" m
| Mw(m)  -> colstr_of_memexpr    "word ptr" m
| Md(m)  -> colstr_of_memexpr   "dword ptr" m
| Mf(m)  -> colstr_of_memexpr   "fword ptr" m
| Mq(m)  -> colstr_of_memexpr   "qword ptr" m
| Mt(m)  -> colstr_of_memexpr   "tbyte ptr" m
| Mdq(m) -> colstr_of_memexpr "xmmword ptr" m

let colstr_of_x86operand = function
| GeneralReg(r)   -> col_reg (string_of_x86_general_reg r)
| ControlReg(r)   -> col_reg (string_of_x86_control_reg r)
| DebugReg(r)     -> col_reg (string_of_x86_debug_reg r)
| SegReg(r)       -> col_reg (string_of_x86_segreg r)
| FPUReg(r)       -> col_reg (string_of_x86_fpureg r)
| MMXReg(r)       -> col_reg (string_of_x86_mmxreg r)
| XMMReg(r)       -> col_reg (string_of_x86_xmmreg r)
| JccTarget(jt,_) -> Printf.sprintf "loc_%08lX" jt
| Immediate(i)    -> col_imm (string_of_x86_immediate i)
| Memexpr(m)      -> colstr_of_x86memexpr m
| FarTarget(a)    -> string_of_x86_far_target a

let colstr_of_pref = function
| Lock  -> col_insn "lock"
| Repne -> col_insn "repne" 
| Rep   -> col_insn "rep"

let colstr_of_x86instr { pref = pref; instr = (m,l) } =
  let p = List.map colstr_of_pref pref in
  let p = match p with
  | [] -> ""
  | _  -> (Util.opt_get (StringUtil.intersperse_string (col_symbol ", ") p))^" "
  in
  let l = List.map colstr_of_x86operand l in
  match l with
  | [] -> col_insn (string_of_x86mnem m)
  | _  -> p^(col_insn (string_of_x86mnem m))^" "^(Util.opt_get (StringUtil.intersperse_string (col_symbol ", ") l))
