(*
type gadget =
| CopyReg of reg * reg * size
| Binop of reg * reg * binop * reg * size
| ReadConst of reg * reg * disp * size
| ReadReg of reg * reg * disp * size
| ReadBinop of reg * binop * reg * disp * size
| WriteReg of reg * disp * reg * size
| WriteBinop of reg * disp * binop * reg * size

let verify_gadget_postcondition tbl = function
| CopyReg(rdst,rsrc,size)               -> verify_reg_copy     tbl vsrc    vdst
| Binop(rdst,rlhs,binop,rrhs,size)      -> verify_reg_binop    tbl vdst    vsrclhs b      vsrcrhs
| ReadConst(rdst,rmem,disp,size)        -> verify_read_const32 tbl vdst    vreg    disp32
| ReadReg(rdst,rmem,disp,size)          -> verify_read_reg     tbl vdst    vmemreg disp32
| ReadBinop(rdst,binop,rmem,disp,size)  -> verify_read_binop   tbl vdst    vsrclhs b      vmemreg disp32
| WriteReg(rmem,disp,rrhs,size)         -> verify_write_reg    tbl vmemreg disp32  vsrc
| WriteBinop(rmem,disp,binop,rrhs,size) -> verify_write_binop  tbl vmemreg disp32  b      vsrcrhs
*)

(*
Generate a random context
This includes the registers as well as some data for the stack
Interpret the sequence in that context

Inspect the following elements:

* Registers:
* * Which registers are preserved?  
    Form of information:  a list of supposedly preserved registers, [reg].
    Implementation:  compare each register at the beginning to the same register at the end.
    Verification query: (var_begin != var_end).

* * What is the difference in the stack values?
    Form of information:  an integer.
    Implementation:  compare the initial and final ESP values.
    Verification query: (esp_begin != esp_end + difference).

* * Do any registers exhibit copy behavior?
    Form of information:  a list of supposedly copied register pairs, [(reg_begin,reg_end)].
    Implementation:  compare each register at the beginning to every other register at the end.
    Verification query: (var1_begin != var2_end).

* * Do any registers exhibit binop behavior?
    Form of information:  a list of supposed binop quadruples, [(reg3, reg1, OP, reg2)].
    Implementation:  enumerate all binop behaviors, compare the final registers to those values.
    Verification query: (var3_end != var_1 OP var_2).

* * Do any registers exhibit unop behavior?
    Form of information:  a list of supposed unop triples, [(reg2, OP, reg1)].
    Implementation:  enumerate all unop behaviors, compare the final registers to those values.
    Verification query: (var2_end != OP var_1).

* Memory:
* * As for memory accesses, consider every memory accessed address to be of the form [reg+disp]
    Implementation:  for every accessed location, make a list [(register,location-regvalue)]

* * Memory reads:
* * * Do any registers exhibit "load constant" behavior?
      Form of information:  a list of stack offsets and registers, [(reg, offset)].
      Implementation:  check final register values against stack values.
      Verification query:  (reg_end != dword(esp+offset)).

* * * Do any registers exhibit "load binop" behavior?
      Form of information:  a list of supposed binop quintuples, [(reg3, reg1, OP, reg2mem, offset)].
      Implementation:  enumerate all binop behaviors, compare the final registers to those values.
      Verification query:  (reg3_end != dword(reg2mem+offset) OP reg1).

* * Memory writes:
* * * Do any memory locations exhibit "write register value" behavior?
      Form of information:  a list of memory offsets and registers, [(regmem, offset, regvalue)].
      Implementation:  check final memory contents against initial register values.
      Verification query:  (regmem[offset] != regvalue).

* * * Do any memory locations exhibit "write binop" behavior?
      Form of information:  a list of supposed binop quadruples, [(reg1, OP, reg2mem, offset)].
      Implementation:  enumerate all binop behaviors, compare the final memory values to those values.
      Verification query:  (dword(reg2mem+offset) != dword(reg2mem+offset) OP reg1).



let hack_popcnt_postcondition = 
  IRUtil.mk_not 
   (IRUtil.mk_eq 
     (IRUtil.mk_evar (Hashtbl.find   hack_tbl X86ToIRUtil.vEax))
     (IRUtil.mk_evar (Hashtbl.find popcnt_tbl X86ToIRUtil.vEax)))


(* Ensure that vdst.post = mem.pre[vmemreg.pre + disp32] *)
let verify_read_reg tbl vdst vmemreg disp32 = 
  IRUtil.mk_ne (mk_load_disp32_pre vmemreg disp32 IR.TypeReg_32) (post tbl vdst)

(* Ensure that vsrc.pre = mem.post[vmemreg.pre + disp32] *)
let verify_write_reg tbl vmemreg disp32 vsrc = 
  IRUtil.mk_ne (mk_load_disp32_post tbl vmemreg disp32 IR.TypeReg_32) (mk_e vsrc)



*)