  let i = { pref = []; instr = (Rcl,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let    me = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let count = 
    ITE(Binop(me,Ult,Imm(X86.Ib( 9l))),me,
    ITE(Binop(me,Ult,Imm(X86.Ib(18l))),Binop(me,Sub,Imm(X86.Ib( 9l))),
    ITE(Binop(me,Ult,Imm(X86.Ib(27l))),Binop(me,Sub,Imm(X86.Ib(18l))),
    Binop(me,Sub,Imm(X86.Ib(27l))))))
  in
  (* Result *)
  let e = 
  ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),
  Binop(
    Binop(
      Binop(Extend(Unsigned,S8,X86Flag(X86.X86F_C)),Or,Binop(X86Reg(X86.Gb(X86.Al)),Shl,Imm(X86.Ib(1l)))),
      Shl,
      Binop(count,Sub,Imm(X86.Ib(1l)))),
    Or,
    Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(Imm(X86.Ib(9l)),Sub,count))))
  in
  let st = RegEquals(X86.Gb(X86.Al),e) in
  (* CF *)
  let e = 
  ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),
    Extend(Low,S1,
    Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(Imm(X86.Ib(8l)),Sub,count))))
    
  in
  let st = FlagEquals(X86.X86F_C,e) in

  (* OF *)
  let e = 
    let shr i = Binop(X86Reg(X86.Gb(X86.Al)),Shr,Imm(X86.Ib(i))) in
    Extend(Low,S1,Binop(shr 7l,Xor,shr 6l))
  in
  let st = FlagEquals(X86.X86F_O,e) in

    (* For each f/is/os tuple *)
    match ((Int32.to_int (is.ecx)) land 31) mod 9 with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed ()
    | 1 -> if (not (f st)) then failed ()
    | _ -> ())

