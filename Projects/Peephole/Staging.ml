(* I can do this without the JIT compiler, which may be easier. *)

let candidate_sequences = 
let allowable_instructions = 
  [(X86.Neg, [AbsGeneralReg(AGd(AbsReg32(0)))]);
   (X86.Neg, [AbsGeneralReg(AGw(AbsReg16(0)))]);
   (X86.Neg, [AbsGeneralReg(AGb(AbsReg8 (0)))]);
   (X86.Not, [AbsGeneralReg(AGd(AbsReg32(0)))]);
   (X86.Not, [AbsGeneralReg(AGw(AbsReg16(0)))]);
   (X86.Not, [AbsGeneralReg(AGb(AbsReg8 (0)))]);
   (X86.Xchg,[AbsGeneralReg(AGd(AbsReg32(0)));AbsGeneralReg(AGd(AbsReg32(1)))]);
   
   (X86.Add ,[AbsGeneralReg(AGd(AbsReg32(0)));AbsGeneralReg(AGd(AbsReg32(1)))]);
   (X86.Add ,[AbsGeneralReg(AGw(AbsReg16(0)));AbsGeneralReg(AGw(AbsReg16(1)))]);
   (X86.Add ,[AbsGeneralReg(AGb(AbsReg8 (0)));AbsGeneralReg(AGb(AbsReg8 (1)))]);
   (X86.Sub ,[AbsGeneralReg(AGd(AbsReg32(0)));AbsGeneralReg(AGd(AbsReg32(1)))]);
   (X86.Sub ,[AbsGeneralReg(AGw(AbsReg16(0)));AbsGeneralReg(AGw(AbsReg16(1)))]);
   (X86.Sub ,[AbsGeneralReg(AGb(AbsReg8 (0)));AbsGeneralReg(AGb(AbsReg8 (1)))]);
   
  ]