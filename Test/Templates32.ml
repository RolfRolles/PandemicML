(* 32-bit
let common_flag_templates =
  { val32 = [];
    val16 = [];
    val8  = [];
    val1  = [(S1,Binop(X86Reg(X86.Gd(X86.Eax)),Eq,Imm(X86.Id(0l))));
             (S1,Bitop(Parity,X86Reg(X86.Gd(X86.Eax))));];
 }
*)

let arithmetic_templates_alone =
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Ecx))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax )),Add,X86Reg(X86.Gw(X86.Cx ))));];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))));];
    val1  = [
      (S1,Binop(X86Reg(X86.Gd(X86.Eax)),Eq,X86Reg(X86.Gd(X86.Eax))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));
      (S1,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax))),Eq,Imm(X86.Id(0l))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))))));
      (S1,Bitop(Parity,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))))));
      (S1,Binop(Unop(Not,Bitop(SignBit,X86Reg(X86.Gd(X86.Eax)))),Xor,Bitop(SignBit,X86Reg(X86.Gd(X86.Eax)))));
   ];
  }

let arithmetic_templates = merge_quads common_flag_templates arithmetic_templates_alone 

let logical_templates_alone = 
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax)),Xor,X86Reg(X86.Gw(X86.Ax))));];
    val8  = [(S8 ,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))));];
    val1  = [(S1,Binop(X86Reg(X86.Gd(X86.Eax)),Eq,X86Reg(X86.Gd(X86.Eax))));];
  }

let logical_templates = merge_quads common_flag_templates logical_templates_alone 

let logical_test_templates_alone = 
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax)),Xor,X86Reg(X86.Gw(X86.Ax))));];
    val8  = [(S8 ,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))));];
    val1  = [
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax)))));
      (S1,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax))),Eq,Imm(X86.Id(0l))));];
  }

let logical_test_templates = merge_quads common_flag_templates logical_test_templates_alone 

let carry_arithmetic_templates_alone =
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Eax)))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax)),Add,Binop(X86Reg(X86.Gw(X86.Ax)),Add,X86Reg(X86.Gw(X86.Ax)))));];
    val8  = [(S32,Binop(X86Reg(X86.Gb(X86.Al)),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));];
    val1  = [(S1,Bitop(Parity,Binop(X86Reg(X86.Gd(X86.Eax)),Xor,X86Reg(X86.Gd(X86.Eax)))));]; }

let carry_arithmetic_templates = merge_quads carry_arithmetic_templates_alone arithmetic_templates 

let flag_manipulating_templates = 
  { val32 = [];
    val16 = [];
    val8  = [];
    val1  = [(S1,Unop(Not,X86Flag(X86.X86F_C)));]
  }

let q_flags_alone =
  let open X86 in
  { val32 = [];
    val16 = [];
    val8 =  [];
    val1 =  [X86Flag(X86.X86F_C);X86Flag(X86.X86F_P);X86Flag(X86.X86F_A);
             X86Flag(X86.X86F_S);X86Flag(X86.X86F_Z);X86Flag(X86.X86F_O);
             X86Flag(X86.X86F_D);]; }

(* This thing is 32-bit specific. *)
let q_eax_cl_alone = 
  { val32 = [X86Reg(X86.Gd(X86.Eax));]; 
    val16 = []; 
    val8 =  [Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(0x1fl)));
             Binop(Imm(X86.Ib(32l)),Sub,Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(0x1fl))));]; 
    val1 =  []; 
  }

let q_eax_cl = merge_quads q_flags_alone q_eax_cl_alone

let q_shift_imms = 
  let open X86 in
  { val32 = [Imm(Id(0x00000000l));]; 
    val16 = [Imm(Iw(0x0000l));]; 
    val8 = [Imm(Ib(0x00l));]; (*Imm(Ib(0x07l));Imm(Ib(0x0Fl));Imm(Ib(0x1Fl));*)
    val1 = [BitImm(true);BitImm(false)]; }

(* Adding the Xor in val32 breaks the output 
Binop(X86Reg(X86.Gb(X86.Cl)),Xor,Imm(X86.Ib(0l)))
This shit is all broken
*)
let shift_flag x = 
  ITE(Binop(X86Reg(X86.Gb(X86.Cl)),Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),x)

let shift_arithmetic_templates_alone = 
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax)),Shl,X86Reg(X86.Gb(X86.Cl))));];
    val8  = [(S8 ,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl))));];
    val1  = [
      (* Does parity *)
      (S1,shift_flag (Bitop(Parity,X86Reg(X86.Gd(X86.Eax)))));
      (* Does constant flags *)
      (S1,shift_flag (BitImm(true)));
      (* Does zero / sign *)
      (S1,shift_flag (Binop(X86Reg(X86.Gd(X86.Eax)),Eq,Imm(X86.Id(0l)))));
      (* Does overflow for shr/sar *)
      (S1,shift_flag (Extend(Low,S1,Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl)))))); 
      (* Does carry for shr/sar *)
      (S1,shift_flag (Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl)))),Eq,Imm(X86.Id(0l)))));
      (* Supposedly does carry for shl? Doesn't work though! *)
      (S1,shift_flag (Binop(Extend(Low,S1,Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl)))),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Eq,Imm(X86.Id(0l))))));
    ]; 
  }

let shift_arithmetic_templates = shift_arithmetic_templates_alone

let rotate_arithmetic_templates_alone = 
  { val32 = [(S32,Binop(Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl))),Xor,Binop(X86Reg(X86.Gd(X86.Eax)),Shl,X86Reg(X86.Gb(X86.Cl)))));];
    val16 = [(S16,Binop(Binop(X86Reg(X86.Gw(X86.Ax)),Shl,X86Reg(X86.Gb(X86.Cl))),Xor,Binop(X86Reg(X86.Gw(X86.Ax)),Shl,X86Reg(X86.Gb(X86.Cl)))));];
    val8  = [(S8 ,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl))),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl)))));];
    val1  = [
      (* Does parity *)
      (S1,shift_flag (Bitop(Parity,X86Reg(X86.Gd(X86.Eax)))));
      (* Does constant flags *)
      (S1,shift_flag (BitImm(true)));
      (* Does zero / sign *)
      (S1,shift_flag (Binop(X86Reg(X86.Gd(X86.Eax)),Eq,Imm(X86.Id(0l)))));
    ]; 
  }

let rotate_arithmetic_templates = rotate_arithmetic_templates_alone

