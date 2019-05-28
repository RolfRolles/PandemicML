(* 
TO DO:

When computing binops, don't compute the commutative ones twice.
So:  divide the binops into commutative versus non-commutative, and compute 
only the non-commutative binops twice.
*)
open X86Predicate

(* Not syntactically commutative *)
let merge_quads q1 q2 =
  { val32 = q1.val32@q2.val32;
    val16 = q1.val16@q2.val16;
    val8  = q1.val8 @q2.val8; 
    val1  = q1.val1 @q2.val1; }

let q_eax = 
  let open X86 in
  { val32 = [X86Reg(Gd(Eax));]; 
    val16 = [X86Reg(Gw(Ax));]; 
    val8 =  [X86Reg(Gb(Al));]; 
    val1 =  []; }

let q_ecx = 
  let open X86 in
  { val32 = [X86Reg(Gd(Ecx));]; 
    val16 = [X86Reg(Gw(Cx));]; 
    val8 =  [X86Reg(Gb(Cl));]; 
    val1 =  []; }

let q_eax_ecx = merge_quads q_eax q_ecx

let q_carry =
  let open X86 in
  { val32 = [Extend(Unsigned,S32,X86Flag(X86.X86F_C));];
    val16 = [Extend(Unsigned,S16,X86Flag(X86.X86F_C));];
    val8  = [Extend(Unsigned,S8 ,X86Flag(X86.X86F_C));];
    val1  = [X86Flag(X86.X86F_C);];} 

let q_eax_ecx_carry = merge_quads q_eax_ecx q_carry

let q_empty_imm_atoms = 
  let open X86 in
  { val32 = []; val16 = []; val8 = []; val1 = []; }

let q_zero_atoms = 
  let open X86 in
  { val32 = [Imm(Id(0x00000000l));]; 
    val16 = [Imm(Iw(0x0000l));]; 
    val8 = [Imm(Ib(0x00l));]; 
    val1 = [BitImm(true);BitImm(false)]; }

let q_one_atoms = 
  let open X86 in
  { val32 = [Imm(Id(0x00000001l));]; 
    val16 = [Imm(Iw(0x0001l));]; 
    val8 = [Imm(Ib(0x01l));]; 
    val1 = []; }

let q_zero_one_atoms = merge_quads q_zero_atoms q_one_atoms

let common_flag_templates =
  { val32 = [];
    val16 = [];
    val8  = [];
    val1  = [(S1,Binop(X86Reg(X86.Gb(X86.Al)),Eq,Imm(X86.Ib(0l))));
             (S1,Bitop(Parity,X86Reg(X86.Gb(X86.Al))));];
  }

let arithmetic_templates_alone =
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Ecx))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax )),Add,X86Reg(X86.Gw(X86.Cx ))));];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))));];
    val1  = [
      (S1,Binop(X86Reg(X86.Gb(X86.Al)),Eq,X86Reg(X86.Gb(X86.Al))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
      (S1,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Eq,Imm(X86.Ib(0l))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))))));
      (S1,Bitop(Parity,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))))));
      (S1,Binop(Unop(Not,Bitop(SignBit,X86Reg(X86.Gb(X86.Al)))),Xor,Bitop(SignBit,X86Reg(X86.Gb(X86.Al)))));
   ];
  }

let arithmetic_templates = merge_quads common_flag_templates arithmetic_templates_alone 

let unary_arithmetic_templates_alone =
  { val32 = [(S32,Binop(X86Reg(X86.Gd(X86.Eax)),Add,X86Reg(X86.Gd(X86.Ecx))));];
    val16 = [(S16,Binop(X86Reg(X86.Gw(X86.Ax )),Add,X86Reg(X86.Gw(X86.Cx ))));];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,Imm(X86.Ib(0l))));];
    val1  = [
      (S1,Binop(X86Reg(X86.Gb(X86.Al)),Eq,X86Reg(X86.Gb(X86.Al))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));
      (S1,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al))),Eq,Imm(X86.Ib(0l))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))))));
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))))));
      (S1,Bitop(Parity,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))))));
      (S1,Binop(Unop(Not,Bitop(SignBit,X86Reg(X86.Gb(X86.Al)))),Xor,Bitop(SignBit,X86Reg(X86.Gb(X86.Al)))));
   ];
  }

let unary_arithmetic_templates = merge_quads common_flag_templates unary_arithmetic_templates_alone 

let logical_templates_alone = 
  { val32 = [];
    val16 = [];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))));];
    val1  = [(S1,Binop(X86Reg(X86.Gb(X86.Al)),Eq,X86Reg(X86.Gb(X86.Al))));];
  }

let logical_templates = merge_quads common_flag_templates logical_templates_alone 

let unary_templates_alone = 
  { val32 = [];
    val16 = [];
    val8  = [(S8,Unop(Inc,X86Reg(X86.Gb(X86.Al))));];
    val1  = [(S1,Binop(X86Reg(X86.Gb(X86.Al)),Eq,X86Reg(X86.Gb(X86.Al))));];
  }

let unary_templates = merge_quads common_flag_templates unary_templates_alone 

let logical_test_templates_alone = 
  { val32 = [];
    val16 = [];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))));];
    val1  = [
      (S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al)))));
      (S1,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al))),Eq,Imm(X86.Ib(0l))));];
  }

let logical_test_templates = merge_quads common_flag_templates logical_test_templates_alone 

let carry_arithmetic_templates_alone =
  { val32 = [];
    val16 = [];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Add,Binop(X86Reg(X86.Gb(X86.Al)),Add,X86Reg(X86.Gb(X86.Al)))));];
    val1  = [(S1,Bitop(Parity,Binop(X86Reg(X86.Gb(X86.Al)),Xor,X86Reg(X86.Gb(X86.Al)))));]; }

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
           (*X86Flag(X86.X86F_D);*)
            ]; }

let q_eax_cl_alone = 
  { val32 = []; 
    val16 = []; 
    val8 =  [X86Reg(X86.Gb(X86.Al));
             Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l)));
           (*Binop(Imm(X86.Ib(8l)),Sub,Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(7l)))); *)
            ];
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
  { val32 = [];
    val16 = [];
    val8  = [(S8,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl))));];
    val1  = [
      (* Does parity *)
      (S1,shift_flag (Bitop(Parity,X86Reg(X86.Gb(X86.Al)))));
      (* Does constant flags *)
      (S1,shift_flag (BitImm(true)));
      (* Does zero / sign *)
      (S1,shift_flag (Binop(X86Reg(X86.Gb(X86.Al)),Eq,Imm(X86.Ib(0l)))));
      (* Does overflow for shr/sar *)
      (S1,shift_flag (Extend(Low,S1,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl)))))); 
      (* Does carry for shr/sar *)
      (S1,shift_flag (Binop(Binop(X86Reg(X86.Gb(X86.Al)),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl)))),Eq,Imm(X86.Ib(0l)))));
      (* Supposedly does carry for shl? Doesn't work though! *)
      (S1,shift_flag (Binop(Extend(Low,S1,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl)))),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Eq,Imm(X86.Ib(0l))))));
(*
*)
    ]; 
  }

let shift_arithmetic_templates = shift_arithmetic_templates_alone

let q_rotate_eax_cl_alone = 
  { val32 = []; 
    val16 = []; 
    val8 =  [X86Reg(X86.Gb(X86.Al));
             Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(7l)));
             Binop(Imm(X86.Ib(8l)),Sub,Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(7l))));
            ];
    val1 =  []; 
  }

let q_rotate_eax_cl = merge_quads q_flags_alone q_rotate_eax_cl_alone

let q_rotate_imms = 
  let open X86 in
  { val32 = [Imm(Id(0x00000000l));]; 
    val16 = [Imm(Iw(0x0000l));]; 
    val8 = [Imm(Ib(0x00l));Imm(Ib(0x01l));]; (*Imm(Ib(0x07l));Imm(Ib(0x0Fl));Imm(Ib(0x1Fl));*)
    val1 = [BitImm(true);BitImm(false)]; }

let rotate_arithmetic_templates_alone = 
  { val32 = [];
    val16 = [];
    val8  = [(S8,Binop(Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl))),Xor,Binop(X86Reg(X86.Gb(X86.Al)),Shl,X86Reg(X86.Gb(X86.Cl)))));];
    val1  = [
      (S1,X86Flag(X86.X86F_C));
      (S1,shift_flag  (Extend(Low,S1,X86Reg(X86.Gb(X86.Al)))));
(*
      (* Does parity *)
      (S1,shift_flag (Bitop(Parity,X86Reg(X86.Gb(X86.Al)))));
      (* Does constant flags *)
      (S1,shift_flag (BitImm(true)));
      (* Does zero / sign *)
      (S1,shift_flag (Binop(X86Reg(X86.Gb(X86.Al)),Eq,Imm(X86.Ib(0l)))));
*)
    ]; 
  }

let rotate_arithmetic_templates = rotate_arithmetic_templates_alone

let rotate_amount x = 
  ITE(Binop(X86Reg(X86.Gb(X86.Cl)),Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Cl)),x)

let rotate_carry_arithmetic_templates_alone = 
  { val32 = [];
    val16 = [];
    val8  = [
    
(S8,
  rotate_amount
 (Binop(
    Binop(
      Binop(
        Extend(Unsigned,S8,X86Flag(X86.X86F_C)),
        Xor,
        Binop(
          X86Reg(X86.Gb(X86.Al)),
          Shl,
          Imm(X86.Ib(0l)))),
        Shl,
        Binop(
          X86Reg(X86.Gb(X86.Cl)),
          Add,
          Imm(X86.Ib(0l)))),
    Xor,
    Binop(
      X86Reg(X86.Gb(X86.Al)),
      Shl,
      X86Reg(X86.Gb(X86.Cl))))));
    
    ];

    val1  = [(S1,X86Flag(X86.X86F_C));]; 
  }

let rotate_carry_arithmetic_templates = rotate_carry_arithmetic_templates_alone

let cmov_templates_alone =
  { val32 = [(S32,ITE(X86Flag(X86.X86F_C),X86Reg(X86.Gd(X86.Eax)),X86Reg(X86.Gd(X86.Eax))));
             (S32,ITE(Unop(Not,X86Flag(X86.X86F_C)),X86Reg(X86.Gd(X86.Eax)),X86Reg(X86.Gd(X86.Eax))));
             (S32,ITE(Binop(X86Flag(X86.X86F_C),Xor,X86Flag(X86.X86F_C)),X86Reg(X86.Gd(X86.Eax)),X86Reg(X86.Gd(X86.Eax))));
             (S32,ITE(Binop(Binop(X86Flag(X86.X86F_C),Xor,X86Flag(X86.X86F_C)),Xor,X86Flag(X86.X86F_C)),X86Reg(X86.Gd(X86.Eax)),X86Reg(X86.Gd(X86.Eax))));
             (S32,ITE(Binop(Binop(X86Flag(X86.X86F_C),Xor,X86Flag(X86.X86F_C)),Xor,Binop(X86Flag(X86.X86F_C),Xor,X86Flag(X86.X86F_C))),X86Reg(X86.Gd(X86.Eax)),X86Reg(X86.Gd(X86.Eax))));
  ];
    val16 = [];
    val8  = [];
    val1  = []; }

let cmov_templates = cmov_templates_alone

let collect ht f_stmt list = 
  Hashtbl.fold (fun f l o -> match l with x::_ -> (f_stmt f x)::o | _ -> o)
    ht
    list

let c_r32 = (fun r32 x ->  RegEquals(X86.Gd(r32),x))
let c_r16 = (fun r32 x ->  RegEquals(X86.Gw(r32),x))
let c_r8  = (fun r32 x ->  RegEquals(X86.Gb(r32),x))
let c_r1  = (fun r32 x -> FlagEquals(r32,x))

let distinguish_1 instr f e1 e2 =
  let open X86SemanticsDivinator in
  let refcand = refine_candidates { val32 = []; val16 = []; val8 = []; val1 = [FlagEquals(f,e1);FlagEquals(f,e2)]; } in
  let _ = JITSampler.blit_instr f_blit instr in
  let _ = Printf.printf "%s:\n%!" (X86Disasm.string_of_x86instr instr) in
  let blah2 = Z3Integration.mk_blah2 [("MODEL", "true");("SOFT_TIMEOUT", "10000")] in
  let m = minimize_candidates_by_size blah2 refcand S1 in
  print_results m

let signdiff x y = Bitop(SignBit,Binop(x,Xor,y))

let l_c  = [X86.X86F_C]
let l_a  = [X86.X86F_A]
let l_ca = [X86.X86F_C;X86.X86F_A]

let test1 () = 
  let open X86 in
  let open X86Predicate in
  distinguish_1 
   { pref = []; instr = (X86.Cmp,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]); } 
   (X86F_O)
   (Binop(Unop(Not,Bitop(SignBit,X86Reg(Gd(Eax)))),And,Bitop(SignBit,X86Reg(Gd(Ecx)))))
   (Bitop(SignBit,Binop(Binop(X86Reg(Gd(Ecx)),Xor,X86Reg(Gd(Eax))),And,Binop(X86Reg(Gd(Eax)),Xor,Binop(X86Reg(Gd(Eax)),Sub,X86Reg(Gd(Ecx)))))))

(*
   { pref = []; instr = (Adc,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]); } 
   (X86F_C)
   (Bitop(SignBit,Binop(Binop(X86Reg(Gd(Eax)),And,X86Reg(Gd(Ecx))),And,Binop(X86Reg(Gd(Ecx)),Or,Extend(Unsigned,S32,X86Flag(X86F_C))))))
   (Bitop(SignBit,Binop(X86Reg(Gd(Eax)),And,X86Reg(Gd(Ecx)))))

IR Translator was wrong!
(Binop(Binop(Binop(X86Reg(Gd(Eax)),Add,X86Reg(Gd(Ecx))),Add,Extend(Unsigned,S32,X86Flag(X86F_C))),Ule,X86Reg(Gd(Eax))))


   (Bitop(SignBit,Binop(X86Reg(Gd(Ecx)),And,X86Reg(Gd(Eax)))))
   (Binop(X86Reg(Gd(Eax)),Slt,Imm(Id(0x0l))))


   (Binop(Unop(Not,Bitop(FifthBit,Extend(Unsigned,S32,X86Flag(X86F_C)))),And,Bitop(SignBit,X86Reg(Gd(Eax)))))
   (Bitop(SignBit,X86Reg(Gd(Eax))))

   (Binop(Binop(Extend(Unsigned,S32,X86Flag(X86F_C)),Xor,X86Reg(Gd(Eax))),Slt,Imm(Id(0x0l))))
   (Bitop(SignBit,Binop(X86Reg(Gd(Ecx)),And,X86Reg(Gd(Eax)))))
   (Binop(X86Reg(Gd(Eax)),Slt,Imm(Id(0x0l))))
   (Bitop(SignBit,Binop(X86Reg(Gd(Ecx)),And,X86Reg(Gd(Eax)))))
*)
(*
let _ = test1 ()
*)

let test2 () = 
  let open X86 in
  let i1 = { pref = []; instr = (Cmp,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]); } in
  let i2 = { pref = []; instr = (Sub,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]); } in
  let open X86Predicate in
  let e1 = Binop(Unop(Not,Bitop(SignBit,X86Reg(Gd(Eax)))),And,Bitop(SignBit,X86Reg(Gd(Ecx)))) in
  let e2 = Bitop(SignBit,Binop(Binop(X86Reg(Gd(Ecx)),Xor,X86Reg(Gd(Eax))),And,Binop(X86Reg(Gd(Eax)),Xor,Binop(X86Reg(Gd(Eax)),Sub,X86Reg(Gd(Ecx)))))) in
  let f  = X86F_O in
  let t  = Z3Integration.mk_blah2 [("MODEL", "true");("SOFT_TIMEOUT", "10000")] in
  let x  = Z3Integration.distinguish_flag t f e1 e2 in
  match x with 
  | [] -> Printf.printf "Expressions are equivalent\n"
  | l  -> 
    let open X86SemanticsDivinator in 
    let c = reify_counterexample l in
    let _ = Printf.printf "Counterexample state:\n" in
    JITSampler.print_x86ctx c;
    let _ = JITSampler.blit_instr f_blit i1 in
    let o1 = f_execute c in
    let _ = Printf.printf "Output for %s:\n" (X86Disasm.string_of_x86instr i1) in
    JITSampler.print_x86ctx o1;
    let _ = JITSampler.blit_instr f_blit i2 in
    let o2 = f_execute c in
    let _ = Printf.printf "Output for %s:\n" (X86Disasm.string_of_x86instr i2) in
    JITSampler.print_x86ctx o2

(*
let _ = test2 ()
*)

let test_shl () =
  let open X86 in
  let i = { pref = []; instr = (Shl,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let count = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let erinner = Binop(X86Reg(X86.Gb(X86.Al)),Shl,count) in
  let er  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),erinner) in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ecinner = Bitop(SignBit,Binop(X86Reg(X86.Gb(X86.Al)),Shl,Binop(count,Sub,Imm(X86.Ib(1l))))) in
  let ec  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),ecinner) in
  let stc = FlagEquals(X86.X86F_C,ec) in
  let eo = Extend(Low,S1,Binop(ecinner,Xor,Bitop(SignBit,erinner))) in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl [] in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match (Int32.to_int (is.ecx)) land 31 with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed eo
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_shl ()
*)

let test_shr () =
  let open X86 in
  let i = { pref = []; instr = (Shr,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let count = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let erinner = Binop(X86Reg(X86.Gb(X86.Al)),Shr,count) in
  let er  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),erinner) in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ecinner = Extend(Low,S1,Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(count,Sub,Imm(X86.Ib(1l))))) in
  let ec  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),ecinner) in
  let stc = FlagEquals(X86.X86F_C,ec) in
  let eo = Bitop(SignBit,X86Reg(X86.Gb(X86.Al))) in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl [] in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match (Int32.to_int (is.ecx)) land 31 with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed eo
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_shr ()
*)

let test_sar () =
  let open X86 in
  let i = { pref = []; instr = (Sar,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let count = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let erinner = Binop(X86Reg(X86.Gb(X86.Al)),Sar,count) in
  let er  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),erinner) in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ecinner = Extend(Low,S1,Binop(X86Reg(X86.Gb(X86.Al)),Sar,Binop(count,Sub,Imm(X86.Ib(1l))))) in
  let ec  = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),ecinner) in
  let stc = FlagEquals(X86.X86F_C,ec) in
  let eo = BitImm(false) in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl [] in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    let icnt = (Int32.to_int (is.ecx)) land 31 in
    if (not (f str)) then failed er;
    if ((not (f stc))) then failed ec;
    match icnt with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed eo
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_sar ()
*)

let test_rcl () =
  let open X86 in
  let i = { pref = []; instr = (Rcl,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let    me = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let count = 
    ITE(Binop(me,Ult,Imm(X86.Ib( 9l))),me,
    ITE(Binop(me,Ult,Imm(X86.Ib(18l))),Binop(me,Sub,Imm(X86.Ib( 9l))),
    ITE(Binop(me,Ult,Imm(X86.Ib(27l))),Binop(me,Sub,Imm(X86.Ib(18l))),
    Binop(me,Sub,Imm(X86.Ib(27l))))))
  in
  let er = 
  ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),
  Binop(
    Binop(
      Binop(Extend(Unsigned,S8,X86Flag(X86.X86F_C)),Or,Binop(X86Reg(X86.Gb(X86.Al)),Shl,Imm(X86.Ib(1l)))),
      Shl,
      Binop(count,Sub,Imm(X86.Ib(1l)))),
    Or,
    Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(Imm(X86.Ib(9l)),Sub,count))))
  in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ec = 
  ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),
    Extend(Low,S1,
    Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(Imm(X86.Ib(8l)),Sub,count))))
    
  in
  let stc = FlagEquals(X86.X86F_C,ec) in

  let eo = 
    let shr i = Binop(X86Reg(X86.Gb(X86.Al)),Shr,Imm(X86.Ib(i))) in
    Extend(Low,S1,Binop(shr 7l,Xor,shr 6l))
  in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl l_c in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match ((Int32.to_int (is.ecx)) land 31) mod 9 with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed eo
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_rcl ()
*)

let test_rol () =
  let open X86 in
  let i = { pref = []; instr = (Rol,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let count = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(7l))) in
  let er = 
    ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),
      Binop(
        Binop(X86Reg(X86.Gb(X86.Al)),Shl,count),
        Or,
        Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(Imm(X86.Ib(8l)),Sub,count))))
  in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ecinner = Extend(Low,S1,er) in
  let  ec = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),ecinner) in
  let stc = FlagEquals(X86.X86F_C,ec) in

  let eo = Binop(Bitop(SignBit,er),Xor,Extend(Low,S1,er)) in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl [] in
  let cf_mask32 = 1l in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    match (Int32.to_int (is.ecx)) land 31 with
    |  0 -> if (Int32.logand cf_mask32 is.eflags) <> (Int32.logand cf_mask32 os.eflags) then (failed ec);
            if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then (failed eo)
    |  1 -> if (not (f sto)) then failed eo
    |  8
    | 16
    | 24 -> if (Int32.logand cf_mask32 is.eax)    <> (Int32.logand cf_mask32 os.eflags) then (failed ec)
    | _  -> if (not (f stc)) then failed ec)
    l_f_stmt

(*
let _ = test_rol ()
*)

let test_ror () =
  let open X86 in
  let i = { pref = []; instr = (Ror,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let count = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(7l))) in
  let er = 
    ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),
      Binop(
        Binop(X86Reg(X86.Gb(X86.Al)),Shr,count),
        Or,
        Binop(X86Reg(X86.Gb(X86.Al)),Shl,Binop(Imm(X86.Ib(8l)),Sub,count))))
  in
  let str = RegEquals(X86.Gb(X86.Al),er) in
  let ecinner = Bitop(SignBit,er) in
  let  ec = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),ecinner) in
  let stc = FlagEquals(X86.X86F_C,ec) in

  let eo = Binop(Bitop(SignBit,er),Xor,Bitop(SecondBit,er)) in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl [] in
  let cf_mask32 = 1l in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    match (Int32.to_int (is.ecx)) land 31 with
    |  0 -> if (Int32.logand cf_mask32 is.eflags) <> (Int32.logand cf_mask32 os.eflags) then (failed ec);
            if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then (failed eo)
    |  1 -> if (not (f sto)) then failed eo
    |  8 -> if (Int32.logand 0x80l is.eax)    <> (Int32.logand cf_mask32 os.eflags) then (failed ec)
    | 16 -> if (Int32.logand 0x80l is.eax)    <> (Int32.logand cf_mask32 os.eflags) then (failed ec)
    | 24 -> if (Int32.logand 0x80l is.eax)    <> (Int32.logand cf_mask32 os.eflags) then (failed ec)
    | _  -> if (not (f stc)) then failed ec)
    l_f_stmt

(*
let _ = test_ror ()
*)

let test_rcr () =
  let open X86 in
  let i = { pref = []; instr = (Rcr,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in

  let    me = Binop(X86Reg(X86.Gb(X86.Cl)),And,Imm(X86.Ib(31l))) in
  let count = 
    ITE(Binop(me,Ult,Imm(X86.Ib( 9l))),me,
    ITE(Binop(me,Ult,Imm(X86.Ib(18l))),Binop(me,Sub,Imm(X86.Ib( 9l))),
    ITE(Binop(me,Ult,Imm(X86.Ib(27l))),Binop(me,Sub,Imm(X86.Ib(18l))),
    Binop(me,Sub,Imm(X86.Ib(27l))))))
  in
  let er = 
  ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Reg(X86.Gb(X86.Al)),
  Binop(
    Binop(
      Binop(
        Binop(Extend(Unsigned,S8,X86Flag(X86.X86F_C)),Shl,Imm(X86.Ib(7l))),
        Or,
        Binop(X86Reg(X86.Gb(X86.Al)),Shr,Imm(X86.Ib(1l)))),
      Shr,
      Binop(count,Sub,Imm(X86.Ib(1l)))),
    Or,
    Binop(X86Reg(X86.Gb(X86.Al)),Shl,Binop(Imm(X86.Ib(9l)),Sub,count))))
  in
  let str = RegEquals(X86.Gb(X86.Al),er) in 
  let ecinner = Binop(X86Reg(X86.Gb(X86.Al)),Shr,Binop(count,Sub,Imm(X86.Ib(1l)))) in
  let ec = ITE(Binop(count,Eq,Imm(X86.Ib(0l))),X86Flag(X86.X86F_C),Extend(Low,S1,ecinner)) in
  let stc = FlagEquals(X86.X86F_C,ec) in

  let eo = 
    Bitop(SignBit,Binop(er,Xor,X86Reg(X86.Gb(X86.Al))))
    
  (*Binop(
      Extend(Low,S1,Binop(X86Reg(X86.Gb(X86.Al)),Shr,Imm(X86.Ib(7l)))),
      Xor,
      X86Flag(X86.X86F_C))*)
  in
  let sto = FlagEquals(X86.X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl l_c in
  let of_mask32 = Int32.shift_left 1l 11 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let open JITRegion in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
         (int32_of_bool (Int32.logand (Int32.shift_left 1l 11) os.eflags <> 0l))
    in
    
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match ((Int32.to_int (is.ecx)) land 31) mod 9 with
    | 0 -> if (Int32.logand of_mask32 is.eflags) <> (Int32.logand of_mask32 os.eflags) then failed eo
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_rcr ()
*)

let test_adc () =
  let open X86 in
  let i = { pref = []; instr = (Adc,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in
  let esum = Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Add,X86Reg(Gb(Cl))) in
  let er = Binop(X86Reg(Gb(Al)),Add,esum) in
  let ec = 
    Binop(
      Binop(er,Ule,X86Reg(Gb(Al))),
      And,
    (*Binop(Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Add,X86Reg(Gb(Cl))),Ult,X86Reg(Gb(Cl))))*)
      Binop(Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Ne,Imm(Ib(0l))),Or,Binop(X86Reg(Gb(Cl)),Ne,Imm(Ib(0l)))))
    (*Binop(X86Reg(Gb(Cl)),Ult,esum))*)

  in
  let ec = 
    Binop(
      Binop(er,Ult,X86Reg(Gb(Al))),
      Or,
    (*Binop(Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Add,X86Reg(Gb(Cl))),Ult,X86Reg(Gb(Cl))))*)
      Binop(esum,Ult,X86Reg(Gb(Cl))))
    (*Binop(Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Ne,Imm(Ib(0l))),Or,Binop(X86Reg(Gb(Cl)),Ne,Imm(Ib(0l)))))*)
    (*Binop(X86Reg(Gb(Cl)),Ult,esum))*)

  in
  let stc = FlagEquals(X86.X86F_C,ec) in
  let eo = Binop(signdiff (X86Reg(Gb(Al))) er,And,signdiff (X86Reg(Gb(Cl))) er) in
  let sto = FlagEquals(X86.X86F_O,eo) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl l_c in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
    in
    if (not (f stc)) then failed ec;
    if (not (f sto)) then failed eo)
    l_f_stmt

(*
let _ = test_adc ()
*)

let test_sbb () =
  let open X86 in
  let i = { pref = []; instr = (Sbb,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in
  let esum = Binop(Extend(Unsigned,S8,X86Flag(X86F_C)),Add,X86Reg(Gb(Cl))) in
  let er = Binop(X86Reg(Gb(Al)),Sub,esum) in
  let ec = Binop(Binop(X86Reg(Gb(Al)),Ult,esum),Or,Binop(esum,Ult,X86Reg(Gb(Cl)))) in
  let stc = FlagEquals(X86.X86F_C,ec) in
  let eo = Binop(signdiff (X86Reg(Gb(Al))) er,And,signdiff (X86Reg(Gb(Al))) (X86Reg(Gb(Cl)))) in
  let sto = FlagEquals(X86.X86F_O,eo) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al_cl l_c in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand 1l os.eflags <> 0l))
    in
    if (not (f stc)) then failed ec;
    if (not (f sto)) then failed eo)
    l_f_stmt

(*
let _ = test_sbb ()
*)

let test_neg () =
  let open X86 in
  let i = { pref = []; instr = (Neg,[GeneralReg(Gb(Al))]); } in
  let open X86Predicate in
  let eo = Binop(Binop(X86Reg(Gb(Al)),Ne,Imm(Ib(0l))),And,Binop(X86Reg(Gb(Al)),Eq,Unop(Neg,X86Reg(Gb(Al))))) in
  let sto = FlagEquals(X86.X86F_O,eo) in
  let ea = Binop(Binop(X86Reg(Gb(Al)),And,Imm(Ib(15l))),Ne,Imm(Ib(0l))) in
  let sta = FlagEquals(X86.X86F_A,ea) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al [] in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f sto)) then failed eo;
    if (not (f sta)) then failed ea)
    l_f_stmt

(*
let _ = test_neg ()
*)

let test_aaa () =
  let open X86 in
  let i = { pref = []; instr = (Aaa,[]); } in
  let open X86Predicate in
  let eca = Binop(X86Flag(X86F_A),Or,Binop(Imm(Ib(9l)),Ult,Binop(Imm(Ib(0xFl)),And,X86Reg(Gb(Al))))) in
  let stc = FlagEquals(X86.X86F_C,eca) in
  let sta = FlagEquals(X86.X86F_A,eca) in
  let er  = Binop(Imm(Iw(0xFF0Fl)),And,ITE(eca,Binop(X86Reg(Gw(Ax)),Add,Imm(Iw(0x106l))),X86Reg(Gw(Ax)))) in
  let str = RegEquals(Gw(Ax),er) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax l_ca in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let cf_mask32 = 1l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand cf_mask32 os.eflags <> 0l))
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed er;
    if (not (f stc)) then failed eca;
    if (not (f sta)) then failed eca)
    l_f_stmt

(*
let _ = test_aaa ()
*)

let test_aas () =
  let open X86 in
  let i = { pref = []; instr = (Aas,[]); } in
  let open X86Predicate in
  let eca = Binop(X86Flag(X86F_A),Or,Binop(Imm(Ib(9l)),Ult,Binop(Imm(Ib(0xFl)),And,X86Reg(Gb(Al))))) in
  let stc = FlagEquals(X86.X86F_C,eca) in
  let sta = FlagEquals(X86.X86F_A,eca) in
  let er  = Binop(Imm(Iw(0xFF0Fl)),And,ITE(eca,Binop(X86Reg(Gw(Ax)),Sub,Imm(Iw(0x106l))),X86Reg(Gw(Ax)))) in
  let str = RegEquals(Gw(Ax),er) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax l_ca in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let cf_mask32 = 1l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand cf_mask32 os.eflags <> 0l))
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed er;
    if (not (f stc)) then failed eca;
    if (not (f sta)) then failed eca)
    l_f_stmt

let _ = test_aas ()
(*
*)

let test_aad () =
  let open X86 in
  let i = { pref = []; instr = (Aad,[Immediate(Ib(0xAl))]); } in
  let open X86Predicate in
  let al = X86Reg(Gb(Al)) in
  let ah = X86Reg(Gb(Ah)) in
  let er  = Extend(Unsigned,S16,Binop(al,Add,Binop(Binop(ah,Shl,Imm(Ib(1l))),Add,Binop(ah,Shl,Imm(Ib(3l)))))) in
  let str = RegEquals(Gw(Ax),er) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax [] in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let cf_mask32 = 1l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand cf_mask32 os.eflags <> 0l))
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed er(*;
    if (not (f stc)) then failed eca;
    if (not (f sta)) then failed eca*)
    )
    l_f_stmt

(*
let _ = test_aad ()
*)

let test_daa () =
  let open X86 in
  let i = { pref = []; instr = (Daa,[]); } in
  let open X86Predicate in

  let al = X86Reg(Gb(Al)) in
  let cond1 = Binop(Imm(Ib(0x09l)),Ult,Binop(Imm(Ib(0xFl)),And,al)) in
  let cond2 = Binop(X86Flag(X86F_A),Or,cond1) in
  let cond3 = Binop(Binop(Imm(Ib(0x99l)),Ult,al),Or,X86Flag(X86F_C)) in
  let al1 = ITE(cond2,Binop(Imm(Ib(0x06l)),Add,al),al) in
  let af  = cond2 in
  let al2 = ITE(cond3,Binop(Imm(Ib(0x60l)),Add,al1),al1) in
  let cf  = cond3 in
  let str = RegEquals(Gb(Al),al2) in
  let stc = FlagEquals(X86F_C,cf) in
  let sta = FlagEquals(X86F_A,af) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al l_ca in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let cf_mask32 = 1l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand cf_mask32 os.eflags <> 0l))
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed al2;
    if (not (f stc)) then failed cf;
    if (not (f sta)) then failed af
    )
    l_f_stmt
    
(*
let _ = test_daa ()
*)

(*
  vCond1 = vAl <u 0x6;
  vCond2 = vAF || (9 <u (vAl & 0xF));
  vCond3 = (0x99 <u vAl) | vCF;

  Write(vAl, vCond2 ? vAl - 6 : vAl);
  vAF    =   vCond2;

  Write(vAl, vCond3 ? vAl - 0x60 : vAl);
  vCF    =   vCond3 ? 1 : vCond2 ? vCF | vCond1 : 0;
*)

let test_das () =
  let open X86 in
  let i = { pref = []; instr = (Das,[]); } in
  let open X86Predicate in

  let al = X86Reg(Gb(Al)) in
  let cond0 = Binop(Imm(Ib(0x09l)),Ult,Binop(Imm(Ib(0xFl)),And,al)) in
  let cond1 = Binop(al,Ult,Imm(Ib(0x06l))) in
  let cond2 = Binop(X86Flag(X86F_A),Or,cond0) in
  let cond3 = Binop(Binop(Imm(Ib(0x99l)),Ult,al),Or,X86Flag(X86F_C)) in
  let al1 = ITE(cond2,Binop(al,Sub,Imm(Ib(0x06l))),al) in
  let af  = cond2 in
  let al2 = ITE(cond3,Binop(al1,Sub,Imm(Ib(0x60l))),al1) in
  let cf  = ITE(cond3,BitImm(true),ITE(cond2,Binop(X86Flag(X86F_C),Or,cond1),BitImm(false))) in
  let str = RegEquals(Gb(Al),al2) in
  let stc = FlagEquals(X86F_C,cf) in
  let sta = FlagEquals(X86F_A,af) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al l_ca in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let cf_mask32 = 1l in
  let af_mask32 = Int32.shift_left 1l 4 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for %s:\n" (string_of_expr e) in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.cf = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand cf_mask32 os.eflags <> 0l))
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed al2;
    if (not (f stc)) then failed cf;
    if (not (f sta)) then failed af
    )
    l_f_stmt
    
(*
let _ = test_das ()
*)

let test_dec () =
  let open X86 in
  let i = { pref = []; instr = (Dec,[GeneralReg(Gb(Al))]); } in
  let open X86Predicate in
  let eo = Binop(X86Reg(Gb(Al)),Slt,Imm(Ib(0l))) in
  let eo = Binop(signdiff (X86Reg(Gb(Al))) (Binop(X86Reg(Gb(Al)),Sub,Imm(Ib(1l)))),And,eo) in
  let sto = FlagEquals(X86.X86F_O,eo) in
  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_al [] in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  let of_mask32 = Int32.shift_left 1l 11 in
  List.iter (fun (f,is,os) -> 
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      let open JITRegion in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.of = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand of_mask32 os.eflags <> 0l))
    in
    if (not (f sto)) then failed eo)
    l_f_stmt

(*
let _ = test_dec ()
*)

let test_shld () =
  let open X86 in
  let i = { pref = []; instr = (Shld,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in
  let count = Binop(X86Reg(Gb(Cl)),And,Imm(Ib(31l))) in
  let ite x y = ITE(Binop(count,Eq,Imm(Ib(0l))),x,y) in
  let er = ite (X86Reg(Gd(Eax))) (Binop(Binop(X86Reg(Gd(Eax)),Shl,count),Or,Binop(X86Reg(Gd(Ebx)),Shr,Binop(Imm(Ib(32l)),Sub,count)))) in
  let str = RegEquals(X86.Gd(X86.Eax),er) in 
  let ecinner = Bitop(SignBit,Binop(X86Reg(Gd(Eax)),Shl,Binop(count,Sub,Imm(Ib(1l))))) in
  let ec  = ite (X86Flag(X86F_C)) ecinner in
  let stc = FlagEquals(X86F_C,ec) in
  let eo  = ite (X86Flag(X86F_O)) (Binop(Bitop(SignBit,X86Reg(Gd(Eax))),Xor,Bitop(SecondBit,X86Reg(Gd(Eax))))) in
  let sto = FlagEquals(X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = mk_random_test_statement_evaluators 1000000 in  
  let af_mask32 = Int32.shift_left 1l 4 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match (Int32.to_int (is.ecx)) land 31 with
    | 0 -> ()
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_shld ()
*)

let test_shrd () =
  let open X86 in
  let i = { pref = []; instr = (Shrd,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx));GeneralReg(Gb(Cl))]); } in
  let open X86Predicate in
  let count = Binop(X86Reg(Gb(Cl)),And,Imm(Ib(31l))) in
  let ite x y = ITE(Binop(count,Eq,Imm(Ib(0l))),x,y) in
  let er = ite (X86Reg(Gd(Eax))) (Binop(Binop(X86Reg(Gd(Eax)),Shr,count),Or,Binop(X86Reg(Gd(Ebx)),Shl,Binop(Imm(Ib(32l)),Sub,count)))) in
  let str = RegEquals(X86.Gd(X86.Eax),er) in 
  let ecinner = Extend(Low,S1,Binop(X86Reg(Gd(Eax)),Shr,Binop(count,Sub,Imm(Ib(1l))))) in
  let ec  = ite (X86Flag(X86F_C)) ecinner in
  let stc = FlagEquals(X86F_C,ec) in
  let eo  = ite (X86Flag(X86F_O)) (Binop(Bitop(SignBit,X86Reg(Gd(Eax))),Xor,Extend(Low,S1,X86Reg(Gd(Ebx))))) in
  let sto = FlagEquals(X86F_O,eo) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = mk_random_test_statement_evaluators 1000000 in  
  let af_mask32 = Int32.shift_left 1l 4 in
  let int32_of_bool = function | true -> 1l | false -> 0l in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf 
        "Evaluated to 0x%lx, outstate.eax = 0x%lx, outstate.af = 0x%lx\n"
          v 
          os.eax 
         (int32_of_bool (Int32.logand af_mask32 os.eflags <> 0l))
    in
    if (not (f str)) then failed er;
    if (not (f stc)) then failed ec;
    match (Int32.to_int (is.ecx)) land 31 with
    | 0 -> ()
    | 1 -> if (not (f sto)) then failed eo
    | _ -> ())
    l_f_stmt

(*
let _ = test_shrd ()
*)

let test_bsr () =
  let open X86 in
  let i = { pref = []; instr = (Bsr,[GeneralReg(Gw(Cx));GeneralReg(Gw(Ax))]); } in
  let open X86Predicate in
  let ite idx e = 
    ITE(
      Binop(Binop(X86Reg(Gw(Ax)),And,Imm(Iw(Int32.of_int (1 lsl idx)))),Ne,Imm(Iw(0l))),
      Imm(Iw(Int32.of_int idx)),
      e)
  in
  let rec aux e i = if i = 16 then e else aux (ite i e) (i+1) in
  let expr = aux (X86Reg(Gw(Cx))) 0 in  
  let str = RegEquals(X86.Gw(X86.Cx),expr) in 

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  Printf.printf "%s\n" (string_of_expr expr);
  let l_f_stmt,_,_ = exhaustive_ax [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.eax = 0x%lx\n" v os.eax 
    in
    if (not (f str)) then failed expr)
    l_f_stmt

(*
let _ = test_bsr ()
*)

let test_bsf () =
  let open X86 in
  let i = { pref = []; instr = (Bsf,[GeneralReg(Gw(Cx));GeneralReg(Gw(Ax))]); } in
  let open X86Predicate in
  let ite idx e = 
    ITE(
      Binop(Binop(X86Reg(Gw(Ax)),And,Imm(Iw(Int32.of_int (1 lsl idx)))),Ne,Imm(Iw(0l))),
      Imm(Iw(Int32.of_int idx)),
      e)
  in
  let rec aux e i = if i = ~-1 then e else aux (ite i e) (i-1) in
  let expr = aux (X86Reg(Gw(Cx))) 15 in  
  let str = RegEquals(X86.Gw(X86.Cx),expr) in 

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  Printf.printf "%s\n" (string_of_expr expr);
  let l_f_stmt,_,_ = exhaustive_ax [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.ecx = 0x%lx\n" v os.ecx 
    in
    if (not (f str)) then failed expr)
    l_f_stmt

(*
let _ = test_bsf ()
*)

let test_bt () =
  let open X86 in
  let i = { pref = []; instr = (Bt,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]); } in
  let open X86Predicate in

  let e = Binop(Imm(Iw(1l)),Shl,Binop(X86Reg(Gb(Cl)),And,Imm(Ib(0xfl)))) in
  let stc = FlagEquals(X86F_C,Binop(Binop(X86Reg(Gw(Ax)),And,e),Ne,Imm(Iw(0l)))) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax_cx4 [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.ecx = 0x%lx\n" v os.ecx 
    in
    if (not (f stc)) then failed e)
    l_f_stmt

(*
let _ = test_bt ()
*)

let test_bts () =
  let open X86 in
  let i = { pref = []; instr = (Bts,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]); } in
  let open X86Predicate in

  let e = Binop(Imm(Iw(1l)),Shl,Binop(X86Reg(Gb(Cl)),And,Imm(Ib(0xfl)))) in
  let str = RegEquals(Gw(Ax),Binop(e,Or,X86Reg(Gw(Ax)))) in
  let stc = FlagEquals(X86F_C,Binop(Binop(X86Reg(Gw(Ax)),And,e),Ne,Imm(Iw(0l)))) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax_cx4 [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.ecx = 0x%lx\n" v os.ecx 
    in
    if (not (f str)) then failed e;
    if (not (f stc)) then failed e
    )
    l_f_stmt

(*
let _ = test_bts ()
*)

let test_btr () =
  let open X86 in
  let i = { pref = []; instr = (Btr,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]); } in
  let open X86Predicate in

  let e = Binop(Imm(Iw(1l)),Shl,Binop(X86Reg(Gb(Cl)),And,Imm(Ib(0xfl)))) in
  let str = RegEquals(Gw(Ax),Binop(Unop(Not,e),And,X86Reg(Gw(Ax)))) in
  let stc = FlagEquals(X86F_C,Binop(Binop(X86Reg(Gw(Ax)),And,e),Ne,Imm(Iw(0l)))) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax_cx4 [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.ecx = 0x%lx\n" v os.ecx 
    in
    if (not (f str)) then failed e;
    if (not (f stc)) then failed e
    )
    l_f_stmt

(*
let _ = test_btr ()
*)

let test_btc () =
  let open X86 in
  let i = { pref = []; instr = (Btr,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]); } in
  let open X86Predicate in

  let e = Binop(Imm(Iw(1l)),Shl,Binop(X86Reg(Gb(Cl)),And,Imm(Ib(0xfl)))) in
  let str = RegEquals(Gw(Ax),Binop(e,Xor,X86Reg(Gw(Ax)))) in
  let stc = FlagEquals(X86F_C,Binop(Binop(X86Reg(Gw(Ax)),And,e),Ne,Imm(Iw(0l)))) in

  let open X86SemanticsDivinator in 
  let _ = JITSampler.blit_instr f_blit i in
  let l_f_stmt,_,_ = exhaustive_ax_cx4 [] in
  List.iter (fun (f,is,os) -> 
    let open JITRegion in
    let failed e = 
      let _ = Printf.printf "Failed for:\n" in
      JITSampler.print_x86ctx is;
      let open X86PredicateEvaluator in 
      let v,_ = eval_expr is os (y_combinator (eval_expr is os)) e in
      Printf.printf "Evaluated to 0x%lx, outstate.ecx = 0x%lx\n" v os.ecx 
    in
    if (not (f str)) then failed e;
    if (not (f stc)) then failed e
    )
    l_f_stmt

(*
let _ = test_btc ()
*)

let main () =
  let l_instr = 
    let open X86 in 
    let e m l = { pref = []; instr = (m,l); } in
    let l_op   = [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))] in
    let l_op8  = [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))] in
    let arith  m = 50000,e m l_op,arithmetic_templates,q_eax_ecx,q_zero_atoms,S32 in
    let larith m =  5000,e m l_op,logical_templates,q_eax_ecx,q_zero_atoms,S32 in
    let lflag  m =  5000,e m l_op,logical_test_templates,q_eax_ecx,q_zero_atoms,S32 in
    let flag   m =  5000,e m [],flag_manipulating_templates,q_carry,q_zero_atoms,S32 in
    let carith m =  5000,e m l_op,carry_arithmetic_templates,q_eax_ecx_carry,q_zero_atoms,S32 in
    let shift  m = 50000,e m l_op8,shift_arithmetic_templates,q_eax_cl,q_shift_imms,S32 in
    let rot    m =   500,e m l_op8,rotate_arithmetic_templates,q_eax_cl,q_shift_imms,S32 in
    let cmov   m =  5000,e m l_op,cmov_templates,{ q_eax_ecx with val1 = q_eax_cl.val1; },q_zero_atoms,S32 in
   [
    arith  (Add) ; arith  (Sub) ; arith   (Cmp) ;
    larith (Xor) ; larith (And) ; larith  (Or)  ; 
    lflag  (Test); 
    flag   (Clc) ; flag   (Stc) ; flag   (Cld) ; flag   (Std) ; flag   (Cmc) ;
    carith (Adc) ; carith (Sbb) ;
    shift  (Shl) ; shift  (Shr) ; shift  (Sar) ;
    rot    (Ror) ; rot    (Rol) ;
    cmov (Cmova ); cmov (Cmovae); cmov (Cmovb ); cmov (Cmovbe);
    cmov (Cmovg ); cmov (Cmovge); cmov (Cmovl ); cmov (Cmovle);
    cmov (Cmovno); cmov (Cmovnp); cmov (Cmovns); cmov (Cmovnz);
    cmov (Cmovo ); cmov (Cmovp ); cmov (Cmovs ); cmov (Cmovz );
   ]
  in
  let semantics = 
    List.fold_left 
     (fun acc (n,x,t,r,i,s) -> 
        let q = X86SemanticsDivinator.divine_semantics_random n t r i x s in 
        let l = collect q.val1 c_r1 (collect q.val8 c_r8 (collect q.val16 c_r16 (collect q.val32 c_r32 []))) in
        (x,l)::acc)
      []
      l_instr
  in
  Printf.printf "Theorem prover time: %f\n%!" !Z3Integration.z3time;
  X86SemanticsDivinator.infinite_test semantics

let main () =
  let l_instr = 
    let open X86 in 
    let e m l = { pref = []; instr = (m,l); } in
    let l_uop  = [GeneralReg(Gb(Al))] in
    let l_op   = [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))] in
    let l_op8  = [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))] in
    let arith  m =  [],e m  l_op,arithmetic_templates,q_eax_ecx,q_zero_atoms,S8 in
    let larith m =  [],e m  l_op,logical_templates,q_eax_ecx,q_zero_atoms,S8 in
    let uarith m =  [],e m l_uop,unary_arithmetic_templates,q_eax,q_one_atoms,S8 in
    let unary  m =  [],e m l_uop,unary_templates,q_eax,q_zero_atoms,S8 in
    let lflag  m =  [],e m  l_op,logical_test_templates,q_eax_ecx,q_zero_atoms,S8 in
    let flag   m = l_c,e m    [],flag_manipulating_templates,q_carry,q_zero_atoms,S8 in
    let carith m = l_c,e m  l_op,carry_arithmetic_templates,q_eax_ecx_carry,q_zero_atoms,S8 in
    let shift  m =  [],e m  l_op,shift_arithmetic_templates,q_eax_cl,q_shift_imms,S8 in
    let rot    m =  [],e m  l_op,rotate_arithmetic_templates,q_rotate_eax_cl,q_rotate_imms,S8 in
    let rct    m =  [],e m  l_op,rotate_carry_arithmetic_templates,q_rotate_eax_cl,q_rotate_imms,S8 in
   [
  (*arith  (Add) ; arith  (Sub) ; arith   (Cmp) ;*)
    uarith (Inc) ; uarith (Dec) ; 
  (*unary  (Neg) ; unary  (Not) ; 
    larith (Xor) ; larith (And) ; larith  (Or)  ; 
    lflag  (Test); 
    flag   (Clc) ; flag   (Stc) ; flag   (Cld) ; flag   (Std) ; flag   (Cmc) ;
    carith (Adc) ; carith (Sbb) ;
    shift  (Shl) ; shift  (Shr) ; shift  (Sar) ;
    rot    (Ror) ; rot    (Rol) ;
    rct    (Rcr) ; rct    (Rcl) ;*)
   ]
  in
  let semantics = 
    List.fold_left 
     (fun acc (b,x,t,r,i,s) -> 
        let q = X86SemanticsDivinator.divine_semantics_exhaustive_8bit b t r i x s in 
        let l = collect q.val1 c_r1 (collect q.val8 c_r8 (collect q.val16 c_r16 (collect q.val32 c_r32 []))) in
        (x,l)::acc)
      []
      l_instr
  in
  semantics

(*
let _ = main ()
*)
