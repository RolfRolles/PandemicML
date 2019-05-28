let eAl    = SynthesisIR.Var(X86ToIRUtil.vAl)
let eCl    = SynthesisIR.Var(X86ToIRUtil.vCl)
let eAh    = SynthesisIR.Var(X86ToIRUtil.vAh)
let eCx    = SynthesisIR.Var(X86ToIRUtil.vCx)
let eAx    = SynthesisIR.Var(X86ToIRUtil.vAx)
let eEax   = SynthesisIR.Var(X86ToIRUtil.vEax)
let eEcx   = SynthesisIR.Var(X86ToIRUtil.vEcx)
let eZF    = SynthesisIR.Var(X86ToIRUtil.vZF)
let eCF    = SynthesisIR.Var(X86ToIRUtil.vCF)
let eAF    = SynthesisIR.Var(X86ToIRUtil.vAF)
let eOF    = SynthesisIR.Var(X86ToIRUtil.vOF)
let eAlAfter = SynthesisIR.Var(X86ToIRUtil.vAlAfter)
let eOFAfter = SynthesisIR.Var(X86ToIRUtil.vOFAfter)

let mk1 i  = SynthesisIR.Const(i,IR.TypeReg_1)
let mk8 i  = SynthesisIR.Const(i,IR.TypeReg_8)
let mk16 i = SynthesisIR.Const(i,IR.TypeReg_16)
let mk32 i = SynthesisIR.Const(i,IR.TypeReg_32)

let e0_8   = mk8 0L
let e1_8   = mk8 1L
let eFF_8  = mk8 255L
let e0_1   = mk1 0L
let e1_1   = mk1 1L

let signdiff l r = SynthesisIR.(BitBinop(l,SignDiff,r))
let ite f e1 e2 = SynthesisIR.(ITE(f,e1,e2))

let eClEq0   = SynthesisIR.(Binop(eCl,EQ,e0_8))
let eClEq1   = SynthesisIR.(Binop(eCl,EQ,e1_8))

let p      e = SynthesisIR.(BitUnop(Parity,e))
let s1     e = ite eClEq0 eZF e
let s8     e = ite eClEq0 eAl e
let s11    e = ite eClEq1 eZF e
let c      e = ite eClEq0 eCl e

let cu8    e = SynthesisIR.(Cast(Unsigned,TypeReg_8,e))
let cu16   e = SynthesisIR.(Cast(Unsigned,TypeReg_16,e))
let cu32   e = SynthesisIR.(Cast(Unsigned,TypeReg_32,e))
let cu64   e = SynthesisIR.(Cast(Unsigned,TypeReg_64,e))
let cs16   e = SynthesisIR.(Cast(Signed,TypeReg_16,e))
let cs32   e = SynthesisIR.(Cast(Signed,TypeReg_32,e))
let cs64   e = SynthesisIR.(Cast(Signed,TypeReg_64,e))
let cl1    e = SynthesisIR.(Cast(Low,TypeReg_1,e))
let cl8    e = SynthesisIR.(Cast(Low,TypeReg_8,e))
let cl16   e = SynthesisIR.(Cast(Low,TypeReg_16,e))
let ch8    e = SynthesisIR.(Cast(High,TypeReg_8,e))
let ch16   e = SynthesisIR.(Cast(High,TypeReg_16,e))
let ch32   e = SynthesisIR.(Cast(High,TypeReg_32,e))


let lAl                    = cl8 eAl
let pAl                    = p eAl
let eAlPlusAl              = SynthesisIR.(Binop(eAl,Add,eAl))
let pAlPlusAl              = p eAlPlusAl
let eAlPlusAlEq0           = SynthesisIR.(Binop(eAlPlusAl,EQ,e0_8))
let eAlPlusAlEqAl          = SynthesisIR.(Binop(eAlPlusAl,EQ,eAl))
let eAlPlusAlPlusAl        = SynthesisIR.(Binop(eAl,Add,eAlPlusAl))
let pAlPlusAlPlusAl        = p eAlPlusAlPlusAl
let eAlXorAl               = SynthesisIR.(Binop(eAl,Xor,eAl))
let pAlXorAl               = p eAlXorAl
let eAlXorAlEq0            = SynthesisIR.(Binop(eAlXorAl,EQ,e0_8))
let eAlXorAlXorAl          = SynthesisIR.(Binop(eAl,Xor,eAlXorAl))
let pAlXorAlXorAl          = p eAlXorAlXorAl
let eAlXorAlXorAlXorAl     = SynthesisIR.(Binop(eAlXorAl,Xor,eAlXorAl))
let pAlXorAlXorAlXorAl     = p eAlXorAlXorAlXorAl
let pAlXorAlXorAlPlusAl    = p (SynthesisIR.(Binop(eAlXorAl,Xor,eAlPlusAl)))
let eAlPlus0               = SynthesisIR.(Binop(eAl,Add,e0_8))
let eAlEq0                 = SynthesisIR.(Binop(eAl,EQ,e0_8))
let eAlEqAl                = SynthesisIR.(Binop(eAl,EQ,eAl))
let eAlEqAlXorAlEqAl       = SynthesisIR.(Binop(eAlEqAl,Xor,eAlEqAl))
let eAlEq0XorAlEqAl        = SynthesisIR.(Binop(eAlEq0,Xor,eAlEqAl))
let eAlEqAlXorAlPlusAlEqAl = SynthesisIR.(Binop(eAlEqAl,Xor,eAlPlusAlEqAl))
let eAlShlCl               = SynthesisIR.(Binop(eAl,Shl,eCl))
let eAlShl0                = SynthesisIR.(Binop(eAl,Shl,e0_8))
let eClPlus0               = SynthesisIR.(Binop(eCl,Add,e0_8))
let eNotAl                 = SynthesisIR.(Unop(Not,eAl))
let eAlShlClXorAlShlCl     = SynthesisIR.(Binop(eAlShlCl,Xor,eAlShlCl))
let sAlShlClXorAlShlCl     = s8 eAlShlClXorAlShlCl
let eNotZF                 = SynthesisIR.(Unop(Not,eZF))
let eZFXorZF               = SynthesisIR.(Binop(eZF,Xor,eZF))
let eZFXorZFXorZF          = SynthesisIR.(Binop(eZF,Xor,eZFXorZF))
let eZFXorZFXorZFXorZF     = SynthesisIR.(Binop(eZFXorZF,Xor,eZFXorZF))
let pNotXor                = SynthesisIR.(Binop(Unop(Not,pAl),Xor,pAl))
let sConst                 = s1 e0_1
let sAlShlCl               = s8 eAlShlCl
let spAl                   = s1 pAl
let spAlXorAl              = s1 pAlXorAl
let sAlEq0                 = s1 eAlEq0
let spAlShlCl              = s1 (p eAlShlCl)
let lAlShlCl               = cl1 eAlShlCl
let lZF                    = ch8 eZF
let sLowAl                 = s1 (cl1 eAl)
let sLowAlShlCl            = s1 (cl1 eAlShlCl)
let sAlXorAlShlClEq0       = s1 (SynthesisIR.(Binop(e0_8,EQ,Binop(eAl,Xor,eAlShlCl))))
let eAlAnd0xFEq0           = SynthesisIR.(Binop(e0_8,EQ,Binop(eAl,And,Const(15L,IR.TypeReg_8))))
let bAlSignDiffAl          = signdiff eAl eAl
let bAlSignDiffAlXorItself = SynthesisIR.(Binop(bAlSignDiffAl,Xor,bAlSignDiffAl))
let sAlSignDiffAl          = s1 bAlSignDiffAl

let eAxEqAlExt             = SynthesisIR.(Binop(eAx,EQ,Cast(Unsigned,TypeReg_16,cl8 eAx)))
let eCasteAl16             = SynthesisIR.(Cast(Unsigned,TypeReg_16,eAl))
let eCasteAl16MulSame      = SynthesisIR.(Binop(eCasteAl16,Mul,eCasteAl16))
let pAx                    = p eAx

let tfCommon      = [eAlEq0;pAl]
let tfMul         = [eAxEqAlExt;pAx]
let tfArith       = [eAlEqAl;bAlSignDiffAlXorItself;pAlXorAlXorAl]@tfCommon
let tfUnary       = [eAlEqAl]@tfCommon
let tfUnaryArith  = [eAlPlus0]@tfCommon
let tfLogical     = [eAlEqAl]@tfCommon
let tfLogicalTest = [pAlXorAl;eAlXorAlEq0]@tfCommon
let tfCarry       = [pAlXorAl;pAlPlusAlPlusAl]@tfCommon
let tfFlag        = [eNotZF]
let tfFlags       = [eZF;eZFXorZF;eZFXorZFXorZF]
let tfShift       = [spAlXorAl;lAlShlCl;spAlShlCl;spAl;sConst;sAlEq0;sLowAlShlCl;sAlXorAlShlClEq0;sAlSignDiffAl]
let tfRotate      = [eZF;sLowAl;spAl]
let tfRotateCarry = [eZF]

let teArith       = [eAlPlusAl;eAl]
let teMul         = [eCasteAl16MulSame]
let teUnary       = [eNotAl]
let teLogical     = [eAlXorAl]
let teCarry       = [eAlPlusAlPlusAl]
let teShift       = [eAlShlCl]
let teRotate      = [sAlShlClXorAlShlCl]
let teRotateCarry = [c SynthesisIR.(Binop(Binop(Binop(lZF,Xor,eAlShl0),Shl,eClPlus0),Xor,Binop(eAl,Shl,eCl)))]
let teCmov        = List.map (fun e -> ite e eEax eEax) tfFlags
let teSetcc       = List.map (fun e -> ite e e1_8 e0_8) tfFlags

let subst1 r w           = SynthesisIR.([r,[w]])
let subst2 r w1 w2       = SynthesisIR.([r,[w1;w2]])
let subst3 r w1 w2 w3    = SynthesisIR.([r,[w1;w2;w3]])
let subst4 r w1 w2 w3 w4 = SynthesisIR.([r,[w1;w2;w3;w4]])

let a_subst_eAl_eAl                = subst1 eAl eAl (* Unnecessary *)
let a_subst_eAl_eCl                = subst1 eAl eCl
let a_subst_eAl_eAleCl             = subst2 eAl eAl eCl
let a_subst_eAl_eAleCleAlMinusCl   = subst3 eAl eAl eCl SynthesisIR.(Binop(eAl,Sub,eCl))
let a_subst_eAl_eCF                = subst1 eAl SynthesisIR.(Cast(Unsigned,TypeReg_8,eCF))
let a_subst_eZF_eCF                = subst1 eZF eCF
let a_subst_eAl_eAleCleCF          = subst3 eAl eAl eCl SynthesisIR.(Binop(eCl,Add,cu8 eCF))
let a_subst_eAl_shift              = subst1 eAl eAl
let a_subst_eCl_shift              = subst2 eCl SynthesisIR.(Binop(eCl,And,Const(31L,IR.TypeReg_8))) 
                                       SynthesisIR.(Binop(Binop(eCl,And,Const(31L,IR.TypeReg_8)),Sub,Const(1L,IR.TypeReg_8)))
let a_subst_eCl_rotate             = subst3 eCl SynthesisIR.(Binop(eCl,And,Const(31L,IR.TypeReg_8))) SynthesisIR.(Binop(eCl,And,Const(7L,IR.TypeReg_8))) SynthesisIR.(Binop(Const(7L,IR.TypeReg_8),And,Binop(Const(8L,IR.TypeReg_8),Sub,eCl)))
let a_subst_e01_e01e11             = subst2 e0_1 e0_1 e1_1
let a_subst_e08_e08e18             = subst2 e0_8 e0_8 e1_8
let a_subst_eAx_eAx                = subst1 eAx eAx

let a_subst_eEax_eEax_eEcx         = subst2 eEax eEax eEcx

let a_subst_eZF_all                = 
  let v x = SynthesisIR.Var(x) in 
 [eZF,X86ToIRUtil.([v vZF;v vCF;v vPF;v vSF;v vOF;v vAF;v vDF])]

let a_subst_eZF_all_not            = 
  let v x = SynthesisIR.Var(x) in 
  let n x = SynthesisIR.(Unop(Not,Var(x))) in 
 [eZF,X86ToIRUtil.([v vZF;v vCF;v vPF;v vSF;v vOF;v vAF;v vDF;n vZF;n vCF;n vPF;n vSF;n vOF;n vAF;n vDF])]

let b_subst_add                  = SynthesisIR.([Add,[Add;Sub;Xor;And;Or]])
let b_subst_xor                  = SynthesisIR.([Xor,[Xor;And;Or]])
let b_subst_shl                  = SynthesisIR.([Shl,[Shl;Shr;Sar]])
let b_subst_eq                   = SynthesisIR.([EQ ,[EQ;NE;SLT;SLE;ULT;ULE]])
let u_subst_not                  = SynthesisIR.([Not,[Not;Neg]])

let c_subst_unsigned             = SynthesisIR.([Unsigned,[Unsigned;Signed]])

let bitunop_subst                = SynthesisIR.([Parity,[Parity;SignBit;SecondBit;FifthBit]])
let bitbinop_subst               = SynthesisIR.([SignDiff,[SignDiff]])

let ht_fill l = 
  let h = Hashtbl.create 10 in 
  List.iter (fun (a,b) -> Hashtbl.replace h a b) l; 
  h

let ht_binop    = ht_fill (b_subst_add@b_subst_xor@b_subst_shl@b_subst_eq)
let ht_unop     = ht_fill u_subst_not
let ht_cast     = ht_fill c_subst_unsigned
let ht_bitunop  = ht_fill bitunop_subst
let ht_bitbinop = ht_fill bitbinop_subst

let _ =
  let al  = X86.(Gb(Al))  and ah  = X86.(Gb(Ah))  and cl  = X86.(Gb(Cl))  in
  let ax  = X86.(Gw(Ax))  and cx  = X86.(Gw(Cx))  and dx  = X86.(Gw(Dx))  in
  let eax = X86.(Gd(Eax)) and ecx = X86.(Gd(Ecx)) and edx = X86.(Gd(Edx)) in
  let ral     = RegUtil.([Reg(al)])  in
  let rah     = RegUtil.([Reg(ah)])  in
  let rax     = RegUtil.([Reg(ax)])  in
  let reax    = RegUtil.([Reg(eax)]) in
  let rcl     = RegUtil.([Reg(cl)])  in
  let rcx     = RegUtil.([Reg(cx)])  in
  let recx    = RegUtil.([Reg(ecx)]) in
  let rdx     = RegUtil.([Reg(dx)])  in
  let redx    = RegUtil.([Reg(edx)]) in
  let rcf     = RegUtil.([Flag(X86.X86F_C)]) in
  let raf     = RegUtil.([Flag(X86.X86F_A)]) in
  let rsf     = RegUtil.([Flag(X86.X86F_S)]) in
  let rpf     = RegUtil.([Flag(X86.X86F_P)]) in
  let rzf     = RegUtil.([Flag(X86.X86F_Z)]) in
  let ralcf   = RegUtil.([Reg(al);Flag(X86.X86F_C)]) in
  let ralcl   = RegUtil.([Reg(al);Reg(cl)]) in
  let raxcx   = RegUtil.([Reg(ax);Reg(cx)]) in
  let reaxecx = RegUtil.([Reg(eax);Reg(ecx)]) in
  let ralclcf = RegUtil.([Reg(al);Reg(cl);Flag(X86.X86F_C)]) in
  let rflag = let f x = RegUtil.Flag(x) in X86.([f X86F_Z;f X86F_O;f X86F_C;f X86F_A;f X86F_S;f X86F_P;f X86F_D]) in
  let l_uop  =X86.([GeneralReg(al)]) in
  let l_uop16=X86.([GeneralReg(ax)]) in
  let l_uop32=X86.([GeneralReg(eax)]) in
  let l_op  = X86.([GeneralReg(al);GeneralReg(cl)]) in
  let l_op16= X86.([GeneralReg(ax);GeneralReg(cx)]) in
  let l_op32= X86.([GeneralReg(eax);GeneralReg(ecx)]) in
  let l_sop16=X86.([GeneralReg(ax);GeneralReg(cl)]) in
  let l_sop32=X86.([GeneralReg(eax);GeneralReg(cl)]) in
  let l_cl  = X86.([GeneralReg(cl)]) in
  let e m l = X86.({ pref = []; instr = (m,l); }) in
  let eCl7 = SynthesisIR.(Binop(eCl,And,Const(7L,IR.TypeReg_8))) in
  let eCli = SynthesisIR.(Binop(Binop(Const(8L,IR.TypeReg_8),Sub,eCl),And,Const(7L,IR.TypeReg_8))) in
  let s = SynthesisIR.(ITE(Binop(Const(0L,IR.TypeReg_8),EQ,eCl7),eAl,Binop(Binop(eAl,Shr,eCl7),Or,Binop(eAl,Shl,eCli)))) in

  let expand ht_atom = List.map (fun l -> (l,ht_atom,ht_binop,ht_unop,ht_bitunop,ht_bitbinop,ht_cast)) in

  let l_binaryarith8 = 
    let arith  m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_eAleCl@a_subst_e01_e01e11)) (teArith@tfArith) in
    let mul2   m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_eAleCl@a_subst_eAx_eAx)) (teMul@tfMul) in
    let comp   m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_eAleCleAlMinusCl@a_subst_e01_e01e11)) (teArith@tfArith) in
    let carith m = e m  l_op, ralclcf,expand (ht_fill (a_subst_eAl_eAleCleCF)) (eAlEqAlXorAlEqAl::teArith@tfArith) in
    let larith m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_eAleCl)) (eAlEq0::tfLogical@teLogical) in
    let test   m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_eAleCl)) (tfLogicalTest@teLogical) in
    X86.([
      arith Add; arith Sub; arith Xadd;
      comp Cmp;
      larith Xor; larith And; larith Or;
      test Test;    
    (*carith Adc; carith Sbb;*)
    ])    
  in
  let l_unaryarith8 = 
    let mul1   m = e m  l_cl, ralcl,  expand (ht_fill (a_subst_eAl_eAleCl@a_subst_eAx_eAx)) (teMul@tfMul) in
    let uarith m = e m  l_uop,ral,    expand (ht_fill (a_subst_eAl_eAl@a_subst_e08_e08e18)) (eAlPlus0::tfArith) in
    let unary  m = e m  l_uop,ral,    expand (ht_fill (a_subst_eAl_eAl)) (eAlAnd0xFEq0::eAlEq0XorAlEqAl::teUnary@tfUnary) in
    X86.([
      uarith Inc; uarith Dec;
      unary  Neg; unary  Not; 
      mul1 Mul; mul1 Imul;
    ])    
  in
  let l_shift8 =
    let shift  m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_shift@a_subst_eCl_shift@a_subst_eZF_all)) (teShift@tfShift) in
    let shiftd m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_shift@a_subst_eCl_rotate@a_subst_eZF_all)) (teRotate@tfShift) in
    let rot    m = e m  l_op, ralcl,  expand (ht_fill (a_subst_eAl_shift@a_subst_eCl_rotate@a_subst_eZF_all)) (teRotate@tfRotate) in
    X86.([
      shift Shl;
      shift Shr; shift Sar;
    (*rot Ror; rot Rol;*)
    (*rct Rcr; rct Rcl;*)
    ])    
  in
  
  let e_atom = Hashtbl.create 1 in
  let e_binop = Hashtbl.create 1 in
  let e_unop  = Hashtbl.create 1 in
  let e_bitunop = Hashtbl.create 1 in
  let e_bitbinop = Hashtbl.create 1 in
  let e_cast = Hashtbl.create 1 in
  let ht_arith_gen16 = ht_fill [eAl,[eAx]; eCl,[eCx]; mk8 0L,[mk16 0L];mk8 1L,[mk16 1L];mk8 16L,[mk16 16L];cu8 eCF,[cu16 eCF]] in
  let ht_arith_gen32 = ht_fill [eAl,[eEax];eCl,[eEcx];mk8 0L,[mk32 0L];mk8 1L,[mk32 1L];mk8 16L,[mk32 16L];cu8 eCF,[cu32 eCF]] in
  let ht_shift_gen16 = ht_fill [eAl,[eAx]; mk8 0L,[mk16 0L]]  in
  let ht_shift_gen32 = ht_fill [eAl,[eEax];mk8 0L,[mk32 0L]] in
  
  let mk_g_generalized l_base (l_op16,ht16) (l_op32,ht32) = 
    Generator.(mk_transform_generator (fun (({X86.pref=p;X86.instr=(m,l_op);} as i8),r,l_tmpl) ->
  (*Printf.printf "\n%s\n%!" (X86Disasm.string_of_x86instr i8);*)
    let r8 = AnalyzeInstruction.(analyze_instr i8 Autodetect (Exhaustive(r)) l_tmpl) in
    let gen e ht = (SynthesisIR.generator_of_expr e ht e_binop e_unop e_bitunop e_bitbinop e_cast) true in
    let mk_l rnew ht = List.fold_left (fun acc (r,o) -> match o with
      | None 
      | Some([]) -> acc
      | Some((s,_)::_) -> 
        let g = gen s ht in
        Printf.printf "%s: %s -gen-> %s\n" 
         (RegUtil.string_of_qtype r) 
         (PpIR.ppExpr false (SynthesisIR.convert_expr s))
         (PpIR.ppExpr false (SynthesisIR.convert_expr g));
       ((if r = List.hd ral then rnew else r),g)::acc)
      []
      r8
    in
    let mode = AnalyzeInstruction.Randomized(100000,[]) in
    let i16 = X86.({pref=p;instr=(m,l_op16);}) and i32 = X86.({pref=p;instr=(m,l_op32);}) in
    let r16 = AnalyzeInstruction.analyze_instr_hypothesis_list i16 mode (mk_l (List.hd rax ) ht16) in
    let r32 = AnalyzeInstruction.analyze_instr_hypothesis_list i32 mode (mk_l (List.hd reax) ht32) in
    let r8 = List.fold_right (fun (r,o) acc -> match o with | None | Some([]) -> acc | Some(s::_) -> (r,[s])::acc) r8 [] in
    [i8,r8;i16,r16;i32,r32])
   (mk_generator l_base))
  
  in
(*let g_binaryarith = mk_g_generalized l_binaryarith8 (l_op16, ht_arith_gen16) (l_op32, ht_arith_gen32) in
  let g_unaryarith  = mk_g_generalized l_unaryarith8  (l_uop16,ht_arith_gen16) (l_uop32,ht_arith_gen32) in
  let g_shift       = mk_g_generalized l_shift8       (l_sop16,ht_shift_gen16) (l_sop32,ht_shift_gen32) in*)
  let g_gen = Generator.mk_sequential_generator [g_binaryarith;g_unaryarith;g_shift] in
  Generator.iter (fun l -> List.iter (fun (i,l) -> 
    Printf.printf "\n%s\n%!" (X86Disasm.string_of_x86instr i);
      List.iter (fun (r,l) -> match l with | [] -> () | (s,_)::_ -> 
        Printf.printf "%s: %s\n" (RegUtil.string_of_qtype r) (PpIR.ppExpr false (SynthesisIR.convert_expr s))) l)
    l)
    g_shift;*)
  
  
  
  let l_nongeneralizable8 =
    let setcc  m = e m  l_uop,rflag,  expand (ht_fill (a_subst_eZF_all_not)) teSetcc in
    let flag32 m = e m l_op32,rflag,  expand (ht_fill (a_subst_eZF_all_not@a_subst_eEax_eEax_eEcx)) teCmov in
    let flag   m = e m    [], rflag,  expand (ht_fill (a_subst_eZF_all)) (tfFlag)   in
    X86.([
      flag Clc; flag Stc; flag Cld; flag Std; flag Cmc;
      flag32 Cmova;  flag32 Cmovg;  flag32 Cmovnz;
      flag32 Cmovae; flag32 Cmovge;
      flag32 Cmovo;  flag32 Cmovp;  flag32 Cmovs;  flag32 Cmovz;
      flag32 Cmovbe; flag32 Cmovb;  flag32 Cmovle; flag32 Cmovl;
      flag32 Cmovno; flag32 Cmovnp; flag32 Cmovns;
      setcc Seta;  setcc Setg;  setcc Setnz;
      setcc Setae; setcc Setge;
      setcc Seto;  setcc Setp;  setcc Sets;  setcc Setz;
      setcc Setbe; setcc Setb;  setcc Setle; setcc Setl;
      setcc Setno; setcc Setnp; setcc Setns;
    ])
  in
  let print_lhyp = function
  | []      -> Printf.printf "FAILED!\n%!"
  | [hyp,_] -> Printf.printf "%s\n%!" (PpIR.ppExpr false (SynthesisIR.convert_expr hyp))
  | _       -> Printf.printf "MULTIPLE? INVALID USE OF analyze_instr_hypothesis_list!\n%!"
  in
(*
  List.iter (fun (instr,r,l_tmpl) -> 
    Printf.printf "\n%s\n%!" (X86Disasm.string_of_x86instr instr);
    let res = AnalyzeInstruction.(analyze_instr instr Autodetect (Exhaustive(r)) l_tmpl) in
    AnalyzeInstruction.print_results res)
    l_tmpl;
*)
(*
  let sahf = List.map 
   (fun f -> let fv = RegUtil.Flag(f) in (fv,
     cl1 SynthesisIR.(Binop(eAh,Shr,mk8 (Int64.of_int (X86Misc.int_of_x86_flags f))))))
    X86.([X86F_S;X86F_Z;X86F_A;X86F_P;X86F_C])
  in
  let lahf = List.fold_left 
   (fun acc f -> 
     SynthesisIR.(Binop(acc,Or,
       Binop(cu8 (Var(Hashtbl.find RegUtil.ht_reg2var (RegUtil.Flag(f)))),
       Shl,
       mk8 (Int64.of_int (X86Misc.int_of_x86_flags f))))))
   (mk8 2L)
    X86.([X86F_S;X86F_Z;X86F_A;X86F_P;X86F_C])
  in
  
  let shl b i = SynthesisIR.(Binop(b,Shl,mk8 i)) in
  let shr b i = SynthesisIR.(Binop(b,Shr,mk8 i)) in
  let cond = SynthesisIR.(Binop(eAF,Or,Binop(mk8 9L,ULT,Binop(eAl,And,mk8 15L)))) in
  let carryhi = SynthesisIR.(Binop(Binop(mk8 0x99L,ULT,eAl),Or,eCF)) in
  let l_individual = AnalyzeInstruction.([
  (*e X86.Salc [],Exhaustive(ralcf),[List.hd ral,(ite eCF eFF_8 e0_8)];
    e X86.Sahf [],Exhaustive(rah),sahf;
    e X86.Lahf [],Exhaustive(rflag),[List.hd rah,lahf];
    e X86.Movzx X86.([GeneralReg(ax);GeneralReg(cl)]),Exhaustive(rcl),[List.hd rax,cu16 eCl];
    e X86.Movzx X86.([GeneralReg(eax);GeneralReg(cl)]),Exhaustive(rcl),[List.hd reax,cu32 eCl];
    e X86.Movzx X86.([GeneralReg(eax);GeneralReg(cx)]),Exhaustive(rcx),[List.hd reax,cu32 eCx];
    e X86.Movsx X86.([GeneralReg(ax);GeneralReg(cl)]),Exhaustive(rcl),[List.hd rax,cs16 eCl];
    e X86.Movsx X86.([GeneralReg(eax);GeneralReg(cl)]),Exhaustive(rcl),[List.hd reax,cs32 eCl];
    e X86.Movsx X86.([GeneralReg(eax);GeneralReg(cx)]),Exhaustive(rcx),[List.hd reax,cs32 eCx];
    e X86.Cbw  [],Exhaustive(ral),[List.hd rax,cs16 eAl];
    e X86.Cwde [],Exhaustive(rax),[List.hd reax,cs32 eAx];
    e X86.Cwd  [],Exhaustive(rax),[List.hd rdx,ch16 (cs32 eAx)];
    e X86.Bswap X86.([GeneralReg(ax)]),Exhaustive(rax),[List.hd rax,mk16 0L];

    e X86.Cdq  [],Randomized(500000,[]),reax,[List.hd redx,ch32 (cs64 eEax)];
    e X86.Bswap X86.([GeneralReg(eax)]),Randomized(50,[]),reax,[List.hd reax,
      let b3,b2,b1,b0 = cu32 (ch8 eEax),cu32 (cl8 (ch16 eEax)),cu32 (ch8 (cl16 eEax)),cu32 (cl8 eEax) in
      SynthesisIR.(Binop(Binop(shl b0 24L,Or,shl b1 16L),Or,Binop(shl b2 8L,Or,b3)))];
    e X86.Popcnt l_op16,Exhaustive(rcx),[List.hd rax,
      let r1 = SynthesisIR.(Binop(Binop(mk16 0x5555L,And,eCx),Add,shr (Binop(mk16 0xAAAAL,And,eCx)) 1L)) in
      let r2 = SynthesisIR.(Binop(Binop(mk16 0x3333L,And,r1), Add,shr (Binop(mk16 0xCCCCL,And,r1))  2L)) in
      let r3 = SynthesisIR.(Binop(Binop(mk16 0x0F0FL,And,r2), Add,shr (Binop(mk16 0xF0F0L,And,r2))  4L)) in
               SynthesisIR.(Binop(Binop(mk16 0x00FFL,And,r3), Add,shr (Binop(mk16 0xFF00L,And,r3))  8L))];
        
    e X86.Popcnt l_op32,Randomized(5000,[]),recx,[List.hd reax,
      let r1 = SynthesisIR.(Binop(Binop(mk32 0x55555555L,And,eEcx),Add,shr (Binop(mk32 0xAAAAAAAAL,And,eEcx)) 1L)) in
      let r2 = SynthesisIR.(Binop(Binop(mk32 0x33333333L,And,r1),  Add,shr (Binop(mk32 0xCCCCCCCCL,And,r1))   2L)) in
      let r3 = SynthesisIR.(Binop(Binop(mk32 0x0F0F0F0FL,And,r2),  Add,shr (Binop(mk32 0xF0F0F0F0L,And,r2))   4L)) in
      let r4 = SynthesisIR.(Binop(Binop(mk32 0x00FF00FFL,And,r3),  Add,shr (Binop(mk32 0xFF00FF00L,And,r3))   8L)) in
               SynthesisIR.(Binop(Binop(mk32 0x0000FFFFL,And,r4),  Add,shr (Binop(mk32 0xFFFF0000L,And,r4))  16L))];
    
    e X86.Aaa [],Exhaustive(rax),[List.hd rcf,cond;List.hd raf,cond;List.hd rax,
      SynthesisIR.(Binop(mk16 0xFF0FL,And,ite cond (Binop(eAx,Add,mk16 0x106L)) eAx))];

    e X86.Aas [],Exhaustive(rax),[List.hd rcf,cond;List.hd raf,cond;List.hd rax,
      SynthesisIR.(Binop(mk16 0xFF0FL,And,ite cond (Binop(eAx,Sub,mk16 0x106L)) eAx))];
      
    
    e X86.Daa [],Exhaustive(ral),
     (let newal = SynthesisIR.(Binop(eAl,Add,Binop(ite cond (mk8 6L) e0_8,Add,ite carryhi (mk8 0x60L) e0_8))) in
     [List.hd rcf,carryhi;List.hd raf,cond;List.hd ral,newal;List.hd rzf,SynthesisIR.(Binop(newal,EQ,e0_8));
      List.hd rsf,SynthesisIR.(BitUnop(SignBit,newal));List.hd rpf,SynthesisIR.(BitUnop(Parity,newal))]);

    
    e X86.Das [],Exhaustive(ral),
     (let newal = SynthesisIR.(Binop(eAl,Sub,Binop(ite cond (mk8 6L) e0_8,Add,ite carryhi (mk8 0x60L) e0_8))) in
     [List.hd rcf,(ite carryhi e1_1 (ite cond (SynthesisIR.(Binop(eCF,Or,Binop(eAl,ULT,mk8 6L)))) e0_1));
      List.hd raf,cond;List.hd ral,newal;List.hd rzf,SynthesisIR.(Binop(newal,EQ,e0_8));
      List.hd rsf,SynthesisIR.(BitUnop(SignBit,newal));List.hd rpf,SynthesisIR.(BitUnop(Parity,newal))]);
    
    e X86.Bsr l_op16,Exhaustive(rcx),
     (let rec aux v i = 
        if i = 16 then v 
        else 
          let ei = mk16 (Int64.of_int i) in
          aux (ite SynthesisIR.(Binop(mk16 0L,NE,Binop(eCx,And,mk16 (Int64.shift_left 1L i)))) ei v) (i+1) 
      in
      let e = aux eAx 0 in
     [List.hd rax,e;List.hd rzf,SynthesisIR.(Binop(eCx,EQ,mk16 0L))]);
      
    e X86.Bsr l_op32,Randomized(5000,[]),recx,
     (let rec aux v i = 
        if i = 32 then v 
        else 
          let ei = mk32 (Int64.of_int i) in
          aux (ite SynthesisIR.(Binop(mk32 0L,NE,Binop(eEcx,And,mk32 (Int64.shift_left 1L i)))) ei v) (i+1) 
      in
      let e = aux eEax 0 in
     [List.hd reax,e;List.hd rzf,SynthesisIR.(Binop(eEcx,EQ,mk32 0L))]);

    e X86.Bsf l_op16,Exhaustive(rcx),
     (let rec aux v i = 
        if i = -1 then v 
        else 
          let ei = mk16 (Int64.of_int i) in
          aux (ite SynthesisIR.(Binop(mk16 0L,NE,Binop(eCx,And,mk16 (Int64.shift_left 1L i)))) ei v) (i-1) 
      in
      let e = aux eAx 15 in
     [List.hd rax,e;List.hd rzf,SynthesisIR.(Binop(eCx,EQ,mk16 0L))]);
      
    e X86.Bsf l_op32,Randomized(5000,[]),recx,
     (let rec aux v i = 
        if i = -1 then v 
        else 
          let ei = mk32 (Int64.of_int i) in
          aux (ite SynthesisIR.(Binop(mk32 0L,NE,Binop(eEcx,And,mk32 (Int64.shift_left 1L i)))) ei v) (i-1) 
      in
      let e = aux eEax 31 in
     [List.hd reax,e;List.hd rzf,SynthesisIR.(Binop(eEcx,EQ,mk32 0L))]);

    e X86.Bt l_op16,Exhaustive(rcx),
     (let bit = SynthesisIR.(Binop(mk16 1L,Shl,Binop(mk8 0xFL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk16 0L,NE,Binop(bit,And,eAx)))]);
    
    e X86.Bt l_op32,Randomized(5000,[]),recx,
     (let bit = SynthesisIR.(Binop(mk32 1L,Shl,Binop(mk8 0x1FL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk32 0L,NE,Binop(bit,And,eEax)))]);

    e X86.Btr l_op16,Exhaustive(rcx),
     (let bit = SynthesisIR.(Binop(mk16 1L,Shl,Binop(mk8 0xFL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk16 0L,NE,Binop(bit,And,eAx)));
      List.hd rax,SynthesisIR.(Binop(Unop(Not,bit),And,eAx))]);
    
    e X86.Btr l_op32,Randomized(5000,[]),recx,
     (let bit = SynthesisIR.(Binop(mk32 1L,Shl,Binop(mk8 0x1FL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk32 0L,NE,Binop(bit,And,eEax)));
      List.hd reax,SynthesisIR.(Binop(Unop(Not,bit),And,eEax))]);

    e X86.Bts l_op16,Exhaustive(rcx),
     (let bit = SynthesisIR.(Binop(mk16 1L,Shl,Binop(mk8 0xFL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk16 0L,NE,Binop(bit,And,eAx)));
      List.hd rax,SynthesisIR.(Binop(bit,Or,eAx))]);
    
    e X86.Bts l_op32,Randomized(5000,[]),recx,
     (let bit = SynthesisIR.(Binop(mk32 1L,Shl,Binop(mk8 0x1FL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk32 0L,NE,Binop(bit,And,eEax)));
      List.hd reax,SynthesisIR.(Binop(bit,Or,eEax))]);
     
    e X86.Btc l_op16,Exhaustive(rcx),
     (let bit = SynthesisIR.(Binop(mk16 1L,Shl,Binop(mk8 0xFL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk16 0L,NE,Binop(bit,And,eAx)));
      List.hd rax,SynthesisIR.(Binop(bit,Xor,eAx))]);
    
    e X86.Btc l_op32,Randomized(5000,[]),recx,
     (let bit = SynthesisIR.(Binop(mk32 1L,Shl,Binop(mk8 0x1FL,And,eCl))) in      
     [List.hd rcf,SynthesisIR.(Binop(mk32 0L,NE,Binop(bit,And,eEax)));
      List.hd reax,SynthesisIR.(Binop(bit,Xor,eEax))]);*)
   
  ])
  in
  List.iter (fun (instr,mode,list) -> 
    Printf.printf "\n%s\n%!" (X86Disasm.string_of_x86instr instr);
    List.iter (fun (reg,l_hyp) -> Printf.printf "%s: " (RegUtil.string_of_qtype reg); 
    print_lhyp l_hyp)
   (AnalyzeInstruction.analyze_instr_hypothesis_list instr mode list))
    l_individual;*)
(*let eClAnd31 = SynthesisIR.(Binop(eCl,And,mk8 31L)) in
  let implies antec conseq = SynthesisIR.(Binop(Unop(Not,antec),Or,conseq)) in
  let antec  = SynthesisIR.(Binop(mk8 1L,EQ,eClAnd31)) in
  let shl    = SynthesisIR.(Binop(eAl,Shl,eClAnd31)) in
  let sd     = signdiff shl eAlAfter in
  let conseq = SynthesisIR.(Binop(eOFAfter,EQ,signdiff shl eAl)) in
  let impl1  = implies antec conseq in
  let antec  = SynthesisIR.(Binop(mk8 0L,EQ,eClAnd31)) in
  let conseq = SynthesisIR.(Binop(eOFAfter,EQ,eOF)) in
  let impl2 = implies antec conseq in
  let l_partial = SynthesisIR.([
      e X86.Shl l_op,AnalyzeInstruction.Exhaustive(ralcl),SynthesisIR.(Binop(impl1,And,impl2));
    ])
  in
  List.iter (fun (instr,mode,input,hyp) -> 
    Printf.printf "\n%s\n%!" (X86Disasm.string_of_x86instr instr);
    List.iter 
     (fun (_,l_hyp) -> print_lhyp l_hyp)
     (AnalyzeInstruction.analyze_instr_single_hypothesis_noreg instr mode input hyp))
    l_partial;*)
  ()
(*
(*
  let g = Generator.(mk_triple_generator_g (mk_generator [3]) (mk_generator [2;3;4;5]) (mk_generator [1;2;3;4])) in
  Generator.iter (fun (a,b,c) -> Printf.printf "(%d,%d,%d)\n" a b c) g;
  Printf.printf "\n";
  Generator.iter (fun (a,b,c) -> Printf.printf "(%d,%d,%d)\n" a b c) g;
  Printf.printf "\n";
  let g = Generator.(mk_triple_generator_g (mk_generator [3;4;5;6]) (mk_generator [2]) (mk_generator [1;2;3;4])) in
  Generator.iter (fun (a,b,c) -> Printf.printf "(%d,%d,%d)\n" a b c) g;
  Printf.printf "\n";
  let g = Generator.(mk_triple_generator_g (mk_generator [3;4;5;6]) (mk_generator [2;3;4;5]) (mk_generator [1])) in
  Generator.iter (fun (a,b,c) -> Printf.printf "(%d,%d,%d)\n" a b c) g;
  Printf.printf "\n";
  let g1 = Generator.(mk_pair_generator_g (mk_generator [3;4;5;6]) (mk_generator [2])) in
  let g2 = Generator.(mk_triple_generator_g (mk_generator [3;4;5;6]) (mk_generator [2]) (mk_generator [1;2;3;4])) in
  let g3 = Generator.generator_combine g1 g2 in
  Generator.iter (fun ((a,b),(c,d,e)) -> Printf.printf "((%d,%d),(%d,%d,%d))\n" a b c d e) g3;
  Printf.printf "\n";
  let ht_atom = ht_fill (a_subst_eEax_eEax_eEcx@a_subst_eZF_all_not) in

  let g1 = 
    Generator.mk_sequential_generator 
     (List.map 
       (fun e -> SynthesisIR.generator_of_expr e ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast) 
        e) 
  in
  Generator.iter (fun s -> Printf.printf "%s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s))) g1;
  Printf.printf "\n"
  let g0 = SynthesisIR.generator_of_expr pAl ht_atom ht_binop ht_unop ht_bitop ht_cast in
  Generator.iter (fun s -> Printf.printf "1: %s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s))) g0;
  Printf.printf "\n";

  (* The problem is that this let _ = g0 false is incrementing the generator state. *)
  let g0 = SynthesisIR.generator_of_expr pAl ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast in
  let _ = g0 false in
  Generator.iter (fun s -> Printf.printf "1: %s\n" (PpIR.ppExpr false (SynthesisIR.convert_expr s))) g0;
  Printf.printf "\n";

  let g0 = SynthesisIR.generator_of_expr pAl ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast in
  let _ = g0 false in
  let g1 = Generator.(map_multiply (fun s -> mk_generator [1,s;2,s]) g0) in
  Generator.iter (fun (d,s) -> Printf.printf "1: %d%s\n" d (PpIR.ppExpr false (SynthesisIR.convert_expr s))) g1

  let g1 = Generator.(map_multiply 
   (fun s -> mk_generator [1,s;2,s]) 
   (let g = SynthesisIR.generator_of_expr pAl ht_atom ht_binop ht_unop ht_bitunop ht_bitbinop ht_cast in let _ = g false in g)) 
  in
  Generator.iter (fun (d,s) -> Printf.printf "1: %d%s\n" d (PpIR.ppExpr false (SynthesisIR.convert_expr s))) g1;*)
*)