open X86
open IR
open IRUtil

let vEax = Variable(0 , TypeReg_32)
let vEcx = Variable(1 , TypeReg_32)
let vEdx = Variable(2 , TypeReg_32)
let vEbx = Variable(3 , TypeReg_32)
let vEsp = Variable(4 , TypeReg_32)
let vEbp = Variable(5 , TypeReg_32)
let vEsi = Variable(6 , TypeReg_32)
let vEdi = Variable(7 , TypeReg_32)
let vES  = Variable(8 , TypeReg_16)
let vCS  = Variable(9 , TypeReg_16)
let vSS  = Variable(10, TypeReg_16)
let vDS  = Variable(11, TypeReg_16)
let vFS  = Variable(12, TypeReg_16)
let vGS  = Variable(13, TypeReg_16)
let vCF  = Variable(14, TypeReg_1 )
let vPF  = Variable(15, TypeReg_1 )
let vAF  = Variable(16, TypeReg_1 )
let vZF  = Variable(17, TypeReg_1 )
let vSF  = Variable(18, TypeReg_1 )
let vOF  = Variable(19, TypeReg_1 )
let vDF  = Variable(20, TypeReg_1 )
let vCR0 = Variable(21, TypeReg_32)
let vCR1 = Variable(22, TypeReg_32)
let vCR2 = Variable(23, TypeReg_32)
let vCR3 = Variable(24, TypeReg_32)
let vCR4 = Variable(25, TypeReg_32)
let vCR5 = Variable(26, TypeReg_32)
let vCR6 = Variable(27, TypeReg_32)
let vCR7 = Variable(28, TypeReg_32)
let vDR0 = Variable(29, TypeReg_32)
let vDR1 = Variable(30, TypeReg_32)
let vDR2 = Variable(31, TypeReg_32)
let vDR3 = Variable(32, TypeReg_32)
let vDR4 = Variable(33, TypeReg_32)
let vDR5 = Variable(34, TypeReg_32)
let vDR6 = Variable(35, TypeReg_32)
let vDR7 = Variable(36, TypeReg_32)
let vMem = Mem(37,Little,TypeReg_32)

(* Beware.  THE IR TRANSLATOR DOES NOT SET THESE VARIABLES.  In other words, if
   you have an instruction like "xor al, al", THE IR WILL NOT CONTAIN ANY 
   REFERENCES WHATSOEVER TO THE vAl VARIABLE.  Instead, it will reference the
   vEax variable above.
   
   I just put these here so that there would be distinguishment for these 
   specific registers, because it does come in handy to use them in parts of
   the framework.  And I am considering perhaps altering the IR translator
   to explicitly assign these variables too. *)
let vAl = Variable(38, TypeReg_8)
let vAh = Variable(39, TypeReg_8)
let vCl = Variable(40, TypeReg_8)
let vCh = Variable(41, TypeReg_8)
let vDl = Variable(42, TypeReg_8)
let vDh = Variable(43, TypeReg_8)
let vBl = Variable(44, TypeReg_8)
let vBh = Variable(45, TypeReg_8)
let vAx = Variable(46, TypeReg_16)
let vCx = Variable(47, TypeReg_16)
let vDx = Variable(48, TypeReg_16)
let vBx = Variable(49, TypeReg_16)
let vSp = Variable(50, TypeReg_16)
let vBp = Variable(51, TypeReg_16)
let vSi = Variable(52, TypeReg_16)
let vDi = Variable(53, TypeReg_16)

let reserved_vars = [
  vEax;vEcx;vEdx;vEbx;vEsp;vEbp;vEsi;vEdi;
  vES;vCS;vSS;vDS;vFS;vGS;
  vCF;vPF;vAF;vZF;vSF;vOF;vDF;
  vCR0;vCR1;vCR2;vCR3;vCR4;vCR5;vCR6;vCR7;
  vDR0;vDR1;vDR2;vDR3;vDR4;vDR5;vDR6;vDR7;
  vMem;
  vAl;vAh;vCl;vCh;vDl;vDh;vBl;vBh;
  vAx;vCx;vDx;vBx;vSp;vBp;vSi;vDi;
]

(* Frequently-needed variables (registers, esp); all reserved variables as a set *)
let l_v_general_registers32_noesp = [vEax;vEcx;vEdx;vEbx;vEbp;vEsi;vEdi]
let l_v_general_registers32       = vEsp::l_v_general_registers32_noesp
let l_v_general_registers16_nosp  = [vAx;vCx;vDx;vBx;vBp;vSi;vDi]
let l_v_general_registers16       = vSp::l_v_general_registers16_nosp
let l_v_general_registers8        = [vAl;vAh;vCl;vCh;vDl;vDh;vBl;vBh]
let l_v_segs                      = [vES;vCS;vSS;vDS;vFS;vGS]
let l_v_flags                     = [vCF;vPF;vAF;vZF;vSF;vOF;vDF]
let l_v_control                   = [vCR0;vCR1;vCR2;vCR3;vCR4;vCR5;vCR6;vCR7]
let l_v_debug                     = [vDR0;vDR1;vDR2;vDR3;vDR4;vDR5;vDR6;vDR7]

let l_v_general_registers_parentage_noesp =
 [(vEax,vAx,Some(vAh,vAl));(vEcx,vCx,Some(vCh,vCl));(vEdx,vDx,Some(vDh,vDl));
  (vEbx,vBx,Some(vBh,vBl));(vEbp,vBp,None);(vEsi,vSi,None);(vEdi,vDi,None)]

let l_v_general_registers_parentage = (vEsp,vSp,None)::l_v_general_registers_parentage_noesp

let s_general_registers32_noesp = IRUtil.var_setify l_v_general_registers32_noesp
let s_general_registers32 = IRUtil.VarSet.add vEsp    s_general_registers32_noesp

let s_general_registers16_nosp = IRUtil.var_setify l_v_general_registers16_nosp
let s_general_registers16 = IRUtil.VarSet.add vSp     s_general_registers16_nosp

let s_general_registers_8 = IRUtil.var_setify l_v_general_registers8

let s_general_registers_noesp_nosp = 
  IRUtil.VarSet.union 
   (IRUtil.VarSet.union 
      s_general_registers32_noesp
      s_general_registers16_nosp)
    s_general_registers_8

let reserved_vars_set = IRUtil.var_setify reserved_vars
let num_reserved_vars = List.length reserved_vars

let m_v32_to_v16 =
  IRUtil.var_mapify
   [vEax,vAx;vEcx,vCx;vEdx,vDx;vEbx,vBx;vEsp,vSp;vEbp,vBp;vEsi,vSi;vEdi,vDi]
   
(* Raises Not_found if x is not a general register variable *)
let v16_of_v32_exn x = IRUtil.VarMap.find x m_v32_to_v16

let m_v32_to_v8p_opt =
  IRUtil.var_mapify
   [vEax,Some(vAh,vAl);vEcx,Some(vCh,vCl);vEdx,Some(vDh,vDl);vEbx,Some(vBh,vBl);
    vEsp,None;vEbp,None;vEsi,None;vEdi,None]

(* Raises Not_found if x is not a general register variable *)
let v8p_opt_of_v32_exn x = IRUtil.VarMap.find x m_v32_to_v8p_opt

let m_v16_to_v8p_opt =
  IRUtil.var_mapify
   [vAx,Some(vAh,vAl);vCx,Some(vCh,vCl);vDx,Some(vDh,vDl);vBx,Some(vBh,vBl);
    vSp,None;vBp,None;vSi,None;vDi,None]

(* Raises Not_found if x is not a general register variable *)
let v8p_opt_of_v16_exn x = IRUtil.VarMap.find x m_v16_to_v8p_opt

let eEax = mk_evar vEax let eEcx = mk_evar vEcx let eEdx = mk_evar vEdx let eEbx = mk_evar vEbx
let eEsp = mk_evar vEsp let eEbp = mk_evar vEbp let eEsi = mk_evar vEsi let eEdi = mk_evar vEdi

let eES  = mk_evar vES  let eCS  = mk_evar vCS  let eSS  = mk_evar vSS  let eDS  = mk_evar vDS
let eFS  = mk_evar vFS  let eGS  = mk_evar vGS

let eCF  = mk_evar vCF  let ePF  = mk_evar vPF  let eAF  = mk_evar vAF  let eZF  = mk_evar vZF
let eSF  = mk_evar vSF  let eOF  = mk_evar vOF  let eDF  = mk_evar vDF

let eCR0 = mk_evar vCR0 let eCR1 = mk_evar vCR1 let eCR2 = mk_evar vCR2 let eCR3 = mk_evar vCR3
let eCR4 = mk_evar vCR4 let eCR5 = mk_evar vCR5 let eCR6 = mk_evar vCR6 let eCR7 = mk_evar vCR7

let eDR0 = mk_evar vDR0 let eDR1 = mk_evar vDR1 let eDR2 = mk_evar vDR2 let eDR3 = mk_evar vDR3
let eDR4 = mk_evar vDR4 let eDR5 = mk_evar vDR5 let eDR6 = mk_evar vDR6 let eDR7 = mk_evar vDR7

let eMem = mk_evar vMem

let var_of_reg32 = function
| Eax -> vEax
| Ecx -> vEcx
| Edx -> vEdx
| Ebx -> vEbx
| Esp -> vEsp
| Ebp -> vEbp
| Esi -> vEsi
| Edi -> vEdi

let expr_of_reg32 = function
| Eax -> eEax
| Ecx -> eEcx
| Edx -> eEdx
| Ebx -> eEbx
| Esp -> eEsp
| Ebp -> eEbp
| Esi -> eEsi
| Edi -> eEdi

let var_of_controlreg = function
| CR0 -> vCR0
| CR1 -> vCR1
| CR2 -> vCR2
| CR3 -> vCR3
| CR4 -> vCR4
| CR5 -> vCR5
| CR6 -> vCR6
| CR7 -> vCR7

let expr_of_controlreg = function
| CR0 -> eCR0
| CR1 -> eCR1
| CR2 -> eCR2
| CR3 -> eCR3
| CR4 -> eCR4
| CR5 -> eCR5
| CR6 -> eCR6
| CR7 -> eCR7

let var_of_debugreg = function
| DR0 -> vDR0
| DR1 -> vDR1
| DR2 -> vDR2
| DR3 -> vDR3
| DR4 -> vDR4
| DR5 -> vDR5
| DR6 -> vDR6
| DR7 -> vDR7

let expr_of_debugreg = function
| DR0 -> eDR0
| DR1 -> eDR1
| DR2 -> eDR2
| DR3 -> eDR3
| DR4 -> eDR4
| DR5 -> eDR5
| DR6 -> eDR6
| DR7 -> eDR7

let var_of_segreg = function
| CS -> vCS
| DS -> vDS
| ES -> vES
| FS -> vFS
| GS -> vGS
| SS -> vSS

let expr_of_segreg = function
| CS -> eCS
| DS -> eDS
| ES -> eES
| FS -> eFS
| GS -> eGS
| SS -> eSS

let var_of_flagreg = function
| X86F_Z -> vZF
| X86F_P -> vPF
| X86F_S -> vSF
| X86F_C -> vCF
| X86F_O -> vOF
| X86F_A -> vAF
| X86F_D -> vDF

let expr_of_flagreg = function
| X86F_Z -> eZF
| X86F_P -> ePF
| X86F_S -> eSF
| X86F_C -> eCF
| X86F_O -> eOF
| X86F_A -> eAF
| X86F_D -> eDF

let eReg8Lower basereg = mk_cast (Low) (TypeReg_8) basereg
let eReg8Upper basereg = mk_cast (Low) (TypeReg_8) (mk_shr basereg (mk_byte 0x8L))

let eAl = eReg8Lower eEax
let eCl = eReg8Lower eEcx
let eDl = eReg8Lower eEdx
let eBl = eReg8Lower eEbx
let eAh = eReg8Upper eEax
let eCh = eReg8Upper eEcx
let eDh = eReg8Upper eEdx
let eBh = eReg8Upper eEbx

let m_v8_to_e8 =
  IRUtil.var_mapify
   [vAl,eAl;vAh,eAh;vCl,eCl;vCh,eCh;vDl,eDl;vDh,eDh;vBl,eBl;vBh,eBh]

let expr_of_reg8 = function
| Al -> eAl
| Cl -> eCl
| Dl -> eDl
| Bl -> eBl
| Ah -> eAh
| Ch -> eCh
| Dh -> eDh
| Bh -> eBh

let var_of_reg8 = function
| Al -> vAl
| Cl -> vCl
| Dl -> vDl
| Bl -> vBl
| Ah -> vAh
| Ch -> vCh
| Dh -> vDh
| Bh -> vBh

let vr32_of_reg8 = function
| Al | Ah -> vEax
| Cl | Ch -> vEcx
| Dl | Dh -> vEdx
| Bl | Bh -> vEbx

let er32_of_reg8 = function
| Al | Ah -> eEax
| Cl | Ch -> eEcx
| Dl | Dh -> eEdx
| Bl | Bh -> eEbx

let eReg16 basereg = mk_cast (Low) (TypeReg_16) basereg

let eAx = eReg16 eEax
let eCx = eReg16 eEcx
let eDx = eReg16 eEdx
let eBx = eReg16 eEbx
let eSp = eReg16 eEsp
let eBp = eReg16 eEbp
let eSi = eReg16 eEsi
let eDi = eReg16 eEdi

let m_v16_to_e16 =
  IRUtil.var_mapify
   [vAx,eAx;vCx,eCx;vDx,eDx;vBx,eBx;vSp,eSp;vBp,eBp;vSi,eSi;vDi,eDi]

let expr_of_reg16 = function
| Ax -> eAx
| Cx -> eCx
| Dx -> eDx
| Bx -> eBx
| Sp -> eSp
| Bp -> eBp
| Si -> eSi
| Di -> eDi

let var_of_reg16 = function
| Ax -> vAx
| Cx -> vCx
| Dx -> vDx
| Bx -> vBx
| Sp -> vSp
| Bp -> vBp
| Si -> vSi
| Di -> vDi

let vr32_of_reg16 = function
| Ax -> vEax
| Cx -> vEcx
| Dx -> vEdx
| Bx -> vEbx
| Sp -> vEsp
| Bp -> vEbp
| Si -> vEsi
| Di -> vEdi

let er32_of_reg16 = function
| Ax -> eEax
| Cx -> eEcx
| Dx -> eEdx
| Bx -> eEbx
| Sp -> eEsp
| Bp -> eEbp
| Si -> eEsi
| Di -> eEdi

let expr_of_general_reg = function
| Gd(x) -> expr_of_reg32 x
| Gw(x) -> expr_of_reg16 x
| Gb(x) -> expr_of_reg8  x

let var_of_general_reg = function
| Gd(x) -> var_of_reg32 x
| Gw(x) -> var_of_reg16 x
| Gb(x) -> var_of_reg8  x

let vEaxAfter = Variable(54, TypeReg_32)
let vEcxAfter = Variable(55, TypeReg_32)
let vEdxAfter = Variable(56, TypeReg_32)
let vEbxAfter = Variable(57, TypeReg_32)
let vEspAfter = Variable(58, TypeReg_32)
let vEbpAfter = Variable(59, TypeReg_32)
let vEsiAfter = Variable(60, TypeReg_32)
let vEdiAfter = Variable(61, TypeReg_32)
let vESAfter  = Variable(62, TypeReg_16)
let vCSAfter  = Variable(63, TypeReg_16)
let vSSAfter  = Variable(64, TypeReg_16)
let vDSAfter  = Variable(65, TypeReg_16)
let vFSAfter  = Variable(66, TypeReg_16)
let vGSAfter  = Variable(67, TypeReg_16)
let vCFAfter  = Variable(68, TypeReg_1 )
let vPFAfter  = Variable(69, TypeReg_1 )
let vAFAfter  = Variable(70, TypeReg_1 )
let vZFAfter  = Variable(71, TypeReg_1 )
let vSFAfter  = Variable(72, TypeReg_1 )
let vOFAfter  = Variable(73, TypeReg_1 )
let vDFAfter  = Variable(74, TypeReg_1 )
let vCR0After = Variable(75, TypeReg_32)
let vCR1After = Variable(76, TypeReg_32)
let vCR2After = Variable(77, TypeReg_32)
let vCR3After = Variable(78, TypeReg_32)
let vCR4After = Variable(79, TypeReg_32)
let vCR5After = Variable(80, TypeReg_32)
let vCR6After = Variable(81, TypeReg_32)
let vCR7After = Variable(82, TypeReg_32)
let vDR0After = Variable(83, TypeReg_32)
let vDR1After = Variable(84, TypeReg_32)
let vDR2After = Variable(85, TypeReg_32)
let vDR3After = Variable(86, TypeReg_32)
let vDR4After = Variable(87, TypeReg_32)
let vDR5After = Variable(88, TypeReg_32)
let vDR6After = Variable(89, TypeReg_32)
let vDR7After = Variable(90, TypeReg_32)
let vMemAfter = Mem(     91,Little,TypeReg_32)
let vAlAfter  = Variable(92, TypeReg_8)
let vAhAfter  = Variable(93, TypeReg_8)
let vClAfter  = Variable(94, TypeReg_8)
let vChAfter  = Variable(95, TypeReg_8)
let vDlAfter  = Variable(96, TypeReg_8)
let vDhAfter  = Variable(97, TypeReg_8)
let vBlAfter  = Variable(98, TypeReg_8)
let vBhAfter  = Variable(99, TypeReg_8)
let vAxAfter  = Variable(100, TypeReg_16)
let vCxAfter  = Variable(101, TypeReg_16)
let vDxAfter  = Variable(102, TypeReg_16)
let vBxAfter  = Variable(103, TypeReg_16)
let vSpAfter  = Variable(104, TypeReg_16)
let vBpAfter  = Variable(105, TypeReg_16)
let vSiAfter  = Variable(106, TypeReg_16)
let vDiAfter  = Variable(107, TypeReg_16)

let l_reg32_after = [vEaxAfter;vEcxAfter;vEdxAfter;vEbxAfter;vEspAfter;vEbpAfter;vEsiAfter;vEdiAfter]
let l_reg16_after = [vAxAfter; vCxAfter; vDxAfter; vBxAfter; vSpAfter; vBpAfter; vSiAfter; vDiAfter]
let l_reg8_after  = [vAlAfter; vClAfter; vDlAfter; vBlAfter; vAhAfter; vChAfter; vDhAfter; vBhAfter]

(* DOES NOT INCLUDE MEMORY *)
let l_p_var_varafter = 
[
  vEax,vEaxAfter;vEcx,vEcxAfter;vEdx,vEdxAfter;vEbx,vEbxAfter;
  vEsp,vEspAfter;vEbp,vEbpAfter;vEsi,vEsiAfter;vEdi,vEdiAfter;
  
  vAx, vAxAfter; vCx, vCxAfter; vDx, vDxAfter; vBx, vBxAfter;
  vSp, vSpAfter; vBp, vBpAfter; vSi, vSiAfter; vDi, vDiAfter;

  vAl, vAlAfter; vCl, vClAfter; vDl, vDlAfter; vBl, vBlAfter;
  vAh, vAhAfter; vCh, vChAfter; vDh, vDhAfter; vBh, vBhAfter;

  vES, vESAfter ;vCS, vCSAfter ;vSS, vSSAfter ;vDS, vDSAfter ;
  vFS, vFSAfter ;vGS, vGSAfter ;vCF, vCFAfter ;vPF, vPFAfter ;
  vAF, vAFAfter ;vZF, vZFAfter ;vSF, vSFAfter ;vOF, vOFAfter ;
  vDF, vDFAfter ;
  
  vCR0,vCR0After;vCR1,vCR1After;vCR2,vCR2After;vCR3,vCR3After;
  vCR4,vCR4After;vCR5,vCR5After;vCR6,vCR6After;vCR7,vCR7After;
  
  vDR0,vDR0After;vDR1,vDR1After;vDR2,vDR2After;vDR3,vDR3After;
  vDR4,vDR4After;vDR5,vDR5After;vDR6,vDR6After;vDR7,vDR7After;
]  

let l_v_general_registers_parentage_after =
 [(vEaxAfter,vAxAfter,Some(vAhAfter,vAlAfter));
  (vEcxAfter,vCxAfter,Some(vChAfter,vClAfter));
  (vEdxAfter,vDxAfter,Some(vDhAfter,vDlAfter));
  (vEbxAfter,vBxAfter,Some(vBhAfter,vBlAfter));
  (vEspAfter,vSpAfter,None);
  (vEbpAfter,vBpAfter,None);
  (vEsiAfter,vSiAfter,None);
  (vEdiAfter,vDiAfter,None)]
  
let cast_dw_to_w  edw = IRUtil.mk_low_cast (IR.TypeReg_16) edw
let cast_dw_to_lb edw = IRUtil.mk_low_cast (IR.TypeReg_8)  edw
let cast_dw_to_hb edw = IRUtil.mk_low_cast (IR.TypeReg_8) (IRUtil.mk_shr edw (IRUtil.mk_dword_of_int32 8l))

let afterize l_p_fullregs l_parentage ht_ssa = 
  let l_subregs = 
    List.fold_left  
     (fun acc (v32a,v16a,pv8a) -> 
        let e32a = IRUtil.mk_evar v32a in
        let acc = (IRUtil.mk_assign v16a (cast_dw_to_w e32a))::acc in
        match pv8a with 
        | Some(v8h,v8l) -> (IRUtil.mk_assign v8h (cast_dw_to_hb e32a))::(IRUtil.mk_assign v8l (cast_dw_to_lb e32a))::acc
        | None -> acc)
      []
      l_parentage
  in
  let ssa vb = try Hashtbl.find ht_ssa vb with Not_found -> vb in
  List.fold_left 
   (fun acc (vb,va) -> (IRUtil.mk_assign va (mk_evar (ssa vb)))::acc) 
    l_subregs 
    l_p_fullregs

let mk_after = afterize l_p_var_varafter l_v_general_registers_parentage_after

let m_vbefore_to_vafter  = IRUtil.var_mapify l_p_var_varafter
let m_vafter_to_vbefore  = IRUtil.var_mapify (List.map (fun (x,y) -> (y,x)) l_p_var_varafter)

let vEaxAfter2 = Variable(108, TypeReg_32)
let vEcxAfter2 = Variable(109, TypeReg_32)
let vEdxAfter2 = Variable(110, TypeReg_32)
let vEbxAfter2 = Variable(111, TypeReg_32)
let vEspAfter2 = Variable(112, TypeReg_32)
let vEbpAfter2 = Variable(113, TypeReg_32)
let vEsiAfter2 = Variable(114, TypeReg_32)
let vEdiAfter2 = Variable(115, TypeReg_32)
let vESAfter2  = Variable(116, TypeReg_16)
let vCSAfter2  = Variable(117, TypeReg_16)
let vSSAfter2  = Variable(118, TypeReg_16)
let vDSAfter2  = Variable(119, TypeReg_16)
let vFSAfter2  = Variable(120, TypeReg_16)
let vGSAfter2  = Variable(121, TypeReg_16)
let vCFAfter2  = Variable(122, TypeReg_1 )
let vPFAfter2  = Variable(123, TypeReg_1 )
let vAFAfter2  = Variable(124, TypeReg_1 )
let vZFAfter2  = Variable(125, TypeReg_1 )
let vSFAfter2  = Variable(126, TypeReg_1 )
let vOFAfter2  = Variable(127, TypeReg_1 )
let vDFAfter2  = Variable(128, TypeReg_1 )
let vCR0After2 = Variable(129, TypeReg_32)
let vCR1After2 = Variable(130, TypeReg_32)
let vCR2After2 = Variable(131, TypeReg_32)
let vCR3After2 = Variable(132, TypeReg_32)
let vCR4After2 = Variable(133, TypeReg_32)
let vCR5After2 = Variable(134, TypeReg_32)
let vCR6After2 = Variable(135, TypeReg_32)
let vCR7After2 = Variable(136, TypeReg_32)
let vDR0After2 = Variable(137, TypeReg_32)
let vDR1After2 = Variable(138, TypeReg_32)
let vDR2After2 = Variable(139, TypeReg_32)
let vDR3After2 = Variable(140, TypeReg_32)
let vDR4After2 = Variable(141, TypeReg_32)
let vDR5After2 = Variable(142, TypeReg_32)
let vDR6After2 = Variable(143, TypeReg_32)
let vDR7After2 = Variable(144, TypeReg_32)
let vMemAfter2 = Mem(     145,Little,TypeReg_32)
let vAlAfter2  = Variable(146, TypeReg_8)
let vAhAfter2  = Variable(147, TypeReg_8)
let vClAfter2  = Variable(148, TypeReg_8)
let vChAfter2  = Variable(149, TypeReg_8)
let vDlAfter2  = Variable(150, TypeReg_8)
let vDhAfter2  = Variable(151, TypeReg_8)
let vBlAfter2  = Variable(152, TypeReg_8)
let vBhAfter2  = Variable(153, TypeReg_8)
let vAxAfter2  = Variable(154, TypeReg_16)
let vCxAfter2  = Variable(155, TypeReg_16)
let vDxAfter2  = Variable(156, TypeReg_16)
let vBxAfter2  = Variable(157, TypeReg_16)
let vSpAfter2  = Variable(158, TypeReg_16)
let vBpAfter2  = Variable(159, TypeReg_16)
let vSiAfter2  = Variable(160, TypeReg_16)
let vDiAfter2  = Variable(161, TypeReg_16)

let l_p_var_varafter2 = 
[
  vEax,vEaxAfter2;vEcx,vEcxAfter2;vEdx,vEdxAfter2;vEbx,vEbxAfter2;
  vEsp,vEspAfter2;vEbp,vEbpAfter2;vEsi,vEsiAfter2;vEdi,vEdiAfter2;
  
  vAx, vAxAfter2; vCx, vCxAfter2; vDx, vDxAfter2; vBx, vBxAfter2;
  vSp, vSpAfter2; vBp, vBpAfter2; vSi, vSiAfter2; vDi, vDiAfter2;

  vAl, vAlAfter2; vCl, vClAfter2; vDl, vDlAfter2; vBl, vBlAfter2;
  vAh, vAhAfter2; vCh, vChAfter2; vDh, vDhAfter2; vBh, vBhAfter2;

  vES, vESAfter2 ;vCS, vCSAfter2 ;vSS, vSSAfter2 ;vDS, vDSAfter2 ;
  vFS, vFSAfter2 ;vGS, vGSAfter2 ;vCF, vCFAfter2 ;vPF, vPFAfter2 ;
  vAF, vAFAfter2 ;vZF, vZFAfter2 ;vSF, vSFAfter2 ;vOF, vOFAfter2 ;
  vDF, vDFAfter2 ;
  
  vCR0,vCR0After2;vCR1,vCR1After2;vCR2,vCR2After2;vCR3,vCR3After2;
  vCR4,vCR4After2;vCR5,vCR5After2;vCR6,vCR6After2;vCR7,vCR7After2;
  
  vDR0,vDR0After2;vDR1,vDR1After2;vDR2,vDR2After2;vDR3,vDR3After2;
  vDR4,vDR4After2;vDR5,vDR5After2;vDR6,vDR6After2;vDR7,vDR7After2;
]

let l_v_general_registers_parentage_after2 =
 [(vEaxAfter2,vAxAfter2,Some(vAhAfter2,vAlAfter2));
  (vEcxAfter2,vCxAfter2,Some(vChAfter2,vClAfter2));
  (vEdxAfter2,vDxAfter2,Some(vDhAfter2,vDlAfter2));
  (vEbxAfter2,vBxAfter2,Some(vBhAfter2,vBlAfter2));
  (vEspAfter2,vSpAfter2,None);
  (vEbpAfter2,vBpAfter2,None);
  (vEsiAfter2,vSiAfter2,None);
  (vEdiAfter2,vDiAfter2,None)]

let mk_after2 = afterize l_p_var_varafter2 l_v_general_registers_parentage_after2

let m_vbefore_to_vafter2 = IRUtil.var_mapify l_p_var_varafter
let m_vafter2_to_vbefore = IRUtil.var_mapify (List.map (fun (x,y) -> (y,x)) l_p_var_varafter2)

let instr_set_r8_lower_in_r32 r8 expr = 
  let vlhs = vr32_of_reg8 r8 in
  let elhs = er32_of_reg8 r8 in
  mk_assign vlhs (mk_or (mk_and elhs (mk_dword 0xFFFFFF00L)) (mk_cast (Unsigned) (TypeReg_32) expr))
  
let instr_set_r8_upper_in_r32 r8 expr = 
  let vlhs = vr32_of_reg8 r8 in
  let elhs = er32_of_reg8 r8 in
  mk_assign vlhs (mk_or (mk_and elhs (mk_dword 0xFFFF00FFL)) (mk_shl (mk_cast (Unsigned) (TypeReg_32) expr) (mk_byte 0x8L)))
  
let instr_setter_of_r8 = function
| Al | Cl | Dl | Bl -> instr_set_r8_lower_in_r32
| Ah | Ch | Dh | Bh -> instr_set_r8_upper_in_r32

let instr_set_r16 r16 expr =
  let vlhs = vr32_of_reg16 r16 in
  let elhs = er32_of_reg16 r16 in
  mk_assign vlhs (mk_or (mk_and elhs (mk_dword 0xFFFF0000L)) (mk_cast (Unsigned) (TypeReg_32) expr))
  
(* Save space by creating "root" elements for the flags expressions *)
let eS  = eSF
let eNS = mk_not eSF
let eO  = eOF
let eNO = mk_not eOF
let eZ  = eZF
let eNZ = mk_not eZF
let eP  = ePF
let eNP = mk_not ePF
let eA  = mk_and (mk_not eCF) (mk_not eZF)
let eAE = mk_not eCF
let eB  = eCF
let eBE = mk_or  eCF eZF
let eL  = mk_xor eSF eOF
let eLE = mk_or  eZF (mk_xor eSF eOF)
let eG  = mk_and (mk_eq eSF eOF) (mk_not eZF)
let eGE = mk_not (mk_xor eSF eOF)
