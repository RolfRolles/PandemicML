(*
* Open issues:  
* * How to incorporate 0x6A?
* * How to do XLAT (0xD7)?
    Predicated(PRED_AddrSize(Direct(Xlat,[oMbBx]),Direct(Xlat,[oMbEbx])));
* * How to represent near and far jumps and calls in the tables?
*)
open X86
open X86InternalOperand
open X86Constraints
open X86TypeCheck

type bytes = int32 list

type x86enc = 
| Literal of bytes
| Native32 of bytes
| Native16 of bytes
| ModRM of bytes
| ModRMGroup of int * bytes

exception InvalidOperands of X86.x86instrpref

type x86opndenc =
| NoEnc
| GPart
| EPart
| Imm

let enctype_of_abstract_operand = function
| OEb
| OEw
| OEd
| OEv
| OEd_q
| OMb
| OMw
| OMd
| OMq
| OMa
| OMp
| OMs
| OMps
| OMpd
| OMd_q
| OMdq
| OM
| OReal4
| OReal8
| OReal10
| OFPEnvLow
| OFPEnv
| OSimdState
| ORv
| ORw
| ORd
| OStN
| ONq
| OWps
| OWss
| OWpd
| OWsd
| OWq
| OWdq
| OUpd
| OUps
| OUq
| OUdq
| OQpi
| OQd
| OQq
| OUdq_Mw
| OUdq_Md
| OUdq_Mq
| ORd_Mb
| ORd_Mw -> EPart
| OGb
| OGw
| OGd
| OGd_q
| OGv
| OGz
| OSw
| OCd
| ODd
| OVps
| OVdq
| OVss
| OVpd
| OVsd
| OVq
| OPpi
| OPq
| OPd -> GPart
| OIb
| OIw
| OIv
| OIz
| OOb
| OOv
| OAp -> Imm
| O1
| OAL
| OrAX
| OCL
| OAX
| ODX
| OES
| OCS
| OSS
| ODS
| OFS
| OGS
| OeAX
| OeCX
| OeDX
| OeBX
| OeSP
| OeBP
| OeSI
| OeDI
| OrAXr8
| OrCXr9
| OrDXr10
| OrBXr11
| OrSPr12
| OrBPr13
| OrSIr14
| OrDIr15
| OALR8L
| OCLR9L
| ODLR10L
| OBLR11L
| OAHR12L
| OCHR13L
| ODHR14L
| OBHR15L
| OSt0
| OXb
| OXw
| OXd
| OYb
| OYw
| OYd
| OXv
| OXz
| OYv
| OYz -> NoEnc
(* FIX THESE *)
| OJz -> NoEnc
| OJb -> NoEnc

let cn   = (OpNone,AddrNone)
let c12  = (Op12SizeEq,AddrNone)
let c123 = (Op123SizeEq,AddrNone)
let c12GvMa = (Op12GvMa,AddrNone)
let c12GvMp = (Op12GvMp,AddrNone)
let c12GzMp = (Op12GzMp,AddrNone)
let cas12 = (OpNone,Addr12SizeEq)
(* Operand list encoding *)
let eGvEvIz    = ([OGv;OEv;OIz],c123)

let eEvGvIb    = ([OEv;OGv;OIb],c12)
let eEvGv      = ([OEv;OGv],c12)
let eGvEv      = ([OGv;OEv],c12)
let erAXIz     = ([OrAX;OIz],c12)
let eEvIz      = ([OEv;OIz],c12)
let eGvEvIb    = ([OGv;OEv;OIb],c12)
let eEvGvCL    = ([OEv;OGv;OCL],c12)
let eeAXEv     = ([OeAX;OEv],c12)
let erAXOv     = ([OrAX;OOv],c12)
let eOvrAX     = ([OOv;OrAX],c12)
let erAXr8Iv   = ([OrAXr8;OIv],c12)
let erCXr9Iv   = ([OrCXr9;OIv],c12)
let erDXr10Iv  = ([OrDXr10;OIv],c12)
let erBXr11Iv  = ([OrBXr11;OIv],c12)
let erSPr12Iv  = ([OrSPr12;OIv],c12)
let erBPr13Iv  = ([OrBPr13;OIv],c12)
let erSIr14Iv  = ([OrSIr14;OIv],c12)
let erDIr15Iv  = ([OrDIr15;OIv],c12)
let erAXrCXr9  = ([OrAX;OrCXr9],c12)
let erAXrDXr10 = ([OrAX;OrDXr10],c12)
let erAXrBXr11 = ([OrAX;OrBXr11],c12)
let erAXrSPr12 = ([OrAX;OrSPr12],c12)
let erAXrBPr13 = ([OrAX;OrBPr13],c12)
let erAXrSIr14 = ([OrAX;OrSIr14],c12)
let erAXrDIr15 = ([OrAX;OrDIr15],c12)
let eGvM       = ([OGv;OM],cn)
let eGvMa      = ([OGv;OMa],c12GvMa)
let eGvMp      = ([OGv;OMp],c12GvMp)
let eGzMp      = ([OGz;OMp],c12GzMp)
let eXwYw      = ([OXw;OYw],cas12)
let eXdYd      = ([OXd;OYd],cas12)

let eGdEv      = ([OGd;OEv],cn)
let eEbGb      = ([OEb;OGb],cn)
let eEdGd      = ([OEd;OGd],cn)
let eGvEb      = ([OGv;OEb],cn)
let eGdEb      = ([OGd;OEb],cn)
let eGbEb      = ([OGb;OEb],cn)
let eGvEw      = ([OGv;OEw],cn)
let eALIb      = ([OAL;OIb],cn)
let eEbIb      = ([OEb;OIb],cn)
let eEvIb      = ([OEv;OIb],cn)
let eStNSt0    = ([OStN;OSt0],cn)
let eSt0StN    = ([OSt0;OStN],cn)
let eEwGw      = ([OEw;OGw],cn)
let eEb1       = ([OEb;O1 ],cn)
let eEv1       = ([OEv;O1 ],cn)
let eEbCL      = ([OEb;OCL],cn)
let eEvCL      = ([OEv;OCL],cn)
let eSt0Real4  = ([OSt0;OReal4],cn)
let eYbDX      = ([OYb;ODX],cn)
let eYwDX      = ([OYw;ODX],cn)
let eYdDX      = ([OYd;ODX],cn)
let eDXXb      = ([ODX;OXb],cn)
let eDXXw      = ([ODX;OXw],cn)
let eDXXd      = ([ODX;OXd],cn)
let eALEb      = ([OAL;OEb],cn)
let eEvSw      = ([OEv;OSw],cn)
let eSwEw      = ([OSw;OEw],cn)
let eALOb      = ([OAL;OOb],cn)
let eObAL      = ([OOb;OAL],cn)
let eALR8LIb   = ([OALR8L;OIb],cn)
let eCLR9LIb   = ([OCLR9L;OIb],cn)
let eDLR10LIb  = ([ODLR10L;OIb],cn)
let eBLR11LIb  = ([OBLR11L;OIb],cn)
let eAHR12LIb  = ([OAHR12L;OIb],cn)
let eCHR13LIb  = ([OCHR13L;OIb],cn)
let eDHR14LIb  = ([ODHR14L;OIb],cn)
let eBHR15LIb  = ([OBHR15L;OIb],cn)
let eSt0Real8  = ([OSt0;OReal8],cn)
let eXbYb      = ([OXb;OYb],cas12)
let eIwIb      = ([OIw;OIb] ,cn)
let eeAXIb     = ([OeAX;OIb],cn)
let eALDX      = ([OAL;ODX] ,cn)
let eeAXDX     = ([OeAX;ODX],cn)
let eIbAL      = ([OIb;OAL] ,cn)
let eIbeAX     = ([OIb;OeAX],cn)
let eDXAL      = ([ODX;OAL] ,cn)
let eDXeAX     = ([ODX;OeAX],cn)
let eRdCd      = ([ORd;OCd],cn)
let eRdDd      = ([ORd;ODd],cn)
let eCdRd      = ([OCd;ORd],cn)
let eDdRd      = ([ODd;ORd],cn)
let eSimdState = ([OSimdState], cn)

let eVpsWps      = ([OVps;OWps],cn)
let eWpsVps      = ([OWps;OVps],cn)
let eVssWss      = ([OVss;OWss],cn)
let eWssVss      = ([OWss;OVss],cn)
let eVpdWpd      = ([OVpd;OWpd],cn)
let eWpdVpd      = ([OWpd;OVpd],cn)
let eVsdWsd      = ([OVsd;OWsd],cn)
let eWsdVsd      = ([OWsd;OVsd],cn)
let eVqMq        = ([OVq;OMq],cn)
let eMqVq        = ([OMq;OVq],cn)
let eVqUq        = ([OVq;OUq],cn)
let eVqWq        = ([OVq;OWq],cn)
let eVpdWq       = ([OVpd;OWq],cn)
let eVpsWq       = ([OVps;OWq],cn)
let eVpsQpi      = ([OVps;OQpi],cn)
let eVssEd_q     = ([OVss;OEd_q],cn)
let eVpdQpi      = ([OVpd;OQpi],cn)
let eVsdEd_q     = ([OVsd;OEd_q],cn)
let eMpsVps      = ([OMps;OVps],cn)
let eMpdVpd      = ([OMpd;OVps],cn)
let ePpiWps      = ([OPpi;OWps],cn)
let eGdWss       = ([OGd;OWss],cn)
let ePpiWpd      = ([OPpi;OWpd],cn)
let eGdWsd       = ([OGd;OWsd],cn)
let eGd_qWsd     = ([OGd_q;OWsd],cn)
let eGd_qWss     = ([OGd_q;OWss],cn)
let eGdUps       = ([OGd;OUps],cn)
let eGdUpd       = ([OGd;OUpd],cn)
let eVpdWps      = ([OVpd;OWps],cn)
let eVpsWpd      = ([OVps;OWpd],cn)
let eVdqWps      = ([OVdq;OWps],cn)
let ePqQd        = ([OPq;OQd],cn)
let eVdqWdq      = ([OVdq;OWdq],cn)
let ePdEd_q      = ([OPd;OEd_q],cn)
let eVdqEd_q     = ([OVdq;OEd_q],cn)
let eEd_qPd      = ([OEd_q;OPd],cn)
let eEd_qVdq     = ([OEd_q;OVdq],cn)
let ePqQq        = ([OPq;OQq],cn)
let eQqPq        = ([OQq;OPq],cn)
let eWdqVdq      = ([OWdq;OVdq],cn)
let ePqQqIb      = ([OPq;OQq;OIb],cn)
let eVdqWdqIb    = ([OVdq;OWdq;OIb],cn)
let eNqIb        = ([ONq;OIb],cn)
let eUdqIb       = ([OUdq;OIb],cn)
let eVpsWpsIb    = ([OVps;OWps;OIb],cn)
let eVssWssIb    = ([OVss;OWss;OIb],cn)
let eVpdWpdIb    = ([OVpd;OWpd;OIb],cn)
let eVsdWsdIb    = ([OVsd;OWsd;OIb],cn)
let eMd_qGd_q    = ([OMd_q;OGd_q],cn)
let ePqEwIb      = ([OPq;OEw;OIb],cn)
let eVdqEwIb     = ([OVdq;OEw;OIb],cn)
let eGdNqIb      = ([OGd;ONq;OIb],cn)
let eGdUdqIb     = ([OGd;OUdq;OIb],cn)
let eVdqNq       = ([OVdq;ONq],cn)
let ePqUq        = ([OPq;OUq],cn)
let eGdNq        = ([OGd;ONq],cn)
let eGdUdq       = ([OGd;OUdq],cn)
let eVpdWdq      = ([OVpd;OWdq],cn)
let eVdqWpd      = ([OVdq;OWpd],cn)
let eMqPq        = ([OMq;OPq],cn)
let eMdqVdq      = ([OMdq;OVdq],cn)
let eVdqMdq      = ([OVdq;OMdq],cn)
let ePqNq        = ([OPq;ONq],cn)
let eVdqUdq      = ([OVdq;OUdq],cn)
let eVdqUdq_Mw   = ([OVdq;OUdq_Mw],cn)
let eVdqUdq_Md   = ([OVdq;OUdq_Md],cn)
let eVdqUdq_Mq   = ([OVdq;OUdq_Mq],cn)
let eRd_MbVdqIb  = ([ORd_Mb;OVdq;OIb],cn)
let eRd_MwVdqIb  = ([ORd_Mw;OVdq;OIb],cn)
let eEdVdqIb     = ([OEd;OVdq;OIb],cn)
let eVdqEdIb     = ([OVdq;OEd;OIb],cn)
let eVdqUdq_MdIb = ([OVdq;OUdq_Md;OIb],cn)

(* Single operands (put these last, as they overwrite bindings) *)
let eeAX      = ([OeAX],cn)
let eeCX      = ([OeCX],cn)
let eeDX      = ([OeDX],cn)
let eeBX      = ([OeBX],cn)
let eeSP      = ([OeSP],cn)
let eeBP      = ([OeBP],cn)
let eeSI      = ([OeSI],cn)
let eeDI      = ([OeDI],cn)
let eEb       = ([OEb],cn)
let eEw       = ([OEw],cn)
let eEd       = ([OEd],cn)
let eEv       = ([OEv],cn)
let eRv       = ([ORv],cn)
let erAXr8    = ([OrAXr8] ,cn)
let erCXr9    = ([OrCXr9] ,cn)
let erDXr10   = ([OrDXr10],cn)
let erBXr11   = ([OrBXr11],cn)
let erSPr12   = ([OrSPr12],cn)
let erBPr13   = ([OrBPr13],cn)
let erSIr14   = ([OrSIr14],cn)
let erDIr15   = ([OrDIr15],cn)
let eES       = ([OES],cn)
let eCS       = ([OCS],cn)
let eSS       = ([OSS],cn)
let eDS       = ([ODS],cn)
let eFS       = ([OFS],cn)
let eGS       = ([OGS],cn)
let eIz       = ([OIz],cn)
let eReal4    = ([OReal4],cn)
let eReal8    = ([OReal8],cn)
let eReal10   = ([OReal10],cn)
let eYb       = ([OYb],cn)
let eXb       = ([OXb],cn)
let eYv       = ([OYv],cn)
let eYz       = ([OYz],cn)
let eXv       = ([OXv],cn)
let eXz       = ([OXz],cn)
let eStN      = ([OStN],cn)
let eFPEnv    = ([OFPEnv],cn)
let eFPEnvLow = ([OFPEnvLow],cn)
let eIw       = ([OIw],cn)
let eIb       = ([OIb],cn)
let eMb       = ([OMb],cn)
let eMw       = ([OMw],cn)
let eMd       = ([OMd],cn)
let eMs       = ([OMs],cn)
let eMq       = ([OMq],cn)
let eAX       = ([OAX],cn)
let eYw       = ([OYw],cn)
let eXw       = ([OXw],cn)
let eYd       = ([OYd],cn)
let eXd       = ([OXd],cn)
let eRw       = ([ORw],cn)

let eNone     = ([],cn)

let mnem_to_encodings = function
| Daa    -> [eNone, Literal([0x27l])]
| Das    -> [eNone, Literal([0x2Fl])]
| Aaa    -> [eNone, Literal([0x37l])]
| Aas    -> [eNone, Literal([0x3Fl])]
| Add    -> [
  eEbGb,        ModRM([0x00l]);
  eEvGv,        ModRM([0x01l]);
  eGbEb,        ModRM([0x02l]); 
  eGvEv,        ModRM([0x03l]); 
  eALIb,      Literal([0x04l]); 
  erAXIz,     Literal([0x05l]); 
  eEbIb, ModRMGroup(0,[0x80l]); 
  eEvIz, ModRMGroup(0,[0x81l]); 
  eEvIb, ModRMGroup(0,[0x83l]); 
  ]
| Or     -> [
  eEbGb,        ModRM([0x08l]);
  eEvGv,        ModRM([0x09l]);
  eGbEb,        ModRM([0x0Al]); 
  eGvEv,        ModRM([0x0Bl]); 
  eALIb,      Literal([0x0Cl]); 
  erAXIz,     Literal([0x0Dl]); 
  eEbIb, ModRMGroup(1,[0x80l]); 
  eEvIz, ModRMGroup(1,[0x81l]); 
  eEvIb, ModRMGroup(1,[0x83l]); 
  ]
| Adc    -> [
  eEbGb,        ModRM([0x10l]);
  eEvGv,        ModRM([0x11l]);
  eGbEb,        ModRM([0x12l]); 
  eGvEv,        ModRM([0x13l]); 
  eALIb,      Literal([0x14l]); 
  erAXIz,     Literal([0x15l]); 
  eEbIb, ModRMGroup(2,[0x80l]); 
  eEvIz, ModRMGroup(2,[0x81l]); 
  eEvIb, ModRMGroup(2,[0x83l]); 
  ]
| Sbb    -> [
  eEbGb,        ModRM([0x18l]);
  eEvGv,        ModRM([0x19l]);
  eGbEb,        ModRM([0x1Al]); 
  eGvEv,        ModRM([0x1Bl]); 
  eALIb,      Literal([0x1Cl]); 
  erAXIz,     Literal([0x1Dl]); 
  eEbIb, ModRMGroup(3,[0x80l]); 
  eEvIz, ModRMGroup(3,[0x81l]); 
  eEvIb, ModRMGroup(3,[0x83l]); 
  ]
| And    -> [
  eEbGb,        ModRM([0x20l]);
  eEvGv,        ModRM([0x21l]);
  eGbEb,        ModRM([0x22l]); 
  eGvEv,        ModRM([0x23l]); 
  eALIb,      Literal([0x24l]); 
  erAXIz,     Literal([0x25l]); 
  eEbIb, ModRMGroup(4,[0x80l]); 
  eEvIz, ModRMGroup(4,[0x81l]); 
  eEvIb, ModRMGroup(4,[0x83l]); 
  ]
| Sub    -> [
  eEbGb,        ModRM([0x28l]);
  eEvGv,        ModRM([0x29l]);
  eGbEb,        ModRM([0x2Al]); 
  eGvEv,        ModRM([0x2Bl]); 
  eALIb,      Literal([0x2Cl]); 
  erAXIz,     Literal([0x2Dl]); 
  eEbIb, ModRMGroup(5,[0x80l]); 
  eEvIz, ModRMGroup(5,[0x81l]); 
  eEvIb, ModRMGroup(5,[0x83l]); 
  ]
| Xor    -> [
  eEbGb,        ModRM([0x30l]);
  eEvGv,        ModRM([0x31l]);
  eGbEb,        ModRM([0x32l]); 
  eGvEv,        ModRM([0x33l]); 
  eALIb,      Literal([0x34l]); 
  erAXIz,     Literal([0x35l]); 
  eEbIb, ModRMGroup(6,[0x80l]); 
  eEvIz, ModRMGroup(6,[0x81l]); 
  eEvIb, ModRMGroup(6,[0x83l]); 
  ]
| Cmp    -> [
  eEbGb,        ModRM([0x38l]);
  eEvGv,        ModRM([0x39l]);
  eGbEb,        ModRM([0x3Al]); 
  eGvEv,        ModRM([0x3Bl]); 
  eALIb,      Literal([0x3Cl]); 
  erAXIz,     Literal([0x3Dl]); 
  eEbIb, ModRMGroup(7,[0x80l]); 
  eEvIz, ModRMGroup(7,[0x81l]); 
  eEvIb, ModRMGroup(7,[0x83l]); 
  ]
| Inc    -> [
  eeAX, Literal([0x40l]);
  eeCX, Literal([0x41l]);
  eeDX, Literal([0x42l]);
  eeBX, Literal([0x43l]);
  eeSP, Literal([0x44l]);
  eeBP, Literal([0x45l]);
  eeSI, Literal([0x46l]);
  eeDI, Literal([0x47l]);
  eEb,  ModRMGroup(0,[0xFEl]);
  eEv,  ModRMGroup(0,[0xFFl]);
  ]
| Dec    -> [
  eeAX, Literal([0x48l]);
  eeCX, Literal([0x49l]);
  eeDX, Literal([0x4Al]);
  eeBX, Literal([0x4Bl]);
  eeSP, Literal([0x4Cl]);
  eeBP, Literal([0x4Dl]);
  eeSI, Literal([0x4El]);
  eeDI, Literal([0x4Fl]);
  eEb,  ModRMGroup(1,[0xFEl]);
  eEv,  ModRMGroup(1,[0xFFl]);
  ]
| Push   -> [
  eES,       Literal([0x06l]);
  eCS,       Literal([0x0El]);
  eSS,       Literal([0x16l]);
  eDS,       Literal([0x1El]);
  erAXr8,    Literal([0x50l]);
  erCXr9,    Literal([0x51l]);
  erDXr10,   Literal([0x52l]);
  erBXr11,   Literal([0x53l]);
  erSPr12,   Literal([0x54l]);
  erBPr13,   Literal([0x55l]);
  erSIr14,   Literal([0x56l]);
  erDIr15,   Literal([0x57l]);
  eIz,       Literal([0x68l]);
  eEv,  ModRMGroup(6,[0xFFl]);
  eFS,       Literal([0x0Fl;0xA0l]);
  eGS,       Literal([0x0Fl;0xA8l]);
  ]
| Pop    -> [
  eES,       Literal([0x07l]);
  eSS,       Literal([0x17l]);
  eDS,       Literal([0x1Fl]);
  erAXr8,    Literal([0x58l]);
  erCXr9,    Literal([0x59l]);
  erDXr10,   Literal([0x5Al]);
  erBXr11,   Literal([0x5Bl]);
  erSPr12,   Literal([0x5Cl]);
  erBPr13,   Literal([0x5Dl]);
  erSIr14,   Literal([0x5El]);
  erDIr15,   Literal([0x5Fl]);
  eEv,  ModRMGroup(0,[0x8Fl]);
  eFS,       Literal([0x0Fl;0xA1l]);
  eGS,       Literal([0x0Fl;0xA9l]);
  ]
| Pushaw -> [eNone, Native16([0x60l])]
| Pushad -> [eNone, Native32([0x60l])]
| Popaw  -> [eNone, Native16([0x61l])]
| Popad  -> [eNone, Native32([0x61l])]
| Bound  -> [eGvMa, ModRM([0x62l])]
| Arpl   -> [eEwGw, ModRM([0x63l])]
| Imul   -> [
  eGvEvIz, ModRM([0x69l]);
  eGvEvIb, ModRM([0x6Bl]);
  eEb,     ModRMGroup(5,[0xF6l]);
  eEv,     ModRMGroup(5,[0xF7l]);
  eGvEv,   ModRM([0x0Fl;0xAFl]);
  ]
| Insb   -> [eYbDX,  Literal([0x6Cl])]
| Insw   -> [eYwDX, Native16([0x6Dl])]
| Insd   -> [eYdDX, Native32([0x6Dl])]
| Outsb  -> [eDXXb,  Literal([0x6El])]
| Outsw  -> [eDXXw, Native16([0x6Fl])]
| Outsd  -> [eDXXd, Native32([0x6Fl])]
| Test   -> [
  eEbGb,  ModRM([0x84l]);
  eEvGv,  ModRM([0x85l]);
  eALIb,  Literal([0xA8l]);
  erAXIz, Literal([0xA9l]);
  eEbIb,  ModRMGroup(0,[0xF6l]);
  eEvIz,  ModRMGroup(0,[0xF7l]);
  ]
| Xchg   -> [
  erAXrCXr9,  Literal([0x91l]);
  erAXrDXr10, Literal([0x92l]);
  erAXrBXr11, Literal([0x93l]);
  erAXrSPr12, Literal([0x94l]);
  erAXrBPr13, Literal([0x95l]);
  erAXrSIr14, Literal([0x96l]);
  erAXrDIr15, Literal([0x97l]);
  eEbGb, ModRM([0x86l]);
  eGbEb, ModRM([0x86l]);
  eEvGv, ModRM([0x87l]);
  eGvEv, ModRM([0x87l]);
  ]
| Mov    -> [
  eEbGb,     ModRM([0x88l]);
  eEvGv,     ModRM([0x89l]);
  eGbEb,     ModRM([0x8Al]);
  eGvEv,     ModRM([0x8Bl]);
  eEvSw,     ModRM([0x8Cl]);
  eSwEw,     ModRM([0x8El]);
  eALOb,     Literal([0xA0l]);
  erAXOv,    Literal([0xA1l]);
  eObAL,     Literal([0xA2l]);
  eOvrAX,    Literal([0xA3l]);      
  eALR8LIb,  Literal([0xB0l]);
  eCLR9LIb,  Literal([0xB1l]);
  eDLR10LIb, Literal([0xB2l]);
  eBLR11LIb, Literal([0xB3l]);
  eAHR12LIb, Literal([0xB4l]);
  eCHR13LIb, Literal([0xB5l]);
  eDHR14LIb, Literal([0xB6l]);
  eBHR15LIb, Literal([0xB7l]);
  erAXr8Iv,  Literal([0xB8l]);
  erCXr9Iv,  Literal([0xB9l]);
  erDXr10Iv, Literal([0xBAl]);
  erBXr11Iv, Literal([0xBBl]);
  erSPr12Iv, Literal([0xBCl]);
  erBPr13Iv, Literal([0xBDl]);
  erSIr14Iv, Literal([0xBEl]);
  erDIr15Iv, Literal([0xBFl]);
  eEbIb,     ModRMGroup(0,[0xC6l]);
  eEvIz,     ModRMGroup(0,[0xC7l]);
  eRdCd,     ModRM([0x0Fl;0x20l]);
  eRdDd,     ModRM([0x0Fl;0x21l]);
  eCdRd,     ModRM([0x0Fl;0x22l]);
  eDdRd,     ModRM([0x0Fl;0x23l]);
  ]
| Lea    -> [eGvM,ModRM([0x8Dl])]
| Nop    -> [
  eNone, Literal([0x90l]);
  eEv,   ModRMGroup(0,[0x0Fl;0x0Dl]);
  eEv,   ModRMGroup(0,[0x0Fl;0x1Fl]);
  ]
| Pause  -> [eNone,Literal([0xF3l;0x90l])]
| Cbw    -> [eNone, Native16([0x98l])]
| Cwde   -> [eNone, Native32([0x98l])]
| Cwd    -> [eNone, Native16([0x99l])]
| Cdq    -> [eNone, Native32([0x99l])]
| Wait   -> [eNone,  Literal([0x9Bl])]
| Pushfw -> [eNone, Native16([0x9Cl])]
| Pushfd -> [eNone, Native32([0x9Cl])]
| Popfw  -> [eNone, Native16([0x9Dl])]
| Popfd  -> [eNone, Native32([0x9Dl])]
| Sahf   -> [eNone,  Literal([0x9El])]
| Lahf   -> [eNone,  Literal([0x9Fl])]
| Movsb  -> [eXbYb,  Literal([0xA4l])]
| Movsw  -> [eXwYw, Native16([0xA5l])]
| Movsd  -> [
  eXdYd, Native32([0xA5l]);
  eVsdWsd, ModRM([0xF2l;0x0Fl;0x10l]);
  eWsdVsd, ModRM([0xF2l;0x0Fl;0x11l]);  
  ]
| Cmpsb  -> [eXbYb,  Literal([0xA6l])]
| Cmpsw  -> [eXwYw, Native16([0xA7l])]
| Cmpsd  -> [
  eXdYd, Native32([0xA7l]);
  eVsdWsdIb, ModRM([0xF2l;0x0Fl;0xC2l]);
  ]
| Stosb  -> [eYb,    Literal([0xAAl])]
| Stosw  -> [eYw,   Native16([0xABl])]
| Stosd  -> [eYd,   Native32([0xABl])]
| Lodsb  -> [eXb,    Literal([0xACl])]
| Lodsw  -> [eXw,   Native16([0xADl])]
| Lodsd  -> [eXd,   Native32([0xADl])]
| Scasb  -> [eYb,    Literal([0xAEl])]
| Scasw  -> [eYw,   Native16([0xAFl])]
| Scasd  -> [eYd,   Native32([0xAFl])]
| Rol    -> [
  eEbIb, ModRMGroup(0,[0xC0l]);
  eEvIb, ModRMGroup(0,[0xC1l]);
  eEb1,  ModRMGroup(0,[0xD0l]);
  eEv1,  ModRMGroup(0,[0xD1l]);
  eEbCL, ModRMGroup(0,[0xD2l]);
  eEvCL, ModRMGroup(0,[0xD3l]);
  ]
| Ror    -> [
  eEbIb, ModRMGroup(1,[0xC0l]);
  eEvIb, ModRMGroup(1,[0xC1l]);
  eEb1,  ModRMGroup(1,[0xD0l]);
  eEv1,  ModRMGroup(1,[0xD1l]);
  eEbCL, ModRMGroup(1,[0xD2l]);
  eEvCL, ModRMGroup(1,[0xD3l]);
  ]
| Rcl    -> [
  eEbIb, ModRMGroup(2,[0xC0l]);
  eEvIb, ModRMGroup(2,[0xC1l]);
  eEb1,  ModRMGroup(2,[0xD0l]);
  eEv1,  ModRMGroup(2,[0xD1l]);
  eEbCL, ModRMGroup(2,[0xD2l]);
  eEvCL, ModRMGroup(2,[0xD3l]);
  ]
| Rcr    -> [
  eEbIb, ModRMGroup(3,[0xC0l]);
  eEvIb, ModRMGroup(3,[0xC1l]);
  eEb1,  ModRMGroup(3,[0xD0l]);
  eEv1,  ModRMGroup(3,[0xD1l]);
  eEbCL, ModRMGroup(3,[0xD2l]);
  eEvCL, ModRMGroup(3,[0xD3l]);
  ]
| Shl    -> [
  eEbIb, ModRMGroup(4,[0xC0l]);
  eEvIb, ModRMGroup(4,[0xC1l]);
  eEb1,  ModRMGroup(4,[0xD0l]);
  eEv1,  ModRMGroup(4,[0xD1l]);
  eEbCL, ModRMGroup(4,[0xD2l]);
  eEvCL, ModRMGroup(4,[0xD3l]);
  ]
| Shr    -> [
  eEbIb, ModRMGroup(5,[0xC0l]);
  eEvIb, ModRMGroup(5,[0xC1l]);
  eEb1,  ModRMGroup(5,[0xD0l]);
  eEv1,  ModRMGroup(5,[0xD1l]);
  eEbCL, ModRMGroup(5,[0xD2l]);
  eEvCL, ModRMGroup(5,[0xD3l]);
  ]
| Sal    -> [
  eEbIb, ModRMGroup(6,[0xC0l]);
  eEvIb, ModRMGroup(6,[0xC1l]);
  eEb1,  ModRMGroup(6,[0xD0l]);
  eEv1,  ModRMGroup(6,[0xD1l]);
  eEbCL, ModRMGroup(6,[0xD2l]);
  eEvCL, ModRMGroup(6,[0xD3l]);
  ]
| Sar    -> [
  eEbIb, ModRMGroup(7,[0xC0l]);
  eEvIb, ModRMGroup(7,[0xC1l]);
  eEb1,  ModRMGroup(7,[0xD0l]);
  eEv1,  ModRMGroup(7,[0xD1l]);
  eEbCL, ModRMGroup(7,[0xD2l]);
  eEvCL, ModRMGroup(7,[0xD3l]);
  ]
| Ret    -> [
  eIw,   Literal([0xC2l]);
  eNone, Literal([0xC3l]);
  ]
| Les    -> [eGzMp, ModRM([0xC4l])]
| Lds    -> [eGzMp, ModRM([0xC5l])]
| Enter  -> [eIwIb, Literal([0xC8l])]
| Leave  -> [eNone, Literal([0xC9l])]
| Retf   -> [
  eIw,   Literal([0xCAl]);
  eNone, Literal([0xCBl]);
  ]
| Int3   -> [eNone,  Literal([0xCCl])]
| Int    -> [eIb,    Literal([0xCDl])]
| Into   -> [eNone,  Literal([0xCEl])]
| Iretw  -> [eNone, Native16([0xCFl])]
| Iretd  -> [eNone, Native32([0xCFl])]
| Aam    -> [eIb,    Literal([0xD4l])]
| Aad    -> [eIb,    Literal([0xD5l])]
| Salc   -> [eNone,  Literal([0xD6l])]
| In     -> [
  eALIb,  Literal([0xE4l]);
  eeAXIb, Literal([0xE5l]);
  eALDX,  Literal([0xECl]);
  eeAXDX, Literal([0xEDl]);
  ]
| Out    -> [
  eIbAL,  Literal([0xE6l]);
  eIbeAX, Literal([0xE7l]);
  eDXAL,  Literal([0xEEl]);
  eDXeAX, Literal([0xEFl]);
  ]
| Icebp  -> [eNone, Literal([0xF1l])]
| Hlt    -> [eNone, Literal([0xF4l])]
| Cmc    -> [eNone, Literal([0xF5l])]
| Clc    -> [eNone, Literal([0xF8l])]
| Stc    -> [eNone, Literal([0xF9l])]
| Cli    -> [eNone, Literal([0xFAl])]
| Sti    -> [eNone, Literal([0xFBl])]
| Cld    -> [eNone, Literal([0xFCl])]
| Std    -> [eNone, Literal([0xFDl])]
| Not    -> [
  eEb, ModRMGroup(2,[0xF6l]);
  eEv, ModRMGroup(2,[0xF7l]);
  ]
| Neg    -> [
  eEb, ModRMGroup(3,[0xF6l]);
  eEv, ModRMGroup(3,[0xF7l]);
  ]
| Mul    -> [
  eEb, ModRMGroup(4,[0xF6l]);
  eEv, ModRMGroup(4,[0xF7l]);
  ]
| Div    -> [
  eEb, ModRMGroup(6,[0xF6l]);
  eEv, ModRMGroup(6,[0xF7l]);
  ]
| Idiv   -> [
  eEb, ModRMGroup(7,[0xF6l]);
  eEv, ModRMGroup(7,[0xF7l]);
  ]
| Call   -> [
  eEv, ModRMGroup(2,[0xFFl]);
  ]
| CallF  -> [
  eEv, ModRMGroup(3,[0xFFl]);
  ]
| Jmp    -> [
  eEv, ModRMGroup(4,[0xFFl]);
  ]
| JmpF   -> [
  eEv, ModRMGroup(5,[0xFFl]);
  ]

| Fadd    -> [
  eSt0Real4, ModRMGroup(0,[0xD8l]);
  eSt0StN,   ModRMGroup(0,[0xD8l]);
  eSt0Real8, ModRMGroup(0,[0xDCl]);
  eStNSt0,   ModRMGroup(0,[0xDCl]);
  ]
| Fmul    -> [
  eSt0Real4, ModRMGroup(1,[0xD8l]);
  eSt0StN,   ModRMGroup(1,[0xD8l]);
  eSt0Real8, ModRMGroup(1,[0xDCl]);
  eStNSt0,   ModRMGroup(1,[0xDCl]);
  ]
| Fcom    -> [
  eSt0Real4, ModRMGroup(2,[0xD8l]);
  eSt0StN,   ModRMGroup(2,[0xD8l]);
  eSt0Real8, ModRMGroup(2,[0xDCl]);
  ]
| Fcomp   -> [
  eSt0Real4, ModRMGroup(3,[0xD8l]);
  eSt0StN,   ModRMGroup(3,[0xD8l]);
  eSt0Real8, ModRMGroup(3,[0xDCl]);
  ]
| Fsub    -> [
  eSt0Real4, ModRMGroup(4,[0xD8l]);
  eSt0StN,   ModRMGroup(4,[0xD8l]);
  eSt0Real8, ModRMGroup(4,[0xDCl]);
  eStNSt0,   ModRMGroup(4,[0xDCl]);
  ]
| Fsubr   -> [
  eSt0Real4, ModRMGroup(5,[0xD8l]);
  eSt0StN,   ModRMGroup(5,[0xD8l]);
  eSt0Real8, ModRMGroup(5,[0xDCl]);
  eStNSt0,   ModRMGroup(5,[0xDCl]);
  ]
| Fdiv    -> [
  eSt0Real4, ModRMGroup(6,[0xD8l]);
  eSt0StN,   ModRMGroup(6,[0xD8l]);
  eSt0Real8, ModRMGroup(6,[0xDCl]);
  eStNSt0,   ModRMGroup(6,[0xDCl]);
  ]
| Fdivr   -> [
  eSt0Real4, ModRMGroup(7,[0xD8l]);
  eSt0StN,   ModRMGroup(7,[0xD8l]);
  eSt0Real8, ModRMGroup(7,[0xDCl]);
  eStNSt0,   ModRMGroup(7,[0xDCl]);
  ]
| Fld     -> [
  eReal4,  ModRMGroup(0,[0xD9l]);
  eSt0StN, ModRMGroup(0,[0xD9l]);
  eReal10, ModRMGroup(5,[0xDBl]);
  eReal8,  ModRMGroup(0,[0xDDl]);
  ]
| Fxch     -> [eSt0StN,ModRMGroup(1,[0xD9l])]
| Fnop     -> [eNone, Literal([0xD9l;0xD0l])]
| Fchs     -> [eNone, Literal([0xD9l;0xE0l])]
| Fabs     -> [eNone, Literal([0xD9l;0xE1l])]
| Ftst     -> [eNone, Literal([0xD9l;0xE4l])]
| Fxam     -> [eNone, Literal([0xD9l;0xE5l])]
| Fld1     -> [eNone, Literal([0xD9l;0xE8l])]
| Fldl2t   -> [eNone, Literal([0xD9l;0xE9l])]
| Fldl2e   -> [eNone, Literal([0xD9l;0xEAl])]
| Fldpi    -> [eNone, Literal([0xD9l;0xEBl])]
| Fldlg2   -> [eNone, Literal([0xD9l;0xECl])]
| Fldln2   -> [eNone, Literal([0xD9l;0xEDl])]
| Fldz     -> [eNone, Literal([0xD9l;0xEEl])]
| F2xm1    -> [eNone, Literal([0xD9l;0xF0l])]
| Fyl2x    -> [eNone, Literal([0xD9l;0xF1l])]
| Fptan    -> [eNone, Literal([0xD9l;0xF2l])]
| Fpatan   -> [eNone, Literal([0xD9l;0xF3l])]
| Fxtract  -> [eNone, Literal([0xD9l;0xF4l])]
| Fprem1   -> [eNone, Literal([0xD9l;0xF5l])]
| Fdecstp  -> [eNone, Literal([0xD9l;0xF6l])]
| Fincstp  -> [eNone, Literal([0xD9l;0xF7l])]
| Fprem    -> [eNone, Literal([0xD9l;0xF8l])]
| Fyl2xp1  -> [eNone, Literal([0xD9l;0xF9l])]
| Fsqrt    -> [eNone, Literal([0xD9l;0xFAl])]
| Fsincos  -> [eNone, Literal([0xD9l;0xFBl])]
| Frndint  -> [eNone, Literal([0xD9l;0xFCl])]
| Fscale   -> [eNone, Literal([0xD9l;0xFDl])]
| Fsin     -> [eNone, Literal([0xD9l;0xFEl])]
| Fcos     -> [eNone, Literal([0xD9l;0xFFl])]
| Fst      -> [
  eReal4, ModRMGroup(2,[0xD9l]);
  eReal8, ModRMGroup(2,[0xDDl]);
  eStN,   ModRMGroup(2,[0xDDl]);
  ]
| Fstp     -> [
  eReal4,  ModRMGroup(3,[0xD9l]);
  eReal10, ModRMGroup(7,[0xDBl]);
  eReal8,  ModRMGroup(3,[0xDDl]);
  eStN,    ModRMGroup(3,[0xDDl]);
  ]
| Fldenv   -> [eFPEnvLow, ModRMGroup(4,[0xD9l])]
| Fldcw    -> [eMw,       ModRMGroup(5,[0xD9l])]
| Fstenv   -> [eFPEnvLow, ModRMGroup(6,[0xD9l])]
| Fstcw    -> [eMw,       ModRMGroup(7,[0xD9l])]
| Fiadd    -> [
  eMd, ModRMGroup(0,[0xDAl]);
  eMw, ModRMGroup(0,[0xDEl]);
  ]
| Fimul    -> [
  eMd, ModRMGroup(1,[0xDAl]);
  eMw, ModRMGroup(1,[0xDEl]);
  ]
| Ficom    -> [
  eMd, ModRMGroup(2,[0xDAl]);
  eMw, ModRMGroup(2,[0xDEl]);
  ]
| Ficomp   -> [
  eMd, ModRMGroup(3,[0xDAl]);
  eMw, ModRMGroup(3,[0xDEl]);
  ]
| Fisub    -> [
  eMd, ModRMGroup(4,[0xDAl]);
  eMw, ModRMGroup(4,[0xDEl]);
  ]
| Fisubr   -> [
  eMd, ModRMGroup(5,[0xDAl]);
  eMw, ModRMGroup(5,[0xDEl]);
  ]
| Fidiv    -> [
  eMd, ModRMGroup(6,[0xDAl]);
  eMw, ModRMGroup(6,[0xDEl]);
  ]
| Fidivr   -> [
  eMd, ModRMGroup(7,[0xDAl]);
  eMw, ModRMGroup(7,[0xDEl]);
  ]
| Fcmovb   -> [eSt0StN, ModRMGroup(0,[0xDAl])]
| Fcmove   -> [eSt0StN, ModRMGroup(1,[0xDAl])]
| Fcmovbe  -> [eSt0StN, ModRMGroup(2,[0xDAl])]
| Fcmovu   -> [eSt0StN, ModRMGroup(3,[0xDAl])]
| Fucompp  -> [eNone,  Literal([0xDAl;0xE9l])]
| Fcmovnb  -> [eSt0StN, ModRMGroup(0,[0xDBl])]
| Fcmovne  -> [eSt0StN, ModRMGroup(1,[0xDBl])]
| Fcmovnbe -> [eSt0StN, ModRMGroup(2,[0xDBl])]
| Fcmovnu  -> [eSt0StN, ModRMGroup(3,[0xDBl])]
| Fclex    -> [eNone,  Literal([0xDBl;0xE2l])]
| Finit    -> [eNone,  Literal([0xDBl;0xE3l])]
| Fucomi   -> [eSt0StN, ModRMGroup(5,[0xDBl])]
| Fcomi    -> [eSt0StN, ModRMGroup(6,[0xDBl])]
| Fild     -> [
  eMd,    ModRMGroup(0,[0xDBl]);
  eMw,    ModRMGroup(0,[0xDFl]);
  eReal8, ModRMGroup(5,[0xDFl]);
  ]
| Fisttp   -> [
  eMd, ModRMGroup(1,[0xDBl]);
  eMq, ModRMGroup(1,[0xDDl]);
  eMw, ModRMGroup(1,[0xDFl]);
  ]
| Fist     -> [
  eMd, ModRMGroup(2,[0xDBl]);
  eMw, ModRMGroup(2,[0xDFl]);
  ]
| Fistp    -> [
  eMd,    ModRMGroup(3,[0xDBl]);
  eMw,    ModRMGroup(3,[0xDFl]);
  eReal8, ModRMGroup(7,[0xDFl]);
  ]
| Frstor   -> [eFPEnv, ModRMGroup(4,[0xDDl])]
| Fsave    -> [eFPEnv, ModRMGroup(6,[0xDDl])]
| Fstsw    -> [
  eMw, ModRMGroup(7,[0xDDl]);
  eAX, Literal([0xDFl;0xE0l]);
  ]
| Ffree    -> [eStN,    ModRMGroup(0,[0xDDl])]
| Fucom    -> [eStN,    ModRMGroup(4,[0xDDl])]
| Fucomp   -> [eStN,    ModRMGroup(5,[0xDDl])]
| Faddp    -> [eStNSt0, ModRMGroup(0,[0xDEl])]
| Fmulp    -> [eStNSt0, ModRMGroup(1,[0xDEl])]
| Fcompp   -> [eNone,  Literal([0xDEl;0xD9l])]
| Fsubrp   -> [eStNSt0, ModRMGroup(4,[0xDEl])]
| Fsubp    -> [eStNSt0, ModRMGroup(5,[0xDEl])]
| Fdivrp   -> [eStNSt0, ModRMGroup(6,[0xDEl])]
| Fdivp    -> [eStNSt0, ModRMGroup(7,[0xDEl])]
| Fbld     -> [eReal10, ModRMGroup(4,[0xDFl])]
| Fbstp    -> [eReal10, ModRMGroup(6,[0xDFl])]
| Fucomip  -> [eSt0StN, ModRMGroup(5,[0xDFl])]
| Fcomip   -> [eSt0StN, ModRMGroup(6,[0xDFl])]
| Sldt     -> [
  eMw, ModRMGroup(0,[0x0Fl;0x00l]);
  eRv, ModRMGroup(0,[0x0Fl;0x00l]);
  ]
| Str      -> [
  eMw, ModRMGroup(1,[0x0Fl;0x00l]);
  eRv, ModRMGroup(1,[0x0Fl;0x00l]);
  ]
| Lldt     -> [eEw, ModRMGroup(2,[0x0Fl;0x00l])]
| Ltr      -> [eEw, ModRMGroup(3,[0x0Fl;0x00l])]
| Verr     -> [eEw, ModRMGroup(4,[0x0Fl;0x00l])]
| Verw     -> [eEw, ModRMGroup(5,[0x0Fl;0x00l])]
| Sgdt     -> [eMs, ModRMGroup(0,[0x0Fl;0x01l])]
| Sidt     -> [eMs, ModRMGroup(1,[0x0Fl;0x01l])]
| Lgdt     -> [eMs, ModRMGroup(2,[0x0Fl;0x01l])]
| Lidt     -> [eMs, ModRMGroup(3,[0x0Fl;0x01l])]
| Smsw     -> [
  eMw, ModRMGroup(4,[0x0Fl;0x01l]);
  eRv, ModRMGroup(4,[0x0Fl;0x01l]);
  ]
| Lmsw     -> [
  eMw, ModRMGroup(6,[0x0Fl;0x01l]);
  eRw, ModRMGroup(6,[0x0Fl;0x01l])
  ]
| Invlpg   -> [eMb, ModRMGroup(7,[0x0Fl;0x01l])]
| Vmcall   -> [eNone, Literal([0x0Fl;0x01l;0xC1l])]
| Vmlaunch -> [eNone, Literal([0x0Fl;0x01l;0xC2l])]
| Vmresume -> [eNone, Literal([0x0Fl;0x01l;0xC3l])]
| Vmxoff   -> [eNone, Literal([0x0Fl;0x01l;0xC4l])]
| Monitor  -> [eNone, Literal([0x0Fl;0x01l;0xC8l])]
| Mwait    -> [eNone, Literal([0x0Fl;0x01l;0xC9l])]
| Lar      -> [eGvEw, ModRM([0x0Fl;0x02l])]
| Lsl      -> [eGvEw, ModRM([0x0Fl;0x03l])]
| Syscall  -> [eNone, Literal([0x0Fl;0x05l])]
| Clts     -> [eNone, Literal([0x0Fl;0x06l])]
| Sysret   -> [eNone, Literal([0x0Fl;0x07l])]
| Invd     -> [eNone, Literal([0x0Fl;0x08l])]
| Wbinvd   -> [eNone, Literal([0x0Fl;0x09l])]
| Ud2      -> [eNone, Literal([0x0Fl;0x0Bl])]
| Movups -> [
  eVpsWps, ModRM([0x0Fl;0x10l]);
  eWpsVps, ModRM([0x0Fl;0x11l]);
  ]
| Movss -> [
  eVssWss, ModRM([0xF3l;0x0Fl;0x10l]);
  eWssVss, ModRM([0xF3l;0x0Fl;0x11l]);
  ]
| Movupd -> [
  eVpdWpd, ModRM([0x66l;0x0Fl;0x10l]);
  eWpdVpd, ModRM([0x66l;0x0Fl;0x11l]);
  ]
| Movlps -> [
  eVqMq, ModRM([0x0Fl;0x12l]);
  eMqVq, ModRM([0x0Fl;0x13l]);
  ]
| Movhlps -> [eVqUq, ModRM([0x0Fl;0x12l])]
| Movsldup -> [eVqWq, ModRM([0xF3l;0x0Fl;0x12l])]
| Movlpd -> [
  eVqMq, ModRM([0x66l;0x0Fl;0x12l]);
  eMqVq, ModRM([0x66l;0x0Fl;0x13l]);
  ]
| Movddup  -> [eVqWq, ModRM([0xF2l;0x0Fl;0x12l])]
| Unpcklpd -> [eVpdWq, ModRM([0x0Fl;0x14l])]
| Unpcklps -> [eVpsWq, ModRM([0x66l;0x0Fl;0x14l])]
| Unpckhpd -> [eVpdWq, ModRM([0x0Fl;0x15l])]
| Unpckhps -> [eVpsWq, ModRM([0x66l;0x0Fl;0x15l])]
| Movhps -> [
  eVqMq, ModRM([0x0Fl;0x16l]);
  eMqVq, ModRM([0x0Fl;0x17l]);
  ]
| Movlhps  -> [eVqUq, ModRM([0x0Fl;0x16l])]
| Movshdup -> [eVqWq, ModRM([0xF3l;0x0Fl;0x16l])]
| Movhpd -> [
  eVqMq, ModRM([0x66l;0x0Fl;0x16l]);
  eMqVq, ModRM([0x66l;0x0Fl;0x17l]);
  ]
| Prefetchnta -> [eMb, ModRMGroup(0,[0x0Fl;0x18l]);]
| Prefetcht0  -> [eMb, ModRMGroup(1,[0x0Fl;0x18l]);]
| Prefetcht1  -> [eMb, ModRMGroup(2,[0x0Fl;0x18l]);]
| Prefetcht2  -> [eMb, ModRMGroup(3,[0x0Fl;0x18l]);]
| Movaps -> [
  eVpdWpd, ModRM([0x0Fl;0x28l]);
  eWpdVpd, ModRM([0x0Fl;0x29l]);
  ]
| Movapd -> [
  eVpsWps, ModRM([0x66l;0x0Fl;0x28l]);
  eWpsVps, ModRM([0x66l;0x0Fl;0x29l]);
  ]
| Cvtpi2ps  -> [eVpsQpi,  ModRM([0x0Fl;0x2Al])]
| Cvtsi2ss  -> [eVssEd_q, ModRM([0xF3l;0x0Fl;0x2Al])]
| Cvtpi2pd  -> [eVpdQpi,  ModRM([0x66l;0x0Fl;0x2Al])]
| Cvtsi2sd  -> [eVsdEd_q, ModRM([0xF2l;0x0Fl;0x2Al])]
| Movntps   -> [eMpsVps, ModRM([0x0Fl;0x2Bl])]
| Movntpd   -> [eMpdVpd, ModRM([0x66l;0x0Fl;0x2Bl])]
| Cvttps2pi -> [ePpiWps, ModRM([0x0Fl;0x2Cl])]
| Cvttss2si -> [eGdWss,  ModRM([0xF3l;0x0Fl;0x2Cl])]
| Cvttpd2pi -> [ePpiWpd, ModRM([0x66l;0x0Fl;0x2Cl])]
| Cvttsd2si -> [eGdWsd,  ModRM([0xF2l;0x0Fl;0x2Cl])]
| Cvtps2pi  -> [ePpiWps,  ModRM([0x0Fl;0x2Dl])]
| Cvtss2si  -> [eGd_qWss, ModRM([0xF3l;0x0Fl;0x2Dl])]
| Cvtpd2pi  -> [ePpiWpd,  ModRM([0x66l;0x0Fl;0x2Dl])]
| Cvtsd2si  -> [eGd_qWsd, ModRM([0xF2l;0x0Fl;0x2Dl])]
| Ucomiss   -> [eVsdWsd, ModRM([0x0Fl;0x2El])]
| Ucomisd   -> [eVssWss, ModRM([0x66l;0x0Fl;0x2El])]
| Comiss    -> [eVsdWsd, ModRM([0x0Fl;0x2Fl])]      
| Comisd    -> [eVssWss, ModRM([0x66l;0x0Fl;0x2Fl])]
| Wrmsr     -> [eNone, Literal([0x0Fl;0x30l])]
| Rdtsc     -> [eNone, Literal([0x0Fl;0x31l])]
| Rdmsr     -> [eNone, Literal([0x0Fl;0x32l])]
| Rdpmc     -> [eNone, Literal([0x0Fl;0x33l])]
| Sysenter  -> [eNone, Literal([0x0Fl;0x34l])]
| Sysexit   -> [eNone, Literal([0x0Fl;0x35l])]
| Getsec    -> [eNone, Literal([0x0Fl;0x37l])]
| Cmovo     -> [eGvEv, Literal([0x0Fl;0x40l])]
| Cmovno    -> [eGvEv, Literal([0x0Fl;0x41l])]
| Cmovb     -> [eGvEv, Literal([0x0Fl;0x42l])]
| Cmovae    -> [eGvEv, Literal([0x0Fl;0x43l])]
| Cmovz     -> [eGvEv, Literal([0x0Fl;0x44l])]
| Cmovnz    -> [eGvEv, Literal([0x0Fl;0x45l])]
| Cmovbe    -> [eGvEv, Literal([0x0Fl;0x46l])]
| Cmova     -> [eGvEv, Literal([0x0Fl;0x47l])]
| Cmovs     -> [eGvEv, Literal([0x0Fl;0x48l])]
| Cmovns    -> [eGvEv, Literal([0x0Fl;0x49l])]
| Cmovp     -> [eGvEv, Literal([0x0Fl;0x4Al])]
| Cmovnp    -> [eGvEv, Literal([0x0Fl;0x4Bl])]
| Cmovl     -> [eGvEv, Literal([0x0Fl;0x4Cl])]
| Cmovge    -> [eGvEv, Literal([0x0Fl;0x4Dl])]
| Cmovle    -> [eGvEv, Literal([0x0Fl;0x4El])]
| Cmovg     -> [eGvEv, Literal([0x0Fl;0x4Fl])]
| Movmskps  -> [eGdUps,  ModRM([0x0Fl;0x50l])]
| Movmskpd  -> [eGdUpd,  ModRM([0x66l;0x0Fl;0x50l])]
| Sqrtps    -> [eVpsWps, ModRM([0x0Fl;0x51l])]
| Sqrtss    -> [eVssWss, ModRM([0xF3l;0x0Fl;0x51l])]
| Sqrtpd    -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x51l])]
| Sqrtsd    -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x51l])]
| Rsqrtps   -> [eVpsWps, ModRM([0x0Fl;0x52l])]
| Rsqrtss   -> [eVssWss, ModRM([0xF3l;0x0Fl;0x52l])]
| Rcpps     -> [eVpsWps, ModRM([0x0Fl;0x53l])]
| Rcpss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x53l])]
| Andps     -> [eVpsWps, ModRM([0x0Fl;0x54l])]
| Andpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x54l])]
| Andnps    -> [eVpsWps, ModRM([0x0Fl;0x55l])]
| Andnpd    -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x55l])]
| Orps      -> [eVpsWps, ModRM([0x0Fl;0x56l])]
| Orpd      -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x56l])]
| Xorps     -> [eVpsWps, ModRM([0x0Fl;0x57l])]
| Xorpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x57l])]
| Addps     -> [eVpsWps, ModRM([0x0Fl;0x58l])]
| Addss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x58l])]
| Addpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x58l])]
| Addsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x58l])]
| Mulps     -> [eVpsWps, ModRM([0x0Fl;0x59l])]
| Mulss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x59l])]
| Mulpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x59l])]
| Mulsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x59l])]
| Cvtps2pd  -> [eVpdWps, ModRM([0x0Fl;0x5Al])]
| Cvtss2sd  -> [eVssWss, ModRM([0xF3l;0x0Fl;0x5Al])]
| Cvtpd2ps  -> [eVpsWpd, ModRM([0x66l;0x0Fl;0x5Al])]
| Cvtsd2ss  -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x5Al])]
| Cvtdq2ps  -> [eVpsWps, ModRM([0x0Fl;0x5Bl])]      
| Cvttps2dq -> [eVdqWps, ModRM([0xF3l;0x0Fl;0x5Bl])]
| Cvtps2dq  -> [eVdqWps, ModRM([0x66l;0x0Fl;0x5Bl])]
| Subps     -> [eVpsWps, ModRM([0x0Fl;0x5Cl])]
| Subss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x5Cl])]
| Subpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x5Cl])]
| Subsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x5Cl])]
| Minps     -> [eVpsWps, ModRM([0x0Fl;0x5Dl])]
| Minss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x5Dl])]
| Minpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x5Dl])]
| Minsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x5Dl])]
| Divps     -> [eVpsWps, ModRM([0x0Fl;0x5El])]
| Divss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x5El])]
| Divpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x5El])]
| Divsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x5El])]
| Maxps     -> [eVpsWps, ModRM([0x0Fl;0x5Fl])]
| Maxss     -> [eVssWss, ModRM([0xF3l;0x0Fl;0x5Fl])]
| Maxpd     -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x5Fl])]
| Maxsd     -> [eVsdWsd, ModRM([0xF2l;0x0Fl;0x5Fl])]
| Punpcklbw -> [
  ePqQd,   ModRM([0x0Fl;0x60l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x60l]);
  ]
| Punpcklwd -> [
  ePqQd,   ModRM([0x0Fl;0x61l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x61l]);
  ]
| Punpckldq -> [
  ePqQd,   ModRM([0x0Fl;0x62l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x62l]);
  ]
| Packsswb  -> [
  ePqQd,   ModRM([0x0Fl;0x63l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x63l]);
  ]
| Pcmpgtb   -> [
  ePqQd,   ModRM([0x0Fl;0x64l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x64l]);
  ]
| Pcmpgtw   -> [
  ePqQd,   ModRM([0x0Fl;0x65l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x65l]);
  ]
| Pcmpgtd   -> [
  ePqQd,   ModRM([0x0Fl;0x66l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x66l]);
  ]
| Packuswb  -> [
  ePqQd,   ModRM([0x0Fl;0x67l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x67l]);
  ]
| Punpckhbw -> [
  ePqQd,   ModRM([0x0Fl;0x68l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x68l]);
  ]
| Punpckhwd -> [
  ePqQd,   ModRM([0x0Fl;0x69l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x69l]);
  ]
| Punpckhdq -> [
  ePqQd,   ModRM([0x0Fl;0x6Al]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x6Al]);
  ]
| Packssdw  -> [
  ePqQd,   ModRM([0x0Fl;0x6Bl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x6Bl]);
  ]
| Punpcklqdq -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x6Cl])]
| Punpckhqdq -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x6Dl])]
| Movd -> [
  ePdEd_q,  ModRM([0x0Fl;0x6El]);
  eVdqEd_q, ModRM([0x66l;0x0Fl;0x6El]);
  eEd_qPd,  ModRM([0x0Fl;0x7El]);
  eEd_qVdq, ModRM([0x66l;0x0Fl;0x7El]);
  ]
| Movq   -> [
  ePqQq, ModRM([0x0Fl;0x6Fl]);
  eVqWq, ModRM([0xF3l;0x0Fl;0x7El]);
  eQqPq, ModRM([0x0Fl;0x7Fl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD6l]);
  ]
| Movdqu -> [
  eVdqWdq, ModRM([0xF3l;0x0Fl;0x6Fl]);
  eWdqVdq, ModRM([0xF3l;0x0Fl;0x7Fl]);
  ]
| Movdqa -> [
  eVdqWdq, ModRM([0x66l;0x0Fl;0x6Fl]);
  eWdqVdq, ModRM([0x66l;0x0Fl;0x7Fl]);
  ]
| Pshufw ->  [ePqQqIb  , ModRM([0x0Fl;0x70l])]
| Pshufhw -> [eVdqWdqIb, ModRM([0xF3l;0x0Fl;0x70l])]
| Pshufd  -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x70l])]
| Pshuflw -> [eVdqWdqIb, ModRM([0xF2l;0x0Fl;0x70l])]
| Psrlw   -> [
  eNqIb,  ModRMGroup(2,[0x0Fl;0x71l]);
  eUdqIb, ModRMGroup(2,[0x66l;0x0Fl;0x71l]);
  ePqQq,   ModRM([0x0Fl;0xD1l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD1l]);
  ]
| Psraw   -> [
  eNqIb,  ModRMGroup(4,[0x0Fl;0x71l]);
  eUdqIb, ModRMGroup(4,[0x66l;0x0Fl;0x71l]);
  ePqQq,   ModRM([0x0Fl;0xE1l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE1l]);
  ]
| Psllw   -> [
  eNqIb,  ModRMGroup(6,[0x0Fl;0x71l]);
  eUdqIb, ModRMGroup(6,[0x66l;0x0Fl;0x71l]);
  ePqQq,   ModRM([0x0Fl;0xF1l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF1l]);
  ]
| Psrld   -> [
  eNqIb,  ModRMGroup(2,[0x0Fl;0x72l]);
  eUdqIb, ModRMGroup(2,[0x66l;0x0Fl;0x72l]);
  ePqQq,   ModRM([0x0Fl;0xD2l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD2l]);
  ]
| Psrad   -> [
  eNqIb,  ModRMGroup(4,[0x0Fl;0x72l]);
  eUdqIb, ModRMGroup(4,[0x66l;0x0Fl;0x72l]);
  ePqQq,   ModRM([0x0Fl;0xE2l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE2l]);
  ]
| Pslld   -> [
  eNqIb,  ModRMGroup(6,[0x0Fl;0x72l]);
  eUdqIb, ModRMGroup(6,[0x66l;0x0Fl;0x72l]);
  ePqQq,   ModRM([0x0Fl;0xF2l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF2l]);
  ]
| Psrlq   -> [
  eNqIb,  ModRMGroup(2,[0x0Fl;0x73l]);
  eUdqIb, ModRMGroup(2,[0x66l;0x0Fl;0x73l]);
  ePqQq,   ModRM([0x0Fl;0xD3l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD3l]);  
  ]
| Psrldq  -> [eUdqIb, ModRMGroup(3,[0x66l;0x0Fl;0x73l])]
| Psllq   -> [
  eNqIb,  ModRMGroup(6,[0x0Fl;0x73l]);
  eUdqIb, ModRMGroup(6,[0x66l;0x0Fl;0x73l]);
  ePqQq,   ModRM([0x0Fl;0xF3l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF3l]);
  ]
| Pslldq  -> [eUdqIb, ModRMGroup(7,[0x66l;0x0Fl;0x73l])]
| Pcmpeqb -> [
  ePqQq,   ModRM([0x0Fl;0x74l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x74l]);
  ]
| Pcmpeqw -> [
  ePqQq,   ModRM([0x0Fl;0x75l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x75l]);
  ]
| Pcmpeqd -> [
  ePqQq,   ModRM([0x0Fl;0x76l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x76l]);
  ]
| Emms    -> [eNone,   ModRM([0x0Fl;0x77l])]
| Vmread  -> [eEdGd,   ModRM([0x0Fl;0x78l])]
| Vmwrite -> [eEdGd,   ModRM([0x0Fl;0x79l])]
| Haddpd  -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x7Cl])]
| Haddps  -> [eVpsWps, ModRM([0xF2l;0x0Fl;0x7Cl])]
| Hsubpd  -> [eVpdWpd, ModRM([0x66l;0x0Fl;0x7Dl])]
| Hsubps  -> [eVpsWps, ModRM([0xF2l;0x0Fl;0x7Dl])]
(* TODO: JCC FROM 0x180-0x190 *)
| Seto     -> [eEb,   ModRMGroup(0,[0x0Fl;0x90l])]
| Setno    -> [eEb,   ModRMGroup(0,[0x0Fl;0x91l])]
| Setb     -> [eEb,   ModRMGroup(0,[0x0Fl;0x92l])]
| Setae    -> [eEb,   ModRMGroup(0,[0x0Fl;0x93l])]
| Setz     -> [eEb,   ModRMGroup(0,[0x0Fl;0x94l])]
| Setnz    -> [eEb,   ModRMGroup(0,[0x0Fl;0x95l])]
| Setbe    -> [eEb,   ModRMGroup(0,[0x0Fl;0x96l])]
| Seta     -> [eEb,   ModRMGroup(0,[0x0Fl;0x97l])]
| Sets     -> [eEb,   ModRMGroup(0,[0x0Fl;0x98l])]
| Setns    -> [eEb,   ModRMGroup(0,[0x0Fl;0x99l])]
| Setp     -> [eEb,   ModRMGroup(0,[0x0Fl;0x9Al])]
| Setnp    -> [eEb,   ModRMGroup(0,[0x0Fl;0x9Bl])]
| Setl     -> [eEb,   ModRMGroup(0,[0x0Fl;0x9Cl])]
| Setge    -> [eEb,   ModRMGroup(0,[0x0Fl;0x9Dl])]
| Setle    -> [eEb,   ModRMGroup(0,[0x0Fl;0x9El])]
| Setg     -> [eEb,   ModRMGroup(0,[0x0Fl;0x9Fl])]
| Cpuid    -> [eNone, Literal([0x0Fl;0xA2l])]
| Bt       -> [
  eEvGv,         ModRM([0x0Fl;0xA3l]);
  eEvIb, ModRMGroup(4, [0x0Fl;0xBAl]);
  ]
| Shld     -> [
  eEvGvIb, ModRM([0x0Fl;0xA4l]);
  eEvGvCL, ModRM([0x0Fl;0xA5l]);
  ]
| Rsm      -> [eNone, Literal([0x0Fl;0xAAl])]
| Bts      -> [
  eEvGv,   ModRM([0x0Fl;0xABl]);
  eEvIb, ModRMGroup(5, [0x0Fl;0xBAl]);
  ]
| Shrd     -> [
  eEvGvIb, ModRM([0x0Fl;0xACl]);
  eEvGvCL, ModRM([0x0Fl;0xADl]);
]
| Fxsave   -> [eSimdState, ModRMGroup(0,[0x0Fl;0xAEl])]
| Fxrstor  -> [eSimdState, ModRMGroup(1,[0x0Fl;0xAEl])]
| Ldmxcsr  -> [eMd,        ModRMGroup(2,[0x0Fl;0xAEl])]
| Stmxcsr  -> [eMd,        ModRMGroup(3,[0x0Fl;0xAEl])]
| Clflush  -> [eMb,        ModRMGroup(7,[0x0Fl;0xAEl])]
| Lfence   -> [eNone,     Literal([0x0Fl;0xAEl;0xE8l])]
| Mfence   -> [eNone,     Literal([0x0Fl;0xAEl;0xF0l])]
| Sfence   -> [eNone,     Literal([0x0Fl;0xAEl;0xF8l])]
| Cmpxchg  -> [
  eEbGb, ModRM([0x0Fl;0xB0l]);
  eEvGv, ModRM([0x0Fl;0xB1l]);
  ]
| Lss      -> [eGvMp, ModRM([0x0Fl;0xB2l])]
| Btr      -> [
  eEvGv,         ModRM([0x0Fl;0xB3l]);
  eEvIb, ModRMGroup(6, [0x0Fl;0xBAl]);
  ]
| Lfs      -> [eGvMp, ModRM([0x0Fl;0xB4l])]
| Lgs      -> [eGvMp, ModRM([0x0Fl;0xB5l])]
| Movzx    -> [
  eGvEb, ModRM([0x0Fl;0xB6l]);
  eGvEw, ModRM([0x0Fl;0xB7l]);
  ]
| Popcnt   -> [eGvEv, ModRM([0x0Fl;0xB8l])]
| Btc      -> [
  eEvIb, ModRMGroup(7, [0x0Fl;0xBAl]);
  eEvGv, ModRM([0x0Fl;0xBBl]);
  ]
| Bsf      -> [eGvEv, ModRM([0x0Fl;0xBCl])]
| Bsr      -> [eGvEv, ModRM([0x0Fl;0xBDl])]
| Movsx    -> [
  eGvEb, ModRM([0x0Fl;0xBEl]);
  eGvEw, ModRM([0x0Fl;0xBFl]);
  ]
| Xadd     -> [
  eEbGb, ModRM([0x0Fl;0xC0l]);  
  eEvGv, ModRM([0x0Fl;0xC1l]);
  ]
| Cmpps     -> [eVpsWpsIb, ModRM([0x0Fl;0xC2l])]
| Cmpss     -> [eVssWssIb, ModRM([0xF3l;0x0Fl;0xC2l])]
| Cmppd     -> [eVpdWpdIb, ModRM([0x66l;0x0Fl;0xC2l])]
| Movnti    -> [eMd_qGd_q, ModRM([0x0Fl;0xC3l])]
| Pinsrw    -> [
  ePqEwIb,  ModRM([0x0Fl;0xC4l]);
  eVdqEwIb, ModRM([0x66l;0x0Fl;0xC4l]);
  ]
| Pextrw    -> [
  eGdNqIb,  ModRM([0x0Fl;0xC5l]);
  eGdUdqIb, ModRM([0x66l;0x0Fl;0xC5l]);
  eRd_MwVdqIb,  ModRM([0x66l;0x0Fl;0x3Al;0x15l]);
  ]
| Shufps    -> [eVpsWpsIb, ModRM([0x0Fl;0xC6l])]
| Shufpd    -> [eVpsWpsIb, ModRM([0x66l;0x0Fl;0xC6l])]
| Cmpxchg8b -> [eMq, ModRMGroup(1,[0x0Fl;0xC7l])]
| Vmptrld   -> [eMq, ModRMGroup(6,[0x0Fl;0xC7l])]
| Vmxon     -> [eMq, ModRMGroup(6,[0xF3l;0x0Fl;0xC7l])]
| Vmclear   -> [eMq, ModRMGroup(6,[0x66l;0x0Fl;0xC7l])]
| Vmptrst   -> [eMq, ModRMGroup(7,[0x0Fl;0xC7l])]
| Bswap    -> [
  eeAX, Literal([0x0Fl;0xC8l]);
  eeCX, Literal([0x0Fl;0xC9l]);
  eeDX, Literal([0x0Fl;0xCAl]);
  eeBX, Literal([0x0Fl;0xCBl]);
  eeSP, Literal([0x0Fl;0xCCl]);
  eeBP, Literal([0x0Fl;0xCDl]);
  eeSI, Literal([0x0Fl;0xCEl]);
  eeDI, Literal([0x0Fl;0xCFl]);
  ]
| Addsubpd -> [eVpdWpd, ModRM([0x66l;0x0Fl;0xD0l])]
| Addsubps -> [eVpsWps, ModRM([0xF2l;0x0Fl;0xD0l])] 
| Paddq  -> [
  ePqQq,   ModRM([0x0Fl;0xD4l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD4l]);
  ]
| Pmullw  -> [
  ePqQq,   ModRM([0x0Fl;0xD5l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD5l]);
  ]
| Movq2dq  -> [eVdqNq,  ModRM([0xF3l;0x0Fl;0xD6l])]
| Movdq2q  -> [ePqUq,   ModRM([0xF2l;0x0Fl;0xD6l])]
| Pmovmskb -> [
  eGdNq,  ModRM([0x0Fl;0xD7l]);
  eGdUdq, ModRM([0x66l;0x0Fl;0xD7l]);
  ]
| Psubusb -> [
  ePqQq,   ModRM([0x0Fl;0xD8l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD8l]);
  ]
| Psubusw -> [
  ePqQq,   ModRM([0x0Fl;0xD9l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xD9l]);
  ]
| Pminub  -> [
  ePqQq,   ModRM([0x0Fl;0xDAl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDAl]);
  ]
| Pand    -> [
  ePqQq,   ModRM([0x0Fl;0xDBl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDBl]);
  ]
| Paddusb -> [
  ePqQq,   ModRM([0x0Fl;0xDCl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDCl]);
  ]
| Paddusw -> [
  ePqQq,   ModRM([0x0Fl;0xDDl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDDl]);
  ]
| Pmaxub  -> [
  ePqQq,   ModRM([0x0Fl;0xDEl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDEl]);
  ]
| Pandn   -> [
  ePqQq,   ModRM([0x0Fl;0xDFl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xDFl]);
  ]
| Pavgb   -> [
  ePqQq,   ModRM([0x0Fl;0xE0l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE0l]);
  ]
| Pavgw   -> [
  ePqQq,   ModRM([0x0Fl;0xE3l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE3l]);
  ]
| Pmulhuw -> [
  ePqQq,   ModRM([0x0Fl;0xE4l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE4l]);
  ]
| Pmulhw  -> [
  ePqQq,   ModRM([0x0Fl;0xE5l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE5l]);
  ]
| Cvtdq2pd  -> [eVpdWdq, ModRM([0xF3l;0x0Fl;0xE6l])]
| Cvttpd2dq -> [eVdqWpd, ModRM([0x66l;0x0Fl;0xE6l])]
| Cvtpd2dq  -> [eVdqWpd, ModRM([0xF2l;0x0Fl;0xE6l])]
| Movntq    -> [eMqPq,   ModRM([0x0Fl;0xE7l])]
| Movntdq   -> [eMdqVdq, ModRM([0x66l;0x0Fl;0xE7l])]
| Psubsb    -> [
  ePqQq,   ModRM([0x0Fl;0xE8l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE8l]);
  ]
| Psubsw    -> [
  ePqQq,   ModRM([0x0Fl;0xE9l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xE9l]);
  ]
| Pminsw    -> [
  ePqQq,   ModRM([0x0Fl;0xEAl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xEAl]);
  ]
| Por       -> [
  ePqQq,   ModRM([0x0Fl;0xEBl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xEBl]);
  ]
| Paddsb    -> [
  ePqQq,   ModRM([0x0Fl;0xECl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xECl]);
  ]
| Paddsw    -> [
  ePqQq,   ModRM([0x0Fl;0xEDl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xEDl]);
  ]
| Pmaxsw    -> [
  ePqQq,   ModRM([0x0Fl;0xEEl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xEEl]);
  ]
| Pxor      -> [
  ePqQq,   ModRM([0x0Fl;0xEFl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xEFl]);
  ]
| Lddqu     -> [eVdqMdq, ModRM([0xF2l;0x0Fl;0xF0l])]
| Pmuludq   -> [
  ePqQq,   ModRM([0x0Fl;0xF4l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF4l]);
  ]
| Pmaddwd   -> [
  ePqQq,   ModRM([0x0Fl;0xF5l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF5l]);
  ]
| Psadbw    -> [
  ePqQq,   ModRM([0x0Fl;0xF6l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF6l]);
  ]
| Maskmovq    -> [ePqNq,   ModRM([0x0Fl;0xF7l])]
| Maskmovdqu  -> [eVdqUdq, ModRM([0x66l;0x0Fl;0xF7l])]
| Psubb     -> [
  ePqQq,   ModRM([0x0Fl;0xF8l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF8l]);
  ]
| Psubw     -> [
  ePqQq,   ModRM([0x0Fl;0xF9l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xF9l]);
  ]
| Psubd     -> [
  ePqQq,   ModRM([0x0Fl;0xFAl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xFAl]);
  ]
| Psubq     -> [
  ePqQq,   ModRM([0x0Fl;0xFBl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xFBl]);
  ]
| Paddb     -> [
  ePqQq,   ModRM([0x0Fl;0xFCl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xFCl]);
  ]
| Paddw     -> [
  ePqQq,   ModRM([0x0Fl;0xFDl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xFDl]);
  ]
| Paddd     -> [
  ePqQq,   ModRM([0x0Fl;0xFEl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0xFEl]);
  ]
| Pshufb    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x00l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x00l]);
  ]
| Phaddw    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x01l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x01l]);
  ]
| Phaddd    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x02l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x02l]);
  ]
| Phaddsw   -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x03l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x03l]);
  ]
| Pmaddubsw -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x04l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x04l]);
  ]
| Phsubw    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x05l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x05l]);
  ]
| Phsubd    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x06l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x06l]);
  ]
| Phsubsw   -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x07l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x07l]);
  ]
| Psignb    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x08l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x08l]);
  ]
| Psignw    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x09l]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x09l]);
  ]
| Psignd    -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x0Al]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x0Al]);
  ]
| Pmulhrsw  -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x0Bl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x0Bl]);
  ]
| Pblendvb  -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x10l])]
| Blendvps  -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x14l])]
| Blendvpd  -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x15l])]
| Ptest     -> [eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x17l])]
| Pabsb     -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x1Cl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x1Cl]);
  ]
| Pabsw     -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x1Dl]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x1Dl]);
  ]
| Pabsd     -> [
  ePqQq,   ModRM([0x0Fl;0x38l;0x1El]);
  eVdqWdq, ModRM([0x66l;0x0Fl;0x38l;0x1El]);
  ]
| Pmovsxbw    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x20l])]
| Pmovsxbd    -> [eVdqUdq_Md, ModRM([0x66l;0x0Fl;0x38l;0x21l])]
| Pmovsxbq    -> [eVdqUdq_Mw, ModRM([0x66l;0x0Fl;0x38l;0x22l])]
| Pmovsxwd    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x23l])]
| Pmovsxwq    -> [eVdqUdq_Md, ModRM([0x66l;0x0Fl;0x38l;0x24l])]
| Pmovsxdq    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x25l])]
| Pmuldq      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x28l])]
| Pcmpeqq     -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x29l])]
| Movntdqa    -> [eVdqMdq,    ModRM([0x66l;0x0Fl;0x38l;0x2Al])]
| Packusdw    -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x2Bl])]
| Pmovzxbw    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x30l])]
| Pmovzxbd    -> [eVdqUdq_Md, ModRM([0x66l;0x0Fl;0x38l;0x31l])]
| Pmovzxbq    -> [eVdqUdq_Mw, ModRM([0x66l;0x0Fl;0x38l;0x32l])]
| Pmovzxwd    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x33l])]
| Pmovzxwq    -> [eVdqUdq_Md, ModRM([0x66l;0x0Fl;0x38l;0x34l])]
| Pmovzxdq    -> [eVdqUdq_Mq, ModRM([0x66l;0x0Fl;0x38l;0x35l])]
| Pcmpgtq     -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x37l])]
| Pminsb      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x38l])]
| Pminsd      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x39l])]
| Pminuw      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3Al])]
| Pminud      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3Bl])]
| Pmaxsb      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3Cl])]
| Pmaxsd      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3Dl])]
| Pmaxuw      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3El])]
| Pmaxud      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x3Fl])]
| Pmulld      -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x40l])]
| Phminposuw  -> [eVdqWdq,    ModRM([0x66l;0x0Fl;0x38l;0x41l])]
| Crc32    -> [
  eGdEb, ModRM([0x0Fl;0x38l;0xF0l]);
  eGdEv, ModRM([0x0Fl;0x38l;0xF1l]);
  ]
| Roundps -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x08l])]
| Roundpd -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x09l])]
| Roundss -> [eVssWssIb, ModRM([0x66l;0x0Fl;0x3Al;0x0Al])]
| Roundsd -> [eVsdWsdIb, ModRM([0x66l;0x0Fl;0x3Al;0x0Bl])]
| Blendps -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x0Cl])]
| Blendpd -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x0Dl])]
| Pblendw -> [eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x0El])]
| Palignr -> [
  ePqQqIb,   ModRM([0x0Fl;0x3Al;0x0Fl]);
  eVdqWdqIb, ModRM([0x66l;0x0Fl;0x3Al;0x0Fl]);
  ]
| Pextrb    -> [eRd_MbVdqIb,  ModRM([0x66l;0x0Fl;0x3Al;0x14l])]
| Pextrd    -> [eEdVdqIb,     ModRM([0x66l;0x0Fl;0x3Al;0x16l])]
| Extractps -> [eEdVdqIb,     ModRM([0x66l;0x0Fl;0x3Al;0x17l])]
| Pinsrb    -> [eVdqEdIb,     ModRM([0x66l;0x0Fl;0x3Al;0x20l])]
| Insertps  -> [eVdqUdq_MdIb, ModRM([0x66l;0x0Fl;0x3Al;0x21l])]
| Pinsrd    -> [eVdqEdIb,     ModRM([0x66l;0x0Fl;0x3Al;0x22l])]
| Dpps      -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x40l])]
| Dppd      -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x41l])]
| Mpsadbw   -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x42l])]
| Pcmpestrm -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x60l])]
| Pcmpestri -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x61l])]
| Pcmpistrm -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x62l])]
| Pcmpistri -> [eVdqWdqIb,    ModRM([0x66l;0x0Fl;0x3Al;0x63l])]

(* 0% speedup *)
(*
let m2ehash = Hashtbl.create 700
let mnem_to_encodings m = 
  try Hashtbl.find m2ehash m
  with Not_found -> (let r = mnem_to_encodings m in (Hashtbl.add m2ehash m r; r))
*)
type x86_operating_mode = OM_16 | OM_32 | OM_64
let operating_mode = ref OM_32

let e_size () =
  match !operating_mode with 
  | OM_16 -> OPSZ_16 
  | OM_32 -> OPSZ_32 
  | _ -> failwith "unsupported operating mode"
let r_size () = e_size ()
let v_size () = e_size ()
let z_size () = e_size ()
let a_size () =
  match !operating_mode with
  | OM_16 -> OPSZ_32
  | OM_32 -> OPSZ_64
  | _ -> failwith "unsupported operating mode"

let may_require_size_override = function
| OAL
| OCL
| OALR8L
| OCLR9L
| ODLR10L
| OBLR11L
| OAHR12L
| OCHR13L
| ODHR14L
| OBHR15L
| OAX
| ODX
| OCS
| ODS
| OES
| OGS
| OFS
| OSS
| OSw
| OCd
| ODd
| OIb
| OIw
| O1
| OGb
| OGw
| OGd
| OGd_q
| ORd
| ORw
| OMb
| OMw
| OMd
| OMs
| OMq
| OMdq
| OMd_q
| OMp
| OMpd
| OMps
| OM
| OEb
| OEw
| OEd
| OEd_q
| OOb
| OAp
| OJb
| OJz
| OXb
| OYb
| OSt0
| OStN
| OFPEnv
| OFPEnvLow
| OReal4
| OReal8
| OReal10
| ONq
| OPd
| OPq
| OPpi
| OVdq
| OVpd
| OVps
| OVsd
| OVss
| OVq
| OUps
| OUpd
| OUq
| OUdq
| OQpi
| OQd
| OQq
| OWdq
| OWps
| OWpd
| OWq
| OWss
| OWsd
| OSimdState
| OUdq_Md
| OUdq_Mq
| OUdq_Mw
| ORd_Mb
| ORd_Mw
| OXv (* Despite nomenclature, these things require *address prefixes* *)
| OYv
| OYw
| OYd
| OXw
| OXd
| OXz
| OYz -> None
| OrAX (* This one might seem to be out of place, but I am rather confused *)
| OeAX (* about x64 encoding at the moment *)
| OeCX
| OeBX
| OeSP
| OeDX
| OeBP
| OeSI
| OeDI -> Some(e_size ())
| OrAXr8
| OrCXr9
| OrDXr10
| OrBXr11
| OrSPr12
| OrBPr13
| OrSIr14
| OrDIr15 -> Some(r_size ())
| OIv
| OGv
| ORv
| OEv
| OOv -> Some(v_size ())
| OIz
| OGz -> Some(z_size ())
| OMa -> Some(a_size ())

let int_of_reg8 = function
| Al -> 0
| Cl -> 1
| Dl -> 2
| Bl -> 3
| Ah -> 4
| Ch -> 5
| Dh -> 6
| Bh -> 7

let int_of_reg16 = function
| Ax -> 0
| Cx -> 1
| Dx -> 2
| Bx -> 3
| Sp -> 4
| Bp -> 5
| Si -> 6
| Di -> 7

let int_of_reg32 = function
| Eax -> 0
| Ecx -> 1
| Edx -> 2
| Ebx -> 3
| Esp -> 4
| Ebp -> 5
| Esi -> 6
| Edi -> 7

let int_of_x86_control_reg = function
| CR0 -> 0
| CR1 -> 1
| CR2 -> 2
| CR3 -> 3
| CR4 -> 4
| CR5 -> 5
| CR6 -> 6
| CR7 -> 7

let int_of_x86_debug_reg = function
| DR0 -> 0
| DR1 -> 1
| DR2 -> 2
| DR3 -> 3
| DR4 -> 4
| DR5 -> 5
| DR6 -> 6
| DR7 -> 7

let int_of_x86_general_reg = function
| Gb(g) -> int_of_reg8  g
| Gw(g) -> int_of_reg16 g
| Gd(g) -> int_of_reg32 g

let int_of_x86_seg_reg = function
| ES -> 0
| CS -> 1
| SS -> 2
| DS -> 3
| FS -> 4
| GS -> 5

let int_of_x86_mmx_reg = function
| MM0 -> 0
| MM1 -> 1
| MM2 -> 2
| MM3 -> 3
| MM4 -> 4
| MM5 -> 5
| MM6 -> 6
| MM7 -> 7

let int_of_x86_xmm_reg = function
| XMM0 -> 0
| XMM1 -> 1
| XMM2 -> 2
| XMM3 -> 3
| XMM4 -> 4
| XMM5 -> 5
| XMM6 -> 6
| XMM7 -> 7

let int_of_x86_fpu_reg = function
| ST0 -> 0
| ST1 -> 1
| ST2 -> 2
| ST3 -> 3
| ST4 -> 4
| ST5 -> 5
| ST6 -> 6
| ST7 -> 7

let int_of_x86operand = function
| GeneralReg(g) -> int_of_x86_general_reg g
| ControlReg(c) -> int_of_x86_control_reg c
| DebugReg(d)   -> int_of_x86_debug_reg d
| SegReg(s)     -> int_of_x86_seg_reg s
| FPUReg(f)     -> int_of_x86_fpu_reg f
| MMXReg(m)     -> int_of_x86_mmx_reg m
| XMMReg(x)     -> int_of_x86_xmm_reg x
| Immediate(_)
| Memexpr(_)
| JccTarget(_,_)
| FarTarget(_) -> invalid_arg "int_of_x86operand"

type x86_insn_encoding = {
  group1pf:   int32 option;
  segpf:      int32 option;
  opsizepf:   int32 option;
  addrsizepf: int32 option;
  stem:       int32 list; (* Instruction stem, 1-4 bytes *)
  modrm:      int32 option;
  sib:        int32 option;
  rmdispl:    x86_immediate option;
  imms:       x86_immediate list;
}

let list_of_immediate i = 
  let b i = Int32.logand i 0xFFl in
  let s = Int32.shift_right_logical in
  match i with
  | Ib(i) -> [i]
  | Iw(i) -> [b i; b (s i 8)]
  | Id(i) -> [b i; b (s i 8); b (s i 16); b (s i 24)]

let get_byte_encoding enc = 
  let get_some_byte l = function | Some(b) -> b::l | None -> l in
  let l = get_some_byte [] enc.group1pf in
  let l = get_some_byte l enc.segpf in
  let l = get_some_byte l enc.opsizepf in
  let l = get_some_byte l enc.addrsizepf in
  let l = (List.rev enc.stem)@l in
  let l = get_some_byte l enc.modrm in
  let l = get_some_byte l enc.sib in
  let l = match enc.rmdispl with | Some(i) -> (List.rev (list_of_immediate i))@l | _ -> l in
  (List.rev l)@(List.concat (List.map list_of_immediate enc.imms))

let mk_default_x86_insn_encoding () = 
{ group1pf   = None;
  segpf      = None;
  opsizepf   = None;
  addrsizepf = None;
  stem       = [];
  modrm      = None;
  sib        = None;
  rmdispl    = None;
  imms       = [];
}

type sib_info = int * int * int

type modrm_info = 
| GInfo of int
| EInfo of int * int * sib_info option (* top2, bottom3, SIB *)

let is_quantity_signed_byte x = x >= 0xffffff80l && x <= 0x7Fl
(*let is_dword_signed_byte x = x >= 0xffffff80l && x <= 0x7Fl*)
(*let is_word_signed_byte  x = x >= 0 ???*)

let epart_of_memexpr16 b s d =
  let aux rm =
    let imm,m0d = 
    match d with
    | Some(imm) -> 
      if is_quantity_signed_byte imm
      then Some(Ib(Int32.logand imm 0xFFl)),1
      else Some(Iw(Int32.logand imm 0xFFFFl)),2
    | None -> None,0
    in (EInfo(m0d,rm,None),imm)
  in
  match b,s,d with
  | Some(Bx),Some(Si),_   -> aux 0
  | Some(Bx),Some(Di),_   -> aux 1
  | Some(Bp),Some(Si),_   -> aux 2 
  | Some(Bp),Some(Di),_   -> aux 3 
  | Some(Si),None,_       -> aux 4
  | Some(Di),None,_       -> aux 5 
  | Some(Bp),None,Some(_) -> aux 6 
  | Some(Bp),None,None    -> EInfo(1,6,None),Some(Ib(0l))
  | Some(Bx),None,_       -> aux 7 
  | None,    None,Some(i) -> (EInfo(0,6,None),Some(Iw(i)))
  | _,_,_                 -> invalid_arg "epart_of_memexpr16: invalid 16-bit mem expression"

let epart_of_memexpr32 b s d =
  let imm,m0d = match d with
  | Some(imm) -> 
    if is_quantity_signed_byte imm
    then Some(Ib(Int32.logand imm 0xFFl)),1
    else Some(Id(imm)),2
  | _ when b = Some(Ebp) && s = None -> Some(Ib(0l)),1
  | None -> None,0
  in
  match b,s,d with 
  | None,None,None             -> invalid_arg "epart_of_memexpr32: empty memory expression"
  | _,Some(Esp,_),_            -> invalid_arg "epart_of_memexpr32: ESP used as scale register"
  | None,None,Some(i)          -> EInfo(0,5,None),Some(Id(i))
  | Some(Esp),None,_           -> EInfo(m0d,4,Some(0,4,4)),imm
  | Some(Ebp),None,None        -> EInfo(1,5,None),Some(Ib(0l))
  | Some(Ebp),None,Some(i)     -> EInfo(m0d,5,None),imm
  | Some(br), None,_           -> EInfo(m0d,int_of_reg32 br,None),imm
  | None,Some(sr,sf),None      -> EInfo(0,4,Some(sf,int_of_reg32 sr,5)),Some(Id(0l))
  | None,Some(sr,sf),Some(i)   -> EInfo(0,4,Some(sf,int_of_reg32 sr,5)),Some(Id(i))
  | Some(Ebp),Some(sr,sf),None -> EInfo(1,4,Some(sf,int_of_reg32 sr,5)),Some(Ib(0l))
  | Some(Ebp),Some(sr,sf),_    -> EInfo(m0d,4,Some(sf,int_of_reg32 sr,5)),imm
  | Some(br), Some(sr,sf),_    -> EInfo(m0d,4,Some(sf,int_of_reg32 sr,int_of_reg32 br)),imm
    
let epart_of_x86_addr_expr = function
| Mem16(_,b,s,d) -> epart_of_memexpr16 b s d
| Mem32(_,b,s,d) -> epart_of_memexpr32 b s d

let gather_epart_of_operand = function
| GeneralReg(g) -> EInfo(3,int_of_x86_general_reg g,None),None
| ControlReg(c) -> EInfo(3,int_of_x86_control_reg c,None),None
| DebugReg(d)   -> EInfo(3,int_of_x86_debug_reg d,None),None
| SegReg(s)     -> EInfo(3,int_of_x86_seg_reg s,None),None
| FPUReg(f)     -> EInfo(3,int_of_x86_fpu_reg f,None),None
| MMXReg(m)     -> EInfo(3,int_of_x86_mmx_reg m,None),None
| XMMReg(x)     -> EInfo(3,int_of_x86_xmm_reg x,None),None
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> epart_of_x86_addr_expr m
| Immediate(_)
| JccTarget(_,_)
| FarTarget(_) -> invalid_arg "int_of_x86operand"

let make_encoded_byte t m b = 
  if not (0 <= m && m <= 7) then invalid_arg "make_encoded_byte: invalid g-part";
  if not (0 <= t && t <= 3) then invalid_arg "make_encoded_byte: invalid t-part";
  if not (0 <= b && b <= 7) then invalid_arg "make_encoded_byte: invalid b-part";
  Int32.of_int ((t lsl 6) lor (m lsl 3) lor b)

let make_modrm_sib_rmdispl enc = function
| [] -> { enc with modrm = None; sib = None; }
| GInfo(g)::EInfo(t,b,s)::[]
| EInfo(t,b,s)::GInfo(g)::[] ->
  { enc with modrm = Some(make_encoded_byte t g b); 
    sib = match s with None -> None | Some(t,m,b) -> Some(make_encoded_byte t m b); }
| _ -> invalid_arg "make_modrm_sib_rmdispl: invalid list of modrm parts"

let gather_gpart_of_operand opnd = GInfo(int_of_x86operand opnd)

let byte_of_seg = function
| CS -> 0x2El
| SS -> 0x36l
| DS -> 0x3El
| ES -> 0x26l
| FS -> 0x64l
| GS -> 0x65l

let get_seg_override aop memexpr = 
  let s = X86Util.default_seg memexpr in
  let s'' = X86Util.get_seg   memexpr in
  match aop,s'' with
  | OYb,ES | OYw,ES | OYd,ES | OYv,ES | OYz,ES -> None
  | _,_ when s <> s'' -> Some(byte_of_seg s'')
  | _,_ -> None

let get_seg_override aop = function
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> get_seg_override aop m
| Immediate(_)
| FarTarget(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> None

let gather_immediate = function
| Immediate(i) -> [i]
| FarTarget(Ap32(seg,offs)) -> [Id(offs);Iw(seg)]
| FarTarget(Ap16(seg,offs)) -> [Iw(offs);Iw(seg)]
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m)) -> 
 (match m with
  | Mem16(_,None,None,Some(i)) -> [Iw(i)]
  | Mem32(_,None,None,Some(i)) -> [Id(i)]
  | _ -> invalid_arg "gather_immediate:  tried to extract immediate from complex memory expression")
| Memexpr(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> invalid_arg "gather_immediate"

let needs_addr_prefix = function
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> 
 (match m,!operating_mode with
  | Mem16(_,_,_,_),OM_32
  | Mem32(_,_,_,_),OM_16 -> Some(0x67l)
  | _,_ -> None)
| FarTarget(ap) ->
 (match ap,!operating_mode with
  | Ap16(_,_),OM_32
  | Ap32(_,_),OM_16 -> Some(0x67l)
  | _,_ -> None)
| Immediate(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> None

let ensure_all_somes_equal list failmsg = 
  match (List.filter (fun s -> s <> None) list) with
  | [] -> None
  | s::ss -> 
    if List.for_all (fun z -> s = z) ss 
    then s 
    else failwith failmsg

let get_seg_prefix encl oplist = 
  let seg_overrides = List.map2 (fun aop op -> get_seg_override aop op) encl oplist in
  ensure_all_somes_equal seg_overrides "Multiple conflictory segment prefixes"
  
let get_addrsize_prefix oplist =
  let addr_prefixes = List.map needs_addr_prefix oplist in
  ensure_all_somes_equal addr_prefixes "Multiple conflictory address prefixes"

let get_opsize_prefix aoplist oplist =
  let sslist = 
    List.fold_left2 
     (fun acc aopnd opnd -> 
        match (may_require_size_override aopnd) with
        | Some(s) -> (Some(s <> size_of_operand opnd))::acc
        | None -> acc)
     []
     aoplist
     oplist
  in
  let s = 
    ensure_all_somes_equal 
      sslist 
      "Some operands required size prefixes, some did not (shouldn't happen due to type-checking)" 
  in match s with
  | Some(true) -> Some(0x66l)
  | _ -> None

let does_encoding_match enclist (scstr,acstr) oplist =
  let rec aux el ol = match el,ol with
  | [],[] -> true
  | eo::es,oo::os -> X86TypeCheck.typecheck_operand eo oo && aux es os
  | _ -> false
  in aux enclist oplist 
  && satisfies_size_constraints oplist scstr 
  && satisfies_addr_constraints oplist acstr 

let print_byte = Printf.printf "%02lx"
let print_list = List.iter print_byte

let pp_x86_insn_encoding enc =
  let print_some_byte descr = function | Some(b) -> Printf.printf "%s: %02lx\n" descr b | _ -> () in
  print_some_byte "Group1 prefix" enc.group1pf;
  print_some_byte "Segment prefix" enc.segpf;
  print_some_byte "Operand size prefix" enc.opsizepf;
  print_some_byte "Address size prefix" enc.addrsizepf;
  Printf.printf "Stem: "; print_list enc.stem; print_endline "";
  print_some_byte "Mod R/M byte" enc.modrm;
  print_some_byte "SIB byte" enc.sib;
 (match enc.rmdispl with
  | Some(i) -> Printf.printf "Mod R/M displacement: %s\n" (X86Disasm.string_of_x86_immediate i)
  | None -> ());
  match enc.imms with
  | [] -> ()
  | list -> Printf.printf "Immediates: "; List.iter (fun i -> Printf.printf "%s " (X86Disasm.string_of_x86_immediate i)) list; print_endline ""

let native_size_pf e = match e,!operating_mode with
| Native32(_),OM_32
| Native16(_),OM_16 -> Some(false)
| Native32(_),OM_16
| Native16(_),OM_32 -> Some(true)
| Native16(_),_
| Native32(_),_ -> failwith "Unsupported operating mode"
| _ -> None

(* Take all of the encodings that match a given instruction. Due to the way the tables
   are arranged, we should have the shortest and most specific encodings first. *)
let get_list_of_valid_instruction_encodings ({ pref=pref; instr=(mnem,oplist) } as i) = 
  let rec aux outlist = function
  | [] when outlist = [] -> raise (InvalidOperands(i))
  | [] -> List.rev outlist
  | ((el,cnstr),ee)::es -> 
    if does_encoding_match el cnstr oplist 
    then aux ((el,ee)::outlist) es 
    else aux outlist es
  in aux [] (mnem_to_encodings mnem)

(* Iterate through each operand in the list and pick out the partial encoding pieces,
   e.g. the G-part, E-part, list of operands.  Return the partially-built instruction
   encoding  *)
let collect_parts =
  let rec aux (insn,mrmlist,mimmopt) el ol =
    match el,ol with
    | [],[] -> (insn,mrmlist,mimmopt)
    | optype::es,x86opnd::os ->
      let insn,mrmlist,mimmopt = 
        match (enctype_of_abstract_operand optype) with
        | GPart -> (insn,(gather_gpart_of_operand x86opnd)::mrmlist,mimmopt)
        | EPart -> let ep,mio = gather_epart_of_operand x86opnd in (insn,ep::mrmlist,mio)
        | Imm   -> ({ insn with imms = insn.imms@(gather_immediate x86opnd) },mrmlist,mimmopt)
        | NoEnc -> (insn,mrmlist,mimmopt)
      in aux (insn,mrmlist,mimmopt) es os
    | _,_ -> failwith "Impossible internal error:  despite type-checking, encoding and operand lists differed in size"
  in
  (* Starting with an empty encoding, collect up the immediates (in the insn) and ModR/M parts (separate) *)
  aux (mk_default_x86_insn_encoding (),[],None)

let encode_instruction_inner { pref=pref; instr=(mnem,oplist) } (encl,encv) (insn,mrmlist,mimmopt) =
  (* If we had a ModR/M group, add its GPart to the modrm list *)
  let mrmlist = match encv with | ModRMGroup(g,_) -> (GInfo(g))::mrmlist | _ -> mrmlist in
  (* Add the stem to the encoding *)
  let insn = { insn with stem = match encv with | Literal(b) | Native32(b) | Native16(b) | ModRM(b) | ModRMGroup(_,b) -> b } in
  (* Add the ModR/M and perhaps the SIB byte to the encoding *)
  let insn = make_modrm_sib_rmdispl insn mrmlist in
  (* Add the ModR/M immediate, and segment, operand size, and address prefixes, optionally *) 
  let opszpf = 
    match native_size_pf encv with
    | Some(true) -> Some(0x66l)
    | Some(false) -> None
    | None -> get_opsize_prefix encl oplist
  in
  let insn = { insn with 
    rmdispl = mimmopt;
    segpf = get_seg_prefix encl oplist;
    opsizepf = opszpf;
    addrsizepf = get_addrsize_prefix oplist; } 
  in
 (List.map (function | Lock -> 0xF0l | Repne -> 0xF2l | Rep -> 0xF3l) pref)@(get_byte_encoding insn)
  
type instruction_choice_strategy =
| ICSShortest
| ICSRandom

let get_instruction_encoding_parts ({ pref=pref; instr=(mnem,oplist) } as i) ics = 
  let encoding_list = get_list_of_valid_instruction_encodings i in
  match ics with
  | ICSShortest -> List.hd encoding_list
  (* opt_get is safe here because get_list_of_valid_instruction_encodings throws 
     an exception rather than returning an empty list *)
  | ICSRandom   -> Util.opt_get (ListUtil.pick_random_element encoding_list)

let encode_instruction_from_parts ({ pref=pref; instr=(mnem,oplist) } as i) ((encl,encv) as e) =
  encode_instruction_inner i e (collect_parts encl oplist)

let get_all_instruction_encodings i =
  List.map (encode_instruction_from_parts i) (get_list_of_valid_instruction_encodings i)  

let encode_instruction_by_strategy ics i =
  encode_instruction_from_parts i (get_instruction_encoding_parts i ics)

let encode_instruction_randomly = encode_instruction_by_strategy (ICSRandom)
let encode_instruction_shortest = encode_instruction_by_strategy (ICSShortest)
  
let encode_instruction = encode_instruction_shortest
let mnem_to_encodings_full = mnem_to_encodings

let mnem_to_encodings m = List.map fst (mnem_to_encodings m)