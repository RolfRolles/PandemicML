open X86
open X86InternalOperand

let tExact e = fun x -> x = e

let tAL  = tExact (GeneralReg(Gb(Al)))   let tALR8L  = tAL
let tCL  = tExact (GeneralReg(Gb(Cl)))   let tCLR9L  = tCL
let tDL  = tExact (GeneralReg(Gb(Dl)))   let tDLR10L = tDL
let tBL  = tExact (GeneralReg(Gb(Bl)))   let tBLR11L = tBL
let tAH  = tExact (GeneralReg(Gb(Ah)))   let tAHR12L = tAH
let tCH  = tExact (GeneralReg(Gb(Ch)))   let tCHR13L = tCH
let tDH  = tExact (GeneralReg(Gb(Dh)))   let tDHR14L = tDH
let tBH  = tExact (GeneralReg(Gb(Bh)))   let tBHR15L = tBH
let tAx  = tExact (GeneralReg(Gw(Ax)))
let tCx  = tExact (GeneralReg(Gw(Cx)))
let tDx  = tExact (GeneralReg(Gw(Dx)))
let tBx  = tExact (GeneralReg(Gw(Bx)))
let tSp  = tExact (GeneralReg(Gw(Sp)))
let tBp  = tExact (GeneralReg(Gw(Bp)))
let tSi  = tExact (GeneralReg(Gw(Si)))
let tDi  = tExact (GeneralReg(Gw(Di)))
let tEax = tExact (GeneralReg(Gd(Eax)))
let tEcx = tExact (GeneralReg(Gd(Ecx)))
let tEdx = tExact (GeneralReg(Gd(Edx)))
let tEbx = tExact (GeneralReg(Gd(Ebx)))
let tEsp = tExact (GeneralReg(Gd(Esp)))
let tEbp = tExact (GeneralReg(Gd(Ebp)))
let tEsi = tExact (GeneralReg(Gd(Esi)))
let tEdi = tExact (GeneralReg(Gd(Edi)))
let teAX o = tAx o || tEax o   let trAXr8  = teAX   let trAX = teAX 
let teCX o = tCx o || tEcx o   let trCXr9  = teCX
let teDX o = tDx o || tEdx o   let trDXr10 = teDX
let teBX o = tBx o || tEbx o   let trBXr11 = teBX
let teSP o = tSp o || tEsp o   let trSPr12 = teSP
let teBP o = tBp o || tEbp o   let trBPr13 = teBP
let teSI o = tSi o || tEsi o   let trSIr14 = teSI
let teDI o = tDi o || tEdi o   let trDIr15 = teDI

let tCS  = tExact (SegReg(CS))
let tDS  = tExact (SegReg(DS))
let tES  = tExact (SegReg(ES))
let tFS  = tExact (SegReg(FS))
let tGS  = tExact (SegReg(GS))
let tSS  = tExact (SegReg(SS))

let tSw  = function | SegReg(_)     -> true | _ -> false
let tCd  = function | ControlReg(_) -> true | _ -> false
let tDd  = function | DebugReg(_)   -> true | _ -> false

let tIb  = function | Immediate(Ib(_)) -> true | _ -> false
let tIw  = function | Immediate(Iw(_)) -> true | _ -> false
let tId  = function | Immediate(Id(_)) -> true | _ -> false
let tIv  o = tIw o || tId o
let tIz  = tIv
let t1 = function 
| Immediate(Ib(i))
| Immediate(Iw(i))
| Immediate(Id(i)) when i = 0x1l -> true
| _ -> false

let tGb = function | GeneralReg(Gb(_)) -> true | _ -> false
let tGw = function | GeneralReg(Gw(_)) -> true | _ -> false
let tGd = function | GeneralReg(Gd(_)) -> true | _ -> false
let tGv o = tGw o || tGd o
let tGd_q = tGd
let tGz = tGv

let tRd = tGd
let tRw = tGw
let tRv = tGv

let tM   = function | Memexpr(_)      -> true | _ -> false
let tMb  = function | Memexpr(Mb (_)) -> true | _ -> false
let tMw  = function | Memexpr(Mw (_)) -> true | _ -> false
let tMd  = function | Memexpr(Md (_)) -> true | _ -> false
let tMs  = function | Memexpr(Mf (_)) -> true | _ -> false
let tMq  = function | Memexpr(Mq (_)) -> true | _ -> false
let tMdq = function | Memexpr(Mdq(_)) -> true | _ -> false
let tMd_q = tMd
let tMa o = tMd o || tMq o
let tMp o = tMd o || tMs o
let tMpd  = tMdq
let tMps  = tMdq

let tEb o = tGb o || tMb o
let tEw o = tGw o || tMw o
let tEd o = tGd o || tMd o
let tEv o = tEw o || tEd o
let tEd_q = tEd

let tOb = function | Memexpr(Mb(Mem16(_,None,None,Some(i))))
                   | Memexpr(Mb(Mem32(_,None,None,Some(i)))) -> true
                   | _ -> false

let tOw = function | Memexpr(Mw(Mem16(_,None,None,Some(i))))
                   | Memexpr(Mw(Mem32(_,None,None,Some(i)))) -> true
                   | _ -> false

let tOd = function | Memexpr(Md(Mem16(_,None,None,Some(i))))
                   | Memexpr(Md(Mem32(_,None,None,Some(i)))) -> true
                   | _ -> false

let tOv o = tOw o || tOd o
let tAp = function | FarTarget(_)   -> true | _ -> false
let tJb = function | JccTarget(_,_) -> true | _ -> false  let tJz = tJb

let tXb = function | Memexpr(Mb(Mem16(_,Some(Si), None,None)))
                   | Memexpr(Mb(Mem32(_,Some(Esi),None,None))) -> true
                   | _ -> false
let tXw = function | Memexpr(Mw(Mem16(_,Some(Si), None,None)))
                   | Memexpr(Mw(Mem32(_,Some(Esi),None,None))) -> true
                   | _ -> false
let tXd = function | Memexpr(Md(Mem16(_,Some(Si), None,None)))
                   | Memexpr(Md(Mem32(_,Some(Esi),None,None))) -> true
                   | _ -> false

let tXv o = tXw o || tXd o   let tXz = tXv

let tYb = function | Memexpr(Mb(Mem16(ES,Some(Di), None,None)))
                   | Memexpr(Mb(Mem32(ES,Some(Edi),None,None))) -> true
                   | _ -> false
let tYw = function | Memexpr(Mw(Mem16(ES,Some(Di), None,None)))
                   | Memexpr(Mw(Mem32(ES,Some(Edi),None,None))) -> true
                   | _ -> false
let tYd = function | Memexpr(Md(Mem16(ES,Some(Di), None,None)))
                   | Memexpr(Md(Mem32(ES,Some(Edi),None,None))) -> true
                   | _ -> false

let tYv o = tYw o || tYd o   let tYz = tYv

let tSt0      = tExact (FPUReg(ST0))
let tStN      = function | FPUReg(_) -> true | _ -> false
let tFPEnv    = tMd
let tFPEnvLow = tMd
let tReal4    = tMd
let tReal8    = tMq
let tReal10   = function | Memexpr(Mt(_)) -> true | _ -> false

let tNq  = function | MMXReg(_) -> true | _ -> false   let tPd,tPq,tPpi,tNd = tNq,tNq,tNq,tNq
let tVdq = function | XMMReg(_) -> true | _ -> false   let tVpd,tVps,tVsd,tVss,tVq = tVdq,tVdq,tVdq,tVdq,tVdq
let tUps = tVdq
let tUpd,tUq,tUdq = tUps,tUps,tUps

let tQpi o = tMq  o || tNq  o   let tQd,tQq = tQpi,tQpi
let tWdq o = tMdq o || tVdq o   let tWps,tWpd,tWq = tWdq,tWdq,tWdq
let tWss o = tMd  o || tVdq o
let tWsd o = tMq  o || tVdq o

let tSimdState = tMd

let tUdq_Md o = tMd o || tVdq o
let tUdq_Mq o = tMq o || tVdq o
let tUdq_Mw o = tMw o || tVdq o

let tRd_Mb o = tRd o || tMb o
let tRd_Mw o = tRd o || tMw o

let operand_to_typechecker = function
| OAL        -> tAL
| OALR8L     -> tALR8L
| OCL        -> tCL
| OCLR9L     -> tCLR9L
| ODLR10L    -> tDLR10L
| OBLR11L    -> tBLR11L
| OAHR12L    -> tAHR12L
| OCHR13L    -> tCHR13L
| ODHR14L    -> tDHR14L
| OBHR15L    -> tBHR15L
| OAX        -> tAx
| ODX        -> tDx
| OeAX       -> teAX
| OrAXr8     -> trAXr8
| OrAX       -> trAX
| OeCX       -> teCX
| OrCXr9     -> trCXr9
| OeDX       -> teDX
| OrDXr10    -> trDXr10
| OeBX       -> teBX
| OrBXr11    -> trBXr11
| OeSP       -> teSP
| OrSPr12    -> trSPr12
| OeBP       -> teBP
| OrBPr13    -> trBPr13
| OeSI       -> teSI
| OrSIr14    -> trSIr14
| OeDI       -> teDI
| OrDIr15    -> trDIr15
| OCS        -> tCS
| ODS        -> tDS
| OES        -> tES
| OGS        -> tGS
| OFS        -> tFS
| OSS        -> tSS
| OSw        -> tSw
| OCd        -> tCd
| ODd        -> tDd
| OIb        -> tIb
| OIw        -> tIw
| OIv        -> tIv
| OIz        -> tIz
| O1         -> t1
| OGb        -> tGb
| OGw        -> tGw
| OGd        -> tGd
| OGv        -> tGv
| OGd_q      -> tGd_q
| OGz        -> tGz
| ORd        -> tRd
| ORw        -> tRw
| ORv        -> tRv
| OMb        -> tMb
| OMw        -> tMw
| OMd        -> tMd
| OMs        -> tMs
| OMq        -> tMq
| OMdq       -> tMdq
| OMd_q      -> tMd_q
| OMa        -> tMa
| OMp        -> tMp
| OMpd       -> tMpd
| OMps       -> tMps
| OM         -> tM
| OEb        -> tEb
| OEw        -> tEw
| OEd        -> tEd
| OEv        -> tEv
| OEd_q      -> tEd_q
| OOb        -> tOb
| OOv        -> tOv
| OAp        -> tAp
| OJb        -> tJb
| OJz        -> tJz
| OXb        -> tXb
| OXw        -> tXw
| OXd        -> tXd
| OXv        -> tXv
| OXz        -> tXz
| OYb        -> tYb
| OYw        -> tYw
| OYd        -> tYd
| OYv        -> tYv
| OYz        -> tYz
| OSt0       -> tSt0
| OStN       -> tStN
| OFPEnv     -> tFPEnv
| OFPEnvLow  -> tFPEnvLow
| OReal4     -> tReal4
| OReal8     -> tReal8
| OReal10    -> tReal10
| ONq        -> tNq
| OPd        -> tPd
| OPq        -> tPq
| OPpi       -> tPpi
| OVdq       -> tVdq
| OVpd       -> tVpd
| OVps       -> tVps
| OVsd       -> tVsd
| OVss       -> tVss
| OVq        -> tVq
| OUps       -> tUps
| OUpd       -> tUpd
| OUq        -> tUq
| OUdq       -> tUdq
| OQpi       -> tQpi
| OQd        -> tQd
| OQq        -> tQq
| OWdq       -> tWdq
| OWps       -> tWps
| OWpd       -> tWpd
| OWq        -> tWq
| OWss       -> tWss
| OWsd       -> tWsd
| OUdq_Md    -> tUdq_Md
| OUdq_Mq    -> tUdq_Mq
| OUdq_Mw    -> tUdq_Mw
| ORd_Mb     -> tRd_Mb
| ORd_Mw     -> tRd_Mw
| OSimdState -> tSimdState

let typecheck_operand aopnd opnd = (operand_to_typechecker aopnd) opnd

type opndsize = 
| OPSZ_8  
| OPSZ_16 
| OPSZ_32 
| OPSZ_48 
| OPSZ_64 
| OPSZ_80 
| OPSZ_128

let size_of_operand = function
| GeneralReg(g) -> (match g with
  | Gb(_) -> OPSZ_8
  | Gw(_) -> OPSZ_16
  | Gd(_) -> OPSZ_32)
| ControlReg(_) -> OPSZ_32
| DebugReg(_)   -> OPSZ_32
| SegReg(_)     -> OPSZ_16
| FPUReg(_)     -> OPSZ_80
| MMXReg(_)     -> OPSZ_64
| XMMReg(_)     -> OPSZ_128
| Immediate(i) -> (match i with
  | Ib(_) -> OPSZ_8
  | Iw(_) -> OPSZ_16
  | Id(_) -> OPSZ_32)
| Memexpr(m) -> (match m with
  | Mb (_) -> OPSZ_8
  | Mw (_) -> OPSZ_16
  | Md (_) -> OPSZ_32
  | Mf (_) -> OPSZ_48
  | Mq (_) -> OPSZ_64
  | Mt (_) -> OPSZ_80
  | Mdq(_) -> OPSZ_128)
| JccTarget(_) -> invalid_arg "size_of_operand: JccTarget"
| FarTarget(_) -> invalid_arg "size_of_operand: FarTarget"

let addr_of_operand = function
| Memexpr(m) -> (match m with
  | Mb (Mem16(_,_,_,_))
  | Mw (Mem16(_,_,_,_))
  | Md (Mem16(_,_,_,_))
  | Mf (Mem16(_,_,_,_))
  | Mq (Mem16(_,_,_,_))
  | Mt (Mem16(_,_,_,_))
  | Mdq(Mem16(_,_,_,_)) -> OPSZ_16
  | Mb (Mem32(_,_,_,_))
  | Mw (Mem32(_,_,_,_))
  | Md (Mem32(_,_,_,_))
  | Mf (Mem32(_,_,_,_))
  | Mq (Mem32(_,_,_,_))
  | Mt (Mem32(_,_,_,_))
  | Mdq(Mem32(_,_,_,_)) -> OPSZ_32)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| Immediate(_)  
| JccTarget(_)
| FarTarget(_) -> invalid_arg "addr_of_operand"