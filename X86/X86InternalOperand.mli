(** This module should not be used outside of the typechecker and assembler.  It
    defines a union describing the "abstract operand type" of a given operand.
    This information allows us to typecheck a given instruction:  for example, 
    consider the erroneous instruction [mov 10, 11].  There is no instruction
    encoding for [mov] where the left-hand side is an immediate, therefore the
    left-hand side can not possibly conform to any instruction encoding of the
    [mov] instruction, therefore it is an invalid instruction. 
    
    These encodings are taken straight out of the Intel manuals' tables:  if the
    instruction is represented in the tables as [in AL, Dx], this corresponds to
    [(In,\[OAL;ODx\])].
    *)

(**/**)

type x86_abstract_operand = 
| OAL
| OALR8L
| OCL
| OCLR9L
| ODLR10L
| OBLR11L
| OAHR12L
| OCHR13L
| ODHR14L
| OBHR15L
| OAx
| ODx
| OeAX
| OrAXr8
| OrAX
| OeCX
| OrCXr9
| OeDX
| OrDXr10
| OeBX
| OrBXr11
| OeSP
| OrSPr12
| OeBP
| OrBPr13
| OeSI
| OrSIr14
| OeDI
| OrDIr15
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
| OIv
| OIz
| O1
| OGb
| OGw
| OGd
| OGv
| OGd_q
| OGz
| ORw
| ORd
| ORv
| OMb
| OMw
| OMd
| OMs
| OMq
| OMdq
| OMd_q
| OMa
| OMp
| OMpd
| OMps
| OM
| OEb
| OEw
| OEd
| OEv
| OEd_q
| OOb
| OOv
| OAp
| OJb
| OJz
| OXb
| OXv
| OXz
| OYb
| OYv
| OYz
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
| OXw
| OXd
| OYw
| OYd

val string_of_x86_abstract_operand : x86_abstract_operand -> string

