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

let string_of_x86_abstract_operand = function
| OAL        -> "OAL"
| OALR8L     -> "OALR8L"
| OCL        -> "OCL"
| OCLR9L     -> "OCLR9L"
| ODLR10L    -> "ODLR10L"
| OBLR11L    -> "OBLR11L"
| OAHR12L    -> "OAHR12L"
| OCHR13L    -> "OCHR13L"
| ODHR14L    -> "ODHR14L"
| OBHR15L    -> "OBHR15L"
| OAx        -> "OAX"
| ODx        -> "ODX"
| OeAX       -> "OeAX"
| OrAXr8     -> "OrAXr8"
| OrAX       -> "OrAX"
| OeCX       -> "OeCX"
| OrCXr9     -> "OrCXr9"
| OeDX       -> "OeDX"
| OrDXr10    -> "OrDXr10"
| OeBX       -> "OeBX"
| OrBXr11    -> "OrBXr11"
| OeSP       -> "OeSP"
| OrSPr12    -> "OrSPr12"
| OeBP       -> "OeBP"
| OrBPr13    -> "OrBPr13"
| OeSI       -> "OeSI"
| OrSIr14    -> "OrSIr14"
| OeDI       -> "OeDI"
| OrDIr15    -> "OrDIr15"
| OCS        -> "OCS"
| ODS        -> "ODS"
| OES        -> "OES"
| OGS        -> "OGS"
| OFS        -> "OFS"
| OSS        -> "OSS"
| OSw        -> "OSw"
| OCd        -> "OCd"
| ODd        -> "ODd"
| OIb        -> "OIb"
| OIw        -> "OIw"
| OIv        -> "OIv"
| OIz        -> "OIz"
| O1         -> "O1"
| OGb        -> "OGb"
| OGw        -> "OGw"
| OGd        -> "OGd"
| OGv        -> "OGv"
| OGd_q      -> "OGd_q"
| OGz        -> "OGz"
| ORw        -> "ORw"
| ORd        -> "ORd"
| ORv        -> "ORv"
| OMb        -> "OMb"
| OMw        -> "OMw"
| OMd        -> "OMd"
| OMs        -> "OMs"
| OMq        -> "OMq"
| OMdq       -> "OMdq"
| OMd_q      -> "OMd_q"
| OMa        -> "OMa"
| OMp        -> "OMp"
| OMpd       -> "OMpd"
| OMps       -> "OMps"
| OM         -> "OM"
| OEb        -> "OEb"
| OEw        -> "OEw"
| OEd        -> "OEd"
| OEv        -> "OEv"
| OEd_q      -> "OEd_q"
| OOb        -> "OOb"
| OOv        -> "OOv"
| OAp        -> "OAp"
| OJb        -> "OJb"
| OJz        -> "OJz"
| OXb        -> "OXb"
| OXv        -> "OXv"
| OXz        -> "OXz"
| OYb        -> "OYb"
| OYv        -> "OYv"
| OYz        -> "OYz"
| OSt0       -> "OSt0"
| OStN       -> "OStN"
| OFPEnv     -> "OFPEnv"
| OFPEnvLow  -> "OFPEnvLow"
| OReal4     -> "OReal4"
| OReal8     -> "OReal8"
| OReal10    -> "OReal10"
| ONq        -> "ONq"
| OPd        -> "OPd"
| OPq        -> "OPq"
| OPpi       -> "OPpi"
| OVdq       -> "OVdq"
| OVpd       -> "OVpd"
| OVps       -> "OVps"
| OVsd       -> "OVsd"
| OVss       -> "OVss"
| OVq        -> "OVq"
| OUps       -> "OUps"
| OUpd       -> "OUpd"
| OUq        -> "OUq"
| OUdq       -> "OUdq"
| OQpi       -> "OQpi"
| OQd        -> "OQd"
| OQq        -> "OQq"
| OWdq       -> "OWdq"
| OWps       -> "OWps"
| OWpd       -> "OWpd"
| OWq        -> "OWq"
| OWss       -> "OWss"
| OWsd       -> "OWsd"
| OSimdState -> "OSimdState"
| OUdq_Md    -> "OUdq_Md"
| OUdq_Mq    -> "OUdq_Mq"
| OUdq_Mw    -> "OUdq_Mw"
| ORd_Mb     -> "ORd_Mb"
| ORd_Mw     -> "ORd_Mw"
| OXw        -> "OXw"
| OXd        -> "OXd"
| OYw        -> "OYw"
| OYd        -> "OYd"
