type mips32_gpr =
| Zero
| At
| V0
| V1
| A0
| A1
| A2
| A3
| T0
| T1
| T2
| T3
| T4
| T5
| T6
| T7
| T8
| T9
| S0
| S1
| S2
| S3
| S4
| S5
| S6
| S7
| S8
| K0
| K1
| GP
| SP
| RA

type mips32_spr =
| HI
| LO
| PC

type mips32_fpr =
| F0
| F1
| F2
| F3
| F4
| F5
| F6
| F7
| F8
| F9
| F10
| F11
| F12
| F13
| F14
| F15
| F16
| F17
| F18
| F19
| F20
| F21
| F22
| F23
| F24
| F25
| F26
| F27
| F28
| F29
| F30
| F31

type mips32_fspr =
| FIR
| FCCR
| FEXR
| FENR
| FCSR

type mips32_opnd =
| GPR  of mips32_gpr
| SPR  of mips32_spr
| FPR  of mips32_fpr
| FSPR of mips32_fspr
| Imm  of int

type mips32_fpr_format =
| S
| D
| W
| L
| PS

type mips32_gpr_fpr_format =
| G
| F of mips32_format

type mips32_mnem =
| Abs of mips32_fpr_format
| Add of mips32_gpr_fpr_format
| Addi
| Addui
| Addu
| AlnvPS