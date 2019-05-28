let set_cursor _ = ()

let disassemble ea = 
  set_cursor ea;
  match consume_byte () with
  | 0x00l -> Nop
  | 0x01l -> Break
  | 0x02l -> Ldarg_0
  | 0x03l -> Ldarg_1
  | 0x04l -> Ldarg_2
  | 0x05l -> Ldarg_3
  | 0x06l -> Ldloc_0
  | 0x07l -> Ldloc_1
  | 0x08l -> Ldloc_2
  | 0x09l -> Ldloc_3
  | 0x0Al -> Stloc_0
  | 0x0Bl -> Stloc_1
  | 0x0Cl -> Stloc_2
  | 0x0Dl -> Stloc_3
  | 0x0El -> Ldarg_s
  | 0x0Fl -> Ldarga_s
  | 0x10l -> Starg_s
  | 0x11l -> Ldloc_s
  | 0x12l -> Ldloca_s
  | 0x13l -> Stloc_s
  | 0x14l -> Ldnull
  | 0x15l -> Ldc_i4_m1
  | 0x16l -> Ldc_i4_0
  | 0x17l -> Ldc_i4_1
  | 0x18l -> Ldc_i4_2
  | 0x19l -> Ldc_i4_3
  | 0x1Al -> Ldc_i4_4
  | 0x1Bl -> Ldc_i4_5
  | 0x1Cl -> Ldc_i4_6
  | 0x1Dl -> Ldc_i4_7
  | 0x1El -> Ldc_i4_8
  | 0x1Fl -> Ldc_i4_s
  | 0x20l -> Ldc_i4
  | 0x21l -> Ldc_i8
  | 0x22l -> Ldc_r4
  | 0x23l -> Ldc_r8
  | 0x25l -> Dup
  | 0x26l -> Pop
  | 0x27l -> Jmp
  | 0x28l -> Call
  | 0x29l -> Calli
  | 0x2Al -> Ret
  | 0x2Bl -> Br_s
  | 0x2Cl -> Brfalse_s
  | 0x2Dl -> Brtrue_s
  | 0x2El -> Beq_s
  | 0x2Fl -> Bge_s
  | 0x30l -> Bgt_s
  | 0x31l -> Ble_s
  | 0x32l -> Blt_s
  | 0x33l -> Bne_un_s
  | 0x34l -> Bge_un_s
  | 0x35l -> Bgt_un_s
  | 0x36l -> Ble_un_s
  | 0x37l -> Blt_un_s
  | 0x38l -> Br
  | 0x39l -> Brfalse
  | 0x3Al -> Brtrue
  | 0x3Bl -> Beq
  | 0x3Cl -> Bge
  | 0x3Dl -> Bgt
  | 0x3El -> Ble
  | 0x3Fl -> Blt
  | 0x40l -> Bne_un
  | 0x41l -> Bge_un
  | 0x42l -> Bgt_un
  | 0x43l -> Ble_un
  | 0x44l -> Blt_un
  | 0x45l -> Switch
  | 0x46l -> Ldind_i1
  | 0x47l -> Ldind_u1
  | 0x48l -> Ldind_i2
  | 0x49l -> Ldind_u2
  | 0x4Al -> Ldind_i4
  | 0x4Bl -> Ldind_u4
  | 0x4Cl -> Ldind_i8
  | 0x4Dl -> Ldind_i
  | 0x4El -> Ldind_r4
  | 0x4Fl -> Ldind_r8
  | 0x50l -> Ldind_ref
  | 0x51l -> Stind_ref
  | 0x52l -> Stind_i1
  | 0x53l -> Stind_i2
  | 0x54l -> Stind_i4
  | 0x55l -> Stind_i8
  | 0x56l -> Stind_r4
  | 0x57l -> Stind_r8
  | 0x58l -> Add
  | 0x59l -> Sub
  | 0x5Al -> Mul
  | 0x5Bl -> Div
  | 0x5Cl -> Div_un
  | 0x5Dl -> Rem
  | 0x5El -> Rem_un
  | 0x5Fl -> And
  | 0x60l -> Or
  | 0x61l -> Xor
  | 0x62l -> Shl
  | 0x63l -> Shr
  | 0x64l -> Shr_un
  | 0x65l -> Neg
  | 0x66l -> Not
  | 0x67l -> Conv_i1
  | 0x68l -> Conv_i2
  | 0x69l -> Conv_i4
  | 0x6Al -> Conv_i8
  | 0x6Bl -> Conv_r4
  | 0x6Cl -> Conv_r8
  | 0x6Dl -> Conv_u4
  | 0x6El -> Conv_u8
  | 0x6Fl -> Callvirt
  | 0x70l -> Cpobj
  | 0x71l -> Ldobj
  | 0x72l -> Ldstr
  | 0x73l -> Newobj
  | 0x74l -> Castclass
  | 0x75l -> Isinst
  | 0x76l -> Conv_r_un
  | 0x79l -> Unbox
  | 0x7Al -> Throw
  | 0x7Bl -> Ldfld
  | 0x7Cl -> Ldflda
  | 0x7Dl -> Stfld
  | 0x7El -> Ldsfld
  | 0x7Fl -> Ldsflda
  | 0x80l -> Stsfld
  | 0x81l -> Stobj
  | 0x82l -> Conv_ovf_i1_un
  | 0x83l -> Conv_ovf_i2_un
  | 0x84l -> Conv_ovf_i4_un
  | 0x85l -> Conv_ovf_i8_un
  | 0x86l -> Conv_ovf_u1_un
  | 0x87l -> Conv_ovf_u2_un
  | 0x88l -> Conv_ovf_u4_un
  | 0x89l -> Conv_ovf_u8_un
  | 0x8Al -> Conv_ovf_i_un
  | 0x8Bl -> Conv_ovf_u_un
  | 0x8Cl -> Box
  | 0x8Dl -> Newarr
  | 0x8El -> Ldlen
  | 0x8Fl -> Ldelema
  | 0x90l -> Ldelem_i1
  | 0x91l -> Ldelem_u1
  | 0x92l -> Ldelem_i2
  | 0x93l -> Ldelem_u2
  | 0x94l -> Ldelem_i4
  | 0x95l -> Ldelem_u4
  | 0x96l -> Ldelem_i8
  | 0x97l -> Ldelem_i
  | 0x98l -> Ldelem_r4
  | 0x99l -> Ldelem_r8
  | 0x9Al -> Ldelem_ref
  | 0x9Bl -> Stelem_i
  | 0x9Cl -> Stelem_i1
  | 0x9Dl -> Stelem_i2
  | 0x9El -> Stelem_i4
  | 0x9Fl -> Stelem_i8
  | 0xA0l -> Stelem_r4
  | 0xA1l -> Stelem_r8
  | 0xA2l -> Stelem_ref
  | 0xA3l -> Ldelem
  | 0xA4l -> Stelem
  | 0xA5l -> Unbox_any
  | 0xB3l -> Conv_ovf_i1
  | 0xB4l -> Conv_ovf_u1
  | 0xB5l -> Conv_ovf_i2
  | 0xB6l -> Conv_ovf_u2
  | 0xB7l -> Conv_ovf_i4
  | 0xB8l -> Conv_ovf_u4
  | 0xB9l -> Conv_ovf_i8
  | 0xBAl -> Conv_ovf_u8
  | 0xC2l -> Refanyval
  | 0xC3l -> Ckfinite
  | 0xC6l -> Mkrefany
  | 0xD0l -> Ldtoken
  | 0xD1l -> Conv_u2
  | 0xD2l -> Conv_u1
  | 0xD3l -> Conv_i
  | 0xD4l -> Conv_ovf_i
  | 0xD5l -> Conv_ovf_u
  | 0xD6l -> Add_ovf
  | 0xD7l -> Add_ovf_un
  | 0xD8l -> Mul_ovf
  | 0xD9l -> Mul_ovf_un
  | 0xDAl -> Sub_ovf
  | 0xDBl -> Sub_ovf_un
  | 0xDCl -> Endfinally
  | 0xDDl -> Leave
  | 0xDEl -> Leave_s
  | 0xDFl -> Stind_i
  | 0xE0l -> Conv_u
  | 0xFEl -> (match consume_byte () with
    | 0x00l -> Arglist
    | 0x01l -> Ceq
    | 0x02l -> Cgt
    | 0x03l -> Cgt_un
    | 0x04l -> Clt
    | 0x05l -> Clt_un
    | 0x06l -> Ldftn
    | 0x07l -> Ldvirtftn
    | 0x09l -> Ldarg
    | 0x0Al -> Ldarga
    | 0x0Bl -> Starg
    | 0x0Cl -> Ldloc
    | 0x0Dl -> Ldloca
    | 0x0El -> Stloc
    | 0x0Fl -> Localloc
    | 0x11l -> Endfilter
    | 0x12l -> Unaligned_   (* Prefix for ldind, stind, ldfld, stfld, ldobj, stobj, initblk, or cpblk *)
    | 0x13l -> Volatile_    (* Prefix for ldind, stind, ldfld, stfld, ldobj, stobj, initblk, or cpblk *)
    | 0x14l -> Tail_        (* Prefix for call, calli, or callvirt *)
    | 0x15l -> Initobj
    | 0x16l -> Constrained_ (* Prefix for callvirt *)
    | 0x17l -> Cpblk
    | 0x18l -> Initblk
    | 0x19l -> No_

 (* No_:
    The prefix can be used in the following circumstances:
    0x01: typecheck (castclass, unbox, ldelema, stelem, stelem).  The CLI can optionally skip any type 
    checks normally performed as part of the execution of the subsequent instruction.  
    InvalidCastException can optionally still be thrown if the check would fail. 
    0x02: rangecheck (ldelem.*, ldelema, stelem.*).  The CLI can optionally skip any array range checks 
    normally performed as part of the execution of the subsequent instruction.  IndexOutOfRangeException
    can optionally still be thrown if the check would fail.
    0x04: nullcheck (ldfld, stfld, callvirt, ldvirtftn, ldelem.*, stelem.*, ldelema). The CLI can optionally skip 
    any null-reference checks normally performed as part of the execution of the subsequent instruction.  
    NullReferenceException can optionally still be thrown if the check would fail.
    The byte values can be OR-ed; e.g.; a value of 0x05 indicates that both typecheck and nullcheck can
    optionally be omitted. *)

    | 0x1Al -> Rethrow
    | 0x1Cl -> Sizeof
    | 0x1Dl -> Refanytype
    | 0x1El -> Readonly (* This prefix can only appear only immediately preceding the ldelema instruction and calls to the special Address method on arrays. *)
    )