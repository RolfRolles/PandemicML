open AVM2Pervasives

exception InvalidNamespace of int32
exception InvalidMultiname of int32
exception InvalidOptional of int32
exception InvalidTraitType
exception InvalidOpcode of int32

type namespace_kind =
(* Namespace kinds *)
| CONSTANT_Namespace
| CONSTANT_PackageNamespace
| CONSTANT_PackageInternalNs
| CONSTANT_ProtectedNamespace
| CONSTANT_ExplicitNamespace
| CONSTANT_StaticProtectedNs
| CONSTANT_PrivateNs

(* Multiname kinds *)
| CONSTANT_QName
| CONSTANT_QNameA
| CONSTANT_RTQName
| CONSTANT_RTQNameA
| CONSTANT_RTQNameL
| CONSTANT_RTQNameLA
| CONSTANT_Multiname
| CONSTANT_MultinameA
| CONSTANT_MultinameL
| CONSTANT_MultinameLA

(* Optional kinds *)
| CONSTANT_Int
| CONSTANT_UInt
| CONSTANT_Double
| CONSTANT_Utf8
| CONSTANT_True
| CONSTANT_False
| CONSTANT_Null
| CONSTANT_Undefined
(* followed by the namespace kinds *)

let namespace_kind_of_int32 = function
| 0x08l -> CONSTANT_Namespace
| 0x16l -> CONSTANT_PackageNamespace
| 0x17l -> CONSTANT_PackageInternalNs
| 0x18l -> CONSTANT_ProtectedNamespace
| 0x19l -> CONSTANT_ExplicitNamespace
| 0x1Al -> CONSTANT_StaticProtectedNs
| 0x05l -> CONSTANT_PrivateNs
| x -> raise (InvalidNamespace(x))

let multiname_kind_of_int32 = function
| 0x00l -> (* Empty string *)
| 0x07l -> CONSTANT_QName
| 0x0Dl -> CONSTANT_QNameA
| 0x0Fl -> CONSTANT_RTQName
| 0x10l -> CONSTANT_RTQNameA
| 0x11l -> CONSTANT_RTQNameL
| 0x12l -> CONSTANT_RTQNameLA
| 0x09l -> CONSTANT_Multiname
| 0x0El -> CONSTANT_MultinameA
| 0x1Bl -> CONSTANT_MultinameL
| 0x1Cl -> CONSTANT_MultinameLA
| x -> raise (InvalidMultiname(x))

let optional_kind_of_int32 = function
| 0x03l -> CONSTANT_Int
| 0x04l -> CONSTANT_UInt
| 0x06l -> CONSTANT_Double
| 0x01l -> CONSTANT_Utf8
| 0x0Bl -> CONSTANT_True
| 0x0Al -> CONSTANT_False
| 0x0Cl -> CONSTANT_Null
| 0x00l -> CONSTANT_Undefined
| 0x08l -> CONSTANT_Namespace
| 0x16l -> CONSTANT_PackageNamespace
| 0x17l -> CONSTANT_PackageInternalNs
| 0x18l -> CONSTANT_ProtectedNamespace
| 0x19l -> CONSTANT_ExplicitNamespace
| 0x1Al -> CONSTANT_StaticProtectedNs
| 0x05l -> CONSTANT_PrivateNs
| x -> raise (InvalidOptional(x))

type multiname_kind =
| MultinameKind_QName of u30 * u30
| MultinameKind_RTQName of u30
| MultinameKind_RTQNameL
| MultinameKind_Multiname of u30 * u30
| MultinameKind_MultinameL of u30

let if_CONSTANT_ClassSealed      = 0x01l
let if_CONSTANT_ClassFinal       = 0x02l
let if_CONSTANT_ClassInterface   = 0x04l
let if_CONSTANT_ClassProtectedNs = 0x08l

type trait_type = 
| Trait_Slot
| Trait_Method
| Trait_Getter
| Trait_Setter
| Trait_Class
| Trait_Function
| Trait_Const

let ta_ATTR_Final    = 0x01l
let ta_ATTR_Override = 0x02l
let ta_ATTR_Metadata = 0x04l

let trait_type_of_int32 = function
| 0x00l -> Trait_Slot
| 0x01l -> Trait_Method
| 0x02l -> Trait_Getter
| 0x03l -> Trait_Setter
| 0x04l -> Trait_Class
| 0x05l -> Trait_Function
| 0x06l -> Trait_Const
| 0x07l -> raise InvalidTraitType
| _ -> failwith "impossible"

type avm2_insn =
| Add
| Add_i
| Astype of u30
| Astypelate
| Bitand
| Bitnot
| Bitor 
| Bitxor
| Call of u30
| Callmethod of u30 * u30
| Callproperty of u30 * u30
| Callproplex of u30 * u30
| Callpropvoid of u30 * u30
| Callstatic of u30 * u30
| Callsuper of u30 * u30
| Callsupervoid of u30 * u30
| Checkfilter
| Coerce of u30
| Coerce_a
| Coerce_s
| Construct of u30
| Constructprop of u30 * u30
| Constructsuper of u30
| Convert_b
| Convert_i
| Convert_d
| Convert_o
| Convert_u
| Convert_s
| Debug of u8 * u30 * u8 * u30 (* "unsigned bytes" *)
| Debugfile of u30
| Debugline of u30
| Declocal of u30
| Declocal_i of u30
| Decrement
| Decrement_i
| Deleteproperty of u30
| Divide
| Dup
| Dxns of u30
| Dxnslate
| Equals
| Esc_xattr
| Esc_xelem
| Findproperty of u30
| Findpropstrict of u30
| Getdescendants of u30
| Getglobalscope
| Getglobalslot of u30
| Getlex of u30
| Getlocal of u30
| Getlocal_0
| Getlocal_1
| Getlocal_2
| Getlocal_3
| Getproperty of u30
| Getscopeobject
| Getslot of u30
| Getsuper of u30
| Greaterequals
| Greaterthan
| Hasnext
| Hasnext2 of u30 * u30 (* Manual calls them "uints", thx for consistency mofos *)
| Ifeq of s24
| Iffalse of s24
| Ifge of s24
| Ifgt of s24
| Ifle of s24
| Iflt of s24
| Ifnge of s24
| Ifngt of s24
| Ifnle of s24
| Ifnlt of s24
| Ifne of s24
| Ifstricteq of s24
| Ifstrictne of s24
| Iftrue of s24
| In
| Inclocal of u30
| Inclocal_i of u30
| Increment
| Increment_i
| Initproperty of u30
| Instanceof
| Istype of u30
| Istypelate
| Jump of s24
| Kill of u30
| Label
| Lessequals
| Lessthan
| Lookupswitch of s24 * u30 * (s24 array)
| Lshift
| Modulo
| Multiply
| Multiply_i
| Negate
| Negate_i
| Newactivation
| Newarray of u30
| Newcatch of u30
| Newclass of u30
| Newfunction of u30
| Newobject of u30
| Nextname 
| Nextvalue
| Nop
| Not
| Pop
| Popscope
| Pushbyte of u8 (* "unsigned byte", thx again mofos *)
| Pushdouble of u30
| Pushfalse
| Pushint of u30
| Pushnamespace of u30
| Pushnan
| Pushnull
| Pushscope
| Pushshort of u30
| Pushstring of u30
| Pushtrue
| Pushuint of u30
| Pushundefined
| Pushwith
| Returnvalue
| Returnvoid
| Rshift
| Setlocal of u30
| Setlocal_0
| Setlocal_1
| Setlocal_2
| Setlocal_3
| Setglobalslot of u30
| Setproperty of u30
| Setslot of u30
| Setsuper of u30
| Strictequals
| Subtract
| Subtract_i
| Swap
| Throw
| Typeof
| Urshift

let decode ea =
  set_cursor ea;
  match (consume_u8 ()) with

  | 0x02l -> Nop
  | 0x03l -> Throw
  | 0x04l -> Getsuper(consume_u30 ())
  | 0x05l -> Setsuper(consume_u30 ())
  | 0x06l -> Dxns(consume_u30 ())
  | 0x07l -> Dxnslate
  | 0x08l -> Kill(consume_u30 ())
  | 0x09l -> Label
  | 0x0Cl -> Ifnlt(consume_s24 ())
  | 0x0Dl -> Ifnle(consume_s24 ())
  | 0x0El -> Ifngt(consume_s24 ())
  | 0x0Fl -> Ifnge(consume_s24 ())
  | 0x10l -> Jump(consume_s24 ())
  | 0x11l -> Iftrue(consume_s24 ())
  | 0x12l -> Iffalse(consume_s24 ())
  | 0x13l -> Ifeq(consume_s24 ())
  | 0x14l -> Ifne(consume_s24 ())
  | 0x15l -> Iflt(consume_s24 ())
  | 0x16l -> Ifle(consume_s24 ())
  | 0x17l -> Ifgt(consume_s24 ())
  | 0x18l -> Ifge(consume_s24 ())
  | 0x19l -> Ifstricteq(consume_s24 ())
  | 0x1Al -> Ifstrictne(consume_s24 ())
  | 0x1Bl -> 
    let default_offset = consume_s24 () in
    let case_count = consume_u30 () in
    let case_offsets = consume_s24_array case_count in
    Lookupswitch(default_offset, case_count, case_offsets)
  | 0x1Cl -> Pushwith
  | 0x1Dl -> Popscope
  | 0x1El -> Nextname 
  | 0x1Fl -> Hasnext
  | 0x20l -> Pushnull
  | 0x21l -> Pushundefined
  | 0x23l -> Nextvalue
  | 0x24l -> Pushbyte(consume_u8 ())
  | 0x25l -> Pushshort(consume_u30 ())
  | 0x26l -> Pushtrue
  | 0x27l -> Pushfalse
  | 0x28l -> Pushnan
  | 0x29l -> Pop
  | 0x2Al -> Dup
  | 0x2Bl -> Swap
  | 0x2Cl -> Pushstring(consume_u30 ())
  | 0x2Dl -> Pushint(consume_u30 ())
  | 0x2El -> Pushuint(consume_u30 ())
  | 0x2Fl -> Pushdouble(consume_u30 ())
  | 0x30l -> Pushscope
  | 0x31l -> Pushnamespace(consume_u30 ())
  | 0x32l -> 
    let object_reg = consume_u30 () in
    let index_reg  = consume_u30 () in
    Hasnext2(object_reg,index_reg)
  | 0x40l -> Newfunction(consume_u30 ())
  | 0x41l -> Call(consume_u30 ())
  | 0x42l -> Construct(consume_u30 ())
  | 0x43l -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callmethod(index, arg_count)
  | 0x44l -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callstatic(index, arg_count)
  | 0x45l -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callsuper(index, arg_count)
  | 0x46l -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callproperty(index, arg_count)
  | 0x47l -> Returnvoid
  | 0x48l -> Returnvalue
  | 0x49l -> Constructsuper(consume_u30 ())
  | 0x4Al -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Constructprop(index, arg_count)
  | 0x4Cl -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callproplex(index, arg_count)
  | 0x4El -> 
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callsupervoid(index, arg_count)
  | 0x4Fl ->
    let index = consume_u30 () in
    let arg_count = consume_u30 () in
    Callpropvoid(index, arg_count)
  | 0x55l -> Newobject(consume_u30 ())
  | 0x56l -> Newarray(consume_u30 ())
  | 0x57l -> Newactivation
  | 0x58l -> Newclass(consume_u30 ())
  | 0x59l -> Getdescendants(consume_u30 ())
  | 0x5Al -> Newcatch(consume_u30 ())
  | 0x5Dl -> Findpropstrict(consume_u30 ())
  | 0x5El -> Findproperty(consume_u30 ())
  | 0x60l -> Getlex(consume_u30 ())
  | 0x61l -> Setproperty(consume_u30 ())
  | 0x62l -> Getlocal(consume_u30 ())
  | 0x63l -> Setlocal(consume_u30 ())
  | 0x64l -> Getglobalscope
  | 0x65l -> Getscopeobject
  | 0x66l -> Getproperty(consume_u30 ())
  | 0x68l -> Initproperty(consume_u30 ())
  | 0x6Al -> Deleteproperty(consume_u30 ())
  | 0x6Cl -> Getslot(consume_u30 ())
  | 0x6Dl -> Setslot(consume_u30 ())
  | 0x6El -> Getglobalslot(consume_u30 ())
  | 0x6Fl -> Setglobalslot(consume_u30 ())
  | 0x70l -> Convert_s
  | 0x71l -> Esc_xelem
  | 0x72l -> Esc_xattr
  | 0x73l -> Convert_i
  | 0x74l -> Convert_u
  | 0x75l -> Convert_d
  | 0x76l -> Convert_b
  | 0x77l -> Convert_o
  | 0x78l -> Checkfilter
  | 0x80l -> Coerce(consume_u30 ())
  | 0x82l -> Coerce_a
  | 0x85l -> Coerce_s
  | 0x86l -> Astype(consume_u30 ())
  | 0x87l -> Astypelate
  | 0x90l -> Negate
  | 0x91l -> Increment
  | 0x92l -> Inclocal(consume_u30 ())
  | 0x93l -> Decrement
  | 0x94l -> Declocal(consume_u30 ())
  | 0x95l -> Typeof
  | 0x96l -> Not
  | 0x97l -> Bitnot
  | 0xA0l -> Add
  | 0xA1l -> Subtract
  | 0xA2l -> Multiply
  | 0xA3l -> Divide
  | 0xA4l -> Modulo
  | 0xA5l -> Lshift
  | 0xA6l -> Rshift
  | 0xA7l -> Urshift
  | 0xA8l -> Bitand
  | 0xA9l -> Bitor
  | 0xAAl -> Bitxor
  | 0xABl -> Equals
  | 0xACl -> Strictequals
  | 0xADl -> Lessthan
  | 0xAEl -> Lessequals
  | 0xAFl -> Greaterequals (* Bug in the manual, same opcode *)
  | 0xAFl -> Greaterthan   (* Bug in the manual, same opcode *)
  | 0xB1l -> Instanceof
  | 0xB2l -> Istype(consume_u30 ())
  | 0xB3l -> Istypelate
  | 0xB4l -> In
  | 0xC0l -> Increment_i
  | 0xC1l -> Decrement_i
  | 0xC2l -> Inclocal_i(consume_u30 ())
  | 0xC3l -> Declocal_i(consume_u30 ())
  | 0xC4l -> Negate_i
  | 0xC5l -> Add_i
  | 0xC6l -> Subtract_i
  | 0xC7l -> Multiply_i
  | 0xD0l -> Getlocal_0
  | 0xD1l -> Getlocal_1
  | 0xD2l -> Getlocal_2
  | 0xD3l -> Getlocal_3
  | 0xD4l -> Setlocal_0
  | 0xD5l -> Setlocal_1
  | 0xD6l -> Setlocal_2
  | 0xD7l -> Setlocal_3
  (* "unsigned bytes" for 1,3, thanks again *)
  | 0xEFl -> 
    let debug_type = consume_u8 () in
    let index = consume_u30 () in
    let reg = consume_u8 () in
    let extra = consume_u30 () in
    Debug(debug_type,index,reg,extra)
  | 0xF0l -> Debugline(consume_u30 ())
  | 0xF1l -> Debugfile(consume_u30 ())
  | x -> raise (InvalidOpcode(x))