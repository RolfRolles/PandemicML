type access_flags =
| ACC_PUBLIC       (* 0x0001 *)
| ACC_PRIVATE      (* 0x0002 *)
| ACC_PROTECTED    (* 0x0004 *)
| ACC_STATIC       (* 0x0008 *)
| ACC_FINAL        (* 0x0010 *)
| ACC_SUPER        (* 0x0020 *) (* OVERLOADED, ALSO MEANS SYNCHRONIZED FOR A METHOD *)
| ACC_VOLATILE     (* 0x0040 *) (* OVERLOADED, ALSO MEANS BRIDGE FOR A METHOD *)
| ACC_TRANSIENT    (* 0x0080 *) (* OVERLOADED, ALSO MEANS VARARGS FOR A METHOD *)
| ACC_NATIVE       (* 0x0100 *)
| ACC_INHERITANCE  (* 0x0200 *) (* OVERLOADED, ALSO MEANS INTERFACE FOR A NESTED CLASS *)
| ACC_ABSTRACT     (* 0x0400 *)
| ACC_STRICT       (* 0x0800 *)
| ACC_SYNTHETIC    (* 0x1000 *)
| ACC_ANNOTATION   (* 0x2000 *)
| ACC_ENUM         (* 0x4000 *)

type constant_pool_tag = 
| CONSTANT_Class
| CONSTANT_Fieldref
| CONSTANT_Methodref
| CONSTANT_InterfaceMethodref
| CONSTANT_String
| CONSTANT_Integer
| CONSTANT_Float
| CONSTANT_Long
| CONSTANT_Double
| CONSTANT_NameAndType
| CONSTANT_Utf8

let constant_pool_tag_of_int = function
| 1  -> CONSTANT_Utf8
| 3  -> CONSTANT_Integer
| 4  -> CONSTANT_Float
| 5  -> CONSTANT_Long
| 6  -> CONSTANT_Double
| 7  -> CONSTANT_Class
| 8  -> CONSTANT_String
| 9  -> CONSTANT_Fieldref
| 10 -> CONSTANT_Methodref
| 11 -> CONSTANT_InterfaceMethodref
| 12 -> CONSTANT_NameAndType

type constant_entry =
| CONSTANT_Class_info              of int               (* name_index *)
| CONSTANT_Fieldref_info           of int   * int       (* class_index, name_and_type_index *)
| CONSTANT_Methodref_info          of int   * int       (* class_index, name_and_type_index *)
| CONSTANT_InterfaceMethodref_info of int   * int       (* class_index, name_and_type_index *)
| CONSTANT_String_info             of int               (* string_index *)
| CONSTANT_Integer_info            of int32             (* bytes *)
| CONSTANT_Float_info              of int32             (* bytes *)
| CONSTANT_Long_info               of int32 * int32     (* high_bytes, low_bytes *)
| CONSTANT_Double_info             of int32 * int32     (* high_bytes, low_bytes *)
| CONSTANT_NameAndType_info        of int   * int       (* name_index, descriptor_index *)
| CONSTANT_Utf8_info               of int   * int array (* length * bytes *)

type field_info = {
  access_flags: access_flags list;
  name_index: int;
  descriptor_index: int;
  attributes_count: int;
  attributes: attribute_info array;
}

type 'a attribute = {
  attribute_name_index: int;
  attribute_length: int32;
  attribute: 'a
}

type constantvalue = { constantvalue_index: int } (* cp index specifying a long/float/double/integer/string *)

type constantvalue_attribute = constantvalue attribute

type jexception = {
  int start_pc;
  int end_pc;
  int handler_pc;
  int catch_type;
}

type exception_attribute = jexception attribute
  
type code = {
  max_stack: int;
  max_locals: int;
  code_length: int32;
  code: int array;
  exception_table_length: int;
  exception_table: jexception array;
  attributes_count: int;
  attributes: attribute_info array; (* mutually recursive? *)
}

type code_attribute = code attribute

type classes = {
  inner_class_info_index: int;
  outer_class_info_index: int;
  inner_name_index: int;
  inner_class_access_flags: int; (* Change this to a list? *)
}

type classes_attribute = classes attribute

type innerclasses = {
  number_of_classes: int;
  classes: classes array;
}

type innerclasses_attribute = innerclases attribute
type synthetic_attribute = unit attribute
type sourcefile = { sourcefile_index: int }
type sourcefile_attribute = sourcefile attribute

type line_number_table = {
  start_pc: int;
  line_number: int;
}

type linenumbertable = {
  line_number_table_length: int;
  line_number_table: line_number_table array
}

type local_variable_table = {
  start_pc: int;
  length: int;
  name_index: int;
  descriptor_index: int;
  index: int;
}

type localvariabletable = {
  local_variable_table_length: int;
  local_variable_table: local_variable_table array;
}

type localvariabletable_attribute = localvariabletable attribute

type deprecated_attribute = unit attribute  

type class_file = {
  magic: int32;
  minor_version: int;
  major_version: int;
  constant_pool_count: int;
  constant_pool: ??? array ???;
  access_flags: access_flags list;
  this_class: int;  (* Index into constant pool *)
  super_class: int; (* Index into constant pool or zero if none *)
  interfaces_count: int;
  interfaces: ??? array ???;
  fields_count: int;
  fields: ??? array ???;
  methods_count: int;
  methods: ??? array ???;
  attributes_count: int;
  attributes: ??? array ???;
}

type array_type = 
| T_BOOLEAN
| T_CHAR   
| T_FLOAT  
| T_DOUBLE 
| T_BYTE   
| T_SHORT  
| T_INT    
| T_LONG   


let array_type_of_int = function
| 4l  -> T_BOOLEAN
| 5l  -> T_CHAR
| 6l  -> T_FLOAT
| 7l  -> T_DOUBLE
| 8l  -> T_BYTE
| 9l  -> T_SHORT
| 10l -> T_INT
| 11l -> T_LONG
| x -> raise (InvalidArrayType x)

type jvm_instr = 
| Aaload
| Aastore
| Aload
| Aload_0
| Aload_1
| Aload_2
| Aload_3
| Anewarray of int
| Areturn
| Arraylength
| Astore of int
| Astore_0
| Astore_1
| Astore_2
| Astore_3
| Athrow
| Baload
| Bastore
| Caload
| Castore
| Checkcast of int
| D2f
| D2i
| D2l
| Dadd
| Daload
| Dastore
| Dcmpg
| Dcmpl
| Dconst_0
| Dconst_1
| Ddiv
| Dload of int
| Dload_0
| Dload_1
| Dload_2
| Dload_3
| Dmul
| Dneg
| Drem
| Dreturn
| Dstore of int
| Dstore_0
| Dstore_1
| Dstore_2
| Dstore_3
| Dsub
| Dup
| Dup_x1
| Dup_x2
| Dup2
| Dup2_x1
| Dup2_x2
| F2d
| F2i
| F2l
| Fadd
| Faload
| Fastore
| Fcmpg
| Fcmpl
| Fconst_0
| Fconst_1
| Fconst_2
| Fdiv
| Fload
| Fload_0
| Fload_1
| Fload_2
| Fload_3
| Fmul
| Fneg
| Frem
| Freturn
| Fstore
| Fstore_0
| Fstore_1
| Fstore_2
| Fstore_3
| Fsub
| Getfield of int
| Getstatic of int
| Goto of int
| Goto_w of int32
| I2b
| I2c
| I2d
| I2f
| I2l
| I2s
| Iadd
| Iaload
| Iand
| Iastore
| Iconst_m1
| Iconst_0
| Iconst_1
| Iconst_2
| Iconst_3
| Iconst_4
| Iconst_5
| Idiv
| If_acmpeq of int
| If_acmpne of int
| If_icmpeq of int
| If_icmpne of int
| If_icmplt of int
| If_icmpge of int
| If_icmpgt of int
| If_icmple of int
| Ifeq of int
| Ifne of int
| Iflt of int
| Ifge of int
| Ifgt of int
| Ifle of int
| Ifnonnull of int
| Ifnull of int
| Iinc of int * int
| Iload of int
| Iload_0
| Iload_1
| Iload_2
| Iload_3
| Imul
| Ineg
| Instanceof of int
| Invokeinterface of int * int * int
| Invokespecial of int
| Invokestatic of int
| Invokevirtual of int
| Ior
| Irem
| Ireturn
| Ishl
| Ishr
| Istore of int
| Istore_0
| Istore_1
| Istore_2
| Istore_3
| Isub
| Iushr
| Ixor
| Jsr of int
| Jsr_w of int32
| L2d
| L2f
| L2i
| Ladd
| Laload
| Land
| Lastore
| Lcmp
| Lconst_0
| Lconst_1
| Ldc of int
| Ldc_w of int
| Ldc2_w of int
| Ldiv
| Lload of int
| Lload_0
| Lload_1
| Lload_2
| Lload_3
| Lmul
| Lneg
| Lookupswitch of int32 * int32 * ???
| Lor
| Lrem
| Lreturn
| Lshl
| Lshr
| Lstore
| Lstore_0
| Lstore_1
| Lstore_2
| Lstore_3
| Lsub
| Lushr
| Lxor
| Monitorenter
| Monitorexit
| Multianewarray of int * int
| New of int
| Newarray of array_type
| Nop
| Pop
| Pop2
| Putfield of int
| Putstatic of int
| Ret of int
| Return



































let get_byte () = ???
let get_word () = 
  let hi = get_byte () in
  let lo = get_byte () in
  Int32.logor (Int32.shift_left hi  8) lo 
let get_dword () =
  let hi = get_word () in
  let lo = get_word () in
  Int32.logor (Int32.shift_left hi 16) lo 
let get_byte_int () = Int32.to_int (get_byte ())
let get_word_int () = Int32.to_int (get_word ())
  
let jvm_instr_of_stream () =
  match get_byte () with
  | 0x32l -> Aaload
  | 0x53l -> Aastore
  | 0x19l -> Aload
  | 0x2al -> Aload_0
  | 0x2bl -> Aload_1
  | 0x2cl -> Aload_2
  | 0x2dl -> Aload_3
  | 0xbdl -> Anewarray(get_word_int ())
  | 0xb0l -> Areturn
  | 0xbel -> Arraylength
  | 0x3al -> Astore(get_byte_int ())
  | 0x4bl -> Astore_0
  | 0x4cl -> Astore_1
  | 0x4dl -> Astore_2
  | 0x4el -> Astore_3
  | 0xbfl -> Athrow
  | 0x33l -> Baload
  | 0x54l -> Bastore
  | 0x10l -> Bipush(get_byte_int ())
  | 0x34l -> Caload
  | 0x55l -> Castore
  | 0xc0l -> Checkcast(get_word_int ())
  | 0x90l -> D2f
  | 0x8el -> D2i
  | 0x8fl -> D2l
  | 0x63l -> Dadd
  | 0x31l -> Daload
  | 0x52l -> Dastore
  | 0x98l -> Dcmpg
  | 0x97l -> Dcmpl
  | 0x0el -> Dconst_0
  | 0x0fl -> Dconst_1
  | 0x6fl -> Ddiv
  | 0x18l -> Dload(get_byte_int ())
  | 0x26l -> Dload_0
  | 0x27l -> Dload_1
  | 0x28l -> Dload_2
  | 0x29l -> Dload_3
  | 0x6bl -> Dmul
  | 0x77l -> Dneg
  | 0x73l -> Drem
  | 0xafl -> Dreturn
  | 0x39l -> Dstore(get_byte_int ())
  | 0x47l -> Dstore_0
  | 0x48l -> Dstore_1
  | 0x49l -> Dstore_2
  | 0x4al -> Dstore_3
  | 0x67l -> Dsub
  | 0x59l -> Dup
  | 0x5al -> Dup_x1
  | 0x5bl -> Dup_x2
  | 0x5cl -> Dup2
  | 0x5dl -> Dup2_x1
  | 0x5el -> Dup2_x2
  | 0x8dl -> F2d
  | 0x8bl -> F2i
  | 0x8cl -> F2l
  | 0x62l -> Fadd
  | 0x30l -> Faload
  | 0x51l -> Fastore
  | 0x96l -> Fcmpg
  | 0x95l -> Fcmpl
  | 0x0bl -> Fconst_0
  | 0x0cl -> Fconst_1
  | 0x0dl -> Fconst_2
  | 0x6el -> Fdiv
  | 0x17l -> Fload(get_byte_int ())
  | 0x22l -> Fload_0
  | 0x23l -> Fload_1
  | 0x24l -> Fload_2
  | 0x25l -> Fload_3
  | 0x6al -> Fmul
  | 0x76l -> Fneg
  | 0x72l -> Frem
  | 0xael -> Freturn
  | 0x38l -> Fstore(get_byte_int ())
  | 0x22l -> Fstore_0
  | 0x23l -> Fstore_1
  | 0x24l -> Fstore_2
  | 0x25l -> Fstore_3
  | 0x66l -> Fsub
  | 0xb4l -> Getfield(get_word_int ())
  | 0xb2l -> Getstatic(get_word_int ())
  | 0xa7l -> Goto(get_word_int ())
  | 0xc8l -> Goto_w(get_dword ())
  | 0x91l -> I2b
  | 0x92l -> I2c
  | 0x87l -> I2d
  | 0x86l -> I2f
  | 0x85l -> I2l
  | 0x93l -> I2s
  | 0x60l -> Iadd
  | 0x2el -> Iaload
  | 0x7el -> Iand
  | 0x4fl -> Iastore
  | 0x02l -> Iconst_m1
  | 0x03l -> Iconst_0
  | 0x04l -> Iconst_1
  | 0x05l -> Iconst_2
  | 0x06l -> Iconst_3
  | 0x07l -> Iconst_4
  | 0x08l -> Iconst_5
  | 0x6cl -> Idiv
  | 0xa5l -> If_acmpeq(get_word_int ())
  | 0xa6l -> If_acmpne(get_word_int ())
  | 0x9fl -> If_icmpeq(get_word_int ())
  | 0xa0l -> If_icmpne(get_word_int ())
  | 0xa1l -> If_icmplt(get_word_int ())
  | 0xa2l -> If_icmpge(get_word_int ())
  | 0xa3l -> If_icmpgt(get_word_int ())
  | 0xa4l -> If_icmple(get_word_int ())
  | 0x99l -> Ifeq(get_word_int ())
  | 0x9al -> Ifne(get_word_int ())
  | 0x9bl -> Iflt(get_word_int ())
  | 0x9cl -> Ifge(get_word_int ())
  | 0x9dl -> Ifgt(get_word_int ())
  | 0x9el -> Ifle(get_word_int ())
  | 0xc7l -> Ifnonnull(get_word_int ())
  | 0xc6l -> Ifnull(get_word_int ())
  | 0x84l -> let b1 = get_byte() in let b2 = get_byte() in Iinc(b1,b2)
  | 0x15l -> Iload(get_byte_int ())
  | 0x1al -> Iload_0
  | 0x1bl -> Iload_1
  | 0x1cl -> Iload_2
  | 0x1dl -> Iload_3
  | 0x68l -> Imul
  | 0x74l -> Ineg
  | 0xc1l -> Instanceof(get_word_int ())
  | 0xb9l -> 
    let b1 = get_word_int () in
    let b3 = get_byte() in 
    let b4 = get_byte() in
    Invokeinterface(b1,b3,b4)
  | 0xb7l -> Invokespecial(get_word_int ())
  | 0xb8l -> Invokestatic(get_word_int ())
  | 0xb6l -> Invokevirtual(get_word_int ())
  | 0x80l -> Ior
  | 0x70l -> Irem
  | 0xacl -> Ireturn
  | 0x78l -> Ishl
  | 0x7al -> Ishr
  | 0x36l -> Istore(get_byte_int ())
  | 0x3bl -> Istore_0
  | 0x3cl -> Istore_1
  | 0x3dl -> Istore_2
  | 0x3el -> Istore_3
  | 0x64l -> Isub
  | 0x7cl -> Iushr
  | 0x82l -> Ixor
  | 0xa8l -> Jsr(get_word_int ())
  | 0xc9l -> Jsr_w(get_dword ())
  | 0x8al -> L2d
  | 0x89l -> L2f
  | 0x88l -> L2i
  | 0x61l -> Ladd
  | 0x2fl -> Laload
  | 0x7fl -> Land
  | 0x50l -> Lastore
  | 0x94l -> Lcmp
  | 0x09l -> Lconst_0
  | 0x0al -> Lconst_1
  | 0x12l -> Ldc(get_byte_int ())
  | 0x13l -> Ldc_w(get_word_int ())
  | 0x14l -> Ldc2_w(get_word_int ())
  | 0x6dl -> Ldiv
  | 0x16l -> Lload(get_byte_int ())
  | 0x1el -> Lload_0
  | 0x1fl -> Lload_1
  | 0x20l -> Lload_2
  | 0x21l -> Lload_3
  | 0x69l -> Lmul
  | 0x75l -> Lneg
  | 0xabl -> 
    eat_padding ();
    let default = get_dword () in
    let npairs  = get_dword () in
    Lookupswitch(default,npairs,???)
  | 0x81l -> Lor
  | 0x71l -> Lrem
  | 0xadl -> Lreturn
  | 0x79l -> Lshl
  | 0x7bl -> Lshr
  | 0x37l -> Lstore
  | 0x3fl -> Lstore_0
  | 0x40l -> Lstore_1
  | 0x41l -> Lstore_2
  | 0x42l -> Lstore_3
  | 0x65l -> Lsub
  | 0x7dl -> Lushr
  | 0x83l -> Lxor
  | 0xc2l -> Monitorenter
  | 0xc3l -> Monitorexit
  | 0xc5l -> 
    let index = get_word_int () in
    let dim   = get_byte_int () in
    Multianewarray(index,dim)
  | 0xbbl -> New(get_word_int ())
  | 0xbcl -> Newarray(array_type_of_int (Int32.get_byte ()))
  | 0x00l -> Nop
  | 0x57l -> Pop
  | 0x58l -> Pop2
  | 0xb5l -> Putfield(get_word_int ())
  | 0xb3l -> Putstatic(get_word_int ())
  | 0xa9l -> Ret(get_byte_int ())
  | 0xb1l -> Return
  | 0x35l -> Saload
  | 0x56l -> Sastore
  | 0x11l -> Sipush(get_word_int ())
  | 0x5fl -> Swap
  | 0xaal -> 
    eat_padding ();
    let default = get_dword () in
    let low     = get_dword () in
    let high    = get_dword () in
    Lookupswitch(default,low,high,???)
  | 0xc4l -> ???