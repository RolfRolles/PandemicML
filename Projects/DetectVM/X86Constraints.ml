open X86TypeCheck

type opnd_size_constraint =
| OpNone
| Op12SizeEq
| Op123SizeEq
| Op12GvMa
| Op12GvMp
| Op12GzMp

type opnd_addr_constraint =
| AddrNone
| Addr12SizeEq

let get_op1_op2_size = function
| o1::o2::_ -> (size_of_operand o1,size_of_operand o2)
| _ -> failwith "get_op1_op2_size:  Not enough operands"

let get_op1_op2_op3_size = function
| o1::o2::o3::_ -> (size_of_operand o1,size_of_operand o2,size_of_operand o3)
| _ -> failwith "get_op1_op2_op3_size:  Not enough operands"

let get_op1_op2_addr = function
| o1::o2::_ -> (addr_of_operand o1,addr_of_operand o2)
| _ -> failwith "get_op1_op2_addr:  Not enough operands"

let satisfying_size_configurations2 = function
| Op12SizeEq     -> [OPSZ_16,OPSZ_16;OPSZ_32,OPSZ_32]
| Op12GvMa       -> [OPSZ_16,OPSZ_32;OPSZ_32,OPSZ_64]
| Op12GvMp       -> [OPSZ_16,OPSZ_32;OPSZ_32,OPSZ_48]
| Op12GzMp       -> [OPSZ_16,OPSZ_32;OPSZ_32,OPSZ_48] (* duplicated from above *)
| _ -> invalid_arg "satisfying_size_configurations2"

let satisfying_addr_configurations2 = function
| Addr12SizeEq     -> [OPSZ_16,OPSZ_16;OPSZ_32,OPSZ_32]
| _ -> invalid_arg "satisfying_addr_configurations2"

let satisfying_size_configurations3 = function
| Op123SizeEq -> [OPSZ_16,OPSZ_16,OPSZ_16;OPSZ_32,OPSZ_32,OPSZ_32]
| _ -> invalid_arg "satisfying_size_configurations3"

let satisfies_size_constraints oplist c = match c with
| OpNone      -> true
| Op12SizeEq
| Op12GvMa
| Op12GvMp
| Op12GzMp    -> List.mem (get_op1_op2_size     oplist) (satisfying_size_configurations2 c)
(* duplicated from above *)
| Op123SizeEq -> List.mem (get_op1_op2_op3_size oplist) (satisfying_size_configurations3 c)

let satisfies_addr_constraints oplist c = match c with
| AddrNone     -> true
| Addr12SizeEq -> List.mem (get_op1_op2_addr oplist) (satisfying_addr_configurations2 c)

let satisfies_constraints oplist sc ac = 
  satisfies_size_constraints oplist sc && 
  satisfies_addr_constraints oplist ac