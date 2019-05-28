module Z3 = Z3.V3

let mk_var ctx name ty = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) ty
let ctx     = mk_context [||]

(* Array of 256 8-byte variables *)
let mcvars   = Array.init 256 (fun i -> IR.Variable(i,IR.TypeReg_8));;

let get_byte eEip =
  let open IRUtil in
  ArrayUtil.fold_righti 
    (fun i arrel acc -> mk_ite (mk_eq (mk_byte i) eEip) (mk_evar arrel) acc)
     mcvars
    (mk_evar mcvars.(255))
     
let simpleMCsimulate eEip state =
  let open IRUtil in
  let mk_addr i = mk_add eEip (mk_byte i) in
  let eEipP1,eEipP2,eEipP5 = mk_addr 1,    mk_addr 2,      mk_addr 5       in
  let eByte0,eByte1,eByte2 = get_byte eEip,get_byte eEipP1,get_byte eEipP2 in
  let eByte3,eByte4        = get_byte (mk_addr 3),get_byte (mk_addr 4)     in
  let eTop2 = mk_and eByte0 (mk_byte 0xC0) in
  let eXorRegReg =
    mk_ite (mk_eq eTop2 (mk_byte 0x00)
