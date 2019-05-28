open DataStructures
open IOPair

(* Given an i/o structure, get the ESP differential as an option *)
let determine_esp_delta io =
  let esp = X86ToIRUtil.vEsp in
  let open IR in
  match ht_find_opt io.post esp, ht_find_opt io.pre esp with
  | Some(Const(inval,TypeReg_32)),Some(Const(outval,TypeReg_32)) -> 
    Some(Int64.to_int32 (Int64.sub inval outval))
  | _,_ -> None
  
(* Given a list of i/o structures, get the ESP differential across all of them
   as an option *)
let determine_aggregate_esp_delta ioc =
  let rec aux acc = function 
  | x::xs ->	
      let res = determine_esp_delta x.val32 in
     (match res,acc with
      | Some(i32),Some(acc) when i32 = acc -> aux res xs
      | Some(_),None -> aux res xs
      | _,_ -> res)
  | [] -> acc
  in aux (None) ioc.l_io

(* Ensure that esp.post = esp.pre + disp32 *)
let verify_stack_differential tbl disp32 =
  let open VerifyUtil in
  IRUtil.mk_ne (post tbl vesp) (IRUtil.mk_add eesp (irdw_of_i32 disp32))

(* Reject a sequence immediately if ESP-delta is not:
 * * consistent
 * * positive, greater than or equal to 4
 * * a multiple of 4
 *)
let determine_and_verify_esp_delta verify ssa_tbl ioc =
  match determine_aggregate_esp_delta ioc with
  | Some(disp32) -> 
    if (Int32.logand 3l disp32 <> 0l) || (disp32 <= 0l)
    then None
    else
     (if (verify (verify_stack_differential ssa_tbl disp32))
      then Some(disp32)
      else None)
  | None -> None