external get_screen_ea    : unit -> int32 = "IDAOCaml_get_screen_ea"
external set_cmt          : int32 -> string -> bool -> unit = "IDAOCaml_set_cmt"
external set_xterior_cmt  : int32 -> string -> bool -> unit = "IDAOCaml_set_xterior_cmt"
external set_func_cmt     : int32 -> string -> bool -> unit = "IDAOCaml_set_func_cmt"
external imsg             : string -> unit = "IDAOCaml_msg"
external iwarning         : string -> unit = "IDAOCaml_warning"
external find_func_begin  : int32 -> int32 option = "IDAOCaml_find_func_begin"
external get_byte         : int32 -> int32 = "IDAOCaml_get_byte"
external put_byte         : int32 -> int32 -> bool = "IDAOCaml_put_byte"
external jumpto           : int32 -> bool  = "IDAOCaml_jumpto"
external get_mem_byte     : int32 -> int32 = "IDAOCaml_get_mem_byte"
external add_ocaml_hotkey : string -> int -> bool = "IDAOCaml_add_ocaml_hotkey"
external del_ocaml_hotkey : string -> bool = "IDAOCaml_del_ocaml_hotkey"
external step_into        : unit -> bool = "IDAOCaml_step_into"

let set_anterior_cmt  ea s = set_xterior_cmt ea s true
let set_posterior_cmt ea s = set_xterior_cmt ea s false
let msg fmt_etc = Printf.ksprintf imsg fmt_etc
let warning fmt_etc = Printf.ksprintf iwarning fmt_etc

let get_many_bytes beg ending =
  if Int32.compare beg ending >= 0 then invalid_arg "Ida.get_many_bytes";
  let rec aux ea list =
    if ea = ending
    then list
    else aux (Int32.succ ea) ((get_byte ea)::list)
  in
  List.rev (aux beg [])
  
let get_word ea =
  let bl,bh = get_byte ea,get_byte (Int32.succ ea) in
  Int32.logor (Int32.shift_left bh 8) bl

let get_dword ea =
  let wl,wh = get_word ea,get_word (Int32.add ea 2l) in
  Int32.logor (Int32.shift_left wh 16) wl

(*List.iter (Ida.msg "%02lx ") (get_many_bytes 0xCl 0x52l);;*)