open AVM2Pervasives

(* mif = method_info flags *)
let mif_NEED_ARGUMENTS  = 0x01l
let mif_NEED_ACTIVATION = 0x02l
let mif_NEED_REST       = 0x04l
let mif_HAS_OPTIONAL    = 0x08l
let mif_SET_DXNS        = 0x40l
let mif_HAS_PARAM_NAMES = 0x80l

type t = 
{ param_count: u30;
  return_type: u30;
  param_type: u30 array;
  name: u30;
  flags: u30;
  options: AVM2option_info.t option;
  param_names: AVM2param_info.t option;
}

val parse : prim_parser -> t