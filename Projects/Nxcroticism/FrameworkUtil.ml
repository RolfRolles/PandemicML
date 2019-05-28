let logfile = open_out "c:\\temp\\log.txt";;
(*let rf_printf = ref (fun fmt_etc -> Printf.fprintf logfile fmt_etc);;*)

let f_printf fmt_etc = Printf.fprintf logfile fmt_etc;;
let f_printf fmt_etc = Printf.printf fmt_etc;;
(*let f_printf fmt_etc = IDA.msg fmt_etc;;*)