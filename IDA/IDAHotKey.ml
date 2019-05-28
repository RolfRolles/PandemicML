(* IDAHotKey.ml *)
let int2fn  = Hashtbl.create 16
let str2int = Hashtbl.create 16
let num  = ref 0

let invoke i = 
  let fn = try Hashtbl.find int2fn i with Not_found -> (fun () -> ()) in
  fn ()
  
let _ = Callback.register "HotkeyCallback" invoke
  
let register hk fn = 
  let b = IDA.add_ocaml_hotkey hk !num in
  Hashtbl.replace int2fn !num fn;
  Hashtbl.replace str2int hk !num;
  incr num;
  b

let unregister hk = 
  let i = try Hashtbl.find str2int hk with Not_found -> ~-1 in
  Hashtbl.remove str2int hk;
  Hashtbl.remove int2fn i;
  ignore(IDA.del_ocaml_hotkey hk)