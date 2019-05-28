external cvoutput : string -> int -> int -> unit = "OCaml_WriteBuffer"
external cvflush  : unit -> unit = "OCaml_FlushBuffer"
external readernowait : string -> int -> int = "OCaml_ReaderCallbackNowait" 

let output s ofs len =
  if ofs < 0 || len < 0 || ofs > String.length s - len
  then invalid_arg "output"
  else cvoutput s ofs len

let ida_ppf = Format.make_formatter output cvflush
let ida_lb = Lexing.from_function readernowait

let inner_loop_init () = 
  Format.set_formatter_output_functions output cvflush;
  Format.fprintf ida_ppf "        Objective Caml version %s@.@." Config.version;
  Toploop.initialize_toplevel_env ();
  Location.input_name := "";
  Location.input_lexbuf := Some ida_lb

exception PPerror

let inner_loop () =
  let snap = Btype.snapshot () in
  try
    Lexing.flush_input ida_lb;
    Location.reset();
    let phr = try !Toploop.parse_toplevel_phrase ida_lb with Exit -> raise PPerror in
    ignore(Toploop.execute_phrase true ida_ppf phr)
  with
  | End_of_file -> ()
  | Sys.Break -> Format.fprintf ida_ppf "Interrupted.@."; Btype.backtrack snap
  | PPerror -> ()
  | x -> Errors.report_error ida_ppf x; Btype.backtrack snap
  
let _ = Callback.register "RolfInnerLoop" inner_loop
let _ = Callback.register "RolfInnerLoopInit" inner_loop_init
