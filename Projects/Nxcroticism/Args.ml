let relocpairs = ref []
let noreloc = ref []

(* Argument parsing *)
let l_argument_specifiers = 
[
  ("-modulereloc", 
    Arg.Tuple(
      let addr = ref 0L in
      [
        Arg.String (fun a -> addr := Int64.of_string a);
        Arg.String (fun a -> relocpairs := (!addr,a)::(!relocpairs))
      ]),
    ": <baseaddress> <path to executable object> Address plus executable (relocated)"
  );
  ("-module", 
    Arg.String (fun a -> noreloc := a::(!noreloc)),
    ": <path to executable object> Executable (not relocated)"
  )
]

let usage_message = 
  Printf.sprintf
    "Usage: %s [-modulereloc <new base address> <path to executable>]* [-module <path to executable>]*"
    (Filename.basename Sys.argv.(0))

let get_args () =
  let _ = relocpairs := [] in
  let _ = noreloc := [] in
  let _ = 
    Arg.parse 
      l_argument_specifiers
     (fun x -> raise(Arg.Bad("Unknown argument: '"^x^"'"))) 
      usage_message 
  in
  let r = !relocpairs in
  let n = !noreloc in
  relocpairs := [];
  noreloc := [];
 (n,r)