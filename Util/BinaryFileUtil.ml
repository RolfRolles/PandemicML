(* Belongs in a BinaryFileUtil module *)
let transformed_int_array_of_filename f fname = 
  let fh = open_in_bin fname in
  let rl_i32 = ref [] in
  let add i32 = rl_i32 := i32::(!rl_i32) in
  let rec aux () =
    let _ = add (f (input_byte fh)) in
    aux ()
  in 
    try
      aux ()
    with End_of_file ->
      let _ = close_in fh in
      Array.of_list (List.rev !rl_i32)

let int32_array_of_filename = transformed_int_array_of_filename Int32.of_int
let   int_array_of_filename = transformed_int_array_of_filename (fun x -> x)