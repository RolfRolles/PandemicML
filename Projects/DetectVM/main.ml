(* Features to use:
   * Byte-value distribution (256)
   * Byte-value entropy (1)
   * Mnemonic (560)
   * Mnemonic entropy (1)
   * Intel canonicalization (< 1000)
   * Intel entropy (1)
   * Size (1) -- probably remove this one
   * Percentage of decoded instructions that are single-byte opcodes (1)
   * Percentage of decoded instructions that are double-byte opcodes (1)
   * Percentage of decoded instructions that are 0x0F 0x38 opcodes (1)
   * Percentage of decoded instructions that are 0x0F 0x3A opcodes (1)
   * Entropy of category distribution (1)
   * Instruction length distribution (15)
   * Length entropy (1)
   
   Total < 2000
   Squared < 3,218,436
   
   If I can generate one sample per second, that is 86400 samples/day.
   That would be 38.21 computer-days, or 917.08 computer-hours.
   At $0.12/hr, that would be $110.05.
  
   Generate a new VMProtect sample.  Extract its last section as a byte array.
   Feed it into the above generating procedures and print it

*)

type features = 
{
  byte_distribution: float array;     (* 256 f *)
  byte_distribution_entropy:  float;  (* 1   f *)
  mnemonicno: float array;            (* 560 f *)
  mnemonic_entropy: float;            (* 1   f *)
  intel: float array;                 (* ??? f *)
  intel_entropy: float;               (* 1   f *)
  length: float array;                (* 15  f *)
  length_entropy: float;              (* 1   f *)
  category: float array;              (* 4   f *)
  category_entropy: float;            (* 1   f *)
  size: int;                          (* 0   d -- UNUSED *)
  (* ----------------------------------------- *)
                                     (* 1814 f *)
}

let num_mnems = Array.length X86FeatureUtil.x86_mnem_arr
let num_encodings,encodings_map = X86FeatureUtil.make_canonical_map ()
let number_encoding e = Hashtbl.find encodings_map e

(* binary = 1.44269504088896340735992468 *)
let entropy_of_histogram_array histogram sample_size = 
  let d1log2 = 1.0 /. (log (float_of_int (Array.length histogram))) in
  Array.fold_left (fun entropy byte_dist_ent -> 
    let proportion = (float_of_int byte_dist_ent) /. (float_of_int sample_size) in
 	  if proportion > 0.0 then entropy -. (proportion *. (log proportion) *. d1log2) else entropy)
    0.0
    histogram
    
(*
    | 0xF0l -> Some({ ctxt with group1pf   = (Lock )::(ctxt.group1pf) })
    | 0xF2l -> Some({ ctxt with group1pf   = (Repne)::(ctxt.group1pf) })
    | 0xF3l -> Some({ ctxt with group1pf   = (Rep  )::(ctxt.group1pf) })
    | 0x2El -> Some({ ctxt with segmentpf  = (CS)::(ctxt.segmentpf) })
    | 0x36l -> Some({ ctxt with segmentpf  = (SS)::(ctxt.segmentpf) })
    | 0x3El -> Some({ ctxt with segmentpf  = (DS)::(ctxt.segmentpf) })
    | 0x26l -> Some({ ctxt with segmentpf  = (ES)::(ctxt.segmentpf) })
    | 0x64l -> Some({ ctxt with segmentpf  = (FS)::(ctxt.segmentpf) })
    | 0x65l -> Some({ ctxt with segmentpf  = (GS)::(ctxt.segmentpf) })
    | 0x66l -> Some({ ctxt with opsizepf   = (PF3_OpSize)  ::(ctxt.opsizepf)   })
    | 0x67l -> Some({ ctxt with addrsizepf = (PF4_AddrSize)::(ctxt.addrsizepf) })
*)
let map_into_features section =
  let get_section_byte i = section.(Int32.to_int i) in
  (* Initialize disassembler object to read bytes from section *)
  let _ = X86DecodeCanonical.init get_section_byte in
  let rec skip_prefixes ea = 
    match get_section_byte ea with
    | 0xF0l | 0xF2l | 0xF3l | 0x2El | 0x36l | 0x3El | 0x26l | 0x64l | 0x65l | 0x66l | 0x67l -> skip_prefixes (Int32.succ ea)
    | _ -> ea
  in
  let make_syntactic_instruction_designator ea =
    match get_section_byte ea with
    | 0x0Fl ->
     (match get_section_byte (Int32.succ ea) with
      | 0x38l -> 2
      | 0x3Al -> 3
      | _     -> 1)
    | _ -> 0
  in
  let byte_distribution = Array.make 256 0 in
  let   mnem_array = Array.make num_mnems 0 in
  let  intel_array = Array.make num_encodings 0 in  
  let length_array = Array.make 15 0 in
  let category_array = Array.make 4 0 in
  let num_disasm = ref 0 in

  let incr_mnem mnem  = let idx = X86FeatureUtil.number_mnem mnem in mnem_array.(idx) <- mnem_array.(idx) + 1 in
  let incr_intel enc  = let idx = number_encoding enc in intel_array.(idx) <- intel_array.(idx) + 1 in
  let incr_length l   = length_array.(l-1) <- length_array.(l-1) + 1 in
  let incr_category c = category_array.(c) <- category_array.(c) + 1 in
  let incr_instruction_counters ea l m aol =
    let _ = incr num_disasm in
    let _ = incr_category (make_syntactic_instruction_designator (skip_prefixes ea)) in
    let _ = incr_length l in
    let _ = incr_intel (m,aol) in
    incr_mnem m
  in
  let incr_byte_distribution bv = let idx = Int32.to_int bv in byte_distribution.(idx) <- (byte_distribution.(idx)) + 1 in
  let rec bruteforce_disassemble_loop ea =
    if ea >= Int32.of_int (Array.length section)
    then ()
    else 
      let _ = incr_byte_distribution (get_section_byte ea) in
      let _ =
        try 
          let i,len,_,aol = X86DecodeCanonical.decode ea in
          let (m,_) = let open X86 in i.instr in
          incr_instruction_counters ea len m aol;
          ()
        with _ -> ()
      in        
      bruteforce_disassemble_loop (Int32.succ ea)
  in
  let _ = bruteforce_disassemble_loop 0l in
  let make_distribution_array intarray num_elements = Array.map (fun f -> (float_of_int f) /. (float_of_int num_elements)) intarray in
  { byte_distribution = make_distribution_array byte_distribution (Array.length section);
    byte_distribution_entropy = (*ArrayUtil.*)entropy_of_histogram_array byte_distribution (Array.length section);
    mnemonicno = make_distribution_array mnem_array !num_disasm;
    mnemonic_entropy = (*ArrayUtil.*)entropy_of_histogram_array mnem_array !num_disasm;
    intel = make_distribution_array intel_array !num_disasm;
    intel_entropy = (*ArrayUtil.*)entropy_of_histogram_array intel_array !num_disasm;
    length = make_distribution_array length_array !num_disasm;
    length_entropy = entropy_of_histogram_array length_array !num_disasm;
    category = make_distribution_array category_array !num_disasm;
    category_entropy = entropy_of_histogram_array category_array !num_disasm;
    size = Array.length section }

exception Done of int32 list
let read_textual_section file =
  let f = open_in file in
  let rec process_line outl = 
    let l = try input_line f with End_of_file -> "" in
    if l = "" then List.rev outl
    else
     (let make_byte s pos = Int32.of_string ("0x"^String.sub s pos 2) in
      let rec make_bytes list s pos =
        if pos >= String.length s
        then list
        else make_bytes ((make_byte s pos)::list) s (pos+2)
      in
      process_line (make_bytes outl l 0))
  in process_line []


let generate_new_vmprotect_example () = 
  let fh = open_in_bin "vmp.bin" in
  let rec aux list =
    let byte = try Int32.of_int (input_byte fh) with End_of_file -> raise (Done(list)) in
    aux (byte::list)
  in
  try aux []; failwith "generate_new_vmprotect_example" with Done(l) -> Array.of_list(List.rev l)

let main () =
  let number_of_iterations = int_of_string (*Sys.argv.(1)*) "1" in
  
  let feature_vector_file = open_out_gen [Open_append] 0 "results.mat" in
  
  let write_feature_vector file feature = 
    let print_float = Printf.fprintf file "%.16f\t" in
  (*let print_int   = Printf.fprintf file "%d\n" in*)
    let _ = print_float feature.byte_distribution_entropy in
    let _ = print_float feature.mnemonic_entropy in
    let _ = print_float feature.intel_entropy in
    let _ = print_float feature.length_entropy in
    let _ = print_float feature.category_entropy in
  (*let _ = print_int feature.size in*)
    let _ = Array.iter print_float feature.byte_distribution in
    let _ = Array.iter print_float feature.mnemonicno in
    let _ = Array.iter print_float feature.intel in
    let _ = Array.iter print_float feature.length in
    let _ = Array.iter print_float feature.category in
    let _ = Printf.fprintf file "\n" in
    ()
  in
  let write_feature_vector = write_feature_vector feature_vector_file in
  let rec train i =
    let _ = Printf.printf "%d: Reading data\n" i in
    let section = Array.of_list (read_textual_section Sys.argv.(1)) in (*generate_new_vmprotect_example () in*)
  (*Printf.printf "%d: %08lx\n" (Array.length section) (section.(0));*)
    Printf.printf "  Harvesting features\n";
    let features = map_into_features section in
    Printf.printf "  Writing features\n";    
    let _ = write_feature_vector features in
    if i < number_of_iterations 
    then train (i+1)
    else ()

  in train 1
  
let _ = main ()