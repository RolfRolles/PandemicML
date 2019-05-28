open FrameworkUtil
open DataStructures

type quantized_memory_location = IR.var * int32
type s_structured_memory_location = VarInt32Set.t

(* The result of interpreting the IR in a given context. *)
type io =
{
  (* The 32-bit registers before emulation. *)
  pre:  (IR.var,IR.expr) Hashtbl.t;
  
  (* The 32-bit registers after emulation. *)
  post: (IR.var,IR.expr) Hashtbl.t;
  
  (* Value -> variables containing that value after emulation. *)
  rev_post: (IR.expr,IR.var list) Hashtbl.t;
  
  (* Register context. (variable, value32) list. Not strictly necessary. *)
  register_data: (IR.var * int32) list;

  (* Values read from memory during emulation. (address, candidate set, value) list *)
  reads: (int32 * s_structured_memory_location * int32) list;

  (* Values written to memory during emulation. (address, candidate set, value) list *)
  writes: (int32 * s_structured_memory_location * int32) list;
}


let print_var_expr_ht = 
  Hashtbl.iter (fun v e -> 
    f_printf "%s -> %s\n" (PpIR.ppVar v) (PpIR.ppExpr true e))

let print_memaccess_set =
  VarInt32Set.iter (fun (v,i32) -> f_printf "[%s.pre+0x%lx] " (PpIR.ppVar v) i32)

let print_memaccess_list =
  List.iter (fun (a32,set,v32) ->
    f_printf "[0x%lx] -> 0x%lx (candidates = {" a32 v32;
    print_memaccess_set set;
    f_printf "})\n")

(* Needs refactoring since the addition of register_data and stack_value *)
let print_io io =
  f_printf "Dumping I/O pair\n\n";
  f_printf "pre context:\n";
  print_var_expr_ht io.pre;
  f_printf "\npost context:\n";
  print_var_expr_ht io.post;
  f_printf "\nmemory reads:\n";
  print_memaccess_list io.reads;
  f_printf "\nmemory writes:\n";
  print_memaccess_list io.writes

type io_container =
{
  (* A list of io structures for the randomized runs *)
  l_io: io triplicate list;
  
  (* Supposed stack displacement for the return address *)
  retaddr_displacement_opt: int32 option;

  (* The set of memory locations read, in terms of [reg32+disp32] *)
  common_reads: s_structured_memory_location triplicate;
  
  (* The set of memory locations written, in terms of [reg32+disp32] *)
  common_writes: s_structured_memory_location triplicate;

  (* The set of memory locations that were both read AND written *)
  common_readwrites: s_structured_memory_location triplicate;
}

let print_io_container ioc =
  let _ = f_printf "\nCommon reads (8-bit): " in
  print_memaccess_set ioc.common_reads.val8;
  let _ = f_printf "\nCommon reads (16-bit): " in
  print_memaccess_set ioc.common_reads.val16;
  let _ = f_printf "\nCommon reads (32-bit): " in
  print_memaccess_set ioc.common_reads.val32;
  let _ = f_printf "\nCommon writes (8-bit): " in
  print_memaccess_set ioc.common_writes.val8;
  let _ = f_printf "\nCommon writes (16-bit): " in
  print_memaccess_set ioc.common_writes.val16;
  let _ = f_printf "\nCommon writes (32-bit): " in
  print_memaccess_set ioc.common_writes.val32;
  let _ = f_printf "\nCommon read/writes (8-bit): " in
  print_memaccess_set ioc.common_readwrites.val8;
  let _ = f_printf "\nCommon read/writes (16-bit): " in
  print_memaccess_set ioc.common_readwrites.val16;
  let _ = f_printf "\nCommon read/writes (32-bit): " in
  print_memaccess_set ioc.common_readwrites.val32;
  f_printf "\n";
  List.iter (print_triplicate print_io) ioc.l_io;
  f_printf "\n%!"
