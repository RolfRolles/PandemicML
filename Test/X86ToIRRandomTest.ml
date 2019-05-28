open X86
open X86ToIRUtil

let e i = { pref = []; instr = i }
(*let e i = i*)

let  input_struct_stack_pos = 4l
let output_struct_stack_pos = 8l

let input_lhs_pos = None
let input_rhs_pos = Some(4l)
let input_input3_pos = Some(8l)
let input_flags_pos = Some(12l)

let output_lhs_pos = None
let output_output2_pos = Some(4l)
let output_flags_pos = Some(8l)

let get_inout_structures = [
  e (Mov,[GeneralReg(Gd(Esi));Memexpr(Md(Mem32(SS,Some(Esp),None,Some(input_struct_stack_pos))))]);
  e (Mov,[GeneralReg(Gd(Edi));Memexpr(Md(Mem32(SS,Some(Esp),None,Some(output_struct_stack_pos))))])]

let get_inout_structures_assembled = List.concat (List.map X86Encode.encode_instruction get_inout_structures)

let set_flags_from_eax = [
  e (Mov,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Esi),None,input_flags_pos)))]);
  e (Xchg,[GeneralReg(Gb(Ah));GeneralReg(Gb(Al))]);
  e (Sahf,[]);
  e (Pushfd,[]);
  e (Movzx,[GeneralReg(Gd(Eax));GeneralReg(Gb(Al))]);
  e (And,[GeneralReg(Gb(Al));Immediate(Ib(8l))]);
  e (Shl,[GeneralReg(Gd(Eax));Immediate(Ib(8l))]);
  e (And,[Memexpr(Md(Mem32(SS,Some(Esp),None,None)));Immediate(Id(Int32.lognot 0x800l))]);
  e (Or,[Memexpr(Md(Mem32(SS,Some(Esp),None,None)));GeneralReg(Gd(Eax))]);
  e (Popfd,[])]

let set_flags_from_eax_assembled = List.concat (List.map X86Encode.encode_instruction set_flags_from_eax)

let get_binary_inputs = [
  e (Mov,[GeneralReg(Gd(Eax));Memexpr(Md(Mem32(DS,Some(Esi),None,input_lhs_pos)))]);
  e (Mov,[GeneralReg(Gd(Ecx));Memexpr(Md(Mem32(DS,Some(Esi),None,input_rhs_pos)))]);
  e (Mov,[GeneralReg(Gd(Edx));Memexpr(Md(Mem32(DS,Some(Esi),None,input_input3_pos)))])]

let get_binary_inputs_assembled = List.concat (List.map X86Encode.encode_instruction get_binary_inputs)

let jit_preamble = get_inout_structures_assembled@set_flags_from_eax_assembled@get_binary_inputs_assembled

let set_outputs = [
  e (Mov,[Memexpr(Md(Mem32(DS,Some(Edi),None,output_lhs_pos)));GeneralReg(Gd(Eax))]);
  e (Mov,[Memexpr(Md(Mem32(DS,Some(Edi),None,output_output2_pos)));GeneralReg(Gd(Edx))])]

let set_outputs_assembled = List.concat (List.map X86Encode.encode_instruction set_outputs)

let get_flags_into_eax = [
  e (Pushfd,[]);
  e (Pop,[GeneralReg(Gd(Eax))]);
  e (And,[GeneralReg(Gd(Eax));Immediate(Id(0x8D5l))])]

let get_flags_into_eax_assembled = List.concat (List.map X86Encode.encode_instruction get_flags_into_eax)

let set_flags_output = [e (Mov,[Memexpr(Md(Mem32(DS,Some(Edi),None,output_flags_pos)));GeneralReg(Gd(Eax))])]
let set_flags_output_assembled = List.concat (List.map X86Encode.encode_instruction set_flags_output)

let return = [e (Ret,[Immediate(Iw(8l))])]
let return_assembled = List.concat (List.map X86Encode.encode_instruction return)

let jit_postamble = set_outputs_assembled@get_flags_into_eax_assembled@set_flags_output_assembled@return_assembled

let region = ref None
let blit_starting_at = ref 0
let init () = 
  let r = JITRegion.allocate 4096 in
  let r = match r with Some(r) -> r | None -> failwith "couldn't allocate 4096 bytes" in
  region := Some(r);
  let jit_preamble_array = Array.of_list jit_preamble in
  if not (JITRegion.blit r jit_preamble_array) then failwith "couldn't blit preamble";
  blit_starting_at := Array.length jit_preamble_array

let rec get_region () = 
  match !region with
  | Some(r) -> r
  | None -> init (); get_region ()

let get_x86_result instr lhs rhs input3 flags = 
  let open JITRegion in 
  let x86l = (X86Encode.encode_instruction instr)@jit_postamble in
  let jit_instr_and_postamble_array = Array.of_list x86l in
  let r = get_region () in
  if not (JITRegion.blit_at r !blit_starting_at jit_instr_and_postamble_array) 
  then failwith "couldn't blit instruction and postamble";
  let instate = { lhs_in = lhs; rhs_in = rhs; input3 = input3; flags_in = flags } in
  let outstate = JITRegion.execute (get_region ()) instate in
  (outstate.lhs_out,outstate.output2,outstate.flags_out)

open X86TestUtil

type 'a cmpresult = (* ' *)
| Matched of string
| Unmatched of string * 'a * 'a 
| NotConstant of string

let get_var v2cmap v = try Some(Hashtbl.find v2cmap v) with Not_found -> None
let cmp_flag v2cmap vf inf sf =
  let open IR in
  match (get_var v2cmap vf) with
  | Some(Const(i,TypeReg_1)) when (Int64.to_int i) = inf -> Matched(sf)
  | Some(Const(i,TypeReg_1)) -> Unmatched(sf,inf,Int64.to_int i)
  | Some(_) -> failwith "cmp_flag:  non-constant value in constant hashmap, impossible"
  | None -> NotConstant(sf)

let cmp_int32 v2cmap v i32 s =
  let open IR in
  match (get_var v2cmap v) with
  | Some(Const(i,TypeReg_32)) when (Int64.to_int32 i) = i32 -> Matched(s)
  | Some(Const(i,TypeReg_32)) -> Unmatched(s,i32,Int64.to_int32 i)
  | Some(_) -> failwith "cmp_lhs:  non-constant value in constant hashmap, impossible"
  | None -> NotConstant(s)

let fl_um r i  = let s = Printf.sprintf ": Real %d, Interpreted %d\n" r i in s
let i32_um r i = let s = Printf.sprintf ": Real %08lx, Interpreted %08lx\n" r i in s
let string_of_cmpres um = function
| Matched(_) -> (* woot *) []
| Unmatched(s,r,i) -> [s^(um r i)]
| NotConstant(s) -> [s^" did not evaluate to a constant (interpreter bug)\n"]

let curry3 f (a,b,c) = f a b c
let curry2 f (a,b)   = f a b 

let mk_fl_cmp_list ignlist (xsf,xzf,xaf,xpf,xcf,xofl) =
  let alist = [] in
  let alist = if not (List.mem (X86F_C) ignlist) then (vCF,xcf, "CF")::alist else alist in
  let alist = if not (List.mem (X86F_P) ignlist) then (vPF,xpf, "PF")::alist else alist in
  let alist = if not (List.mem (X86F_A) ignlist) then (vAF,xaf, "AF")::alist else alist in
  let alist = if not (List.mem (X86F_S) ignlist) then (vSF,xsf, "SF")::alist else alist in
  let alist = if not (List.mem (X86F_Z) ignlist) then (vZF,xzf, "ZF")::alist else alist in
  let alist = if not (List.mem (X86F_O) ignlist) then (vOF,xofl,"OF")::alist else alist in
  alist

let x86_local_dse instrs = IRLocalOpt.local_dse instrs X86ToIRUtil.reserved_vars

(* I like it when my software does what I expect it to do, so I subject it to 
   heavy testing, often of the random variety.  What better way to test an IR 
   translator than to interpret an instruction's translation and compare it 
   directly against what the CPU produces when the instruction is executed in
   a JIT fashion? *)
let test_ir_translation x86instr ignlist = 
  let rb () = Random.int 2 in
  let ri32 () = Random.int32 Int32.max_int in
  
  (* Make a random assignment to the flags, LHS and RHS of the binary operation *)
  let cf,zf,sf,af,pf,ofl = rb(),rb(),rb(),rb(),rb(),rb() in
  let lhs,rhs,input3 = ri32(),ri32(),ri32() in
  let flags = fl2eflags sf zf af pf cf ofl in

  (* JIT and execute the instruction on the real CPU; get real results *)
  let (lhs_x86_result,edx_x86_result,flags_x86_result) = get_x86_result x86instr lhs rhs input3 flags in

  (* Make IR assignments to the flags and LHS & RHS *)
  let v2cmap = mk_initial_state3 sf zf af pf cf ofl lhs rhs input3 in
  (* Translate *)
  let irtrans = X86ToIR.translate_instr 0l x86instr in
  (* Interpret *)
  let optimized = x86_local_dse (IRLocalOpt.local_opt_state_in v2cmap irtrans) in

  (* Compare results *)
  let fl = mk_fl_cmp_list ignlist (eflags2fl flags_x86_result) in
  let fl = List.map (curry3 (cmp_flag v2cmap)) fl in
  let ll = (vEax,lhs_x86_result,"EAX")::[(vEdx,edx_x86_result,"EDX")] in
  let ll = List.map (curry3 (cmp_int32 v2cmap)) ll in
  let mk_error_list fn init list = List.fold_left (fun sl cr -> (string_of_cmpres fn cr)@sl) init list in
  let errors = List.rev (mk_error_list fl_um (mk_error_list i32_um [] ll) fl) in
  
  (* Print results *)
  match errors with
  | [] -> () (* No errors; succeed silently *)
  | l  -> (
    print_endline "BEGIN";
    Printf.printf "%s :: LHS %08lx, RHS %08lx SF %d ZF %d AF %d PF %d CF %d OF %d\n%!" 
     (X86Disasm.string_of_x86instr x86instr)
      lhs rhs sf zf af pf cf ofl;
    List.iter print_string l;
    print_endline "\nPre-optimized IR:";
    List.iter (fun i -> Printf.printf "  %s\n" (PpIR.ppInstr i)) irtrans;
    print_endline "\nPost-optimized IR:";
    List.iter (fun i -> Printf.printf "  %s\n%!" (PpIR.ppInstr i)) (x86_local_dse optimized);
    print_endline "END";
    )
      
let arith_producer () = Random.int32 Int32.max_int
let one_producer () = 1l
let shift_producer () = Random.int32 32l

let ig_zsap  = [X86F_Z;X86F_S;X86F_A;X86F_P]
let ig_a     = [X86F_A]
let ig_ao    = [X86F_A;X86F_O]
let ig_o     = [X86F_O]
let ig_c     = [X86F_C]
let ig_aoc   = [X86F_A;X86F_O;X86F_C]
let ig_cosap = [X86F_C;X86F_O;X86F_S;X86F_A;X86F_P]
let ig_zosap = [X86F_Z;X86F_O;X86F_S;X86F_A;X86F_P]
let ig_all   = [X86F_Z;X86F_O;X86F_S;X86F_A;X86F_P;X86F_C]

(* CURRENTLY FAILING:  Nothing *)

let test instrlist modulus strdesc = 
  Random.self_init ();
  let rec aux i j = 
    let i,j = if i <> 0 && i mod modulus = 0 then (Printf.eprintf "Iteration %d %s\n%!%!%!" (j+1) strdesc; (0,j+1)) else (i+1,j) in
    List.iter (curry2 test_ir_translation) instrlist;
    aux i j
  in
  aux 0 0

let cant_test = [
    (* -- Can't randomly test LAHF as a single instruction, since other flags may be set -- *)
    (Lahf,[]),[];
    (* -- Can't modify direction flag while OCaml process is running, causes crashes -- *)
    (Cld ,[]),[];
    (Std ,[]),[];
    (* -- Can't test this just yet, as I lack udiv and umod support in the interpreter -- *)
    (Aam,[Immediate(Ib(10l))]), ig_aoc;
    
    (* Lacking support for udiv and umod; also, the x86 versions throw exceptions even when the divisor is not zero *)
    (Div, [GeneralReg(Gb(Cl))]), ig_all;
    (Idiv,[GeneralReg(Gb(Cl))]), ig_all;
    
    (* No real point in going through the effort necessary to test these *)
    (Pushad,[]),[];
    (Pushaw,[]),[];
    (Popad ,[]),[];
    (Popaw ,[]),[];
    (Pushfd,[]),[];
    (Pushfw,[]),[];
]

(* Everything in here passes over 5 million random iterations *)
let big_test = [
    (* -- Need to test with randomly-generated immediates, but it works for 0x10 -- *)
    e (Aad,[Immediate(Ib(10l))]), ig_aoc;
    
    (* These pass, but we need to support A, O, C flags too *)
    e (Shld,[GeneralReg(Gd(Eax));GeneralReg(Gd(Edx));GeneralReg(Gb(Cl))]), ig_aoc;
    e (Shrd,[GeneralReg(Gd(Eax));GeneralReg(Gd(Edx));GeneralReg(Gb(Cl))]), ig_aoc;

    (* -- These all pass when the LHS is a register; have not implemented memory support for these -- *)
    e (Bsf,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_cosap;
    e (Bsr,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_cosap;
    e (Bsf,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_cosap;
    e (Bsr,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_cosap;
    e (Bt ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_zosap;
    e (Btc,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_zosap;
    e (Bts,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_zosap;
    e (Btr,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_zosap;
    e (Bt ,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_zosap;
    e (Btc,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_zosap;
    e (Bts,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_zosap;
    e (Btr,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_zosap;

    e (Aaa,[]), ig_zsap;
    e (Aas,[]), ig_zsap;
    e (Daa,[]), ig_o;
    e (Das,[]), ig_o;

    e (Sal, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;
    e (Shl, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;
    e (Sar, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;
    e (Shr, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;
    e (Rol, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;
    e (Ror, [GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), ig_ao;

    e (Sal, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;
    e (Shl, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;
    e (Sar, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;
    e (Shr, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;
    e (Rol, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;
    e (Ror, [GeneralReg(Gd(Eax));Immediate(Ib(1l))]), ig_a;

    e (Imul,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_zsap;
    e (Imul,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  ig_zsap;

    e (Cbw ,[]),[];
    e (Cwde,[]),[];
    e (Cwd ,[]),[];
    e (Cdq ,[]),[];
    e (Sahf,[]),[];
    e (Salc,[]),[];
    e (Cmc ,[]),[];
    e (Clc ,[]),[];
    e (Stc ,[]),[];

    e (Mul, [GeneralReg(Gd(Ecx))]), ig_zsap;
    e (Mul, [GeneralReg(Gw(Cx))]),  ig_zsap;
    e (Mul, [GeneralReg(Gb(Cl))]),  ig_zsap;
    e (Imul,[GeneralReg(Gd(Ecx))]), ig_zsap;
    e (Imul,[GeneralReg(Gw(Cx))]),  ig_zsap;
    e (Imul,[GeneralReg(Gb(Cl))]),  ig_zsap;

    e (Movzx,[GeneralReg(Gd(Eax));GeneralReg(Gw(Cx))]), [];
    e (Movzx,[GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), [];
    e (Movsx,[GeneralReg(Gd(Eax));GeneralReg(Gw(Cx))]), [];
    e (Movsx,[GeneralReg(Gd(Eax));GeneralReg(Gb(Cl))]), [];

    e (Add, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Adc, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmp, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Sub, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Sbb, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Xor, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_a;
    e (And, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_a;
    e (Or , [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_a;
    e (Test,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), ig_a;
    e (Bswap,[GeneralReg(Gd(Eax))]), [];
    e (Bswap,[GeneralReg(Gw(Ax))]),  [];

    e (Add, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Adc, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmp, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Sub, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Sbb, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Xor, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), ig_a;
    e (And, [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), ig_a;
    e (Or , [GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), ig_a;
    e (Test,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), ig_a;

    e (Add, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), [];
    e (Adc, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), [];
    e (Cmp, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), [];
    e (Sub, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), [];
    e (Sbb, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), [];
    e (Xor, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), ig_a;
    e (And, [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), ig_a;
    e (Or , [GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), ig_a;
    e (Test,[GeneralReg(Gb(Al));GeneralReg(Gb(Cl))]), ig_a;

    e (Cmovs ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovns,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovo ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovno,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovz ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovnz,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovp ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovnp,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmova ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovae,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovb ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovbe,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovl ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovle,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovg ,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Cmovge,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];

    e (Cmovs ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovns,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovo ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovno,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovz ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovnz,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovp ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovnp,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmova ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovae,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovb ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovbe,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovl ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovle,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovg ,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];
    e (Cmovge,[GeneralReg(Gw(Ax));GeneralReg(Gw(Cx))]), [];

    e (Sets ,[GeneralReg(Gb(Al))]), [];
    e (Setns,[GeneralReg(Gb(Al))]), [];
    e (Seto ,[GeneralReg(Gb(Al))]), [];
    e (Setno,[GeneralReg(Gb(Al))]), [];
    e (Setz ,[GeneralReg(Gb(Al))]), [];
    e (Setnz,[GeneralReg(Gb(Al))]), [];
    e (Setp ,[GeneralReg(Gb(Al))]), [];
    e (Setnp,[GeneralReg(Gb(Al))]), [];
    e (Seta ,[GeneralReg(Gb(Al))]), [];
    e (Setae,[GeneralReg(Gb(Al))]), [];
    e (Setb ,[GeneralReg(Gb(Al))]), [];
    e (Setbe,[GeneralReg(Gb(Al))]), [];
    e (Setl ,[GeneralReg(Gb(Al))]), [];
    e (Setle,[GeneralReg(Gb(Al))]), [];
    e (Setg ,[GeneralReg(Gb(Al))]), [];
    e (Setge,[GeneralReg(Gb(Al))]), [];

    e (Adc, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Adc, [GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  [];
    e (Adc, [GeneralReg(Gb(Al)); GeneralReg(Gb(Cl))]),  [];
    e (Sbb, [GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx))]), [];
    e (Sbb, [GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx))]),  [];
    e (Sbb, [GeneralReg(Gb(Al)); GeneralReg(Gb(Cl))]),  [];

    e (Imul,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx));Immediate(Ib(Random.int32 0x100l))]),        ig_zsap;
    e (Imul,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ecx));Immediate(Id(Random.int32 Int32.max_int))]), ig_zsap;
    e (Imul,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx)); Immediate(Ib(Random.int32 0x100l))]),        ig_zsap;
    e (Imul,[GeneralReg(Gw(Ax)); GeneralReg(Gw(Cx)); Immediate(Iw(Random.int32 0x10000l))]),      ig_zsap;

    e (Inc,[GeneralReg(Gd(Eax))]), ig_c;
    e (Inc,[GeneralReg(Gw(Ax))]),  ig_c;
    e (Inc,[GeneralReg(Gb(Al))]),  ig_c;
    e (Dec,[GeneralReg(Gd(Eax))]), ig_c;
    e (Dec,[GeneralReg(Gw(Ax))]),  ig_c;
    e (Dec,[GeneralReg(Gb(Al))]),  ig_c;

    e (Neg,[GeneralReg(Gd(Eax))]), [];
    e (Neg,[GeneralReg(Gw(Ax))]),  [];
    e (Neg,[GeneralReg(Gb(Al))]),  [];
    e (Not,[GeneralReg(Gd(Eax))]), [];
    e (Not,[GeneralReg(Gw(Ax))]),  [];
    e (Not,[GeneralReg(Gb(Al))]),  [];
]

let current_test = big_test (*[]*)

let _ = test current_test 10000 "10000s"

(* This tests assigning a 32-bit value to a 32-bit register, and assigning a 32-bit value to a 16-bit register *)
(* Need to do 16->16 and 16->32 *)
let test_lea () =
  let open X86Random in
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),None,None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,0),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,1),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,2),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,3),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,0),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,1),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,2),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,3),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),None,Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,0),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,1),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,2),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gd(Eax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,3),Some(Random.int32 Int32.max_int))))])) ig_all;

  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),None,None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,0),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,1),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,2),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,3),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,0),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,1),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,2),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,3),None)))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),None,Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,0),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,1),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,2),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),Some(Ecx),Some(Edx,3),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,0),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,1),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,2),Some(Random.int32 Int32.max_int))))])) ig_all;
  test_ir_translation (e (Lea,[GeneralReg(Gw(Ax));Memexpr(Mb(Mem32(rnd_seg (),None,Some(Edx,3),Some(Random.int32 Int32.max_int))))])) ig_all

let _ = test_lea ()

(*
let write_bin name list =
  let oc = open_out_bin name in
  List.iter (fun i32 -> output_byte oc (Int32.to_int i32)) list;
  close_out oc

let test_jit () = 
  let open JITRegion in
  let region = JITRegion.allocate 4096 in
  let region = match region with Some(r) -> r | None -> failwith "couldn't allocate 4096 bytes" in
  let ai = (Add,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) in
  let x86l = jit_preamble@(X86Encode.encode_instruction ai)@jit_postamble in
  let x86a = Array.of_list x86l in
  let b = JITRegion.blit region x86a in
  if not b then (Printf.printf "Couldn't blit\n"; exit 0);
  let instate = { lhs_in = 1l; rhs_in = 2l; input3 = 3l; flags_in = 0 } in
  let outstate = JITRegion.execute region instate in
  Printf.printf "%08lx %x\n%!" outstate.lhs_out outstate.flags_out;
  write_bin "executed.bin" x86l  
*)