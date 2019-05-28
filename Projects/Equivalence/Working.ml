#use "c:\\paframework\\framework.ml";;
(*
#use "c:\\paframework\\Projects\\Equivalence\\Working.ml";;
*)

let add_sequence_x86 = 
  let open X86 in
  [
   { pref = []; instr = (Clc,[]) };
   { pref = []; instr = (Add,[GeneralReg(Gd(Eax));Immediate(Id(1l))]) };   
  ]

   (*
   { pref = []; instr = (Mov, [GeneralReg(Gd(Esp));Immediate(Id(0x12345678l))]) };
   { pref = []; instr = (Push,[Immediate(Id(0l))]) };
   *)
 (*
   { pref = []; instr = (Popfd,[]) }
 *)
   (*
   *)
   
let inc_sequence_x86 = 
  let open X86 in
  [
   { pref = []; instr = (Clc,[]) };
   { pref = []; instr = (Inc,[GeneralReg(Gd(Eax))]) };
 (*
   { pref = []; instr = (Mov, [GeneralReg(Gd(Esp));Immediate(Id(0x12345678l))]) };
   { pref = []; instr = (Lea, [GeneralReg(Gd(Esp));Memexpr(Md(Mem32(DS,Some(Esp),None,Some(0xFFFFFFFCl))))]) };
   { pref = []; instr = (Mov, [Memexpr(Md(Mem32(DS,Some(Esp),None,None)));Immediate(Id(0l))]) };
   { pref = []; instr = (Add,[GeneralReg(Gd(Eax));Immediate(Id(1l))]) };
   { pref = []; instr = (Popfd,[]) }
 *)
  ]


  (*
 (*{ pref = []; instr = (Pushfd,[]) };*)
   { pref = []; instr = (Popfd,[]) }]


  [{ pref = []; instr = (Mov,[Memexpr(Md(Mem32(DS,None,None,Some(0x123l))));Immediate(Id(0x12345678l))])}]

  [{ pref = []; instr = (Mov,[Memexpr(Mb(Mem32(DS,None,None,Some(0x123l))));Immediate(Ib(0x78l))])};
   { pref = []; instr = (Mov,[Memexpr(Mb(Mem32(DS,None,None,Some(0x124l))));Immediate(Ib(0x56l))])};
   { pref = []; instr = (Mov,[Memexpr(Mb(Mem32(DS,None,None,Some(0x125l))));Immediate(Ib(0x34l))])};
   { pref = []; instr = (Mov,[Memexpr(Mb(Mem32(DS,None,None,Some(0x126l))));Immediate(Ib(0x12l))])}]
   *)
   
let make_ir_sequence x86l = List.concat (List.map (X86ToIR.translate_instr 0l) x86l)

let add_sequence_ir = make_ir_sequence add_sequence_x86
let inc_sequence_ir = make_ir_sequence inc_sequence_x86
(*
let hack_sequence_ir = make_ir_sequence hack_sequence_x86
let popcnt_sequence_ir = make_ir_sequence popcnt_sequence_x86
*)

let print_ir_sequence = List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i))

let _ = print_ir_sequence add_sequence_ir
let _ = print_ir_sequence inc_sequence_ir

(* OK... that was the easy part. *)

(* Now, I want to SSA-ize the sequences. *)

let add_tbl,add_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars add_sequence_ir
let inc_tbl,inc_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars inc_sequence_ir
(*
let hack_tbl,hack_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars hack_sequence_ir
let popcnt_tbl,popcnt_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars popcnt_sequence_ir
*)


let _ = print_ir_sequence add_sequence_ir_ssa
let _ = print_ir_sequence inc_sequence_ir_ssa


(* This is a quick hack, and it sucks.  If a variable is say, pushed and then popped, 
   its value will not have changed, but it will be present in the SSA variable table.
   If on the other hand, the value is not used or defined at all, it will not be
   present in the SSA table, and therefore this particular piece of code will decide
   that we should not compare the variable.  That's bad.
   
   There is a quick solution to this little dilemma:  make sure the SSA table is 
   initially populated with (var,var) for all pairs.  I will do that later. *)
let resultant_variable_pairs tbl1 tbl2 =
  Hashtbl.fold (fun ov sv1 list ->
    let sv2_opt = try Some(Hashtbl.find tbl2 ov) with Not_found -> None in
    match sv2_opt with
    | Some(sv2) -> (sv1,sv2)::list
    | None -> list)
    tbl1
    []

let add_inc_variable_pairs = resultant_variable_pairs add_tbl inc_tbl

let _ = List.iter (fun (sv_add,sv_inc) -> IDA.msg "%s,%s\n" (PpIR.ppVar sv_add) (PpIR.ppVar sv_inc)) add_inc_variable_pairs

let add_inc_conjunction = 
  List.fold_left 
   (fun conj (v1,v2) -> IRUtil.mk_and (IRUtil.mk_eq (IRUtil.mk_evar v1) (IRUtil.mk_evar v2)) conj)
    IRUtil.mk_true
    add_inc_variable_pairs

let _ = IDA.msg "%s\n" (PpIR.ppExpr false add_inc_conjunction)
    
let add_inc_postcondition = IRUtil.mk_not add_inc_conjunction

(*
let add_inc_postcondition = 
  IRUtil.mk_not 
   (IRUtil.mk_eq 
     (IRUtil.mk_evar (Hashtbl.find add_tbl X86ToIRUtil.vMem))
     (IRUtil.mk_evar (Hashtbl.find inc_tbl X86ToIRUtil.vMem)))
*)

(*
let hack_popcnt_postcondition = 
  IRUtil.mk_not 
   (IRUtil.mk_eq 
     (IRUtil.mk_evar (Hashtbl.find hack_tbl X86ToIRUtil.vMem))
     (IRUtil.mk_evar (Hashtbl.find popcnt_tbl X86ToIRUtil.vMem)))
*)
let z3ctx = Z3SymbolicExecute.mk_context ()

let model = Z3SymbolicExecute.symbolic_execute z3ctx (add_sequence_ir_ssa@inc_sequence_ir_ssa) add_inc_postcondition

(*let model = Z3SymbolicExecute.symbolic_execute z3ctx (hack_sequence_ir_ssa@popcnt_sequence_ir_ssa) hack_popcnt_postcondition*)

(* The negation of the conjunction is unsatisfiable, therefore the equivalence is valid *)

