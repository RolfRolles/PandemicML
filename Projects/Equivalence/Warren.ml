#use "c:\\paframework\\framework.ml";;
(*
#use "c:\\paframework\\Projects\\Equivalence\\Warren.ml";;
*)

(* Proves that the two sequences of code, hack_sequence_x86 and popcnt_sequence_x86, 
   have the same effect on the eax register. *)

let hack_sequence_x86 = 
  let open X86 in
  [{ pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };         (* mov eax, ebx       *)
   { pref = []; instr = (And,[GeneralReg(Gd(Eax));Immediate(Id(0x55555555l))]) };  (* and eax, 55555555h *)
   { pref = []; instr = (Shr,[GeneralReg(Gd(Ebx));Immediate(Ib(1l))]) };           (* shr ebx, 1         *)
   { pref = []; instr = (And,[GeneralReg(Gd(Ebx));Immediate(Id(0x55555555l))]) };  (* and ebx, 55555555h *)
   { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };         (* add ebx, eax       *)
   { pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };         (* mov eax, ebx       *)
   { pref = []; instr = (And,[GeneralReg(Gd(Eax));Immediate(Id(0x33333333l))]) };  (* and eax, 33333333h *)
   { pref = []; instr = (Shr,[GeneralReg(Gd(Ebx));Immediate(Ib(2l))]) };           (* shr ebx, 2         *)
   { pref = []; instr = (And,[GeneralReg(Gd(Ebx));Immediate(Id(0x33333333l))]) };  (* and ebx, 33333333h *)
   { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };         (* add ebx, eax       *)
   { pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };         (* mov eax, ebx       *)
   { pref = []; instr = (And,[GeneralReg(Gd(Eax));Immediate(Id(0x0F0F0F0Fl))]) };  (* and eax, 0F0F0F0Fh *)
   { pref = []; instr = (Shr,[GeneralReg(Gd(Ebx));Immediate(Ib(4l))]) };           (* shr ebx, 4         *)
   { pref = []; instr = (And,[GeneralReg(Gd(Ebx));Immediate(Id(0x0F0F0F0Fl))]) };  (* and ebx, 0F0F0F0Fh *)
   { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };         (* add ebx, eax       *)
   { pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };         (* mov eax, ebx       *)
   { pref = []; instr = (And,[GeneralReg(Gd(Eax));Immediate(Id(0x00FF00FFl))]) };  (* and eax, 00FF00FFh *)
   { pref = []; instr = (Shr,[GeneralReg(Gd(Ebx));Immediate(Ib(8l))]) };           (* shr ebx, 8         *)
   { pref = []; instr = (And,[GeneralReg(Gd(Ebx));Immediate(Id(0x00FF00FFl))]) };  (* and ebx, 00FF00FFh *)
   { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };         (* add ebx, eax       *)
   { pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) };         (* mov eax, ebx       *)
   { pref = []; instr = (And,[GeneralReg(Gd(Eax));Immediate(Id(0x0000FFFFl))]) };  (* and eax, 0000FFFFh *)
   { pref = []; instr = (Shr,[GeneralReg(Gd(Ebx));Immediate(Ib(16l))]) };          (* shr ebx, 16        *)
   { pref = []; instr = (And,[GeneralReg(Gd(Ebx));Immediate(Id(0x0000FFFFl))]) };  (* and ebx, 0000FFFFh *)
   { pref = []; instr = (Add,[GeneralReg(Gd(Ebx));GeneralReg(Gd(Eax))]) };         (* add ebx, eax       *)
   { pref = []; instr = (Mov,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) }          (* mov eax, ebx       *)
   ]

let popcnt_sequence_x86 = 
  let open X86 in
  [{ pref = []; instr = (Popcnt,[GeneralReg(Gd(Eax));GeneralReg(Gd(Ebx))]) }]      (* popcnt eax, ebx *)

let make_ir_sequence x86l = List.concat (List.map (X86ToIR.translate_instr 0l) x86l)

let print_ir_sequence = List.iter (fun i -> IDA.msg "%s\n" (PpIR.ppInstr i))

let hack_sequence_ir   = make_ir_sequence   hack_sequence_x86
let popcnt_sequence_ir = make_ir_sequence popcnt_sequence_x86

let _ = IDA.msg "*** IR translation for hack sequence ***"
let _ = print_ir_sequence   hack_sequence_ir
let _ = IDA.msg "*** IR translation for popcnt ***"
let _ = print_ir_sequence popcnt_sequence_ir

let   hack_tbl,  hack_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars   hack_sequence_ir
let popcnt_tbl,popcnt_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars popcnt_sequence_ir

let hack_popcnt_postcondition = 
  IRUtil.mk_not 
   (IRUtil.mk_eq 
     (IRUtil.mk_evar (Hashtbl.find   hack_tbl X86ToIRUtil.vEax))
     (IRUtil.mk_evar (Hashtbl.find popcnt_tbl X86ToIRUtil.vEax)))

let z3ctx = Z3SymbolicExecute.mk_context ()

(* We query the theorem prover as to whether eax from the ends of the respective 
   sequences are equal.  We do this by asking the theorem prover to generate an
   example where they are not equal, which it is unable to do (the formula is
   unsatisfiable), and therefore they are equal on all inputs. *)
let before = Sys.time ()
let model = 
  try Z3SymbolicExecute.symbolic_execute z3ctx (hack_sequence_ir_ssa@popcnt_sequence_ir_ssa) hack_popcnt_postcondition
  with _ -> "unsat"

let after = Sys.time ()
let total = after -. before

let _ = IDA.msg "%f: %s\n" total model