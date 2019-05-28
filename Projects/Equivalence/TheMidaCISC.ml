(*
#use "c:\\paframework\\Projects\\Equivalence\\Working.ml";;
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\BreakCV.ml";;
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\CVMain.ml";;
#use "c:\\paframework\\Projects\\Equivalence\\TheMidaCISC.ml";;
*)

(*
let obf_deo_conjunction = 
  let obf_esp = Hashtbl.find obf_tbl X86ToIRUtil.vEsp in
  let deo_esp = Hashtbl.find deo_tbl X86ToIRUtil.vEsp in
  let _ = IDA.msg "SSA ESP variables:  %s,%s\n" (PpIR.ppVar obf_esp) (PpIR.ppVar deo_esp) in
(*IRUtil.mk_not *)
   (IRUtil.mk_eq 
     (IRUtil.mk_evar obf_esp)
     (IRUtil.mk_evar deo_esp))
*)

let x86_local_dse instrs = IRLocalOpt.local_dse instrs X86ToIRUtil.reserved_vars

let buttress_sequence s =
  let open X86 in
  let pushes =
    let rec aux i list =
      if i < 10
      then aux (i+1) ({ pref = []; instr = (Push,[Immediate(Id(0l))]) }::list)
      else list
    in aux 0 []
  in
  ({ pref = []; instr = (Mov, [GeneralReg(Gd(Esp));Immediate(Id(0x30l))]) }::
   { pref = []; instr = (Mov, [GeneralReg(Gd(Edi));Immediate(Id(0x1000l))]) }::s)@pushes
      
let get_jump_free_x86 ea = 
  let _,cfg = CVVMAnalyze.get_handler_cfg_exn ea 0x00406356l in
  let v = C.find_vertex cfg ea in
  let ir = C.get_ir cfg v in
  let _ = ignore(IDAX86Graph.show_x86_cfg cfg (Printf.sprintf "Handler %08lx" ea)) in
  ir

let get_dwords_in_range low hi =
  let rec aux ea list =
    if ea < hi
    then aux (Int32.add ea 4l) ((IDA.get_dword ea)::list)
    else List.rev list
  in aux low []

let equivalence_check_themidacisc ea timeoutint = 
  let ir = get_jump_free_x86 ea in
  let oir = UnSmartMetamorph.go ir in
  if ir = oir
  then let _ = IDA.msg "0:  unsat (syntactic equivalence)\n" in ()
  else
    let ir,oir = buttress_sequence ir,buttress_sequence oir in
    let irir,oirir = make_ir_sequence ir,make_ir_sequence oir in
    let optimize_ir ir = ir 
    (*let local_opt_ir = IRLocalOpt.local_opt ir in
      let bitwise_ai_ir = let open IR in BitwiseAI.bitwise_abstract_interpret_ir [X86ToIRUtil.vEsp,Const(0x30L,TypeReg_32)] local_opt_ir in
      let dse_ir = x86_local_dse bitwise_ai_ir in
      bitwise_ai_ir *)
    in
    let irir,oirir = optimize_ir irir,optimize_ir oirir in
    let obf_tbl,obf_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars irir in
    let deo_tbl,deo_sequence_ir_ssa = IRSSA.bb_to_ssa_state_out X86ToIRUtil.num_reserved_vars oirir in
    
    (*
    let _ = print_ir_sequence obf_sequence_ir_ssa in
    let _ = IDA.msg "\n\n" in
    let _ = print_ir_sequence deo_sequence_ir_ssa in
    let _ = IDA.msg "\n\n" in
    *)
    
    let obf_deo_variable_pairs = resultant_variable_pairs obf_tbl deo_tbl in
    let obf_deo_variable_pairs = 
      List.filter 
       (fun (v1,_) -> 
        let open IR in 
        match v1 with | Variable(_,TypeReg_1) -> false | _ -> true) 
        obf_deo_variable_pairs 
    in
    let obf_deo_conjunction = 
      List.fold_left 
       (fun conj (v1,v2) -> IRUtil.mk_and (IRUtil.mk_eq (IRUtil.mk_evar v1) (IRUtil.mk_evar v2)) conj)
        IRUtil.mk_true
        obf_deo_variable_pairs
    in
    let obf_deo_postcondition = IRUtil.mk_not obf_deo_conjunction in
    
    let z3ctx = Z3.mk_context_x [|("MODEL", "true");("SOFT_TIMEOUT", string_of_int timeoutint^"000")|] in
    
    (*let _ = IDA.msg "*** %s ***\n" (PpIR.ppExpr false obf_deo_postcondition) in*)
    let before = Sys.time () in
    let model = 
      try Z3SymbolicExecute.symbolic_execute z3ctx (obf_sequence_ir_ssa@deo_sequence_ir_ssa) obf_deo_postcondition
      with | Failure "Z3: invalid argument" -> "Equivalent"
           | e -> raise e
    in
    let after = Sys.time () in
    let total = after -. before in
    
    let _ = IDA.msg "%f: %s\n" total model in
    ()

let all_dwords = get_dwords_in_range 0x0040604Cl 0x004062ECl

(* Failed cases *)
let failed_dwords = 
[
(* CJMP *)
0x00407de7l;
0x004092fbl;
0x0040b06bl;
0x0040646cl;
0x004076cel;

(* JMP *)
0x00406af6l;

(* Type error *)
0x0040b5afl;
0x00407830l;
0x004091ael;
0x0040b56el;
0x0040a411l;
0x0040b90cl;
0x0040bc90l;
0x00408463l;
0x0040c0ddl;
0x0040b0eal;
0x0040c849l;
0x0040ba4el;
0x00407355l;
0x0040c3e3l;
0x0040ad3cl;
0x00407427l;
0x0040711fl;
0x004073b9l;
0x00409bc4l;
]

let unknown_dwords =
[
]

let unsat_dwords =
[
0x00409329l; (* no obf *)
0x0040be4el; (* no obf *)
0x004087dal; (* no obf *)
0x00408286l; (* no obf *)
0x00406616l; (* no obf *)
0x0040c5bcl; (* no obf *)
0x00407c4dl; (* no obf *)
0x0040887bl; (* no obf *)
0x0040955dl; (* no obf *)
0x00408202l; (* no obf -- rcl dword *)
0x0040a9f3l; (* no obf -- rcl word *)
0x0040b136l; (* no obf -- rcl byte *)
0x00409f5bl; (* no obf -- rcr dword *)
0x0040b03dl; (* no obf -- rcr word *)
0x00407cbdl; (* no obf -- rcr byte *)
0x00408f18l; (* no obf -- adc dword *)
0x0040abcbl; (* no obf -- adc word *)
0x004080f2l; (* no obf -- adc byte *)
0x004095e9l; (* no obf -- sbb dword *)
0x004078a7l; (* no obf -- sbb word *)
0x00408fc3l; (* no obf -- sbb byte *)
0x00408ec2l; (* almost no obf *)
0x0040b2cel; (* almost no obf *)
0x00407e36l;
0x004071d7l;
0x0040a3f6l;
0x004082fal;
0x0040736el;
0x00406e3el;
0x0040a41al;
0x004090abl;
0x004070ddl;
0x00409de0l;
0x0040b888l;
0x00408e4dl;
0x0040c7bdl;
0x0040aec0l;
0x0040b765l;
0x004082eal;
0x0040829al;
0x0040b756l;
0x004076e5l;
0x00407ff5l;
0x004064fal;
0x0040c504l;
0x0040a630l;
0x0040c2dal;
0x00407dccl;
0x00408009l;
0x0040659dl;
0x00406eb4l;
0x0040ae3al;
0x0040733dl;
0x00409120l;
0x0040a3ael;
0x004096a4l;
0x0040b591l;
0x00409165l;
0x0040bfb0l;
0x00407209l;
0x0040787fl;
0x00406968l;
0x00409fcbl;
0x00409acfl;
0x00406366l;
0x00407685l;
0x004087f5l;
0x004094ebl;
0x0040c6ebl;
0x00406993l;
0x0040b810l;
0x0040a5e5l;
0x0040a14al;
0x0040739bl;
0x004067e5l;
0x004065a6l;
0x0040b32fl;
0x004091c0l;
0x0040a44fl;
0x00406393l;
0x0040a992l;
0x00406456l;
0x00408e20l;
0x0040b54bl;
0x0040b8a6l;
0x00408858l;
0x004074bbl;
0x00406fb6l;
0x0040ad4cl;
0x0040bcbcl;
(* ******************** BITWISE ******************** *)
(* Theory:  caused by bad IR translation of "undefined" flags *)
0x0040af5el;
(*
; pop eax                   
; or dword ptr ss:[esp], eax
; pushfd                    
*)
0x00407ad1l;
(*
; pop ax
; or word ptr ss:[esp], ax
; pushfd
*)
0x0040a1ccl;
(*
; pop ax
; or byte ptr ss:[esp], al
; pushfd
*)
0x00406539l;
(*
; pop eax
; and dword ptr ss:[esp], eax
; pushfd
*)
0x0040a279l;
(*
; pop ax
; and word ptr ss:[esp], ax
; pushfd
*)
0x0040ceecl;
(*
; pop ax
; and byte ptr ss:[esp], al
; pushfd
*)
0x00409a10l;
(*
; pop eax
; xor dword ptr ss:[esp], eax
; pushfd
*)
0x0040ac01l;
(*
; pop ax
; xor word ptr ss:[esp], ax
; pushfd
*)
0x00409027l;
(*
; pop ax
; xor byte ptr ss:[esp], al
; pushfd
*)
0x00409056l;
(*
; pop eax
; pop ecx
; test eax, ecx
; pushfd
*)
0x00406e35l;
(*
; pop ax
; pop cx
; test ax, cx
; pushfd
*)
0x0040ba10l;
(*
; pop ax
; pop cx
; test al, cl
; pushfd
*)

]

let sat_dwords =
[

]

let sat_dwords_badir =
[
(* ******************** IMUL ******************** *)
(* Theory:  caused by bad IR translation of "undefined" flags *)
0x00409ce5l;
(*
; pop ax
; pop cx
; imul cx, ax
; push cx
; pushfd
*)
0x00406dd1l;
(*
; pop eax
; pop ecx
; imul ecx, eax
; push ecx
; pushfd
*)
0x0040bd65l;
(*
; pop cx
; pop ax
; imul cl
; movzx cx, ah
; push cx
; movzx cx, al
; push cx
; pushfd
*)
]

let sat_dwords_vmbugs =
[
(* ******************** SHIFT/ROTATE ******************** *)
(* Theory:  legitimate bugs in the VM *)
0x00406c2cl;
(*
; pop cx
; ror dword ptr ss:[esp], cl
; pushfd
*)
0x00407d4fl;
(*
; pop cx
; shl byte ptr ss:[esp], cl
; pushfd
*)
0x00408ab6l;
(*
; pop cx
; sar dword ptr ss:[esp], cl
; pushfd
*)
0x0040991al;
(*
; pop cx
; rol dword ptr ss:[esp], cl
; pushfd
*)
0x0040647dl;
(*
; pop cx
; sar byte ptr ss:[esp], cl
; pushfd
*)
0x00407ec4l;
(*
; pop cx
; shl word ptr ss:[esp], cl
; pushfd
*)
0x00406743l;
(*
; pop cx
; shr word ptr ss:[esp], cl
; pushfd
*)
0x00409203l;
(*
; pop cx
; sar word ptr ss:[esp], cl
; pushfd
*)
0x004087e3l;
(*
; pop cx
; ror byte ptr ss:[esp], cl
; pushfd
*)
0x00409392l;
(*
; pop cx
; rol byte ptr ss:[esp], cl
; pushfd
*)
0x0040b95al;
(*
; pop cx
; ror word ptr ss:[esp], cl
; pushfd
*)
0x0040a3del; 
(*
; pop cx
; rol word ptr ss:[esp], cl
; pushfd
*)
0x0040805dl;
(*
; pop cx                    
; shl dword ptr ss:[esp], cl
; pushfd                                                                                                          
*)
0x0040a1fal;
(*
; pop cx                    
; shl dword ptr ss:[esp], cl
; pushfd                    
*)
0x00409fbfl;
(*
; pop cx
; shl word ptr ss:[esp], cl
; pushfd
*)
0x0040a808l;
(*
; pop cx
; shl byte ptr ss:[esp], cl
; pushfd
*)
0x0040b7aal;
(*
; pop cx
; shr dword ptr ss:[esp], cl
; pushfd
*)
0x00408b6al;
(*
; pop cx
; shr byte ptr ss:[esp], cl
; pushfd
*)

(* ******************** INC/DEC ******************** *)
(* Theory:  legitimate bugs in the VM *)
0x0040ab53l;
(*
; pop eax
; dec dword ptr ss:[esp]
; pushfd
*)
0x00408f8dl;
(*
; pop ax
; dec word ptr ss:[esp]
; pushfd
*)
0x0040706el;
(*
; pop ax
; dec byte ptr ss:[esp]
; pushfd
*)
0x0040b1e7l;
(*
; pop eax
; inc dword ptr ss:[esp]
; pushfd
*)
0x0040753cl;
(*
; pop ax
; inc word ptr ss:[esp]
; pushfd
*)
0x00406693l;
(*
; pop ax
; inc byte ptr ss:[esp]
; pushfd
*)

(* ******************** LODSD => MEMORY ACCESS ******************** *)
0x0040b2b3l;
(*
; lodsd dword ptr ds:[esi]
; sub eax, ebx
; xor eax, 7134B21Ah
; add eax, 2564E385h
; xor ebx, eax
; movzx ax, byte ptr ds:[eax]
; push ax
*)
0x0040c7a8l;
(*
; lodsd dword ptr ds:[esi]
; sub eax, 197C7BBDh
; sub eax, ebx
; sub eax, 11C5E542h
; sub ebx, eax
; pop dx
; mov byte ptr ds:[eax], dl
*)
0x00408babl;
(*
; lodsd dword ptr ds:[esi]
; xor eax, ebx
; xor eax, 1A8C5492h
; add eax, 42B67C4Fh
; sub ebx, eax
; push word ptr ds:[eax]
*)
0x0040761bl;
(*
; lodsd dword ptr ds:[esi]
; sub eax, 773B7B89h
; sub eax, ebx
; add eax, 33BE2518h
; xor ebx, eax
; push dword ptr ds:[eax]
*)
]

let sat_dwords_known =
[
(* ******************** MISCELLANEOUS ******************** *)
(* Theories:  none. *)
0x0040a0a6l;
(*
; push edx
*)
0x00406b22l;
(*
; pop eax
; add dword ptr ss:[esp], eax
*)
0x00407c60l;
(*
; pop eax
; push dword ptr ds:[eax]
*)
]

let edi_dwords =
[
(* ******************** INVOLVES EDI ******************** *)
(* Theory:  the idea that edi can alias the stack.  Hack our way around it by setting up EDI at the beginning *)
(* When I added initialization for edi, this one is still satisfiable *)
0x00409e86l;
(*
; lodsb byte ptr ds:[esi]
; sub al, bl
; xor al, F0h
; sub al, 67h
; xor bl, al
; movzx eax, al
; mov eax, dword ptr ds:[edi+eax*4]
; push word ptr ds:[eax]
*)

(* When I added initialization for edi, this one is still satisfiable *)
0x004093e8l;
(*
; lodsb byte ptr ds:[esi]
; xor al, bl
; sub al, 98h
; xor al, 4Ah
; add bl, al
; movzx eax, al
; mov eax, dword ptr ds:[edi+eax*4]
; movzx ax, byte ptr ds:[eax]
; push ax
*)

(* When I added initialization for edi, this one is still satisfiable *)
0x004077del;
(*
; lodsb byte ptr ds:[esi]
; sub al, bl
; xor al, FAh
; xor al, A1h
; xor bl, al
; movzx eax, al
; pop word ptr ds:[edi+eax*4]
*)
]

let edi_dwords2 = 
[
(* When I added initialization for edi, this one became unsat *)
0x0040b0c4l;
(*
; and dword ptr ds:[edi+0000001C], FFFFFDFFh
*)

(* When I added initialization for edi, this one became unsat *)
0x0040be56l;
(*
; lodsb byte ptr ds:[esi]
; sub al, bl
; xor al, 58h
; sub al, 64h
; xor bl, al
; mov byte ptr ds:[edi+00000028], al
*)

(* When I added initialization for edi, this one became unsat *)
0x0040809bl;
(*
; lodsb byte ptr ds:[esi]
; add al, bl
; sub al, B7h
; sub al, ADh
; add bl, al
; mov byte ptr ds:[edi+00000038], al
*)
(* When I added initialization for edi, this one became unsat *)
0x0040ca7fl;
(*
; lodsb byte ptr ds:[esi]                 
; add al, bl                              
; add al, 93h                             
; xor al, 4Ch                             
; add bl, al                              
; movzx eax, al                           
; pop dx                                  
; mov byte ptr ds:[edi+eax*4+00000001], dl                                                                                                    
*)

]


type result =
| Equivalent of string
| Inequivalent of string
| Failed
| StupidHeuristic of string

let check_all dwords timeoutint =
  let rec aux = function
  | [] -> ()
  | ea::xs ->
    IDA.msg "%08lx:  " ea;
   ((*try*) (equivalence_check_themidacisc ea timeoutint)
    (*with _ -> IDA.msg "Failed\n"*));
    aux xs
  in aux dwords

let check_failed    = check_all failed_dwords
let check_unknown   = check_all unknown_dwords
let check_edi       = check_all edi_dwords
let check_sat       = check_all sat_dwords
let check_sat_badir = check_all sat_dwords_badir

let h = Hashtbl.create 500
let _ = List.iter (fun i -> Hashtbl.replace h i ()) (failed_dwords@unknown_dwords@unsat_dwords@sat_dwords@sat_dwords_known@edi_dwords@edi_dwords2)
let remainder = List.filter (fun i -> try Hashtbl.find h i; false with Not_found -> true) all_dwords
let check_remainder = check_all remainder

let unsat = unsat_dwords@edi_dwords2
(*let _ = List.iter (fun ea -> IDA.msg "%08lx: %d\n" ea (List.length (get_jump_free_x86 ea))) unsat*)

let print_x86_sequence = List.iter (fun x -> IDA.msg "%s\n" (X86Disasm.string_of_x86instr x))