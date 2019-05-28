(*
* Open issues:  
* * How to incorporate 0x6A?
* * How to do XLAT (0xD7)?
    Predicated(PRED_AddrSize(Direct(Xlat,[oMbBx]),Direct(Xlat,[oMbEbx])));
* * How to represent near and far jumps and calls in the tables?
*)
open X86
open X86InternalOperand
open X86Constraints
open X86TypeCheck
open X86EncodeUtil
open X86EncodeTable

exception InvalidOperands of X86.x86instrpref

type x86opndenc =
| NoEnc
| GPart
| EPart
| Imm

let enctype_of_abstract_operand = function
| OEb
| OEw
| OEd
| OEv
| OEd_q
| OMb
| OMw
| OMd
| OMq
| OMa
| OMp
| OMs
| OMps
| OMpd
| OMd_q
| OMdq
| OM
| OReal4
| OReal8
| OReal10
| OFPEnvLow
| OFPEnv
| OSimdState
| ORv
| ORw
| ORd
| OStN
| ONq
| OWps
| OWss
| OWpd
| OWsd
| OWq
| OWdq
| OUpd
| OUps
| OUq
| OUdq
| OQpi
| OQd
| OQq
| OUdq_Mw
| OUdq_Md
| OUdq_Mq
| ORd_Mb
| ORd_Mw -> EPart
| OGb
| OGw
| OGd
| OGd_q
| OGv
| OGz
| OSw
| OCd
| ODd
| OVps
| OVdq
| OVss
| OVpd
| OVsd
| OVq
| OPpi
| OPq
| OPd -> GPart
| OIb
| OIw
| OIv
| OIz
| OOb
| OOv
| OAp -> Imm
| O1
| OAL
| OrAX
| OCL
| OAx
| ODx
| OES
| OCS
| OSS
| ODS
| OFS
| OGS
| OeAX
| OeCX
| OeDX
| OeBX
| OeSP
| OeBP
| OeSI
| OeDI
| OrAXr8
| OrCXr9
| OrDXr10
| OrBXr11
| OrSPr12
| OrBPr13
| OrSIr14
| OrDIr15
| OALR8L
| OCLR9L
| ODLR10L
| OBLR11L
| OAHR12L
| OCHR13L
| ODHR14L
| OBHR15L
| OSt0
| OXb
| OXw
| OXd
| OYb
| OYw
| OYd
| OXv
| OXz
| OYv
| OYz -> NoEnc
(* FIX THESE *)
| OJz -> NoEnc
| OJb -> NoEnc

type x86_operating_mode = OM_16 | OM_32 | OM_64
let operating_mode = ref OM_32

let e_size () =
  match !operating_mode with 
  | OM_16 -> OPSZ_16 
  | OM_32 -> OPSZ_32 
  | _ -> failwith "unsupported operating mode"
let r_size () = e_size ()
let v_size () = e_size ()
let z_size () = e_size ()
let a_size () =
  match !operating_mode with
  | OM_16 -> OPSZ_32
  | OM_32 -> OPSZ_64
  | _ -> failwith "unsupported operating mode"

let may_require_size_override = function
| OAL
| OCL
| OALR8L
| OCLR9L
| ODLR10L
| OBLR11L
| OAHR12L
| OCHR13L
| ODHR14L
| OBHR15L
| OAx
| ODx
| OCS
| ODS
| OES
| OGS
| OFS
| OSS
| OSw
| OCd
| ODd
| OIb
| OIw
| O1
| OGb
| OGw
| OGd
| OGd_q
| ORd
| ORw
| OMb
| OMw
| OMd
| OMs
| OMq
| OMdq
| OMd_q
| OMp
| OMpd
| OMps
| OM
| OEb
| OEw
| OEd
| OEd_q
| OOb
| OAp
| OJb
| OJz
| OXb
| OYb
| OSt0
| OStN
| OFPEnv
| OFPEnvLow
| OReal4
| OReal8
| OReal10
| ONq
| OPd
| OPq
| OPpi
| OVdq
| OVpd
| OVps
| OVsd
| OVss
| OVq
| OUps
| OUpd
| OUq
| OUdq
| OQpi
| OQd
| OQq
| OWdq
| OWps
| OWpd
| OWq
| OWss
| OWsd
| OSimdState
| OUdq_Md
| OUdq_Mq
| OUdq_Mw
| ORd_Mb
| ORd_Mw
| OXv (* Despite nomenclature, these things require *address prefixes* *)
| OYv
| OYw
| OYd
| OXw
| OXd
| OXz
| OYz -> None
| OrAX (* This one might seem to be out of place, but I am rather confused *)
| OeAX (* about x64 encoding at the moment *)
| OeCX
| OeBX
| OeSP
| OeDX
| OeBP
| OeSI
| OeDI -> Some(e_size ())
| OrAXr8
| OrCXr9
| OrDXr10
| OrBXr11
| OrSPr12
| OrBPr13
| OrSIr14
| OrDIr15 -> Some(r_size ())
| OIv
| OGv
| ORv
| OEv
| OOv -> Some(v_size ())
| OIz
| OGz -> Some(z_size ())
| OMa -> Some(a_size ())

type x86_insn_encoding = {
  group1pf:   int32 option;
  segpf:      int32 option;
  opsizepf:   int32 option;
  addrsizepf: int32 option;
  stem:       int32 list; (* Instruction stem, 1-4 bytes *)
  modrm:      int32 option;
  sib:        int32 option;
  rmdispl:    x86_immediate option;
  imms:       x86_immediate list;
}

let get_byte_encoding enc = 
  let get_some_byte l = function | Some(b) -> b::l | None -> l in
  let l = get_some_byte [] enc.group1pf in
  let l = get_some_byte l enc.segpf in
  let l = get_some_byte l enc.opsizepf in
  let l = get_some_byte l enc.addrsizepf in
  let l = (List.rev enc.stem)@l in
  let l = get_some_byte l enc.modrm in
  let l = get_some_byte l enc.sib in
  let l = match enc.rmdispl with | Some(i) -> (List.rev (list_of_immediate i))@l | _ -> l in
  (List.rev l)@(List.concat (List.map list_of_immediate enc.imms))

let mk_default_x86_insn_encoding () = 
{ group1pf   = None;
  segpf      = None;
  opsizepf   = None;
  addrsizepf = None;
  stem       = [];
  modrm      = None;
  sib        = None;
  rmdispl    = None;
  imms       = [];
}

type sib_info = int * int * int

type modrm_info = 
| GInfo of int
| EInfo of int * int * sib_info option (* top2, bottom3, SIB *)

let epart_of_memexpr16 b s d =
  let aux rm =
    let imm,m0d = 
    match d with
    | Some(imm) -> 
      if is_quantity_signed_byte imm
      then Some(Ib(Int32.logand imm 0xFFl)),1
      else Some(Iw(Int32.logand imm 0xFFFFl)),2
    | None -> None,0
    in (EInfo(m0d,rm,None),imm)
  in
  match b,s,d with
  | Some(Bx),Some(Si),_   -> aux 0
  | Some(Bx),Some(Di),_   -> aux 1
  | Some(Bp),Some(Si),_   -> aux 2 
  | Some(Bp),Some(Di),_   -> aux 3 
  | Some(Si),None,_       -> aux 4
  | Some(Di),None,_       -> aux 5 
  | Some(Bp),None,Some(_) -> aux 6 
  | Some(Bp),None,None    -> EInfo(1,6,None),Some(Ib(0l))
  | Some(Bx),None,_       -> aux 7 
  | None,    None,Some(i) -> (EInfo(0,6,None),Some(Iw(i)))
  | _,_,_                 -> invalid_arg "epart_of_memexpr16: invalid 16-bit mem expression"

let epart_of_memexpr32 b s d =
  let imm,m0d = match d with
  | Some(imm) -> 
    if is_quantity_signed_byte imm
    then Some(Ib(Int32.logand imm 0xFFl)),1
    else Some(Id(imm)),2
  | _ when b = Some(Ebp) && s = None -> Some(Ib(0l)),1
  | None -> None,0
  in
  match b,s,d with 
  | None,None,None             -> invalid_arg "epart_of_memexpr32: empty memory expression"
  | _,Some(Esp,_),_            -> invalid_arg "epart_of_memexpr32: ESP used as scale register"
  | None,None,Some(i)          -> EInfo(0,5,None),Some(Id(i))
  | Some(Esp),None,_           -> EInfo(m0d,4,Some(0,4,4)),imm
  | Some(Ebp),None,None        -> EInfo(1,5,None),Some(Ib(0l))
  | Some(Ebp),None,Some(i)     -> EInfo(m0d,5,None),imm
  | Some(br), None,_           -> EInfo(m0d,int_of_reg32 br,None),imm
  | None,Some(sr,sf),None      -> EInfo(0,4,Some(sf,int_of_reg32 sr,5)),Some(Id(0l))
  | None,Some(sr,sf),Some(i)   -> EInfo(0,4,Some(sf,int_of_reg32 sr,5)),Some(Id(i))
  | Some(Ebp),Some(sr,sf),None -> EInfo(1,4,Some(sf,int_of_reg32 sr,5)),Some(Ib(0l))
  | Some(Ebp),Some(sr,sf),_    -> EInfo(m0d,4,Some(sf,int_of_reg32 sr,5)),imm
  | Some(br), Some(sr,sf),_    -> EInfo(m0d,4,Some(sf,int_of_reg32 sr,int_of_reg32 br)),imm
    
let epart_of_x86_addr_expr = function
| Mem16(_,b,s,d) -> epart_of_memexpr16 b s d
| Mem32(_,b,s,d) -> epart_of_memexpr32 b s d

let gather_epart_of_operand = function
| GeneralReg(g) -> EInfo(3,int_of_x86_general_reg g,None),None
| ControlReg(c) -> EInfo(3,int_of_x86_control_reg c,None),None
| DebugReg(d)   -> EInfo(3,int_of_x86_debug_reg d,None),None
| SegReg(s)     -> EInfo(3,int_of_x86_seg_reg s,None),None
| FPUReg(f)     -> EInfo(3,int_of_x86_fpu_reg f,None),None
| MMXReg(m)     -> EInfo(3,int_of_x86_mmx_reg m,None),None
| XMMReg(x)     -> EInfo(3,int_of_x86_xmm_reg x,None),None
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> epart_of_x86_addr_expr m
| Immediate(_)
| JccTarget(_,_)
| FarTarget(_) -> invalid_arg "int_of_x86operand"

let make_modrm_sib_rmdispl enc = function
| [] -> { enc with modrm = None; sib = None; }
| GInfo(g)::EInfo(t,b,s)::[]
| EInfo(t,b,s)::GInfo(g)::[] ->
  { enc with modrm = Some(make_encoded_byte t g b); 
    sib = match s with None -> None | Some(t,m,b) -> Some(make_encoded_byte t m b); }
| _ -> invalid_arg "make_modrm_sib_rmdispl: invalid list of modrm parts"

let gather_gpart_of_operand opnd = GInfo(int_of_x86operand opnd)

(* ROLF 12/2013:
let get_seg_override aop mem = match aop,mem with
| (OYb|OYw|OYd|OYv|OYz),Mem16(ES,_,_,_) -> None
| (OYb|OYw|OYd|OYv|OYz),Mem32(ES,_,_,_) -> None
| _,Mem16(s,Some(Bp),_,_)  when s <> SS
| _,Mem32(s,Some(Esp),_,_) when s <> SS
| _,Mem32(s,Some(Ebp),_,_) when s <> SS 
| _,Mem16(s,_,_,_)         when s <> DS
| _,Mem32(s,_,_,_)         when s <> DS -> Some(byte_of_seg s)
| _ -> None
*)

let get_seg_override aop memexpr = 
  let s = X86Util.default_seg memexpr in
  let s'' = X86Util.get_seg   memexpr in
  match aop,s'' with
  | OYb,ES | OYw,ES | OYd,ES | OYv,ES | OYz,ES -> None
  | _,_ when s <> s'' -> Some(byte_of_seg s'')
  | _,_ -> None

let get_seg_override aop = function
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> get_seg_override aop m
| Immediate(_)
| FarTarget(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> None

let gather_immediate = function
| Immediate(i) -> [i]
| FarTarget(Ap32(seg,offs)) -> [Id(offs);Iw(seg)]
| FarTarget(Ap16(seg,offs)) -> [Iw(offs);Iw(seg)]
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m)) -> 
 (match m with
  | Mem16(_,None,None,Some(i)) -> [Iw(i)]
  | Mem32(_,None,None,Some(i)) -> [Id(i)]
  | _ -> invalid_arg "gather_immediate:  tried to extract immediate from complex memory expression")
| Memexpr(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> invalid_arg "gather_immediate"

let needs_addr_prefix = function
| Memexpr(Mb(m))
| Memexpr(Mw(m))
| Memexpr(Md(m))
| Memexpr(Mf(m))
| Memexpr(Mq(m))
| Memexpr(Mt(m))
| Memexpr(Mdq(m)) -> 
 (match m,!operating_mode with
  | Mem16(_,_,_,_),OM_32
  | Mem32(_,_,_,_),OM_16 -> Some(0x67l)
  | _,_ -> None)
| FarTarget(ap) ->
 (match ap,!operating_mode with
  | Ap16(_,_),OM_32
  | Ap32(_,_),OM_16 -> Some(0x67l)
  | _,_ -> None)
| Immediate(_)
| GeneralReg(_)
| ControlReg(_)
| DebugReg(_)
| SegReg(_)
| FPUReg(_)
| MMXReg(_)
| XMMReg(_)
| JccTarget(_,_) -> None

let ensure_all_somes_equal list failmsg = 
  match (List.filter (fun s -> s <> None) list) with
  | [] -> None
  | s::ss -> 
    if List.for_all (fun z -> s = z) ss 
    then s 
    else failwith failmsg

let get_seg_prefix encl oplist = 
  let seg_overrides = List.map2 (fun aop op -> get_seg_override aop op) encl oplist in
  ensure_all_somes_equal seg_overrides "Multiple conflictory segment prefixes"
  
let get_addrsize_prefix oplist =
  let addr_prefixes = List.map needs_addr_prefix oplist in
  ensure_all_somes_equal addr_prefixes "Multiple conflictory address prefixes"

let get_opsize_prefix aoplist oplist =
  let sslist = 
    List.fold_left2 
     (fun acc aopnd opnd -> 
        match (may_require_size_override aopnd) with
        | Some(s) -> (Some(s <> size_of_operand opnd))::acc
        | None -> acc)
     []
     aoplist
     oplist
  in
  let s = 
    ensure_all_somes_equal 
      sslist 
      "Some operands required size prefixes, some did not (shouldn't happen due to type-checking)" 
  in match s with
  | Some(true) -> Some(0x66l)
  | _ -> None

let does_encoding_match enclist (scstr,acstr) oplist =
  let rec aux el ol = match el,ol with
  | [],[] -> true
  | eo::es,oo::os -> X86TypeCheck.typecheck_operand eo oo && aux es os
  | _ -> false
  in aux enclist oplist 
  && satisfies_size_constraints oplist scstr 
  && satisfies_addr_constraints oplist acstr 

let print_byte = Printf.printf "%02lx"
let print_list = List.iter print_byte

let pp_x86_insn_encoding enc =
  let print_some_byte descr = function | Some(b) -> Printf.printf "%s: %02lx\n" descr b | _ -> () in
  print_some_byte "Group1 prefix" enc.group1pf;
  print_some_byte "Segment prefix" enc.segpf;
  print_some_byte "Operand size prefix" enc.opsizepf;
  print_some_byte "Address size prefix" enc.addrsizepf;
  Printf.printf "Stem: "; print_list enc.stem; print_endline "";
  print_some_byte "Mod R/M byte" enc.modrm;
  print_some_byte "SIB byte" enc.sib;
 (match enc.rmdispl with
  | Some(i) -> Printf.printf "Mod R/M displacement: %s\n" (X86Disasm.string_of_x86_immediate i)
  | None -> ());
  match enc.imms with
  | [] -> ()
  | list -> Printf.printf "Immediates: "; List.iter (fun i -> Printf.printf "%s " (X86Disasm.string_of_x86_immediate i)) list; print_endline ""

let native_size_pf e = match e,!operating_mode with
| Native32(_),OM_32
| Native16(_),OM_16 -> Some(false)
| Native32(_),OM_16
| Native16(_),OM_32 -> Some(true)
| Native16(_),_
| Native32(_),_ -> failwith "Unsupported operating mode"
| _ -> None

let encode_instruction ({ pref=pref; instr=(mnem,oplist) } as i) = 
  let rec aux = function
  | [] -> raise (InvalidOperands(i))
  | ((el,cnstr),ee)::es -> if does_encoding_match el cnstr oplist then (el,ee) else aux es
  in 
  let (encl,encv) = aux (mnem_to_encodings_full mnem) in
  let rec collect_parts (insn,mrmlist,mimmopt) el ol =
    match el,ol with
    | [],[] -> (insn,mrmlist,mimmopt)
    | optype::es,x86opnd::os ->
      let insn,mrmlist,mimmopt = 
        match (enctype_of_abstract_operand optype) with
        | GPart -> (insn,(gather_gpart_of_operand x86opnd)::mrmlist,mimmopt)
        | EPart -> let ep,mio = gather_epart_of_operand x86opnd in (insn,ep::mrmlist,mio)
        | Imm   -> ({ insn with imms = insn.imms@(gather_immediate x86opnd) },mrmlist,mimmopt)
        | NoEnc -> (insn,mrmlist,mimmopt)
      in collect_parts (insn,mrmlist,mimmopt) es os
    | _,_ -> failwith "Impossible internal error:  despite type-checking, encoding and operand lists differed in size"
  in
  (* Starting with an empty encoding, collect up the immediates (in the insn) and ModR/M parts (separate) *)
  let insn,mrmlist,mimmopt = collect_parts (mk_default_x86_insn_encoding (),[],None) encl oplist in
  (* If we had a ModR/M group, add its GPart to the modrm list *)
  let mrmlist = match encv with | ModRMGroup(g,_) -> (GInfo(g))::mrmlist | _ -> mrmlist in
  (* Add the stem to the encoding *)
  let insn = { insn with stem = match encv with | Literal(b) | Native32(b) | Native16(b) | ModRM(b) | ModRMGroup(_,b) -> b } in
  (* Add the ModR/M and perhaps the SIB byte to the encoding *)
  let insn = make_modrm_sib_rmdispl insn mrmlist in
  (* Add the ModR/M immediate, and segment, operand size, and address prefixes, optionally *) 
  let opszpf = 
    match native_size_pf encv with
    | Some(true) -> Some(0x66l)
    | Some(false) -> None
    | None -> get_opsize_prefix encl oplist
  in
  let insn = { insn with 
    rmdispl = mimmopt;
    segpf = get_seg_prefix encl oplist;
    opsizepf = opszpf;
    addrsizepf = get_addrsize_prefix oplist; } 
  in
 (List.map (function | Lock -> 0xF0l | Repne -> 0xF2l | Rep -> 0xF3l) pref)@(get_byte_encoding insn)
