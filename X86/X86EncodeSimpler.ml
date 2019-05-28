(* 

Stuff I still need to attend to:
* Read over the multi-operand constraints and generalize them
* Code the rest of the update functions
* Decide how to make it a 16/32-bit capable disassembler

*)

(* Need the X86 type declarations and abstract operands *)
open X86
open X86EncodeUtil
open X86InternalOperand

exception InvalidOperands of X86.x86instrpref

(* Sizes for quantities and ModRMs *)
type size       = S8  | S16 | S32 | S48 | S64 | S80 | S128
type modrm_size = M16 | M32 | M64

(* Defaults for a given processor's encoder *)
type defaults =
{
  e: size;
  r: size;
  v: size;
  z: size;
  modrm_size: modrm_size;
}

(* Settings for 32-bit decoding *)
let defaults32 =
{
  e = S32;
  r = S32;
  v = S32;
  z = S32;
  modrm_size = M32;
}

(* The prefixes that an instruction may have *)
type prefixes =
{
  rep:      bool;
  repne:    bool;
  lock:     bool;
  opsize:   bool;
  addrsize: bool;
  seg:      X86.x86_segreg option;
}

let empty_prefixes = { rep = false; repne = false; lock = false; opsize = false; addrsize = false; seg = None; }

(* Function to encode prefixes into a list of int32s *)
let encode_prefixes p =
  let l = if p.rep      then [0xF3l]   else [] in
  let l = if p.repne    then  0xF2l::l else l  in
  let l = if p.lock     then  0xF0l::l else l  in
  let l = if p.opsize   then  0x66l::l else l  in
  let l = if p.addrsize then  0x67l::l else l  in
  match p.seg with
  | Some(s) -> (byte_of_seg s)::l
  | None -> l

(* Type with everything that a ModRM should have *)
type modrm =
{
  size: modrm_size;
  m0d: int;
  gpart: int;
  rm: int;
  (* SF, SR, BR (scale factor, scale register, base register) *)
  sib: (int * int * int) option;
  (* Have to change this to an immediate Ib/Iw/Id variant *)
  displ: X86.x86_immediate option;
}

(* Default ModRM binding, containing nothing. *)
let empty_modrm = { size = M32; m0d = 0; gpart = 0; rm = 0; sib = None; displ = None; }

(* Function to encode a ModRM. *)
let encode_modrm_opt = function
| None -> []
| Some(m) -> 
  let mrm = [make_encoded_byte m.m0d m.gpart m.rm] in
  let mrm = List.rev (
    match m.sib with 
    | Some(sf,sreg,breg) -> (make_encoded_byte sf sreg breg)::mrm
    | None -> mrm)
  in
  match m.displ with
  | None -> mrm
  | Some(d) -> mrm@(list_of_immediate d)

(* Context for encoding. *)
type encode =
{
  defaults: defaults;
  prefixes: prefixes;
  stem: int32 list;
  modrm: modrm option;
  imms: int32 list;
}

(* Empty encoding structure with nothing in it. *)
let empty_enc = { defaults = defaults32; prefixes = empty_prefixes; stem = []; modrm = None; imms = []; }

(* Turns an encode structure into an array of bytes. *)
let encode e = (encode_prefixes e.prefixes)@(e.stem)@(encode_modrm_opt e.modrm)@(e.imms)
(*List.rev (e.imms@(encode_modrm_opt_rev e.modrm)@(List.rev e.stem)@(encode_prefixes e.prefixes))*)

(* Monadic injection *)
let return s = Some(s)

(* Monadic state *)
type state = 
{
  enc: encode;
  last_operand_size: size option;
  last_address_size: modrm_size option;
}

let bind f = function
| Some(s) -> f s
| None -> None

let empty_state32 = 
{
  enc = empty_enc;
  last_operand_size = None;
  last_address_size = None;
}

(* Add an opsize prefix *)
let opsize_prefix    s = { s with enc = { s.enc with prefixes = { s.enc.prefixes with   opsize = true; }; }; }
(* Add an address size prefix *)
let addrsize_prefix  s = { s with enc = { s.enc with prefixes = { s.enc.prefixes with addrsize = true; }; }; }
(* Add a segment prefix *)
let segment_prefix p s = { s with enc = { s.enc with prefixes = { s.enc.prefixes with   seg = Some(p); }; }; }

(* Used for variable-sized arguments (i.e. -v, -z).  If not present, mark the 
   operand size field and add a size prefix if it differs from the default.  
   If present, ensure that the sizes match.  If they don't match, this is an 
   error and so return None (terminating the monadic computation).  If they
   match, then return the state the way it is, albeit monadically wrapped. *)
let add_opsize default s state =
  match state.last_operand_size with
  | Some(s2) when s2 = s -> Some(state)
  | Some(_) -> None
  | None -> 
    let st = { state with last_operand_size = Some(s); } in 
    Some(if s <> default then opsize_prefix st else st)

(* Wrappers for the above, specifically for -v and -z operands *)
let e_add_opsize s state = add_opsize state.enc.defaults.e s state
let r_add_opsize s state = add_opsize state.enc.defaults.r s state
let v_add_opsize s state = add_opsize state.enc.defaults.v s state
let z_add_opsize s state = add_opsize state.enc.defaults.z s state

(* Used to ensure that encodings with multiple memory operands (of which there
   are very few) have the same address size (i.e. 16 vs. 32-bit) for each
   operand.  If they don't, terminate the monadic computation.  Also, if the 
   address size differs from the default, emit an address-size prefix. *)
let add_addrsize s state =
  match state.last_address_size with
  | Some(s2) when s2 = s -> Some(state)
  | Some(_) -> None
  | None -> 
    let st = { state with last_address_size = Some(s); } in 
    Some(if s <> state.enc.defaults.modrm_size then addrsize_prefix st else st)

(* Add the G-Part to a ModRM encoding. *)
let update_gpart i state = 
  let enc   = state.enc in let modrm = enc.modrm in
  let modrm = match modrm with | Some(m) -> m | None -> empty_modrm in
  { state with enc = { enc with modrm = Some({ modrm with gpart = i; }); }; }

(* d/q operands aren't processed specially at this time. *)
let dq_update_gpart i s state = update_gpart i state

(* Wrappers for the above, specifically for -v and -z operands *)
let  v_update_gpart i s state = v_add_opsize s (update_gpart i state)
let  z_update_gpart i s state = z_add_opsize s (update_gpart i state)

(* Add an immediate to the state. *)
let        update_imms i state = 
  { state with enc = { state.enc with imms = state.enc.imms@(list_of_immediate i); }; }

(* Wrappers for the above, specifically for -v and -z operands *)
let      v_update_imms i s state = v_add_opsize s (update_imms i state)
let      z_update_imms i s state = z_add_opsize s (update_imms i state)

(* Add the E-Part to a ModRM encoding.  This is the case where the E-Part
   specifies some non-memory quantity, i.e. one with m0d = 3. *)
let        update_epart_int i state =
  let enc   = state.enc in let modrm = enc.modrm in
  let modrm = match modrm with | Some(m) -> m | None -> empty_modrm in
  { state with enc = { enc with modrm = Some({ modrm with m0d = 3; rm = i; sib = modrm.sib; }); }; }

(* d/q operands aren't processed specially at this time. *)
let     dq_update_epart_int i s state = update_epart_int i state

(* Wrappers for the above, specifically for -v operands. *)
let      v_update_epart_int i s state = v_add_opsize s (update_epart_int i state)

(* This is a little hack so I don't have to go do surgery on the codebase.  
   The O1 operand type is used in two encodings:  Eb O1 and Ev O1.  We need
   to know the size of the last operand in order to know whether the O1 is
   of the correct size (Ib/Iw/Id).  Therefore, we force the encoder to record
   the size of the Eb operand, so we can typecheck O1 operands properly. *)
let cohere_update_epart_int i s state = let st = update_epart_int i state in { st with last_operand_size = Some(s); }

let acknowledge_address_size s state =
  if s <> state.enc.defaults.modrm_size
  then addrsize_prefix state
  else state

let v_acknowledge_address_size a o state = v_add_opsize o (acknowledge_address_size a state)
let z_acknowledge_address_size a o state = z_add_opsize o (acknowledge_address_size a state)

(* This function decodes a 16-bit ModRM into its component parts (m0d, rm, and
   displ) and adds all necessary prefixes (i.e. segment override). *)
let epart_of_memexpr16 seg b s d state =
  let state = acknowledge_address_size M16 state in
  let modrm = match state.enc.modrm with | Some(m) -> m | None -> empty_modrm in
  let modrm = { modrm with size = M16; } in
  let state = if seg <> (match b with | Some(Bp) -> SS | _ -> DS) then segment_prefix seg state else state in
  let upd m0d rm displ = 
    { state with enc = 
      { state.enc with modrm = Some({ modrm with m0d = m0d; rm = rm; displ = displ; sib = None; }); } }
  in
  let aux rm =
    let imm,m0d = 
    match d with
    | Some(imm) -> 
      if is_quantity_signed_byte imm
      then Some(Ib(Int32.logand imm 0xFFl)),1
      else Some(Iw(Int32.logand imm 0xFFFFl)),2
    | None -> None,0
    in upd m0d rm imm
  in
  match b,s,d with
  | Some(Bx),Some(Si),_   -> aux 0
  | Some(Bx),Some(Di),_   -> aux 1
  | Some(Bp),Some(Si),_   -> aux 2 
  | Some(Bp),Some(Di),_   -> aux 3 
  | Some(Si),None,_       -> aux 4
  | Some(Di),None,_       -> aux 5 
  | Some(Bp),None,Some(_) -> aux 6 
  | Some(Bp),None,None    -> upd 1 6 (Some(Ib(0l)))
  | Some(Bx),None,_       -> aux 7 
  | None,    None,Some(i) -> upd 0 6 (Some(Iw(i)))
  | _,_,_                 -> invalid_arg "epart_of_memexpr16: invalid 16-bit mem expression"

(* This function decodes a 32-bit ModRM into its component parts (m0d, rm, and
   displ, and possibly also a SIB), and adds all necessary prefixes (i.e. 
   segment override).
   *)
let epart_of_memexpr32 seg b s d state =
  let state = acknowledge_address_size M32 state in
  let modrm = match state.enc.modrm with | Some(m) -> m | None -> empty_modrm in
  let modrm = { modrm with size = M32; } in
  let state = if seg <> (match b with | Some(Esp) | Some(Ebp) -> SS | _ -> DS) then segment_prefix seg state else state in
  let upd m0d rm displ sib = 
    { state with enc = 
      { state.enc with modrm = Some({ modrm with m0d = m0d; rm = rm; displ = displ; sib = sib; }); } }
  in

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
  | None,None,Some(i)          -> upd   0 5                (Some(Id(i)))   None
  | Some(Esp),None,_           -> upd m0d 4                 imm           (Some(0,4,4))
  | Some(Ebp),None,None        -> upd   1 5                (Some(Ib(0l)))  None
  | Some(Ebp),None,Some(i)     -> upd m0d 5                 imm            None
  | Some(br), None,_           -> upd m0d (int_of_reg32 br) imm            None
  | None,Some(sr,sf),None      -> upd   0 4                (Some(Id(0l))) (Some(sf,int_of_reg32 sr,5))
  | None,Some(sr,sf),Some(i)   -> upd   0 4                (Some(Id(i)))  (Some(sf,int_of_reg32 sr,5))
  | Some(Ebp),Some(sr,sf),None -> upd   1 4                (Some(Ib(0l))) (Some(sf,int_of_reg32 sr,5))
  | Some(Ebp),Some(sr,sf),_    -> upd m0d 4                 imm           (Some(sf,int_of_reg32 sr,5))
  | Some(br), Some(sr,sf),_    -> upd m0d 4                 imm           (Some(sf,int_of_reg32 sr,int_of_reg32 br))
    
let update_epart_mem m state = match m with
| Mem16(seg,b,s,d) -> epart_of_memexpr16 seg b s d state
| Mem32(seg,b,s,d) -> epart_of_memexpr32 seg b s d state

(* Wrappers for the above, specifically for -v operands. *)
let v_update_epart_mem m s state = v_add_opsize s (update_epart_mem m state)

(* d/q operands aren't processed specially at this time. *)
let     dq_update_epart_mem m s state = update_epart_mem m state

(* See the comment for cohere_update_epart_int as for why this is here. *)
let cohere_update_epart_mem m s state = let st = update_epart_mem m state in { st with last_operand_size = Some(s); }

let   update_non_ds_seg f seg asize state =
  let state = acknowledge_address_size asize state in
  match f state with
  | Some(s) ->
    if seg <> DS
    then 
     (if s.enc.prefixes.seg <> None
      then None
      else Some(segment_prefix seg s))
    else Some(state)
  | None -> None

let v_update_non_ds_seg seg asize s state = update_non_ds_seg (v_add_opsize s) seg asize state
let z_update_non_ds_seg seg asize s state = update_non_ds_seg (z_add_opsize s) seg asize state
let   update_non_ds_seg seg asize   state = update_non_ds_seg return seg asize state

let update_fartarget seg offs s state =
  let state = update_imms seg state in
  let state = update_imms offs state in
  acknowledge_address_size s state
  
let cohere_operand_sizes s state = 
  match state.last_operand_size with
  | Some(s2) when s2 = s -> return state
  | Some(_) -> None
  | None -> return { state with last_operand_size = Some(s); }

let cohere_address_sizes s state = 
  match state.last_address_size with
  | Some(s2) when s2 = s -> return state
  | Some(_) -> None
  | None -> return { state with last_address_size = Some(s); }

let cohere_mp s state =
  match state.last_operand_size with
  | Some(S16) when s = S32 -> return state
  | Some(S32) when s = S48 -> return state
  | Some(_) -> None
  | None -> invalid_arg "cohere_mp"

let cohere_ma s state =
  match state.last_operand_size with
  | Some(S16) when s = S32 -> return state
  | Some(S32) when s = S64 -> return state
  | Some(_) -> None
  | None -> invalid_arg "cohere_ma"

let encode_operand aop rop state = match aop,rop with
(* Implicitly-encoded operands *)
| OAL,    GeneralReg(Gb(Al))  -> return state
| OALR8L, GeneralReg(Gb(Al))  -> return state
| OCL,    GeneralReg(Gb(Cl))  -> return state
| OCLR9L, GeneralReg(Gb(Al))  -> return state
| ODLR10L,GeneralReg(Gb(Dl))  -> return state
| OBLR11L,GeneralReg(Gb(Bl))  -> return state
| OAHR12L,GeneralReg(Gb(Ah))  -> return state
| OCHR13L,GeneralReg(Gb(Ch))  -> return state
| ODHR14L,GeneralReg(Gb(Dh))  -> return state
| OBHR15L,GeneralReg(Gb(Bh))  -> return state
| OAx,    GeneralReg(Gw(Ax))  -> return state
| ODx,    GeneralReg(Gw(Dx))  -> return state
| OCS,    SegReg(CS)          -> return state
| ODS,    SegReg(DS)          -> return state
| OES,    SegReg(ES)          -> return state
| OGS,    SegReg(GS)          -> return state
| OFS,    SegReg(FS)          -> return state
| OSS,    SegReg(SS)          -> return state
| OSt0,   FPUReg(ST0)         -> return state

(* Admissible size depending upon last operand. *)
| O1,     Immediate(Ib(1l))   -> return state
| O1,     Immediate(Iw(1l)) when state.last_operand_size = Some(S16) -> return state
| O1,     Immediate(Id(1l)) when state.last_operand_size = Some(S32) -> return state

| OeAX,   GeneralReg(Gw(Ax))  -> e_add_opsize S16 state
| OeAX,   GeneralReg(Gd(Eax)) -> e_add_opsize S32 state
| OeCX,   GeneralReg(Gw(Cx))  -> e_add_opsize S16 state
| OeCX,   GeneralReg(Gd(Ecx)) -> e_add_opsize S32 state
| OeDX,   GeneralReg(Gw(Dx))  -> e_add_opsize S16 state
| OeDX,   GeneralReg(Gd(Edx)) -> e_add_opsize S32 state
| OeBX,   GeneralReg(Gw(Bx))  -> e_add_opsize S16 state
| OeBX,   GeneralReg(Gd(Ebx)) -> e_add_opsize S32 state
| OeSP,   GeneralReg(Gw(Sp))  -> e_add_opsize S16 state
| OeSP,   GeneralReg(Gd(Esp)) -> e_add_opsize S32 state
| OeBP,   GeneralReg(Gw(Bp))  -> e_add_opsize S16 state
| OeBP,   GeneralReg(Gd(Ebp)) -> e_add_opsize S32 state
| OeSI,   GeneralReg(Gw(Si))  -> e_add_opsize S16 state
| OeSI,   GeneralReg(Gd(Esi)) -> e_add_opsize S32 state
| OeDI,   GeneralReg(Gw(Di))  -> e_add_opsize S16 state
| OeDI,   GeneralReg(Gd(Edi)) -> e_add_opsize S32 state

| OrAXr8, GeneralReg(Gw(Ax))  -> r_add_opsize S16 state
| OrAXr8, GeneralReg(Gd(Eax)) -> r_add_opsize S32 state
| OrAX,   GeneralReg(Gw(Ax))  -> r_add_opsize S16 state
| OrAX,   GeneralReg(Gd(Eax)) -> r_add_opsize S32 state
| OrCXr9, GeneralReg(Gw(Cx))  -> r_add_opsize S16 state
| OrCXr9, GeneralReg(Gd(Ecx)) -> r_add_opsize S32 state
| OrDXr10,GeneralReg(Gw(Dx))  -> r_add_opsize S16 state
| OrDXr10,GeneralReg(Gd(Edx)) -> r_add_opsize S32 state
| OrBXr11,GeneralReg(Gw(Bx))  -> r_add_opsize S16 state
| OrBXr11,GeneralReg(Gd(Ebx)) -> r_add_opsize S32 state
| OrSPr12,GeneralReg(Gw(Sp))  -> r_add_opsize S16 state
| OrSPr12,GeneralReg(Gd(Esp)) -> r_add_opsize S32 state
| OrBPr13,GeneralReg(Gw(Bp))  -> r_add_opsize S16 state
| OrBPr13,GeneralReg(Gd(Ebp)) -> r_add_opsize S32 state
| OrSIr14,GeneralReg(Gw(Si))  -> r_add_opsize S16 state
| OrSIr14,GeneralReg(Gd(Esi)) -> r_add_opsize S32 state
| OrDIr15,GeneralReg(Gw(Di))  -> r_add_opsize S16 state
| OrDIr15,GeneralReg(Gd(Edi)) -> r_add_opsize S32 state

| OSw,    SegReg(s)           -> return (update_gpart (int_of_x86_seg_reg s)     state)
| OCd,    ControlReg(c)       -> return (update_gpart (int_of_x86_control_reg c) state)
| ODd,    DebugReg(d)         -> return (update_gpart (int_of_x86_debug_reg d)   state)

| OIb,    Immediate(Ib(i))    -> return (update_imms (Ib(i)) state)
| OIw,    Immediate(Iw(i))    -> return (update_imms (Iw(i)) state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OIv,    Immediate(Iw(i))    -> v_update_imms (Iw(i)) S16 state
| OIv,    Immediate(Id(i))    -> v_update_imms (Id(i)) S32 state

| OIz,    Immediate(Iw(i))    -> z_update_imms (Iw(i)) S16 state
| OIz,    Immediate(Id(i))    -> z_update_imms (Id(i)) S32 state

| OGb,    GeneralReg(Gb(r))   -> return (update_gpart (int_of_reg8  r) state)
| OGw,    GeneralReg(Gw(r))   -> return (update_gpart (int_of_reg16 r) state)
| OGd,    GeneralReg(Gd(r))   -> return (update_gpart (int_of_reg32 r) state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OGv,    GeneralReg(Gw(r))   -> v_update_gpart (int_of_reg16 r) S16 state
| OGv,    GeneralReg(Gd(r))   -> v_update_gpart (int_of_reg32 r) S32 state

| OGz,    GeneralReg(Gw(r))   -> z_update_gpart (int_of_reg16 r) S16 state
| OGz,    GeneralReg(Gd(r))   -> z_update_gpart (int_of_reg32 r) S32 state

| ORw,    GeneralReg(Gw(r))   -> return (update_epart_int (int_of_reg16 r) state)
| ORd,    GeneralReg(Gd(r))   -> return (update_epart_int (int_of_reg32 r) state)

(* Baked-in business logic: 64-bit doesn't exist *)
| ORv,    GeneralReg(Gw(r))   -> v_update_epart_int (int_of_reg16 r) S16  state
| ORv,    GeneralReg(Gd(r))   -> v_update_epart_int (int_of_reg32 r) S32  state

| ORd_Mb, GeneralReg(Gd(r))   -> return (update_epart_int (int_of_reg32 r) state)
| ORd_Mb, Memexpr(Mb(m))      -> return (update_epart_mem m state)

| ORd_Mw, GeneralReg(Gd(r))   -> return (update_epart_int (int_of_reg32 r) state)
| ORd_Mw, Memexpr(Mw(m))      -> return (update_epart_mem m state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OGd_q,  GeneralReg(Gd(r))   -> return (dq_update_gpart (int_of_reg32 r) S32 state)

(* Need to update the size on this one, as a hack because O1b/O1v are both O1 presently *)
| OEb,    GeneralReg(Gb(r))   -> return (cohere_update_epart_int (int_of_reg8 r) S8 state)
| OEb,    Memexpr(Mb(m))      -> return (cohere_update_epart_mem m S8 state)

| OEw,    GeneralReg(Gw(r))   -> return (update_epart_int (int_of_reg16 r) state)
| OEw,    Memexpr(Mw(m))      -> return (update_epart_mem m state)
| OEd,    GeneralReg(Gd(r))   -> return (update_epart_int (int_of_reg32 r) state)
| OEd,    Memexpr(Md(m))      -> return (update_epart_mem m state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OEd_q,  GeneralReg(Gd(r))   -> return (dq_update_epart_int (int_of_reg32 r) S32  state)
| OEd_q,  Memexpr(Md(m))      -> return (dq_update_epart_mem m S32  state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OEv,    GeneralReg(Gw(r))   -> v_update_epart_int (int_of_reg16 r) S16 state
| OEv,    Memexpr(Mw(m))      -> v_update_epart_mem m S16 state
| OEv,    GeneralReg(Gd(r))   -> v_update_epart_int (int_of_reg32 r) S32 state
| OEv,    Memexpr(Md(m))      -> v_update_epart_mem m S32 state

| OMb,    Memexpr(Mb(m))      -> return (update_epart_mem m state)
| OMw,    Memexpr(Mw(m))      -> return (update_epart_mem m state)
| OMd,    Memexpr(Md(m))      -> return (update_epart_mem m state)
| OMs,    Memexpr(Mf(m))      -> return (update_epart_mem m state)
| OMq,    Memexpr(Mq(m))      -> return (update_epart_mem m state)
| OMdq,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OMpd,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OMps,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)

(* Only for LEA -- this will be less ugly after refactoring the type constructor *)
| OM,     Memexpr(Mb(m))      -> return (update_epart_mem m state)
| OM,     Memexpr(Mw(m))      -> return (update_epart_mem m state)
| OM,     Memexpr(Md(m))      -> return (update_epart_mem m state)
| OM,     Memexpr(Mf(m))      -> return (update_epart_mem m state)
| OM,     Memexpr(Mq(m))      -> return (update_epart_mem m state)
| OM,     Memexpr(Mdq(m))     -> return (update_epart_mem m state)

(* Baked-in business logic: 64-bit doesn't exist *)
| OMd_q,  Memexpr(Md(m))      -> return (dq_update_epart_mem m S32 state)

(* The memory sizes are obviously wrong *)
| OStN,      FPUReg(f)        -> return (update_epart_int (int_of_x86_fpu_reg f) state)
| OFPEnv,    Memexpr(Md(m))   -> return (update_epart_mem m state)
| OFPEnvLow, Memexpr(Md(m))   -> return (update_epart_mem m state)
| OReal4,    Memexpr(Md(m))   -> return (update_epart_mem m state)
| OReal8,    Memexpr(Mq(m))   -> return (update_epart_mem m state)
| OReal10,   Memexpr(Mt(m))   -> return (update_epart_mem m state)
| OSimdState,Memexpr(Md(m))   -> return (update_epart_mem m state)

| ONq,    MMXReg(m)           -> return (update_epart_int (int_of_x86_mmx_reg m) state)

(* I pass on a comment from the decoder: is this correct? *)
| OPd,    MMXReg(m)           -> return (update_gpart (int_of_x86_mmx_reg m) state)
| OPq,    MMXReg(m)           -> return (update_gpart (int_of_x86_mmx_reg m) state)
| OPpi,   MMXReg(m)           -> return (update_gpart (int_of_x86_mmx_reg m) state)

| OVdq,   XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)
| OVpd,   XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)
| OVps,   XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)
| OVsd,   XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)
| OVss,   XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)
| OVq,    XMMReg(m)           -> return (update_gpart (int_of_x86_xmm_reg m) state)

| OUps,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUpd,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUq,    XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUdq,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)

(* Are the memory sizes correct? *)
| OQpi,   MMXReg(m)           -> return (update_epart_int (int_of_x86_mmx_reg m) state)
| OQd ,   MMXReg(m)           -> return (update_epart_int (int_of_x86_mmx_reg m) state)
| OQq ,   MMXReg(m)           -> return (update_epart_int (int_of_x86_mmx_reg m) state)
| OQpi,   Memexpr(Mq(m))      -> return (update_epart_mem m state)
| OQd ,   Memexpr(Mq(m))      -> return (update_epart_mem m state)
| OQq ,   Memexpr(Mq(m))      -> return (update_epart_mem m state)

(* Are the memory sizes correct? *)
| OWdq,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OWps,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OWpd,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OWq ,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OWss,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OWsd,   XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)

| OWdq,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OWps,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OWpd,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OWq ,   Memexpr(Mdq(m))     -> return (update_epart_mem m state)
| OWss,   Memexpr(Md(m))      -> return (update_epart_mem m state)
| OWsd,   Memexpr(Mq(m))      -> return (update_epart_mem m state)

| OUdq_Md,XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUdq_Mq,XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUdq_Mw,XMMReg(m)           -> return (update_epart_int (int_of_x86_xmm_reg m) state)
| OUdq_Md,Memexpr(Md(m))      -> return (update_epart_mem m state)
| OUdq_Mq,Memexpr(Mq(m))      -> return (update_epart_mem m state)
| OUdq_Mw,Memexpr(Mw(m))      -> return (update_epart_mem m state)

(* Hmmm ... look into this *)
| OMa,Memexpr(Md(m))          -> cohere_ma S32 (update_epart_mem m state)
| OMa,Memexpr(Mq(m))          -> cohere_ma S64 (update_epart_mem m state)
| OMp,Memexpr(Md(m))          -> cohere_mp S32 (update_epart_mem m state)
| OMp,Memexpr(Mf(m))          -> cohere_mp S48 (update_epart_mem m state)

| OAp,FarTarget(Ap16(s,o))    -> return (update_fartarget (Iw(s)) (Iw(o)) M16 state)
| OAp,FarTarget(Ap32(s,o))    -> return (update_fartarget (Iw(s)) (Id(o)) M32 state)

| OJb,JccTarget(t,_)          -> None
| OJz,JccTarget(t,_)          -> None

(* Need to specify address size *)
| OXb,Memexpr(Mb(Mem16(s,Some(Si), None,None))) -> update_non_ds_seg s M16 state
| OXw,Memexpr(Mw(Mem16(s,Some(Si), None,None))) -> update_non_ds_seg s M16 state
| OXd,Memexpr(Md(Mem16(s,Some(Si), None,None))) -> update_non_ds_seg s M16 state

(* Need to specify address size *)
| OXb,Memexpr(Mb(Mem32(s,Some(Esi),None,None))) -> update_non_ds_seg s M32 state
| OXw,Memexpr(Mw(Mem32(s,Some(Esi),None,None))) -> update_non_ds_seg s M32 state
| OXd,Memexpr(Md(Mem32(s,Some(Esi),None,None))) -> update_non_ds_seg s M32 state

(* Baked-in business logic: 64-bit doesn't exist *)
| OXv,Memexpr(Mw(Mem16(s,Some(Si), None,None))) -> v_update_non_ds_seg s M16 S16 state
| OXv,Memexpr(Md(Mem16(s,Some(Si), None,None))) -> v_update_non_ds_seg s M16 S32 state

| OXv,Memexpr(Mw(Mem32(s,Some(Esi),None,None))) -> v_update_non_ds_seg s M32 S16 state
| OXv,Memexpr(Md(Mem32(s,Some(Esi),None,None))) -> v_update_non_ds_seg s M32 S32 state

(* Baked-in business logic: OPSIZE = 32 *)
| OXz,Memexpr(Mw(Mem16(s,Some(Si), None,None))) -> z_update_non_ds_seg s M16 S16 state
| OXz,Memexpr(Md(Mem16(s,Some(Si), None,None))) -> z_update_non_ds_seg s M16 S32 state

| OXz,Memexpr(Mw(Mem32(s,Some(Esi),None,None))) -> z_update_non_ds_seg s M32 S16 state
| OXz,Memexpr(Md(Mem32(s,Some(Esi),None,None))) -> z_update_non_ds_seg s M32 S32 state

| OYb,Memexpr(Mb(Mem16(ES,Some(Di), None,None))) -> return (acknowledge_address_size M16 state)
| OYw,Memexpr(Mw(Mem16(ES,Some(Di), None,None))) -> return (acknowledge_address_size M16 state)
| OYd,Memexpr(Md(Mem16(ES,Some(Di), None,None))) -> return (acknowledge_address_size M16 state)

| OYb,Memexpr(Mb(Mem32(ES,Some(Edi),None,None))) -> return (acknowledge_address_size M32 state)
| OYw,Memexpr(Mw(Mem32(ES,Some(Edi),None,None))) -> return (acknowledge_address_size M32 state)
| OYd,Memexpr(Md(Mem32(ES,Some(Edi),None,None))) -> return (acknowledge_address_size M32 state)

(* Baked-in business logic: OPSIZE = 32, 64-bit doesn't exist *)
| OYv,Memexpr(Mw(Mem16(ES,Some(Di), None,None))) -> v_acknowledge_address_size M16 S16 state
| OYv,Memexpr(Md(Mem16(ES,Some(Di), None,None))) -> v_acknowledge_address_size M16 S32 state
| OYv,Memexpr(Mw(Mem32(ES,Some(Edi),None,None))) -> v_acknowledge_address_size M32 S16 state
| OYv,Memexpr(Md(Mem32(ES,Some(Edi),None,None))) -> v_acknowledge_address_size M32 S32 state

| OYz,Memexpr(Mw(Mem16(ES,Some(Di), None,None))) -> z_acknowledge_address_size M16 S16 state
| OYz,Memexpr(Md(Mem16(ES,Some(Di), None,None))) -> z_acknowledge_address_size M16 S32 state

| OYz,Memexpr(Mw(Mem32(ES,Some(Edi),None,None))) -> z_acknowledge_address_size M32 S16 state
| OYz,Memexpr(Md(Mem32(ES,Some(Edi),None,None))) -> z_acknowledge_address_size M32 S32 state

(* Need 16-bit addr size for first one *)
| OOb,Memexpr(Mb(Mem16(s,None,None,Some(w)))) -> return (update_imms (Iw(w)) (acknowledge_address_size M16 state))
| OOb,Memexpr(Mb(Mem32(s,None,None,Some(w)))) -> return (update_imms (Id(w)) (acknowledge_address_size M32 state))

| OOv,Memexpr(Mw(Mem16(s,None,None,Some(w)))) -> v_update_imms (Iw(w)) S16 (acknowledge_address_size M16 state)
| OOv,Memexpr(Mw(Mem32(s,None,None,Some(w)))) -> v_update_imms (Id(w)) S32 (acknowledge_address_size M32 state)
| OOv,Memexpr(Md(Mem16(s,None,None,Some(w)))) -> v_update_imms (Iw(w)) S16 (acknowledge_address_size M16 state)
| OOv,Memexpr(Md(Mem32(s,None,None,Some(w)))) -> v_update_imms (Id(w)) S32 (acknowledge_address_size M32 state)

| _,_ -> None

let typecheck_operand aop rop =
  match encode_operand aop rop empty_state32 with
  | Some(_) -> true
  | None -> false

let mk_default_state enc = let open X86EncodeTable in match enc with
| Literal(s)  -> { empty_state32 with enc = { empty_enc with stem = s; }; }
| Native32(s) -> { empty_state32 with enc = { empty_enc with stem = s; }; }
| Native16(s) -> { empty_state32 with enc = { empty_enc with stem = s; prefixes = { empty_prefixes with opsize = true; } }; }
| ModRM(s)    -> { empty_state32 with enc = { empty_enc with stem = s; }; }
| ModRMGroup(g,s) -> { empty_state32 with enc = { empty_enc with stem = s; modrm = Some({empty_modrm with gpart = g;}); } }

let encode_instruction ({ pref=pref; instr=(mnem,oplist) } as i) = 
  let rec aux = function
  | [] -> raise (InvalidOperands(i))
  | ((ol,_),enc)::ols ->
    let rec aux2 opl ol state = match opl,ol with
    | [],[] -> return state
    | rop::rops,aop::aops -> bind (aux2 rops aops) (encode_operand aop rop state)
    (* This is not an invalid case; some mnems take differing number of args *)
    | _,_ -> None
    in
    let mstate = aux2 oplist ol (mk_default_state enc) in
    match mstate with
    | Some(s) -> encode s.enc
    | None -> aux ols
  in 
  aux (X86EncodeTable.mnem_to_encodings_full mnem)

let get_all_instruction_encodings { pref=pref; instr=(mnem,oplist) } = 
  let rec aux outlist = function
  | [] -> List.rev outlist
  | ((ol,_),enc)::ols ->
    let rec aux2 opl ol state = match opl,ol with
    | [],[] -> return state
    | rop::rops,aop::aops -> bind (aux2 rops aops) (encode_operand aop rop state)
    (* This is not an invalid case; some mnems take differing number of args *)
    | _,_ -> None
    in
    let mstate = aux2 oplist ol (mk_default_state enc) in
    match mstate with
    | Some(s) -> aux ((encode s.enc)::outlist) ols
    | None -> aux outlist ols
  in 
  aux [] (X86EncodeTable.mnem_to_encodings_full mnem)
