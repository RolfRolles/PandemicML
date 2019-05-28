open X86Predicate
open DataStructures

(* Cheap code to determine register output constant-ness *)
type 'a cplattice = (* ' *)
| Undefined 
| Constant of 'a (* ' *)
| Overdefined

let string_of_cplattice printer = function
| Undefined -> "-"
| Constant(a) -> printer a
| Overdefined -> "+"

let string_of_int_cplattice   = string_of_cplattice string_of_int 
let string_of_int32_cplattice = string_of_cplattice Int32.to_string

let string_of_cplattice which regname el =
  regname^" = "^which el

let string_of_int_cplattice   = string_of_cplattice string_of_int_cplattice  
let string_of_int32_cplattice = string_of_cplattice string_of_int32_cplattice  

type x86_cp =
{
  eax_cp: int32 cplattice;
  ecx_cp: int32 cplattice;
  edx_cp: int32 cplattice;
  ebx_cp: int32 cplattice;
  esp_cp: int32 cplattice;
  ebp_cp: int32 cplattice;
  esi_cp: int32 cplattice;
  edi_cp: int32 cplattice;

  ax_cp: int32 cplattice;
  cx_cp: int32 cplattice;
  dx_cp: int32 cplattice;
  bx_cp: int32 cplattice;
  sp_cp: int32 cplattice;
  bp_cp: int32 cplattice;
  si_cp: int32 cplattice;
  di_cp: int32 cplattice;

  al_cp: int32 cplattice;
  cl_cp: int32 cplattice;
  dl_cp: int32 cplattice;
  bl_cp: int32 cplattice;
  ah_cp: int32 cplattice;
  ch_cp: int32 cplattice;
  dh_cp: int32 cplattice;
  bh_cp: int32 cplattice;

  cf_cp : int cplattice;
  sf_cp : int cplattice;
  zf_cp : int cplattice;
  af_cp : int cplattice;
  of_cp : int cplattice;
  pf_cp : int cplattice;
  df_cp : int cplattice;
}

let join x y = match x,y with
| Undefined,y -> y
| x,Undefined -> x
| Constant(x),Constant(y) when x <> y -> Overdefined
| Constant(_),Constant(_) -> x
| Overdefined,y -> x
| x,Overdefined -> y
  
let print_x86_cp x86_cp =
  let _ = () in
  Printf.printf "%s\n" (string_of_int32_cplattice "eax" x86_cp.eax_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "ecx" x86_cp.ecx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "edx" x86_cp.edx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "ebx" x86_cp.ebx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "esp" x86_cp.esp_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "ebp" x86_cp.ebp_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "esi" x86_cp.esi_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "edi" x86_cp.edi_cp);

  Printf.printf "%s\n" (string_of_int32_cplattice "ax" x86_cp.ax_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "cx" x86_cp.cx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "dx" x86_cp.dx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "bx" x86_cp.bx_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "sp" x86_cp.sp_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "bp" x86_cp.bp_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "si" x86_cp.si_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "di" x86_cp.di_cp);

  Printf.printf "%s\n" (string_of_int32_cplattice "al" x86_cp.al_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "cl" x86_cp.cl_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "dl" x86_cp.dl_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "bl" x86_cp.bl_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "ah" x86_cp.ah_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "ch" x86_cp.ch_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "dh" x86_cp.dh_cp);
  Printf.printf "%s\n" (string_of_int32_cplattice "bh" x86_cp.bh_cp);

  Printf.printf "%s\n" (string_of_int_cplattice   "cf"  x86_cp.cf_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "sf"  x86_cp.sf_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "zf"  x86_cp.zf_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "af"  x86_cp.af_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "of"  x86_cp.of_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "pf"  x86_cp.pf_cp );
  Printf.printf "%s\n" (string_of_int_cplattice   "df"  x86_cp.df_cp )

let default_x86_cp =
{
  eax_cp = Undefined;
  ecx_cp = Undefined;
  edx_cp = Undefined;
  ebx_cp = Undefined;
  esp_cp = Undefined;
  ebp_cp = Undefined;
  esi_cp = Undefined;
  edi_cp = Undefined;

  ax_cp = Undefined;
  cx_cp = Undefined;
  dx_cp = Undefined;
  bx_cp = Undefined;
  sp_cp = Undefined;
  bp_cp = Undefined;
  si_cp = Undefined;
  di_cp = Undefined;

  al_cp = Undefined;
  cl_cp = Undefined;
  dl_cp = Undefined;
  bl_cp = Undefined;
  ah_cp = Undefined;
  ch_cp = Undefined;
  dh_cp = Undefined;
  bh_cp = Undefined;

  cf_cp  = Undefined;
  sf_cp  = Undefined;
  zf_cp  = Undefined;
  af_cp  = Undefined;
  of_cp  = Undefined;
  pf_cp  = Undefined;
  df_cp  = Undefined;
}

let join_x86_cp cp1 cp2 =
{
  eax_cp = join cp1.eax_cp cp2.eax_cp;
  ecx_cp = join cp1.ecx_cp cp2.ecx_cp;
  edx_cp = join cp1.edx_cp cp2.edx_cp;
  ebx_cp = join cp1.ebx_cp cp2.ebx_cp;
  esp_cp = join cp1.esp_cp cp2.esp_cp;
  ebp_cp = join cp1.ebp_cp cp2.ebp_cp;
  esi_cp = join cp1.esi_cp cp2.esi_cp;
  edi_cp = join cp1.edi_cp cp2.edi_cp;

  ax_cp = join cp1.ax_cp cp2.ax_cp;
  cx_cp = join cp1.cx_cp cp2.cx_cp;
  dx_cp = join cp1.dx_cp cp2.dx_cp;
  bx_cp = join cp1.bx_cp cp2.bx_cp;
  sp_cp = join cp1.sp_cp cp2.sp_cp;
  bp_cp = join cp1.bp_cp cp2.bp_cp;
  si_cp = join cp1.si_cp cp2.si_cp;
  di_cp = join cp1.di_cp cp2.di_cp;

  al_cp = join cp1.al_cp cp2.al_cp;
  cl_cp = join cp1.cl_cp cp2.cl_cp;
  dl_cp = join cp1.dl_cp cp2.dl_cp;
  bl_cp = join cp1.bl_cp cp2.bl_cp;
  ah_cp = join cp1.ah_cp cp2.ah_cp;
  ch_cp = join cp1.ch_cp cp2.ch_cp;
  dh_cp = join cp1.dh_cp cp2.dh_cp;
  bh_cp = join cp1.bh_cp cp2.bh_cp;

  cf_cp  = join cp1.cf_cp  cp2.cf_cp;
  sf_cp  = join cp1.sf_cp  cp2.sf_cp;
  zf_cp  = join cp1.zf_cp  cp2.zf_cp;
  af_cp  = join cp1.af_cp  cp2.af_cp;
  of_cp  = join cp1.of_cp  cp2.of_cp;
  pf_cp  = join cp1.pf_cp  cp2.pf_cp;
  df_cp  = join cp1.df_cp  cp2.df_cp;
}

let x86_cp_of_x86_state s =
  let open JITRegion in
  let m16 = Int32.logand 0xffffl in
  let ml8 = Int32.logand 0xffl in
  let mh8 x = Int32.logand (Int32.shift_right_logical x 8) 0xffl in
  let (sf,zf,af,pf,cf,ofl,df) = X86Misc.eflags2fl (Int32.to_int s.eflags) in
  { 
    eax_cp = Constant(s.eax);
    ebx_cp = Constant(s.ebx);
    ecx_cp = Constant(s.ecx);
    edx_cp = Constant(s.edx);
    esp_cp = Constant(s.esp);
    ebp_cp = Constant(s.ebp);
    esi_cp = Constant(s.esi);
    edi_cp = Constant(s.edi); 
    
    ax_cp = Constant(m16 s.eax);
    bx_cp = Constant(m16 s.ebx);
    cx_cp = Constant(m16 s.ecx);
    dx_cp = Constant(m16 s.edx);
    sp_cp = Constant(m16 s.esp);
    bp_cp = Constant(m16 s.ebp);
    si_cp = Constant(m16 s.esi);
    di_cp = Constant(m16 s.edi); 

    al_cp = Constant(ml8 s.eax);
    bl_cp = Constant(ml8 s.ebx);
    cl_cp = Constant(ml8 s.ecx);
    dl_cp = Constant(ml8 s.edx);
    ah_cp = Constant(mh8 s.eax);
    bh_cp = Constant(mh8 s.ebx);
    ch_cp = Constant(mh8 s.ecx);
    dh_cp = Constant(mh8 s.edx);

    cf_cp  = Constant(cf);
    sf_cp  = Constant(sf);    zf_cp  = Constant(zf);    af_cp  = Constant(af);
    of_cp  = Constant(ofl);   pf_cp  = Constant(pf);    df_cp  = Constant(df); }

let get_constant = function | Constant(a) -> Some(a) | _ -> None

let get_constants x86_cp =
  let add_reg32 f r32 set l =
    match get_constant f with
    | Some(a) -> Reg32Set.add r32 set,(RegEquals(X86.Gd(r32),Imm(X86.Id(a))))::l
    | None    -> set,l
  in
    
  let reg32set,l32 = Reg32Set.empty,[] in
  let reg32set,l32 = add_reg32 x86_cp.eax_cp X86.Eax reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.ebx_cp X86.Ebx reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.ecx_cp X86.Ecx reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.edx_cp X86.Edx reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.esp_cp X86.Esp reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.ebp_cp X86.Ebp reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.esi_cp X86.Esi reg32set l32 in
  let reg32set,l32 = add_reg32 x86_cp.edi_cp X86.Edi reg32set l32 in
  
  let add_reg16 f r16 set l =
    match get_constant f with
    | Some(a) -> Reg16Set.add r16 set,(RegEquals(X86.Gw(r16),Imm(X86.Iw(a))))::l
    | None    -> set,l
  in
    
  let reg16set,l16 = Reg16Set.empty,[] in
  let reg16set,l16 = add_reg16 x86_cp.ax_cp X86.Ax reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.bx_cp X86.Bx reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.cx_cp X86.Cx reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.dx_cp X86.Dx reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.sp_cp X86.Sp reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.bp_cp X86.Bp reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.si_cp X86.Si reg16set l16 in
  let reg16set,l16 = add_reg16 x86_cp.di_cp X86.Di reg16set l16 in

  let add_reg8 f r8 set l =
    match get_constant f with
    | Some(a) -> Reg8Set.add r8 set,(RegEquals(X86.Gb(r8),Imm(X86.Ib(a))))::l
    | None    -> set,l
  in
    
  let reg8set,l8 = Reg8Set.empty,[] in
  let reg8set,l8 = add_reg8 x86_cp.al_cp X86.Al reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.bl_cp X86.Bl reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.cl_cp X86.Cl reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.dl_cp X86.Dl reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.ah_cp X86.Ah reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.bh_cp X86.Bh reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.ch_cp X86.Ch reg8set l8 in
  let reg8set,l8 = add_reg8 x86_cp.dh_cp X86.Dh reg8set l8 in

  let add_flag f fl set l =
    match get_constant f with
    | Some(0) -> FlagSet.add fl set,(FlagEquals(fl,BitImm(false)))::l
    | Some(1) -> FlagSet.add fl set,(FlagEquals(fl,BitImm(true)))::l
    | Some(_) -> failwith "Non-boolean flag?"
    | None    -> set,l
  in
  
  let reg1set,l1 = FlagSet.empty,[] in
  let reg1set,l1 = add_flag x86_cp.cf_cp X86.X86F_C reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.sf_cp X86.X86F_S reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.zf_cp X86.X86F_Z reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.af_cp X86.X86F_A reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.of_cp X86.X86F_O reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.pf_cp X86.X86F_P reg1set l1 in
  let reg1set,l1 = add_flag x86_cp.df_cp X86.X86F_D reg1set l1 in

  ((reg32set,reg16set,reg8set,reg1set),(l32,l16,l8,l1))
  
(* This code determines which registers are changed by an instruction *)
let empty_clob = 
{ 
  val32 = Reg32Set.empty; 
  val16 = Reg16Set.empty; 
  val8  = Reg8Set.empty; 
  val1  = FlagSet.empty 
}

let clob =
  let open X86 in
  let m32 r32 = (r32,RegEquals(Gd(r32),X86Reg(Gd(r32)))) in
  let m16 r32 = (r32,RegEquals(Gw(r32),X86Reg(Gw(r32)))) in
  let m8 r32 = (r32,RegEquals(Gb(r32),X86Reg(Gb(r32)))) in
  let m1 f = (f,FlagEquals(f,X86Flag(f))) in
  {
    val32 = [m32 Eax; m32 Ecx; m32 Edx; m32 Ebx; m32 Esp; m32 Ebp; m32 Esi; m32 Edi;];
    val16 = [m16 Ax; m16 Cx; m16 Dx; m16 Bx; m16 Sp; m16 Bp; m16 Si; m16 Di;];
    val8  = [m8 Al; m8 Cl; m8 Dl; m8 Bl; m8 Ah; m8 Ch; m8 Dh; m8 Bh;];
    val1  = [m1 X86F_C; m1 X86F_P; m1 X86F_A; m1 X86F_S; m1 X86F_Z; m1 X86F_O; m1 X86F_D;];
  }

let get_clobbered f_stmt list = 
  List.fold_left (fun acc (r,el) -> if not (f_stmt el) then r::acc else acc) [] list

let get_clobbered f_stmt q_clob =
  let f list = get_clobbered f_stmt list in
  { 
    val32 = List.fold_left (fun set el -> Reg32Set.add el set) q_clob.val32 (f clob.val32);
    val16 = List.fold_left (fun set el -> Reg16Set.add el set) q_clob.val16 (f clob.val16);
    val8  = List.fold_left (fun set el -> Reg8Set.add  el set) q_clob.val8  (f clob.val8);
    val1  = List.fold_left (fun set el -> FlagSet.add  el set) q_clob.val1  (f clob.val1);
  }

let finalize_analysis q_clob x86_cp =
  let (r32set,r16set,r8set,flagset),(l_const32,l_const16,l_const8,l_const1) = get_constants x86_cp in
  let q_const = { val32 = l_const32; val16 = l_const16; val8 = l_const8; val1 = l_const1; } in
  let q_clob = 
  { 
    val32 = Reg32Set.diff q_clob.val32 r32set; 
    val16 = Reg16Set.diff q_clob.val16 r16set; 
    val8  = Reg8Set.diff  q_clob.val8   r8set; 
    val1  = FlagSet.diff  q_clob.val1 flagset;
  } 
  in
  let q_l_f_stmt =
  {
    val32 = Reg32Set.fold (fun r32 l -> (fun x ->  RegEquals(X86.Gd(r32),x))::l) q_clob.val32 [];
    val16 = Reg16Set.fold (fun r32 l -> (fun x ->  RegEquals(X86.Gw(r32),x))::l) q_clob.val16 [];
    val8  = Reg8Set.fold  (fun r32 l -> (fun x ->  RegEquals(X86.Gb(r32),x))::l) q_clob.val8  [];
    val1  = FlagSet.fold  (fun r32 l -> (fun x -> FlagEquals(r32,x))::l)         q_clob.val1  [];
  }
  in
 (q_l_f_stmt,q_const)
