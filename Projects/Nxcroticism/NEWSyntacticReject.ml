(* Reject a sequence immediately if:
* x86 syntactically uses:
* * FS/GS prefix
* * Hard-coded memory accesses
* * 16-bit memory locations when instruction is not LEA
* * Memory locations that are not 8/16/32 bits
* * Control/debug/segment/FPU/XMM/MMX registers
* * Far pointers
* * Jump targets
*)
let x86_syntactic_reject { X86.pref = pref; X86.instr = (m,o); } = 
  let open X86 in
  let reject_operand = function
  (* Don't reject general registers or immediates *)
  | GeneralReg(_) -> false
  | Immediate(_) -> false
  
  (* Reject anything with an FS/GS prefix *)
  | Memexpr(Mb(Mem32((FS|GS),_,_,_))) -> true
  | Memexpr(Mw(Mem32((FS|GS),_,_,_))) -> true
  | Memexpr(Md(Mem32((FS|GS),_,_,_))) -> true

  (* Reject anything with a hard-coded memory address *)
  | Memexpr(Mb(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Mw(Mem32(_,None,None,Some(_)))) -> true
  | Memexpr(Md(Mem32(_,None,None,Some(_)))) -> true

  (* Reject 16-bit memory addresses when the instruction is not LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) when m<>Lea -> true
  | Memexpr(Mw(Mem16(_,_,_,_))) when m<>Lea -> true
  | Memexpr(Md(Mem16(_,_,_,_))) when m<>Lea -> true
  
  (* Accept 16-bit memory access if instruction is LEA *)
  | Memexpr(Mb(Mem16(_,_,_,_))) -> false
  | Memexpr(Mw(Mem16(_,_,_,_))) -> false
  | Memexpr(Md(Mem16(_,_,_,_))) -> false
  
  (* Accept 8/16/32-bit memory access if not rejected above *)
  | Memexpr(Mb(_)) -> false
  | Memexpr(Mw(_)) -> false
  | Memexpr(Md(_)) -> false
  
  (* Reject "large" memory accesses *)
  | Memexpr(Mf(_)) -> true
  | Memexpr(Mq(_)) -> true
  | Memexpr(Mt(_)) -> true
  | Memexpr(Mdq(_)) -> true

  (* Reject anything with a control/debug/segment/FPU/MMX/XMM register *)
  | ControlReg(_) -> true
  | DebugReg(_) -> true
  | SegReg(_) -> true
  | FPUReg(_) -> true
  | MMXReg(_) -> true
  | XMMReg(_) -> true
  
  (* Reject anything with a far pointer *)
  | FarTarget(_) -> true
  
  (* Reject jumps (should have been rejected already anyway) *)
  | JccTarget(_) -> true
  
  in
  (* Reject direction/interrupt flag manipulations/segment modifiers *)
  List.mem m [Cld;Std;Sti;Cli;Lds;Les;Lfs;Lgs] || List.exists reject_operand o

