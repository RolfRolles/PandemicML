#use "c:\\paframework\\Projects\\Nxcroticism\\ProbabilisticCandidateGeneration.ml";;

mov byte ptr ds:[eax], dl
ret

let _ = 
  let open X86 in
  let faulting_sequence = 
    { pref = []; instr = (Xor,[Memexpr(Md(Mem32(DS,Some(Edi),None,None)));GeneralReg(Gd(Esi))]);}::
    { pref = []; instr = (Or, [GeneralReg(Gb(Bh));GeneralReg(Gb(Bh))]);}::
    { pref = []; instr = (Adc,[GeneralReg(Gd(Eax));Immediate(Id(0x08373038l))]);}::
    { pref = []; instr = (Xor,[GeneralReg(Gd(Eax));GeneralReg(Gd(Eax))]);}::
    { pref = []; instr = (Ret,[]) }::
    []
  in
  let ir = List.concat (List.map (X86ToIR.translate_instr 0l) faulting_sequence) in
  determine_sequence_behaviors faulting_sequence ir 3;;
  
