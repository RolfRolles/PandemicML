(* Duplicated from X86Encode, factor out *)
let int_of_reg8 = function
| X86.Al -> 0
| X86.Cl -> 1
| X86.Dl -> 2
| X86.Bl -> 3
| X86.Ah -> 4
| X86.Ch -> 5
| X86.Dh -> 6
| X86.Bh -> 7

let int_of_reg16 = function
| X86.Ax -> 0
| X86.Cx -> 1
| X86.Dx -> 2
| X86.Bx -> 3
| X86.Sp -> 4
| X86.Bp -> 5
| X86.Si -> 6
| X86.Di -> 7

let int_of_reg32 = function
| X86.Eax -> 0
| X86.Ecx -> 1
| X86.Edx -> 2
| X86.Ebx -> 3
| X86.Esp -> 4
| X86.Ebp -> 5
| X86.Esi -> 6
| X86.Edi -> 7

let int_of_x86flag = function
| X86.X86F_C -> 0
| X86.X86F_P -> 1
| X86.X86F_A -> 2
| X86.X86F_S -> 3
| X86.X86F_Z -> 4
| X86.X86F_O -> 5
| X86.X86F_D -> 6

module Reg32Comparator = struct
  type t = X86.x86_reg32
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg32 b1) (int_of_reg32 b2)
end
module Reg32Set = Set.Make(Reg32Comparator)

module Reg16Comparator = struct
  type t = X86.x86_reg16
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg16 b1) (int_of_reg16 b2)
end
module Reg16Set = Set.Make(Reg16Comparator)

module Reg8Comparator = struct
  type t = X86.x86_reg8
  let compare b1 b2 = 
    Pervasives.compare (int_of_reg8 b1) (int_of_reg8 b2)
end
module Reg8Set = Set.Make(Reg8Comparator)

module FlagComparator = struct
  type t = X86.x86_flags
  let compare b1 b2 = 
    Pervasives.compare (int_of_x86flag b1) (int_of_x86flag b2)
end
module FlagSet = Set.Make(FlagComparator)