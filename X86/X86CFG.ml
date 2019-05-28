module LangX86 =
struct
  type t = X86.x86instrpref
  let disasm addr = let x86,_,cf = X86Decode.decode addr in (x86,cf)
  let disasm_ex addr state = let x86,_,cf = X86Decode.decode addr in (x86,cf,state)
end

module X86CFGBuilder = CFGBuild.MakeGraphBuilder(LangX86)
module X86CFG = X86CFGBuilder.C
