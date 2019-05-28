module X86CFG : CFG.CFG with type language = X86.x86instrpref list
module X86CFGBuilder: CFGBuild.S with type lang = X86.x86instrpref