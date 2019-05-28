val get_def_uses : X86.x86instrpref -> IRUtil.VarSet.t * IRUtil.VarSet.t
val block_dse_transfer : X86.x86instrpref list -> IRUtil.VarSet.t -> IRUtil.VarSet.t
val remove_dead_instructions : X86.x86instrpref list -> IRUtil.VarSet.t -> X86.x86instrpref list * bool