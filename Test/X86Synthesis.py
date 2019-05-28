from IR import *
import IRSynthesis

vRes8  = new_var(S8)	
eRes8  = mk_evar(vRes8)
vIn8_1 = new_var(S8)	
eIn8_1 = mk_evar(vIn8_1)
vIn8_2 = new_var(S8)	
eIn8_2 = mk_evar(vIn8_2)

vRes16  = new_var(S16)	
eRes16  = mk_evar(vRes16)
vIn16_1 = new_var(S16)	
eIn16_1 = mk_evar(vIn16_1)
vIn16_2 = new_var(S16)	
eIn16_2 = mk_evar(vIn16_2)

vRes32  = new_var(S32)	
eRes32  = mk_evar(vRes32)
vIn32_1 = new_var(S32)	
eIn32_1 = mk_evar(vIn32_1)
vIn32_2 = new_var(S32)	
eIn32_2 = mk_evar(vIn32_2)

if __name__=="__main__":
	gens = [(mk_add(eIn8_1,eIn8_2),{ Add:[Add,Sub] })]
	egen = IRSynthesis.ExprGenerator([(Binop(eAlAfter,EQ,mk_and(eAl,eBl)),{vBl: [eBl,eCl]})])
	self.legit_exprs = itertools.chain.from_iterable(map(lambda (e,s): e.gen(s),gens))
