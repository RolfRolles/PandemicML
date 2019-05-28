DETECTVMCMX = X86Util.cmx X86Disasm.cmx X86TypeCheck.cmx X86DecodeCanonical.cmx X86Constraints.cmx X86Encode.cmx X86FeatureUtil.cmx main.cmx
DETECTVMCMO = $(DETECTVMCMX:.cmx=.cmo)
DETECTVMCMI = X86InternalOperand.cmi X86Encode.cmi X86Constraints.cmi X86TypeCheck.cmi
#$(DETECTVMCMX:.cmx=.cmi)
DETECTVMOBJ = $(DETECTVMCMX:.cmx=.obj)

INCLDIRS = -I ../../X86 -I ../../Test -I ../../Util

all: $(DETECTVMCMI) $(DETECTVMCMO) $(DETECTVMCMX) main.exe

clean:
	rm -rf $(DETECTVMCMO) $(DETECTVMCMX) $(DETECTVMCMI) $(DETECTVMOBJ) *.bak .depend

.SUFFIXES: .ml .mli .cmx .cmi .cmo

#../../Util/ArrayUtil.cmx 
main.exe: ../../Util/Util.cmx ../../Util/StringUtil.cmx $(DETECTVMCMX) 
	ocamlopt -g -o $@ $^

.ml.cmx:
	ocamlopt $(INCLDIRS) -g -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc.opt $(INCLDIRS) -g -c $<
.depend:
	ocamldep *.ml *.mli > .depend

include .depend
