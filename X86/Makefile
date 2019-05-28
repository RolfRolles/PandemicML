X86CMX  = X86Util.cmx X86TypeCheck.cmx X86Constraints.cmx X86Disasm.cmx X86EncodeUtil.cmx X86EncodeTable.cmx X86Encode.cmx X86Decode.cmx X86CFG.cmx X86LocalOpt.cmx X86CFGAssembler.cmx  X86TextualAssembler.cmx X86InternalOperand.cmx X86EncodeSimpler.cmx
# X86GenerateCDecoder.cmx 
X86CMO  = $(X86CMX:.cmx=.cmo)

INCLDIRS = -I ../Util -I ../Graph
INCLMODULES = 

#X86GenerateCDecoder.exe:  ../Util/Util.cmx ../Util/StringUtil.cmx X86Disasm.cmx X86Decode.cmx X86GenerateCDecoder.cmx
#	ocamlopt -g -o $@ $^

all: $(X86CMX) $(X86CMO) 
#X86GenerateCDecoder.exe

clean:
	rm -rf $(X86CMX) $(X86CMO) *.cmi *.bak .depend
	rm -rf ./ocamldoc/*

.SUFFIXES: .ml .mli .cmx .cmi .cmo

.ml.cmx:
	ocamlopt $(INCLDIRS) -g -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc $(INCLDIRS) -g -c $<
.cmx.exe:
	ocamlopt $(INCLDIRS) -g -o $@ $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
