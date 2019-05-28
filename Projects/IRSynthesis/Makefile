X86CMI=../../X86/X86.cmi

# ../../Util/Generator.cmx 
UTILCMX=../../Util/Util.cmx ../../Util/StringUtil.cmx
X86CMX=../../Test/X86Misc.cmx ../../X86/X86Disasm.cmx ../../X86/X86Util.cmx ../../X86/X86TypeCheck.cmx ../../X86/X86Constraints.cmx ../../X86/X86EncodeUtil.cmx ../../X86/X86EncodeTable.cmx ../../X86/X86Encode.cmx
JITCMX=../../JIT/JITRegion.cmx ../../Test/JITSampler.cmx
JITOBJ=../../JIT/CPPJITRegion.obj ../../JIT/OCamlJITRegion.obj 
IRCMX=../../IR/IRUtil.cmx ../../IR/PpIR.cmx ../../IR/IRTypeCheck.cmx
X86IRCMX=../../IRTranslators/X86ToIRUtil.cmx ../../DPI/Z3SymbolicExecute.cmx
NXCMX=../Nxcroticism/NEWIREvalUtil.cmx ../Nxcroticism/NEWIREval.cmx 
ALLCMX=Generator.cmx RegUtil.cmx SynthesisIR.cmx AnalyzeInstruction.cmx DoX86.cmx

#ALLCMO=$(ALLCMX:.cmx=.cmo)
#UTILCMO=$(UTILCMX:.cmx=.cmo)
#JITCMO=$(JITCMX:.cmx=.cmo)
#IRCMO=$(IRCMX:.cmx=.cmo)
#X86IRCMO=$(X86IRCMX:.cmx=.cmo)

# Harness.exe $(UTILCMO) $(JITCMO) $(X86IRCMO) $(IRCMO) $(ALLCMO) 
all: $(UTILCMX) $(X86CMI) $(X86CMX) $(JITCMX) $(X86CMI) $(X86IRCMX) $(IRCMX) $(NXCMX) $(ALLCMX) DoX86.opt.exe

clean:
	rm -rf z3.* libz3.lib *.exe *.bak *.obj *.cmi *.cmx *.cmo .depend 
	cp z3/* .

.SUFFIXES: .ml .mli .cmx .cmi .cmo .mll .mly .exe

.ml.cmx:
	ocamlopt -g -I ../../JIT -I ../../X86 -I ../../Util -I ../../IR -I ../../IRTranslators -I ../../Test -I ../../DPI -I ../Nxcroticism -c $<
.mli.cmi:
	ocamlopt -g -I ../../JIT -I ../../X86 -I ../../Util -I ../../IR -I ../../IRTranslators -I ../../Test -I ../../DPI -I ../Nxcroticism -c $<
.ml.cmo:
	ocamlc   -g -I ../../JIT -I ../../X86 -I ../../Util -I ../../IR -I ../../IRTranslators -I ../../Test -I ../../DPI -I ../Nxcroticism -c $<

DoX86.opt.exe: Z3.cmxa $(UTILCMX) $(X86CMX) $(JITOBJ) $(JITCMX) $(IRCMX) $(X86IRCMX) $(NXCMX) $(ALLCMX)
	ocamlopt -w -A -o $@ $^

.depend:
	ocamldep *.ml *.mli *.mly *.mll > .depend

include .depend
