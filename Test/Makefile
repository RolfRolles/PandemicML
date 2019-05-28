TESTCMX = X86Misc.cmx X86Predicate.cmx X86PredicateEvaluator.cmx X86PredicateGenerator.cmx JITSampler.cmx DataStructures.cmx batInnerWeaktbl.cmx Z3Integration.cmx IOAnalysis.cmx X86SemanticsDivinator.cmx X86IRSynthesizer.cmx X86Random.cmx X86RandomTest.cmx X86ToIRRandomTest.cmx X86TestUtil.cmx
#generate_blob.cmx
TESTCMO = $(TESTCMX:.cmx=.cmo)
TESTCMI = $(TESTCMX:.cmx=.cmi)
TESTEXE = X86IRSynthesizer.exe X86RandomTest.exe X86ToIRRandomTest.exe 
#generate_blob.exe

INCLDIRS = -I ../X86 -I ../IR -I ../JIT -I ../IRTranslators -I ../Util -I ../Z3

all: $(TESTCMO) $(TESTCMX) $(TESTEXE) 

clean:
	rm -rf $(TESTCMO) $(TESTCMX) $(TESTCMI) *.bak .depend *.obj *.cmi

#generate_blob.exe: ../Util/StringUtil.cmx ../Util/Util.cmx ../X86/X86Disasm.cmx ../X86/X86TypeCheck.cmx ../X86/X86Constraints.cmx ../X86/X86Util.cmx ../X86/X86Decode.cmx ../X86/X86EncodeTable.cmx ../X86/X86EncodeUtil.cmx ../X86/X86Encode.cmx ../X86/X86EncodeSimpler.cmx X86Random.cmx generate_blob.cmx
#	ocamlopt -g -o $@ $^

X86IRSynthesizer.exe: ../Z3/Z3.cmxa batInnerWeaktbl.cmx ../Util/ListUtil.cmx ../Util/StringUtil.cmx ../Util/Util.cmx ../JIT/JITRegion.cmx ../JIT/OCamlJitRegion.obj ../JIT/CPPJitRegion.obj ../X86/X86Disasm.cmx ../X86/X86TypeCheck.cmx ../X86/X86Constraints.cmx ../X86/X86Util.cmx  ../X86/X86EncodeTable.cmx ../X86/X86EncodeUtil.cmx ../X86/X86Encode.cmx X86Misc.cmx X86Predicate.cmx JITSampler.cmx DataStructures.cmx X86PredicateEvaluator.cmx X86PredicateGenerator.cmx Z3Integration.cmx IOAnalysis.cmx X86SemanticsDivinator.cmx X86IRSynthesizer.cmx 
	ocamlopt -w -A -o $@ $^
#	ocamlopt -g -o $@ $^

# This unwieldiness suggests I should move this test into to X86
# However, X86ToIRRandomTest is going to be ugly no matter what
X86RandomTest.exe: ../Util/ListUtil.cmx ../Util/StringUtil.cmx ../Util/Util.cmx ../X86/X86Disasm.cmx ../X86/X86TypeCheck.cmx ../X86/X86Constraints.cmx ../X86/X86Util.cmx ../X86/X86Decode.cmx ../X86/X86EncodeTable.cmx ../X86/X86EncodeUtil.cmx ../X86/X86Encode.cmx ../X86/X86EncodeSimpler.cmx X86Random.cmx X86RandomTest.cmx 
	ocamlopt -g -o $@ $^

X86ToIRRandomTest.exe: ../Util/ListUtil.cmx ../Util/StringUtil.cmx ../Util/Util.cmx ../JIT/JITRegion.cmx ../JIT/OCamlJitRegion.obj ../JIT/CPPJitRegion.obj ../X86/X86Util.cmx ../X86/X86Disasm.cmx ../X86/X86Decode.cmx ../X86/X86TypeCheck.cmx ../X86/X86Constraints.cmx ../X86/X86EncodeTable.cmx ../X86/X86EncodeUtil.cmx ../X86/X86Encode.cmx ../X86/X86EncodeSimpler.cmx X86Random.cmx ../IR/IRUtil.cmx ../IR/PpIR.cmx ../IR/IRTypeCheck.cmx ../IR/IRLocalOpt.cmx ../IRTranslators/X86ToIRUtil.cmx ../IRTranslators/X86ToIR.cmx X86TestUtil.cmx X86ToIRRandomTest.cmx
	ocamlopt -g -o $@ $^


.SUFFIXES: .ml .mli .cmx .cmi .cmo .exe

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
