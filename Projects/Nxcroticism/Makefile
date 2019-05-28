ADDITIONALCMO = ../../Util/FunctionalUtil.cmo ../../Util/BinaryFileUtil.cmo ../../Util/ListUtil.cmo \
      ../../Util/StringUtil.cmo ../../Util/Util.cmo ../../X86/X86Disasm.cmo \
      ../../X86/X86TypeCheck.cmo ../../X86/X86Util.cmo ../../X86/X86Constraints.cmo ../../X86/X86EncodeUtil.cmo \
      ../../X86/X86EncodeTable.cmo ../../X86/X86Encode.cmo ../../X86/X86Decode.cmo ../../PE32/PE32.cmo \
      ../../PE32/PEParse.cmo ../../IR/IRUtil.cmo ../../IR/PpIR.cmo ../../IR/IRTypeCheck.cmo \
      ../../IR/IRLocalOpt.cmo ../../IR/IRSSA.cmo ../../IRTranslators/X86ToIRUtil.cmo \
      ../../IRTranslators/X86ToIR.cmo ../../DPI/Z3SymbolicExecute.cmo
ADDITIONALCMX = $(ADDITIONALCMO:.cmo=.cmx)

CMX = NaryTree.cmx Args.cmx FrameworkUtil.cmx DataStructures.cmx IOPair.cmx \
      IRRandomizedEvaluator.cmx GenerateIOPairs.cmx VerifyUtil.cmx \
      IOEsp.cmx IORead.cmx IOWrite.cmx IOCopy.cmx IOConstant.cmx IOPreserve.cmx \
      IOBinop.cmx \
      Candidate.cmx VerifyCandidates.cmx SequenceVerifier.cmx \
      Encodable.cmx GadgetCache.cmx CacheVerifiedResults.cmx LightTunnel.cmx \
      SequenceGenerator.cmx \
      NXcroticism.cmx 
CMO = $(CMX:.cmx=.cmo)
CMI = NaryTree.cmi Args.cmi 
#$(CMX:.cmx=.cmi)

NEWCMX = Generator.cmx GeneratorDSL.cmx NEWIREvalUtil.cmx NEWIREval.cmx NEWIRRandomizedEvaluator.cmx NEWX86IRRandomizedEvaluator.cmx NEWVerify.cmx \
         NEWGadget.cmx NEWIOStruct.cmx NEWGadgetGenerator.cmx NEWSyntacticReject.cmx NEWMemloc.cmx NEWSmallerROPC.cmx 

NEWCMO = $(NEWCMX:.cmx=.cmo)
NEWCMI = $(NEWCMX:.cmx=.cmi)

PEEPCMX = NEWIREvalUtil.cmx NEWIREval.cmx NEWIRRandomizedEvaluator.cmx NEWX86IRRandomizedEvaluator.cmx NEWVerify.cmx Peep.cmx
PEEPCMO = $(PEEPCMX:.cmx=.cmo)
PEEPCMI = $(PEEPCMX:.cmx=.cmi)

ALL = $(CMI) $(CMX) $(CMO) $(NEWCMI) $(NEWCMX) $(NEWCMO) $(PEEPCMI) $(PEEPCMX) $(PEEPCMO) NXcroticism.exe NXcroticism.opt.exe New.exe Peep.exe

all: $(NEWCMX) $(NEWCMO) $(NEWCMI) $(CMI) $(CMX) $(CMO) New.exe
# NXcroticism.exe NXcroticism.opt.exe 

clean:
	rm -rf $(ALL) *.bak *.obj .depend
#	cp z3/z* .

.SUFFIXES: .ml .mli .cmx .cmi .cmo .mll .mly .exe

.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<
.ml.cmx:
	ocamlopt -g -I ../../Util -I ../../X86 -I ../../PE32 -I ../../IR -I ../../IRTranslators -I ../../Z3 -I ../../DPI -c $<
.mli.cmi:
	ocamlopt -g -I ../../Util -I ../../X86 -I ../../PE32 -I ../../IR -I ../../IRTranslators -c $<
.ml.cmo:
	ocamlc -g -I ../../Util -I ../../X86 -I ../../PE32 -I ../../IR -I ../../IRTranslators -I ../../Z3 -I ../../DPI -c $<

New.exe:  ../../Z3/Z3.cma $(ADDITIONALCMO) $(NEWCMO)
	ocamlc -g -w -A -o $@ $^

Peep.exe:  ../../Z3/Z3.cma $(ADDITIONALCMO) $(PEEPCMO)
	ocamlc -g -w -A -o $@ $^

# Z3.cmxa
NXcroticism.exe:  ../../Z3/Z3.cma $(ADDITIONALCMO) $(CMO)
	ocamlc -g -w -A -o $@ $^

NXcroticism.opt.exe: ../../Z3/Z3.cmxa $(ADDITIONALCMX) $(CMX) 
	ocamlopt -w -A -o $@ $^

.depend:
	ocamldep *.ml *.mli *.mly *.mll > .depend

include .depend
