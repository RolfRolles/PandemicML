X86ANALYZECMX = X86AnalyzeDefUse.cmx X86LocalConstant.cmx X86GlobalOpt.cmx X86Stack.cmx
X86ANALYZECMO = $(X86ANALYZECMX:.cmx=.cmo)
X86ANALYZECMI = $(X86ANALYZECMX:.cmx=.cmi)
X86ANALYZEOBJ = $(X86ANALYZECMX:.cmx=.obj)

INCLDIRS = -I ../X86 -I ../IR -I ../IRTranslators -I ../Util

all: $(X86ANALYZECMO) $(X86ANALYZECMX) $(X86ANALYZECMI)

clean:
	rm -rf $(X86ANALYZECMO) $(X86ANALYZECMX) $(X86ANALYZECMI) $(X86ANALYZEOBJ) *.bak .depend

.SUFFIXES: .ml .mli .cmx .cmi .cmo

.ml.cmx:
	ocamlopt $(INCLDIRS) -g -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc $(INCLDIRS) -g -c $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
