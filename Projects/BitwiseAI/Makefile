BITWISEAICMX = BitwiseAI.cmx BitwiseAINewInterface.cmx
BITWISEAICMO = $(BITWISEAICMX:.cmx=.cmo)
BITWISEAICMI = $(BITWISEAICMX:.cmx=.cmi)
BITWISEAIOBJ = $(BITWISEAICMX:.cmx=.obj)

INCLDIRS = -I ../../X86 -I ../../IR -I ../../IRTranslators -I ../../Util

all: $(BITWISEAICMI) $(BITWISEAICMO) $(BITWISEAICMX)

clean:
	rm -rf $(BITWISEAICMO) $(BITWISEAICMX) $(BITWISEAICMI) $(BITWISEAIOBJ) *.bak .depend

.SUFFIXES: .ml .mli .cmx .cmi .cmo

.ml.cmx:
	ocamlopt $(INCLDIRS) -g -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc.opt $(INCLDIRS) -g -c $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
