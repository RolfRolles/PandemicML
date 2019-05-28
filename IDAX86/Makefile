IDAX86CMX = IDAX86Graph.cmx IDAX86Disasm.cmx
IDAX86CMO = $(IDAX86CMX:.cmx=.cmo)
IDAX86CMI = $(IDAX86CMX:.cmx=.cmi)
IDAX86OBJ = $(IDAX86CMX:.cmx=.obj)

INCLDIRS = -I ../X86 -I ../IR -I ../IRTranslators -I ../Util

all: $(IDAX86CMO) $(IDAX86CMX) $(IDAX86CMI)

clean:
	rm -rf $(IDAX86CMO) $(IDAX86CMX) $(IDAX86CMI) $(IDAX86OBJ) *.bak .depend

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
