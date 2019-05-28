IRTRANSCMX = X86ToIR.cmx X86ToIRUtil.cmx
IRTRANSCMO = $(IRTRANSCMX:.cmx=.cmo)
IRTRANSCMI = $(IRTRANSCMX:.cmx=.cmi)
IRTRANSOBJ = $(IRTRANSCMX:.cmx=.obj)

INCLDIRS = -I ../X86 -I ../IR

all: $(IRTRANSCMO) $(IRTRANSCMX) $(IRTRANSCMI)

clean:
	rm -rf $(IRTRANSCMO) $(IRTRANSCMX) $(IRTRANSCMI) $(IRTRANSOBJ) *.bak .depend

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
