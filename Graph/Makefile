CFGCMX = CFG.cmx CFGBuild.cmx 
CFGCMO = $(CFGCMX:.cmx=.cmo)
CFGCMI = $(CFGCMX:.cmx=.cmi)
CFGOBJ = $(CFGCMX:.cmx=.obj)

INCLDIRS = -I ../Util

all: $(CFGCMO) $(CFGCMX) $(CFGCMI)

clean:
	rm -rf $(CFGCMO) $(CFGCMX) $(CFGCMI) $(CFGOBJ) *.bak .depend

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
