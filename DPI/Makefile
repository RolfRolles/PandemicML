CMIS  = Z3TextualParsedModel.cmi Z3ModelParser.cmi 
CMOS  = Z3SymbolicExecute.cmo Z3ModelParser.cmo Z3ModelLexer.cmo Z3Util.cmo
CMXS  = $(CMOS:.cmo=.cmx)
LEXYACCML  = Z3ModelLexer.ml Z3ModelParser.mli Z3ModelParser.ml
LEXYACCCMX = $(LEXYACCCML:.ml=.cmx)
LEXYACCCMO = $(LEXYACCCML:.ml=.cmo)
LEXYACCCMI = $(LEXYACCCML:.ml=.cmo)
ALL = $(LEXYACCML) $(LEXYACCCMX) $(LEXYACCCMO) $(CMI) $(CMX) $(CMO)

INCLDIRS = -I ../Z3 -I ../IR  -I ../IRTranslators

all: $(LEXYACCML) $(LEXYACCCMX) $(LEXYACCCMO) $(LEXYACCCMI) $(CMIS) $(CMOS) $(CMXS)

.SUFFIXES: .ml .mli .mll .mly .cmx .cmi .cmo .o .c

.ml.cmx:
	ocamlopt -g $(INCLDIRS) -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc $(INCLDIRS) -g -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

clean:
	rm -rf *.cmi *.cmo *.cmx .depend *.bak

.depend:
	ocamldep $(INCLDIRS) *.ml *.mli > .depend

include .depend
