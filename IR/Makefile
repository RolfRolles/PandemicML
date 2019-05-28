IRCMX = IRUtil.cmx PpIR.cmx IRLocalOpt.cmx IRTypeCheck.cmx IRSSA.cmx
IRCMO = $(IRCMX:.cmx=.cmo)

all: IR.cmi $(IRCMO) $(IRCMX)

clean:
	rm -rf *.cmi *.cmx *.bak *.obj *.cmo .depend *.bak
	rm -rf parser.ml parser.mli lexer.ml lexer.mli

.SUFFIXES: .ml .mli .mll .mly .cmx .cmi .cmo

.ml.cmx:
	ocamlopt -g -c $<
.mli.cmi:
	ocamlopt -g -c $<
.ml.cmo:
	ocamlc -g -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
