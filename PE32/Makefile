PE32CMX = PE32.cmx PEParse.cmx 
PE32CMO = $(PE32CMX:.cmx=.cmo)

all: $(PE32CMO) $(PE32CMX) PEParse.exe

clean:
	rm -rf *.cmi *.cmx *.bak *.obj *.cmo .depend *.bak

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

PEParse.exe: $(PE32CMO)
	ocamlc -g -w -A -o $@ $^

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
