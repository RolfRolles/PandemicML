CPART = z3_stubs.c z3_theory_stubs.c
OPART = $(CPART:.c=.o)
CMOS  = z3.cmo
CMXS  = $(CMOS:.cmo=.cmx)
CMIS  = $(CMOS:.cmo=.cmi)

Z3BINDIR = c:\Program Files (x86)\Microsoft Research\Z3-4.1\bin
XLIBPATH = /LIBPATH:"$(Z3BINDIR)"

all: .depend $(CPART) $(OPART) $(CMIS) $(CMOS) $(CMXS) z3.cma z3.cmxa libz3.lib copyobj

.SUFFIXES: .ml .mli .cmx .cmi .cmo .o .c

.ml.cmx:
	ocamlopt -g -c $<
.mli.cmi:
	ocamlopt -g -c $<
.ml.cmo:
	ocamlc -g -c $<
.c.o:
	cl /I"$(OCAMLLIB)" -c $<

libz3.lib:
	lib /nologo $(XLIBPATH) /out:libz3.lib "$(Z3BINDIR)\z3.lib" z3.obj z3_stubs.obj z3_theory_stubs.obj

z3.cma:
	ocamlc -custom -a -o z3.cma -cclib -L"$(Z3BINDIR)" -cclib -lz3 -cclib ole32.lib -cclib -lcamlidl z3.cmo

z3.cmxa:
	ocamlopt -a -o z3.cmxa -cclib -L"$(Z3BINDIR)" -cclib -lz3 -cclib ole32.lib -cclib -lcamlidl z3.cmx

clean:
	rm -rf $(CMIS) $(CMOS) $(CMXS) $(OPART) .depend


copyobj:
	rm -rf z3.lib && cp z3-default.lib z3.lib


.depend:
	ocamldep $(INCLDIRS) *.ml *.mli > .depend

include .depend
