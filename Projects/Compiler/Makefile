CMX = Parser.cmx Lexer.cmx Main.cmx 
CMO = $(CMX:.cmx=.cmo)
CMI = $(CMX:.cmx=.cmi) TypeDecl.cmi
OBJ = $(CMX:.cmx=.obj)

# -I ../../X86 
INCLDIRS =

all: .depend TypeDecl.cmi Parser.ml Lexer.ml Parser.cmi $(CMO) Main.exe

clean:
	rm -rf .depend Lexer.ml Parser.ml $(CMO) $(CMX) $(CMI) $(OBJ) *.bak *.exe

.SUFFIXES: .ml .mli .mll .mly .cmx .cmi .cmo

.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<
.ml.cmx:
	ocamlopt $(INCLDIRS) -g -c $<
.mli.cmi:
	ocamlopt $(INCLDIRS) -g -c $<
.ml.cmo:
	ocamlc $(INCLDIRS) -g -c $<

Main.exe:  Parser.cmo Lexer.cmo Main.cmo
	ocamlc -g -w -A -o $@ $^

.depend:
	ocamldep *.ml *.mli *.mll *.mly > .depend

include .depend
