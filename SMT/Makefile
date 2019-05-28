#ML  = PL.ml
CMX = PL.cmx RandPL.cmx PLTVLAI.cmx CNF.cmx BitVector.cmx BVPrettyPrint.cmx BVTypeCheck.cmx BitBlast.cmx 
CMO = $(CMX:.cmx=.cmo)
CMI = BV.cmi $(CMX:.cmx=.cmi)

# $(ML) 
all: $(CMX) $(CMO) BitBlast.exe RandPL.exe

# $(ML) 
clean:
	rm -rf $(CMI) $(CMX) $(CMO) *.bak *.obj .depend BitBlast.exe

.SUFFIXES: .ml .mli .cmx .cmi .cmo .mll .mly .exe

#PL.ml:
#	cp PL-complete.ml PL.ml

.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<
.ml.cmx:
	ocamlopt -g -I ../../Util -I ../../X86 -c $<
.mli.cmi:
	ocamlopt -g -I ../../Util -I ../../X86 -c $<
.ml.cmo:
	ocamlc -g -I ../../Util -I ../../X86 -c $<

#BitBlast.exe: $(CMX)
#	ocamlopt -w -A -o $@ $^

BitBlast.exe: $(CMO)
	ocamlc -g -w -A -o $@ $^

RandPL.exe: $(CMO)
	ocamlc -g -w -A -o $@ $^

.depend:
	ocamldep *.ml *.mli *.mly *.mll > .depend

include .depend
