UTILCMI = BinaryFileUtil.cmi ASMUtil.cmi #LowLevel.cmi
UTILCMX = BinaryFileUtil.cmx FunctionalUtil.cmx LowLevel.cmx ListUtil.cmx ArrayUtil.cmx StringUtil.cmx Util.cmx
UTILCMO = $(UTILCMX:.cmx=.cmo)

all: $(UTILCMI) $(UTILCMX) $(UTILCMO)

clean:
	rm -rf $(UTILCMI) $(UTILCMX) $(UTILCMO) *.bak *.obj .depend

.SUFFIXES: .ml .mli .cmx .cmi .cmo

.ml.cmx:
	ocamlopt -g -c $<
.mli.cmi:
	ocamlopt -g -c $<
.ml.cmo:
	ocamlc -g -c $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
