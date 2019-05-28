#TARGET = X86RandomTest

#JITTest.exe: all
#	ocamlopt -g -o $@  X86EnDecodeUtil.cmx X86Util.cmx X86TypeCheck.cmx X86Constraints.cmx X86Disasm.cmx X86Encode.cmx X86Decode.cmx X86Random.cmx X86JITBeginnings.cmx

all: JITRegion.cmx CPPJITRegion.obj OCamlJITRegion.obj

CPPJITRegion.obj:
	cl /c CPPJITRegion.c

OCamlJITRegion.obj:
	cl /I"$(OCAMLLIB)" /c OCamlJITRegion.c

install: all
	cp *.cm* "$(OCAMLLIB)"

clean:
	rm -rf *.cmi *.cmx $(TARGET).exe *.bak *.obj
	rm -rf parser.ml parser.mli lexer.ml lexer.mli .depend

mainclean:
	rm -rf X86RandomTest.exe X86RandomTest.cmx
	rm -rf X86JITBeginnings.exe X86JITBeginnings.cmx

.SUFFIXES: .ml .mli .mll .mly .cmx .cmi

.ml.cmx:
	ocamlopt -g -c $<
.mli.cmi:
	ocamlopt -g -c $<
.cmx:
	ocamlopt -g -c $<
.cmi:
	ocamlopt -g -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
