include Config.paths

IDAINCL =$(IDASDK)\include
IDALIB  =$(IDASDK)\libvc.w32\ida.lib
FLEXLINK=flexlink -merge-manifest
FLEXDIR =$(shell $(FLEXLINK) -where)

SYSLIB=$(1).lib

IDACOMMON = idahack.c idagraph_common.c idagraph_c.c
IDAOCAML  = idatop_ocaml.c ocamlside.c idagraph_ocaml.c ocamllib.c idagui_ocaml.c
#idadebug_ocaml.c 
IDACPP    = idaside.cpp idaocaml.cpp idashared.cpp idagraphviewer.cpp  idagui_c.cpp idachooser.cpp
#idatracer.cpp idadebug_c.cpp

IDACOMMONOBJ = $(IDACOMMON:.c=.o)
IDAOCAMLOBJ  = $(IDAOCAML:.c=.o)
IDACPPOBJ    = $(IDACPP:.cpp=.o)

.PHONY: all

IDATopLoop.cmo:
	ocamlc -I "$(OCAMLSRC)/parsing" -I "$(OCAMLSRC)/typing" -I "$(OCAMLSRC)/utils" -I "$(OCAMLSRC)/driver" -c IDATopLoop.ml

OCAMLIDA    = IDA.ml IDATopLoop.ml IDAGraph.ml IDAHotKey.ml IDAGUI.ml IDAColor.ml
#IDATrace.ml
OCAMLIDAINT = $(OCAMLIDA:.ml=.mli)
OCAMLIDACMO = $(OCAMLIDA:.ml=.cmo)
OCAMLIDACMI = $(OCAMLIDA:.ml=.cmi)

EXTRAOCAML = ../Z3/z3.cmo 
EXTRACPP   = ../Z3/z3_stubs.o ../Z3/z3_theory_stubs.o ../Z3/z3.lib 

ida_idatoplib.c: $(EXTRAOCAML) $(OCAMLIDACMO)
	ocamlc -output-obj -o $@ "$(OCAMLSRC)/toplevel/toplevellib.cma" "$(OCAMLSRC)/toplevel/topdirs.cmo" "$(OCAMLLIB)/graph.cmo" $^

ida_idatoplib.o: ida_idatoplib.c
	cl /I"$(OCAMLLIB)" /I"$(FLEXDIR)" -c $< /Fo$@

idaocaml.plw: $(EXTRACPP) $(IDACPPOBJ) $(IDAOCAMLOBJ) $(IDACOMMONOBJ) ida_idatoplib.o
	flexlink -merge-manifest -maindll -o $@ $(call SYSLIB,ws2_32) $(call SYSLIB,ole32) "$(OCAMLLIB)/libcamlidl.lib" "$(IDALIB)" "$(OCAMLLIB)/libcamlrun.lib" $^

clean:
	rm -f $(IDACPPOBJ) $(IDAOCAMLOBJ) $(IDACOMMONOBJ) $(OCAMLIDACMO) $(OCAMLIDACMI) idaocaml.plw ida_idatoplib.c ida_idatoplib.o .depend *.bak

install: $(OCAMLIDACMO) $(OCAMLIDACMI) $(EXTRAOCAML) 
	cp $^ "$(OCAMLLIB)"

all: $(OCAMLIDACMO) idaocaml.plw install

.SUFFIXES: .ml .mli .cmi .cmo .o

INCLDIRS = -I "$(OCAMLLIB)"

.mli.cmi:
	ocamlopt -g -c $<
.ml.cmo:
	ocamlc -g -c $<
.c.o:
	cl /I"$(OCAMLLIB)" -c $< /Fo$@
.cpp.o:
	cl /I"$(OCAMLLIB)" /I"$(IDAINCL)" -D__NT__ -D__IDP__ -c $< /Fo$@

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
