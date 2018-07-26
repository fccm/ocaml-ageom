LIBDIR    = AGeom
OCAML     = ocaml
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt
OCAMLDOC  = ocamldoc
OCAMLFIND = ocamlfind
OCAMLDIR  = $(shell $(OCAMLC) -where)
MKDIR     = mkdir -p
DOC_DIR   = doc
EDITOR    = vim
IMATH_DIR = +intMath
IMATH_INC = -I $(IMATH_DIR)

DESTDIR = $(OCAMLDIR)/$(LIBDIR)

INSTALL_CMD = install
INSTALL_FILES = $(INSTALL_CMD) -m 0644
INSTALL_EXE = $(INSTALL_CMD) -m 0755
INSTALL_DIR = $(INSTALL_CMD) -d

.PHONY: all lib opt
all: lib opt
lib: AGeom.cma
opt: AGeom.cmxa

.PHONY: int intlib intopt
int: intlib intopt
intlib: intAGeom.cma
intopt: intAGeom.cmxa

.SECONDARY:

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cma: %.cmo
	$(OCAMLC) -a -o $@ $<

%.cmxa: %.cmx
	$(OCAMLOPT) -a -o $@ $<

intAGeom.cmo: intAGeom.ml intAGeom.cmi
	$(OCAMLC) -c $(IMATH_INC) $<

intAGeom.cmx: intAGeom.ml intAGeom.cmi
	$(OCAMLOPT) -c $(IMATH_INC) $<

intAGeom.cma: intAGeom.cmo
	$(OCAMLC) -a -o $@ $(IMATH_INC) intMath.cma $<

intAGeom.cmxa: intAGeom.cmx
	$(OCAMLOPT) -a -o $@ $(IMATH_INC) intMath.cmx $<

.PHONY: findinstall
findinstall:
	$(OCAMLFIND) install $(LIBDIR) META *.cm[iax] *.cmx[a] *.mli *.a

.PHONY: manualinstall
manualinstall: META intMath.cma
	$(INSTALL_DIR) $(DESTDIR)
	$(INSTALL_FILES) META *.a *.cm[iax] *.cmx[a] *.mli $(DESTDIR)
	$(INSTALL_EXE) *.cmx[s] $(DESTDIR)

.PHONY: install
install: findinstall

.PHONY: edit
edit:
	$(EDITOR) AGeom.ml AGeom.mli

.PHONY: editint
editint:
	$(EDITOR) intAGeom.ml intAGeom.mli

.PHONY: doc
doc: AGeom.mli intAGeom.mli
	$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -html $^

.PHONY: cleandoc
cleandoc:
	$(RM) -r $(DOC_DIR)

.PHONY: clean
clean:
	$(RM) *.[oa] *.cm[ioxa] *.cmx[as] *.exe *.byte *.opt

