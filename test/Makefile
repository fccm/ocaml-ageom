OCAML = ocaml
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
DIR = ../src/
INC = -I $(DIR)

FLIB = AGeom.cma
ILIB = intAGeom.cma

FLIBOPT = AGeom.cmxa
ILIBOPT = intAGeom.cmxa

.PHONY: all
all: test

.PHONY: test
test: testbyte

test_it.cmo: test_it.ml
	$(OCAMLC) -c $<

test_it.cmx: test_it.ml
	$(OCAMLOPT) -c $<

test_float_%.opt: test_float_%.ml test_it.cmx
	$(OCAMLOPT) -o $@ $(INC) $(FLIBOPT) test_it.cmx $<

test_int_%.opt: test_int_%.ml test_it.cmx
	$(OCAMLOPT) -o $@ $(INC) $(ILIBOPT) test_it.cmx $<

.PHONY: testbyte
testbyte: test_it.cmo
	$(OCAML) $(INC) $(FLIB) $< test_float_quat.ml
	$(OCAML) $(INC) $(ILIB) $< test_int_angle_deg.ml
	$(OCAML) $(INC) $(ILIB) $< test_int_angle_rad.ml

.PHONY: testopt
testopt: test_it.cmx
	$(MAKE) test_float_quat.opt    ; ./test_float_quat.opt
	$(MAKE) test_int_angle_deg.opt ; ./test_int_angle_deg.opt
	$(MAKE) test_int_angle_rad.opt ; ./test_int_angle_rad.opt

.PHONY: clean
clean:
	$(RM) *.o *.cm[ioxa] *.opt

