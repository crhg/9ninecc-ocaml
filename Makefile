TARGET=9ninecc

ifeq ($(shell uname),Linux)
OCAMLC=ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep
MENHIR=menhir
OCAMLLEX=ocamllex

INCLUDES=
OCAMLFLAGS=$(INCLUDES)
PACKAGES=-package ppx_deriving.show,ppx_deriving.runtime
OCAMLOPTFLAGS=$(PACKAGES) $(INCLUDES)

GENERATED_SRCS=lexer.ml parser.ml
SRCS=$(sort $(GENERATED_SRCS) $(wildcard *.ml))
include .sorted_srcs
OBJS=$(SORTED_SRCS:.ml=.cmx)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLOPT) -o $@ -linkpkg $(OCAMLOPTFLAGS) $(OBJS)

parser.cmx: parser.cmi

.PHONY: test
test: 9ninecc
	./test.sh

# suffix rules
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.mli: %.ml
	$(OCAMLC) -i $< > $@

%.cmi: %.mli
	$(OCAMLC) -c $<

%.ml: %.mly
	$(MENHIR) $<

%.ml: %.mll
	$(OCAMLLEX) $<

# Dependencies
.depend: $(SRCS)
	$(OCAMLDEP) $(INCLUDES) $(SRCS) > $@

include .depend

.sorted_srcs: $(SRCS)
	echo SORTED_SRCS=$(shell $(OCAMLDEP) $(INCLUDES) -sort $(SRCS)) > $@

else

all:
	docker-compose run 9ninecc-env make

.PHONY: test
test:
	docker-compose run 9ninecc-env make test

endif

.PHONY: clean
clean:
	rm -f .depend .sorted_srcs parser.ml parser.mli lexer.ml *.cmi *.cmx *.cmo *.o *~ $(TARGET) tmp*

