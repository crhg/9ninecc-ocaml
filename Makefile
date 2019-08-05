RESULT=9ninecc

ifdef 9NINECC_ENV
OCAMLYACC=menhir

PACKS=ppx_deriving.show ppx_deriving.runtime

GENERATED_SRCS=lexer.ml parser.ml parser.mli
SRCS=$(sort $(GENERATED_SRCS) $(wildcard *.ml *.mli))
include .sorted_srcs
SOURCES=$(filter-out parser.mli,$(subst parser.ml,parser.mly,$(subst lexer.ml,lexer.mll,$(SORTED_SRCS))))

TRASH=.sorted_srcs

default: native-code

.PHONY: test
test: native-code
	(cd test_source; make test)

.sorted_srcs: $(SRCS)
	echo SORTED_SRCS=$(shell $(OCAMLDEP) $(INCLUDES) -sort $(SRCS)) > $@

include OCamlMakefile
else

RUN=docker-compose run 9ninecc-env env 9NINECC_ENV=1

.PHONY: default
default:
	$(RUN) make

.PHONY: test
test:
	$(RUN) make test

.PHONY: clean
clean:
	$(RUN) make clean

endif
