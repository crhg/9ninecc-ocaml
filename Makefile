RESULT=9ninecc

ifeq ($(shell uname),Linux)
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

.PHONY: default
default:
	docker-compose run 9ninecc-env make

.PHONY: test
test:
	docker-compose run 9ninecc-env make test

.PHONY: clean
clean:
	docker-compose run 9ninecc-env make clean

endif
