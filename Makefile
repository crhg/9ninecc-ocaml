RESULT=9ninecc

ifdef 9NINECC_ENV
OCAMLYACC=menhir

PACKS=ppx_deriving.show ppx_deriving.runtime str

GENERATED_SRCS=lexer.ml parser.ml parser.mli pp_lexer.ml pp_parser.ml pp_parser.mli
SRCS=$(sort $(GENERATED_SRCS) $(wildcard *.ml *.mli))
include .sorted_srcs
SOURCES=$(filter-out %parser.mli,$(patsubst %parser.ml,%parser.mly,$(patsubst %lexer.ml,%lexer.mll,$(SORTED_SRCS))))

TRASH=.sorted_srcs

default: debug-native-code

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) $(YFLAGS) --external-tokens Token $<

pp_parser.ml pp_parser.mli: pp_parser.mly
	$(OCAMLYACC) $(YFLAGS) --external-tokens Pp_token $<

.PHONY: test
test: default
	(cd test_source; make test ONLY=$(ONLY))

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
	$(RUN) make test ONLY=$(ONLY)

.PHONY: clean
clean:
	$(RUN) make clean

endif
