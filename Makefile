MAKEFILE_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

ifdef 9NINECC_ENV
BUILD_DIR=_build_9ninecc_env
9NINECC=$(MAKEFILE_DIR)/$(BUILD_DIR)/default/main.exe
DUNE_OPT=--build-dir=$(BUILD_DIR)


default: 
	dune build --verbose $(DUNE_OPT) --profile dev main.exe

.PHONY: test
test: default
	(cd test_source; make test ONLY=$(ONLY) 9NINECC=$(9NINECC))

.PHONY: clean
clean:
	dune clean $(DUNE_OPT)

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
