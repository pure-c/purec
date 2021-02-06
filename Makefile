CLANG ?= clang
CFLAGS ?=

SHELL := /bin/bash
SHELLFLAGS := -eo pipefail

PURS := PATH=$$PATH:node_modules/.bin purs
SPAGO := PATH=$$PATH:node_modules/.bin spago

PUREC_JS := purec.js
PUREC := node $(PUREC_JS)
PUREC_WORKDIR := .purec-work
PUREC_LIB := libpurec.a
PUREC_INTERMEDIATE_LIB := libpurec.intermediate.a

BWDGC_V := v8.0.0
BWDGC_LIB := deps/bwdgc/.libs/libgc.a
BWDGC_INCLUDE_DIR := deps/bwdgc/include

RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = \
	$(patsubst %.c,%.o,$(RUNTIME_SOURCES))

TESTS = \
    00-basic \
    01-partialfuns \
    02-foreign \
    03-mutrec \
    04-memory \
    05-datacons \
    06-typeclasses \
    10-prelude \
    11-effects \
    12-rec-fns

ifdef UNIT_TESTING
CFLAGS += \
	-g \
	-D UNIT_TESTING
endif

ifeq ($(USE_GC),1)
CFLAGS+=-DUSE_GC -I$(BWDGC_INCLUDE_DIR)
endif

$(BWDGC_LIB):
	@$(MAKE) -s deps/bwdgc
	@echo 'Building bwdgc static library...'
	@cd deps/bwdgc && \
	    ./autogen.sh && \
	    ./configure --enable-static && \
	    $(MAKE)

$(PUREC_INTERMEDIATE_LIB): $(RUNTIME_OBJECTS)
	@ar csr $@ $^

$(PUREC_LIB): $(PUREC_INTERMEDIATE_LIB)
	@rm -rf .build
	@mkdir -p .build
	@cd .build &&\
		for a in $^; do\
			ar x ../$$a;\
		done &&\
		ar csr $@ $$(find . -type f -name '*.o')&&\
		cp $@ ..

.PHONY: $(PUREC_LIB)

clean:
	@rm -f $(PUREC_LIB)
	@rm -f $(PUREC_INTERMEDIATE_LIB)
	@rm -rf $(PUREC_WORKDIR)
	@rm -f $(RUNTIME_OBJECTS)
	@rm -f $$(find . -type f -name '*.out')
	@rm -f $$(find . -maxdepth 1 -type f -name '*.a')
.PHONY: clean

%.o: %.c
	@echo "Compile" $^
	@$(CLANG) $^ -c -o $@ \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-I runtime \
		-I . \
		$(CFLAGS)

#-------------------------------------------------------------------------------
# Garbage collector
#-------------------------------------------------------------------------------

deps/bwdgc:
	@if [ ! -d deps/bwdgc ]; then \
		if [ ! -f gc.tar.gz ]; then \
			echo "Downloading bwdgc $(BWDGC_V) tarball...";\
			curl -sfLo gc.tar.gz \
				'https://api.github.com/repos/ivmai/bdwgc/tarball/$(BWDGC_V)'; \
		fi && \
		mkdir -p deps/bwdgc && \
		tar -C deps/bwdgc -xzf gc.tar.gz --strip-components 1; \
	fi
.PHONY: deps/bwdgc

#-------------------------------------------------------------------------------
# Tests
#-------------------------------------------------------------------------------

test/c:
	@$(MAKE) -s clean > /dev/null
	@UNIT_TESTING=1 $(MAKE) -s test/c.0
PHONY: test/c

test/c.0:
	@UNIT_TESTING=1 make -s $(PUREC_LIB)
	@$(CLANG) \
		-g \
		-I. \
		-Iruntime \
		-L. \
		ctests/main.c \
		-lpurec \
		-lcmocka \
		-o ctests/a.out > /dev/null
	@./ctests/a.out
.PHONY: test/c.0

test/tests.0: | $(foreach t,$(TESTS),test/tests/$(t))
.PHONY: test/tests.0

test/tests:
	@$(MAKE) -s clean
	@echo "Running tests w/o tracing GC"
	@UNIT_TESTING=1 $(MAKE) -s test/tests.0
PHONY: test/tests

# compile each project under 'tests', run it, and check for leaks using valgrind
define mk_test_case

test/tests/$(1):
	@$(MAKE) -s clean
	UNIT_TESTING=0 $(MAKE) -s test/tests/$(1).0

test/tests/$(1).0: $(BWDGC_LIB)
	@echo "tests/$(1): clean"
	@$(MAKE) -C "tests/$(1)" clean > /dev/null
	@echo "tests/$(1): compile C"
	@\
		CFLAGS=-g \
		LDFLAGS=-lcmocka \
		$(MAKE) -C "tests/$(1)" main
	@echo "tests/$(1): run ouput"
	$(MAKE) -s -C "tests/$(1)" main_leakcheck
.PHONY: test/tests/$(1)

endef

# generate test targets
$(foreach t,$(TESTS),$(eval $(call mk_test_case,$t)))

test/tests/main:
	@$(MAKE) -s clean
	@$(MAKE) -s test/tests/main.0
PHONY: test/tests/main

# compile and execute each project under 'tests/'
test/tests/main.0:
	@set -e; for t in $(TESTS); do\
		echo "tests/main: $$t: clean" &&\
		$(MAKE) > /dev/null -s -C "tests/$$t" clean &&\
		echo "tests/main: $$t: compile" &&\
		$(MAKE) > /dev/null -s -C "tests/$$t" &&\
		echo "tests/main: $$t: run" &&\
		( cd "tests/$$t" && ./main.out; )
	done
.PHONY: test/tests/main.0

test/upstream:
	$(MAKE) clean
	$(SPAGO) test
.PHONY: test/upstream

test:
	@echo '=== test: c-tests ==================================================='
	@$(MAKE) -s test/c
	@echo '=== test: tests ====================================================='
	@$(MAKE) -s test/tests
	@echo '=== test: upstream =================================================='
	@#$(MAKE) -s test/upstream
	@#echo 'success!'
.PHONY: test
