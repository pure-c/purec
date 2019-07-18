CLANG ?= clang
CFLAGS ?=
WITH_GC ?=

SHELL := /bin/bash
SHELLFLAGS := -eo pipefail

PURS := PATH=$$PATH:node_modules/.bin purs
PULP := PATH=$$PATH:node_modules/.bin pulp

PUREC_JS := purec.js
PUREC := node $(PUREC_JS)
PUREC_WORKDIR := .purec-work
PUREC_LIB := libpurec.a
PUREC_INTERMEDIATE_LIB := libpurec.intermediate.a

BWDGC_V := v8.0.0
BWDGC_LIB := deps/bwdgc/.libs/libgc.a

RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = \
	$(patsubst %.c,%.o,$(RUNTIME_SOURCES))

TESTS = $(shell cd tests && find . -maxdepth 1 ! -path . -type d -exec basename {} \;)

ifdef WITH_GC
CFLAGS += \
	-D 'uthash_malloc=GC_malloc' \
	-D 'uthash_free(ptr, sz)=NULL' \
	-D 'vec_realloc=GC_realloc' \
	-D 'vec_free(x)=NULL' \
	-D 'vec_malloc=GC_malloc'
endif

ifdef UNIT_TESTING
CFLAGS += \
	-g \
	-D UNIT_TESTING
endif

$(BWDGC_LIB):
	@$(MAKE) -s deps/bwdgc
	@cd deps/bwdgc && \
	    ./autogen.sh && \
	    ./configure --enable-static && \
	    $(MAKE)

$(PUREC_INTERMEDIATE_LIB): $(RUNTIME_OBJECTS)
	@ar csr $@ $^

ifdef WITH_GC
$(PUREC_LIB): $(PUREC_INTERMEDIATE_LIB) $(BWDGC_LIB)
else
$(PUREC_LIB): $(PUREC_INTERMEDIATE_LIB)
endif
	@rm -rf .build
	@mkdir -p .build
	@cd .build &&\
		for a in $^; do\
			ar x ../$$a;\
		done &&\
		ar csr $@ $$(find . -type f -name '*.o')&&\
		cp $@ ..

.PHONY: $(PUREC_LIB)

$(PUREC_JS):
	@npm run build
.PHONY: $(PUREC_JS)

# deprecated
purec: $(PUREC_JS)
.PHONY: purec

clean:
	@rm -rf $(PUREC_WORKDIR)
	@rm -f $(RUNTIME_OBJECTS)
	@rm -f $$(find . -type f -name '*.out')
	@rm -f $$(find . -maxdepth 1 -type f -name '*.a')
.PHONY: clean

%.o: %.c | $(BWDGC_LIB)
	@echo "Compile" $^
	@$(CLANG) $^ -c -o $@ \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-I runtime \
		-I . \
		$(CFLAGS)

#-------------------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------------------

deps:\
	deps/npm\
	deps/bwdgc
.PHONY: deps

deps/npm:
	@npm install
	@node_modules/.bin/bower install
.PHONY: deps/npm

deps/bwdgc:
	@if [ ! -d deps/bwdgc ]; then \
		if [ ! -f gc.tar.gz ]; then \
			echo "downloading bwdgc tarball...";\
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
	@$(MAKE) -s clean
	@UNIT_TESTING=1 $(MAKE) -s test/c.0
PHONY: test/c

test/c.0:
	@make -s $(PUREC_LIB)
	@$(CLANG) \
		-g \
		-I. \
		-L. \
		ctests/main.c \
		-lpurec \
		-lcmocka \
		-o ctests/a.out
	@./ctests/a.out
.PHONY: test/c.0

test/tests.0: | $(foreach t,$(TESTS),test/tests/$(t))
.PHONY: test/tests.0

test/tests:
	@$(MAKE) -s clean
	@UNIT_TESTING=1 $(MAKE) -s test/tests.0
PHONY: test/tests

# compile each project under 'tests/' as a library, load and execute via
# cmocka.
# note: this necessitates *all* projects under test to:
#   + Have a 'lib' target without an entry point in a module called 'Main'
#   + Export a 'main' function from module 'Main'
define mk_test_case
test/tests/$(1):
	@$(MAKE) -s clean
	@UNIT_TESTING=1 $(MAKE) -s test/tests/$(1).0

test/tests/$(1).0:
	@make -s $(PUREC_LIB) &> /dev/null
	@$(MAKE) -s -C "tests/$(1)" clean
	@$(MAKE) -s -C "tests/$(1)" lib/c
	@cd "tests/$(1)" &&\
		$(CLANG) \
			-g \
			-I. \
			-I../.. \
			-L../.. \
			../main.stub.c \
			./.purec-work/lib/*.c \
			-lpurec \
			-lcmocka \
			-o a.out
	@./"tests/$(1)/a.out"
.PHONY: test/tests/$(1)
endef

# generate test targets
$(foreach t,$(TESTS),$(eval $(call mk_test_case,$(t))))

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

test/upstream: upstream/tests/support/bower_components
	@$(MAKE) -s clean
	@$(PULP) test > /dev/null
.PHONY: test/pulp

test:
	@echo '=== test: c-tests ==================================================='
	@$(MAKE) -s test/c
	@echo '=== test: tests ====================================================='
	@$(MAKE) -s test/tests
	@echo '=== test: upstream =================================================='
	@$(MAKE) -s test/upstream
	@echo 'success!'
.PHONY: test

#-------------------------------------------------------------------------------
# utilities
#-------------------------------------------------------------------------------

%/bower_components:
	@ROOT=$(PWD) &&\
		cd "$(dir $@)" &&\
		"$$ROOT/node_modules/.bin/bower" install
