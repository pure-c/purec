.PHONY: clean deps deps/npm deps/bwdgc deps/blocksruntime purec test test/build

SHELL := /bin/bash
SHELLFLAGS := -eo pipefail

PURS := PATH=$$PATH:node_modules/.bin purs
PULP := PATH=$$PATH:node_modules/.bin pulp
PUREC_JS := purec.js
PUREC := node $(PUREC_JS)
PUREC_WORKDIR := .purec-work

BWDGC_V := v8.0.0
BLOCKSRUNTIME_REV := master

BWDGC_LIB := \
	deps/bwdgc/.libs/libgc.a

BLOCKSRUNTIME_LIB := \
	deps/blocksruntime-$(BLOCKSRUNTIME_REV)/libBlocksRuntime.a

RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = \
	$(patsubst %.c,%.o,$(RUNTIME_SOURCES))

CFLAGS := \
	-fblocks \
	-D 'uthash_malloc=GC_malloc' \
	-D 'uthash_free(ptr, sz)=NULL' \
	-D 'vec_realloc=GC_realloc' \
	-D 'vec_free(x)=NULL' \
	-D 'vec_malloc=GC_malloc'

$(BWDGC_LIB):
	@$(MAKE) -s deps/bwdgc
	@cd deps/bwdgc && \
	    ./autogen.sh && \
	    ./configure --enable-static && \
	    $(MAKE)

$(BLOCKSRUNTIME_LIB):
	@$(MAKE) -s deps/blocksruntime
	@cd 'deps/blocksruntime-$(BLOCKSRUNTIME_REV)' && ./buildlib

libpurec.base.a: $(RUNTIME_OBJECTS)
	@ar cr $@ $^

libpurec.a: libpurec.base.a $(BWDGC_LIB) $(BLOCKSRUNTIME_LIB)
	{\
		echo 'CREATE $@';\
		echo 'ADDLIB libpurec.base.a';\
		echo 'ADDLIB $(BLOCKSRUNTIME_LIB)';\
		echo 'ADDLIB $(BWDGC_LIB)';\
		echo 'SAVE';\
		echo 'END';\
	} | ar -M
.PHONY: libpurec.a

purec:
	@npm run build

clean:
	@rm -rf $(PUREC_WORKDIR)
	@rm -f $(RUNTIME_OBJECTS)
	@rm -f $$(find . -name '*.out')
	@rm -rf $$(find examples -type d -name $(PUREC_WORKDIR))

%.o: %.c | $(BWDGC_LIB) $(BLOCKSRUNTIME_LIB)
	@echo "Compile" $^
	@clang $^ -c -o $@ \
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
	deps/bwdgc\
	deps/blocksruntime

deps/npm:
	@npm install
	@node_modules/.bin/bower install

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

deps/blocksruntime:
	@if [ ! -d 'deps/blocksruntime-$(BLOCKSRUNTIME_REV)' ]; then\
		if [ ! -f 'blocksruntime-$(BLOCKSRUNTIME_REV).zip' ]; then\
			echo "downloading blocksruntime zip...";\
			curl -sfLo 'blocksruntime-$(BLOCKSRUNTIME_REV).zip'\
				'https://github.com/pure-c/blocksruntime/archive/$(BLOCKSRUNTIME_REV).zip';\
		fi &&\
		mkdir -p deps &&\
		>/dev/null unzip -d deps 'blocksruntime-$(BLOCKSRUNTIME_REV).zip' &&\
		ln -s "$$PWD/deps/blocksruntime-$(BLOCKSRUNTIME_REV)" "$$PWD/deps/blocksruntime";\
	fi

#-------------------------------------------------------------------------------
# Tests
#-------------------------------------------------------------------------------

# note: this is temporary while building up the project
test: examples/bower_components

test/examples:
	@$(MAKE) -s examples
	@./examples/example1/main.out <<< "john"
	@./examples/example2/main.out
	@./examples/effect/main.out
.PHONY: test/examples

test/pulp:
	$(PULP) test
.PHONY: test/pulp

test: test/examples test/pulp

#-------------------------------------------------------------------------------
# Examples
#-------------------------------------------------------------------------------

examples: purec examples/bower_components
	@$(MAKE) -s -C examples/example1
	@$(MAKE) -s -C examples/example2
	@$(MAKE) -s -C examples/effect
.PHONY: examples

examples/bower_components:
	@cd examples && ../node_modules/.bin/bower install
