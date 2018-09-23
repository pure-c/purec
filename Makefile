.PHONY: clean deps deps/bwdgc deps/blocksruntime purec test test/build

SHELL := /bin/bash
SHELLFLAGS := -eo pipefail

PURS := PATH=$$PATH:node_modules/.bin purs
PUREC_JS := purec.js
PUREC := node $(PUREC_JS)
PUREC_WORKDIR := .purec-work

BWDGC_V := v8.0.0
BLOCKSRUNTIME_REV := master

BWDGC_LIB := \
	deps/bwdgc/.libs/libgc.a

BLOCKSRUNTIME_LIB := \
	deps/blocksruntime/libBlocksRuntime.a

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
	@$(MAKE) deps/bwdgc
	@cd deps/bwdgc && \
	    ./autogen.sh && \
	    ./configure --enable-static && \
	    $(MAKE)

$(BLOCKSRUNTIME_LIB):
	@$(MAKE) deps/blocksruntime
	@cd 'deps/blocksruntime-$(BLOCKSRUNTIME_REV)' && ./buildlib

libpurec.1.a: $(RUNTIME_OBJECTS)
	@ar cr $@ $^

libpurec.a: libpurec.1.a $(BWDGC_LIB) $(BLOCKSRUNTIME_LIB)
	{\
		echo 'CREATE $@';\
		$(foreach archive,$^,echo 'ADDLIB $(archive)';)\
		echo 'SAVE';\
		echo 'END';\
	} | ar -M

purec:
	@npm run build

clean:
	@rm -rf $(PUREC_WORKDIR)
	@rm -f $(RUNTIME_OBJECTS)
	@rm -f $(find . -name '*.out')

%.o: %.c | $(BWDGC_LIB) $(BLOCKSRUNTIME_LIB)
	@echo "Compile" $^
	@clang $^ -c -o $@ \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-I runtime \
		-I . \
		$(CFLAGS)

%/corefn.json.1: %/corefn.json
	@rsync $< $@

deps:
	npm install
	node_modules/.bin/bower install

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
		if [ ! -f blocksruntime.zip ]; then\
			echo "downloading blocksruntime zip...";\
			curl -sfLo blocksruntime.zip\
				'https://github.com/pure-c/blocksruntime/archive/$(BLOCKSRUNTIME_REV)';\
		fi &&\
		mkdir -p deps &&\
		unzip -d deps blocksruntime.zip &&\
		ln -s "$$PWD/deps/blocksruntime-$(BLOCKSRUNTIME_REV)" deps/blocksruntime;\
	fi

# note: this is temporary while building up the project
test: examples/bower_components
test:
	@$(MAKE) -s examples
	@./examples/example1.out <<< "john"
	@./examples/example2.out
	@./examples/effect.out

#-------------------------------------------------------------------------------

define mk_target_rule

ifeq (,$(3))
$(1)_src_dir := src
else
$(1)_src_dir := $(3)
endif

ifeq (,$(4))
$(1)_deps_dir := bower_components/purescript-*/src
else
$(1)_deps_dir := $(4)
endif

$(1)_srcs = \
	$$(patsubst %.c,%.purs,$$(patsubst %.h,%.purs,$$(shell \
		find $$($(1)_src_dir) \
			-type f \
			-a '(' \
				-name '*.purs' \
				-o -name '*.c' \
				-o -name '*.h' ')' \
			-a -not -name '.\#*')))

$(1)_deps = \
	$$(patsubst %.c,%.purs,$$(patsubst %.h,%.purs,$$(shell \
		2>/dev/null find $$($(1)_deps_dir) \
			-type f \
			-a '(' \
				-name '*.purs' \
				-o -name '*.c' \
				-o -name '*.h' ')' \
			-a -not -name '.\#*')))

$$(PUREC_WORKDIR)/$(1)/.corefns: $$($(1)_srcs) $$($(1)_deps)
	@mkdir -p $$(@D)
	@$$(PURS) compile -g corefn -o $$(@D) $$^
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc: $$(PUREC_WORKDIR)/$(1)/.corefns
$$(PUREC_WORKDIR)/$(1)/.genc:
	@mkdir -p $$(@D)
	@$$(MAKE) -s $$@.1
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc.1: $$(patsubst %,%.1,$$(shell find 2>/dev/null "$$(PUREC_WORKDIR)/$(1)" -type f -name corefn.json))
	@$$(PUREC) -m $(2) $$?
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.build: \
	libpurec.a \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$(1)/*.c))
	@clang $$^ \
		-lm \
		-L . \
		-l:libpurec.a \
		-lpthread \
		-ffunction-sections \
		-Wl,-gc-sections \
		-o "$(1).out"
	@touch $$@

$(1): $$(PUREC_WORKDIR)/$(1)/.genc
$(1): ; @$$(MAKE) -s $$(PUREC_WORKDIR)/$(1)/.build
endef

example_deps = \
    examples/bower_components/purescript-{control,effect,prelude,console,assert,maybe,invariant,newtype}/src/*
define mk_example_rule
	$(call mk_target_rule,examples/$(1),$(2),examples/$(1),$(example_deps))
endef

$(eval $(call mk_example_rule,example1,Example1))
$(eval $(call mk_example_rule,example2,Example2))
$(eval $(call mk_example_rule,effect,Main))

examples: \
	examples/example1 \
	examples/example2 \
	examples/effect

examples/bower_components:
	@cd examples && ../node_modules/.bin/bower install
