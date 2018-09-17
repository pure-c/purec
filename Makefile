.PHONY: clean

SHELL := /bin/bash

MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(notdir $(patsubst %/,%,$(dir $(MKFILE_PATH))))

PUREC_WORKDIR := .purec-work

PURS := PATH=$$PATH:$(CURRENT_DIR)/node_modules/.bin purs
PUREC := node ./purec.js

RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = \
	$(patsubst %.c,%.o,$(RUNTIME_SOURCES))

LDFLAGS = \
	-l:libBlocksRuntime.a \
	-l:libgc.a \
	-lm \
	-lpthread

clean:
	@rm -rf $(PUREC_WORKDIR)

%.o: %.c
	@echo "Compile" $^
	@clang \
		-fblocks \
		-D 'uthash_malloc=GC_MALLOC'\
		-D 'uthash_free(ptr, sz)=NULL'\
		-D 'vec_realloc=GC_realloc'\
		-D 'vec_free(x)=NULL'\
		-D 'vec_malloc=GC_MALLOC'\
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-c \
		-o $@ \
		-I runtime \
		-I . \
		$(CLANG_FLAGS) \
		$^

%/corefn.json.1: %/corefn.json
	@rsync $< $@

define mk_target
$(1)_srcs = \
	$$(patsubst %.c,%.purs,$$(patsubst %.h,%.purs,$$(shell \
		find "examples/$(1)" \
			-type f \
			-name '*.purs' \
			-o -name '*.c' \
			-o -name '*.h')))

$(1)_deps = \
	$$(patsubst %.c,%.purs,$$(patsubst %.h,%.purs,$$(shell \
		find bower_components/purescript-{effect,console,prelude,control}/src \
			-type f \
			-name '*.purs' \
			-o -name '*.c' \
			-o -name '*.h')))

$$(PUREC_WORKDIR)/$(1)/.corefns: $$($(1)_srcs) $$($(1)_deps)
	@mkdir -p $$(@D)
	@$$(PURS) compile -g corefn -o $$(@D) $$^
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc: $$(PUREC_WORKDIR)/$(1)/.corefns
$$(PUREC_WORKDIR)/$(1)/.genc:
	@mkdir -p $$(@D)
	@$$(MAKE) $$@.1
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc.1: $$(patsubst %,%.1,$$(shell find 2>/dev/null "$$(PUREC_WORKDIR)/$(1)" -type f -name corefn.json))
	$$(PUREC) -m $(2) $$?
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.build: \
	$$(RUNTIME_OBJECTS) \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$(1)/*.c))
	@clang $$^ \
		$$(LDFLAGS) \
		-ffunction-sections \
		-Wl,-gc-sections \
		-o "$(1).out"
	@touch $$@

examples/$(1): $$(PUREC_WORKDIR)/$(1)/.genc
examples/$(1):
	$$(MAKE) $$(PUREC_WORKDIR)/$(1)/.build
endef

$(eval $(call mk_target,example1,Example1))
$(eval $(call mk_target,example2,Example1))
