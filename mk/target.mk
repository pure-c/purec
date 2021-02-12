.PHONY: clean

CLANG ?= clang
CFLAGS ?=

PUREC_WORKDIR ?= .purec-work
PURS ?= purs

ifndef PUREC_DIR
$(error PUREC_DIR must be set)
endif

PUREC := node $(PUREC_DIR)/purec.js
PUREC_LIB = $(PUREC_DIR)/libpurec.a
PUREC_LIB_DIR = $(dir $(PUREC_LIB))
PUREC_LIB_NAME = $(notdir %/%,%,$(PUREC_LIB))

BWDGC_LIB_DIR := $(PUREC_DIR)/deps/bwdgc/.libs
BWDGC_INCLUDE_DIR := $(PUREC_DIR)/deps/bwdgc/include

DEPS_DIR = .spago
SPAGO ?= spago
PACKAGE_SOURCES = $(shell [ -d .spago ] && $(SPAGO) sources)

OS := $(shell uname)
ifeq ($(OS),Darwin)
LD_LINKER_FLAGS += -dead_strip
else
LD_LINKER_FLAGS += -gc-sections
endif

ifeq ($(USE_GC),1)
CFLAGS+=-DUSE_GC -I$(BWDGC_INCLUDE_DIR)
LDFLAGS+=-L $(BWDGC_LIB_DIR) -l gc
CFLAGS+=\
	-D 'uthash_malloc=GC_malloc' \
	-D 'uthash_free(ptr, sz)=NULL' \
	-D 'vec_realloc=GC_realloc' \
	-D 'vec_free(x)=NULL' \
	-D 'vec_malloc=GC_malloc'
endif

.spago:
	$(SPAGO) install -c skip
.PHONY: .spago

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

$(PUREC_LIB):
	USE_GC=$(USE_GC) $(MAKE) -C $(PUREC_DIR) libpurec.a
.PHONY: $(PUREC_LIB)

%/corefn.json.1: %/corefn.json
	@rsync $< $@

clean:
	@echo 2>&1 'clean: removing libpurec.a'
	@rm -f "$(PUREC_LIB)"
	@rm -f $$(find "$(PUREC_DIR)" -type f -name '*.o')
	@echo 2>&1 'clean: removing *.o'
	@rm -f $$(find . -type f -name '*.o')
	@echo 2>&1 'clean: removing *.out'
	@rm -f $$(find . -type f -name '*.out')
	@echo 2>&1 'clean: removing dir $(PUREC_WORKDIR)'
	@rm -rf "$(PUREC_WORKDIR)"

%.o: %.c
	@echo "Compile" $^
	$(CLANG) $^ -c -o $@ \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value \
		-I $(PUREC_LIB_DIR)/runtime \
		-I $(PUREC_LIB_DIR) \
		$(CFLAGS)

# Macro to derive a target
# This allows to define multiple project-level targets using a simple macro
# evaluation.
#
# synopsis:
#   purs_mk_target([<name>.out][,<main-module>][,<src-dirs>])
#
#   where:
#   <name>
#     is the name of the target and produced binary, excluding the mandatory .out
#     extension.
#     [default: main]
#   <main-module>
#     is the name of the PureScript module to use as the main module.
#     [default: Main]
#   <src-dirs>
#     are the names of the directory containing sources.
#     [default: src]
#
# usage example:
#    $(eval $(call purs_mk_target,main,Test.Main,src test,))
define purs_mk_target

$(1)_CFLAGS ?= $(CFLAGS)
$(1)_LDFLAGS ?= $(LDFLAGS)

$(1)_main_module := $(2)

ifeq (,$(3))
    $(1)_src_dirs := src
else
    $(1)_src_dirs := $(3)
endif

$(1)_local :=\
	$$(shell find $$($(1)_src_dirs) -type f -name '*.purs')

$(1)_deps :=\
	$$(foreach pkgdir,\
		$(PACKAGE_SOURCES),\
		$$(call rwildcard,$$(firstword $$(subst *, ,$$(pkgdir))),*.purs *.c))

$(1)_srcs := $$($(1)_local) $$($(1)_deps)

$$(PUREC_WORKDIR)/$(1)/.corefns: \
	$$(patsubst %.h,%.purs,$$($(1)_srcs))
	mkdir -p $$(@D)
	$$(PURS) compile -g corefn -o $$(@D) $$(filter %.purs,$$^)
	touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc: $$(PUREC_WORKDIR)/$(1)/.corefns
	mkdir -p $$(@D)
	$$(MAKE) $$@.1
	touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc.1: $$(patsubst %,%.1,$$(call rwildcard,$$(PUREC_WORKDIR)/$(1),corefn.json))
	@echo Compiling from Corefn to C
	$$(PUREC) -m "$$($(1)_main_module)" $$?
	touch $$@

$$(PUREC_WORKDIR)/$(1)/.build: \
	$(PUREC_LIB) \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$(1)/*.c))
	$(CLANG) $$^ \
		-L $(PUREC_LIB_DIR) \
		-L $(PUREC_LIB_DIR)/runtime \
		$(CFLAGS) \
		$($(1)_CFLAGS) \
		-lpurec \
		-lm \
		-ffunction-sections \
		$(LDFLAGS) \
		$($(1)_LDFLAGS) \
		-Wl,$(LD_LINKER_FLAGS) \
		-o "$(1).out"
	touch $$@
	@echo Purec build succeeded!

_$(1): $$(PUREC_WORKDIR)/$(1)/.genc
	@$$(MAKE) $$(PUREC_WORKDIR)/$(1)/.build

$(1): $(DEPS_DIR)
	@$$(MAKE) _$(1)
.PHONY: $(1)

$(1)_leakcheck: $(1)
	valgrind -q \
		"--suppressions=$(PUREC_LIB_DIR)/purec.suppr" \
		--error-exitcode=1 \
		--num-callers=64 \
		--leak-check=full \
		"./$(1).out"
.PHONY: $(1)_leakcheck

$(1)/c: $(DEPS_DIR)
	$$(MAKE) $$(PUREC_WORKDIR)/$(1)/.genc
.PHONY: $(1)/c
endef
