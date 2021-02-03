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

DEPS_DIR = .spago
SPAGO ?= spago
PACKAGE_SOURCES = $(shell [ -d .spago ] && $(SPAGO) sources)

OS := $(shell uname)
ifeq ($(OS),Darwin)
LD_LINKER_FLAGS += -dead_strip
else
LD_LINKER_FLAGS += -gc-sections
endif

.spago.sources.sum:
	$(SPAGO) sources | md5sum > .spago.sources.sum
.PHONY: .spago.sources.sum

.spago: .spago.sources.sum
	$(SPAGO) install -c skip

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

$(PUREC_LIB):
	$(MAKE) -s -C $(PUREC_DIR) libpurec.a
.PHONY: $(PUREC_LIB)

%/corefn.json.1: %/corefn.json
	@rsync $< $@

clean:
	@echo 2>&1 'clean: removing *.o'
	@rm -f $$(find . -type f -name '*.o')
	@echo 2>&1 'clean: removing *.out'
	@rm -f $$(find . -type f -name '*.out')
	@echo 2>&1 'clean: removing dir $(PUREC_WORKDIR)'
	@rm -rf $(PUREC_WORKDIR)

%.o: %.c
	@echo "Compile" $^
	@$(CLANG) $^ -c -o $@ \
		-fblocks \
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
	@mkdir -p $$(@D)
	@$$(PURS) compile -g corefn -o $$(@D) $$(filter %.purs,$$^)
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc: $$(PUREC_WORKDIR)/$(1)/.corefns
	@mkdir -p $$(@D)
	@$$(MAKE) -s $$@.1
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc.1: $$(patsubst %,%.1,$$(call rwildcard,$$(PUREC_WORKDIR)/$(1),corefn.json))
	@$$(PUREC) -m "$$($(1)_main_module)" $$?
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.build: \
	$(PUREC_LIB) \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$(1)/*.c))
	$(CLANG) $$^ \
		-L $(PUREC_LIB_DIR) \
		-L $(PUREC_LIB_DIR)/runtime \
		$($(1)_CFLAGS) \
		-lpurec \
		-lm \
		-ffunction-sections \
		$(LD_FLAGS) \
		$($(1)_LD_FLAGS) \
		-Wl,$(LD_LINKER_FLAGS) \
		-o "$(1).out"
	@touch $$@
	@echo Purec build succeeded!

_$(1): $$(PUREC_WORKDIR)/$(1)/.genc
	@$$(MAKE) -s $$(PUREC_WORKDIR)/$(1)/.build

$(1): $(DEPS_DIR)
	@$$(MAKE) -s _$(1)
.PHONY: $(1)

$(1)/c: $(DEPS_DIR)
	@$$(MAKE) -s $$(PUREC_WORKDIR)/$(1)/.genc
.PHONY: $(1)/c
endef
