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

SPAGO ?= spago
PACKAGE_SOURCES = $(shell [ -d .spago ] && $(SPAGO) sources)

OS := $(shell uname)
ifeq ($(OS),Darwin)
LD_LINKER_FLAGS += -dead_strip
else
LD_LINKER_FLAGS += -gc-sections
endif

## Not all environments support globstar (** dir pattern)
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

$(PUREC_LIB):
	$(MAKE) -s -C $(PUREC_DIR) libpurec.a
.PHONY: $(PUREC_LIB)

%/corefn.json.1: %/corefn.json
	@rsync $< $@

clean:
	@echo 'removing *.o'
	@rm -f $$(find . -type f -name '*.o')
	@echo 'removing *.out'
	@rm -f $$(find . -type f -name '*.out')
	@echo 'removing dir $(PUREC_WORKDIR)'
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

ifeq (,$(1))
    target := main
else
    target := $(1)
endif

ifeq (,$(2))
    $$(target)_main_module := Main
else
    $$(target)_main_module := $(2)
endif

ifeq (,$(3))
    $$(target)_src_dirs := src
else
    $$(target)_src_dirs := $(3)
endif

$$(target)_local :=\
	$$(shell find $$($$(target)_src_dirs) -type f -name '*.purs')

$$(target)_deps :=\
	$$(foreach pkgdir,\
		$(PACKAGE_SOURCES),\
		$$(call rwildcard,$$(firstword $$(subst *, ,$$(pkgdir))),*.purs *.c))

$$(target)_srcs := $$($$(target)_local) $$($$(target)_deps)

$$(PUREC_WORKDIR)/$$(target)/.corefns: \
	$$(patsubst %.h,%.purs,$$($$(target)_srcs))
	@mkdir -p $$(@D)
	@$$(PURS) compile -g corefn -o $$(@D) $$(filter %.purs,$$^)
	@touch $$@

$$(PUREC_WORKDIR)/$$(target)/.genc: $$(PUREC_WORKDIR)/$$(target)/.corefns
	@mkdir -p $$(@D)
	@$$(MAKE) -s $$@.1
	@touch $$@

$$(PUREC_WORKDIR)/$$(target)/.genc.1: $$(patsubst %,%.1,$$(call rwildcard,$$(PUREC_WORKDIR)/$$(target),corefn.json))
	@$$(PUREC) -m "$$($$(target)_main_module)" $$?
	@touch $$@

$$(PUREC_WORKDIR)/$$(target)/.build: \
	$(PUREC_LIB) \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$$(target)/*.c))
	@$(CLANG) $$^ \
		-L $(PUREC_LIB_DIR) \
		-lpurec \
		-lm \
		-lpthread \
		-ffunction-sections \
		$(LD_FLAGS) \
		-Wl,$(LD_LINKER_FLAGS) \
		-o "$$(target).out"
	@touch $$@

_$$(target): $$(PUREC_WORKDIR)/$$(target)/.genc
	@$$(MAKE) -s $$(PUREC_WORKDIR)/$$(target)/.build

$$(target):
	@$$(MAKE) -s _$$(target)
endef
