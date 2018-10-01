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

OS := $(shell uname)
ifeq ($(OS),Darwin)
LD_FLAGS += -dead_strip
else
LD_FLAGS += -gc-sections
endif

$(PUREC_LIB):
	$(MAKE) -s -C $(PUREC_DIR) libpurec.a
.PHONY: $(PUREC_LIB)

%/corefn.json.1: %/corefn.json
	@rsync $< $@

clean:
	@echo 'removing *.o'
	@rm -f $(find . -type f -name '*.o')
	@echo 'removing *.out'
	@rm -f $(find . -type f -name '*.out')
	@echo 'removing working directory $(PUREC_WORKDIR)'
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

define purs_mk_target

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
			-a -not -name '.\#*' \
			-a -not -path '*/$$(PUREC_WORKDIR)/*')))

$(1)_deps = \
	$$(patsubst %.c,%.purs,$$(patsubst %.h,%.purs,$$(shell \
		2>/dev/null find $$($(1)_deps_dir) \
			-type f \
			-a '(' \
				-name '*.purs' \
				-o -name '*.c' \
				-o -name '*.h' ')' \
			-a -not -name '.\#*' \
			-a -not -path '*/$$(PUREC_WORKDIR)/*')))

$$(PUREC_WORKDIR)/$(1)/.corefns: $$($(1)_srcs) $$($(1)_deps)
	@mkdir -p $$(@D)
	@$$(PURS) compile -g corefn -o $$(@D) $$^
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc: $$(PUREC_WORKDIR)/$(1)/.corefns
	@mkdir -p $$(@D)
	@$$(MAKE) -s $$@.1
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.genc.1: $$(patsubst %,%.1,$$(shell find 2>/dev/null "$$(PUREC_WORKDIR)/$(1)" -type f -name corefn.json))
	@$$(PUREC) -m $(2) $$?
	@touch $$@

$$(PUREC_WORKDIR)/$(1)/.build: \
	$(PUREC_LIB) \
	$$(patsubst %.c,%.o,$$(wildcard $$(PUREC_WORKDIR)/$(1)/*.c))
	@$(CLANG) $$^ \
		-L $(PUREC_LIB_DIR) \
		-lpurec \
		-lm \
		-lpthread \
		-ffunction-sections \
		-Wl,$(LD_FLAGS) \
		-o "$(1).out"
	@touch $$@

$(1): $$(PUREC_WORKDIR)/$(1)/.genc
	@$$(MAKE) -s $$(PUREC_WORKDIR)/$(1)/.build
endef
