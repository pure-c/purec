.PHONY: clean

SHELL := /bin/bash

PUREC_WORKDIR := .purec-work

RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = $(patsubst %.c,%.o,$(RUNTIME_SOURCES))

LDFLAGS = -l:libBlocksRuntime.a -l:libgc.a -lm -lpthread

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

example1_srcs = \
	$(patsubst %.c,%.purs,$(patsubst %.h,%.purs,$(shell \
		find examples \
			-type f \
			-name '*.purs' \
			-o -name '*.c' \
			-o -name '*.h')))

example1_deps = \
	$(patsubst %.c,%.purs,$(patsubst %.h,%.purs,$(shell \
		find bower_components/purescript-{effect,console,prelude,control}/src \
			-type f \
			-name '*.purs' \
			-o -name '*.c' \
			-o -name '*.h')))

%/corefn.json.1: %/corefn.json
	rsync $< $@

$(PUREC_WORKDIR)/example1/.corefns: $(example1_srcs) $(example1_deps)
	@mkdir -p $(@D)
	@purs compile -g corefn -o $(@D) $^
	@touch $@

$(PUREC_WORKDIR)/example1/.genc: $(PUREC_WORKDIR)/example1/.corefns
$(PUREC_WORKDIR)/example1/.genc:
	@mkdir -p $(@D)
	@$(MAKE) $@.1
	@touch $@

$(PUREC_WORKDIR)/example1/.genc.1: $(patsubst %,%.1,$(shell find "$(PUREC_WORKDIR)/example1" -type f -name corefn.json))
	@node ./purec -m Example1 $?
	@touch $@

$(PUREC_WORKDIR)/example1/.build: \
	$(RUNTIME_OBJECTS) \
	$(patsubst %.c,%.o,$(wildcard $(PUREC_WORKDIR)/example1/*.c))
	@clang $^ \
		$(LDFLAGS) \
		-ffunction-sections \
		-Wl,-gc-sections \
		-o example1
	@touch $@

example1: $(PUREC_WORKDIR)/example1/.genc
example1: ; $(MAKE) $(PUREC_WORKDIR)/example1/.build
