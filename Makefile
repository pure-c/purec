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
		-I . \
		$(CLANG_FLAGS) \
		$^

example1_srcs = \
	$(shell \
		find examples \
			\( ! -regex '.*/\..*' \) \
			-type f \
			-name '*.purs')

example1_deps = \
	$(shell \
		find bower_components/purescript-{effect,console,prelude}/src \
			\( ! -regex '.*/\..*' \) \
			-type f \
			-name '*.purs')

example1/corefns: $(example1_srcs) $(example1_deps)
	@purs compile \
		-g corefn \
		-o $(patsubst %/corefns,$(PUREC_WORKDIR)/%,$@) \
		$^

example1/genc: example1/corefns
example1/genc:
	@node ./purec -m Example1 \
		$(shell find "$(patsubst %/genc,$(PUREC_WORKDIR)/%,$@)" -type f -name corefn.json)

example1/build: \
	$(RUNTIME_OBJECTS) \
	$(patsubst %.c,%.o,$(wildcard $(PUREC_WORKDIR)/example1/*.c))
	@clang $^ \
		$(LDFLAGS) \
		-ffunction-sections \
		-Wl,-gc-sections \
		-O3 \
		-luv \
		-o $(patsubst %/build,%,$@)

example1: example1/genc
	$(MAKE) example1/build
