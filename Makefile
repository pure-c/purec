RUNTIME_SOURCES = \
	runtime/purescript.c \
	$(shell find ccan -type f -name '*.c') \
	$(shell find vendor -type f -name '*.c')

RUNTIME_OBJECTS = $(patsubst %.c,%.o,$(RUNTIME_SOURCES))

LDFLAGS = -lBlocksRuntime -lgc -lm

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
		-O3 \
		-o $@ \
		-I . \
		$(CLANG_FLAGS) \
		$^

example1_srcs = $(shell find src -type f -name '*.purs')
example1_deps = $(shell find bower_components/purescript-*/src -type f -name '*.purs')
example1/corefns: $(example1_srcs) $(example1_deps)
	@purs compile \
		-g corefn \
		-o $(patsubst %/corefns,.purec-work/%,$@) \
		$^

example1/genc: example1/corefns
	@node ./purec \
		$(shell find "$(patsubst %/genc,.purec-work/%,$@)" -type f -name corefn.json)
