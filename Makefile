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
		-o $@ \
		-I . \
		$(CLANG_FLAGS) \
		$^

example1_sources = $(shell find .tmp/sources/Example1 -type f -name '*.c')
example1_objects = $(patsubst %.c,%.o,$(example1_sources))
example1: CLANG_FLAGS = -I .tmp/sources/Example1
example1: $(RUNTIME_OBJECTS) $(example1_objects)
	@clang \
		$^ \
		$(LDFLAGS) \
		-o example1
