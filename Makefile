.PHONY: reference example1

VENDOR_C_FILES = $(shell find vendor -type f -name '*.c')
CCAN_C_FILES = $(shell find ccan -type f -name '*.c')
REFERENCE_OUTPUT_C_FILES = $(shell find reference/output -type f -name '*.c')
EXAMPLE1_OUTPUT_C_FILES = $(shell find .tmp/sources/Example1 -type f -name '*.c')

reference:
	clang \
		-o a.out \
		-g \
		-fblocks \
		-lBlocksRuntime \
		-lgc \
		-lm \
		-D 'uthash_malloc=GC_MALLOC'\
		-D 'uthash_free(ptr, sz)=NULL'\
		-D 'vec_realloc=GC_realloc'\
		-D 'vec_free(x)=NULL'\
		-D 'vec_malloc=GC_MALLOC'\
		$(VENDOR_C_FILES) \
		$(CCAN_C_FILES) \
		-I . \
		runtime/purescript.c \
		\
		-I reference/output \
		$(REFERENCE_OUTPUT_C_FILES) \
		reference/main.c \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value

example1:
	clang \
		-o a.out \
		-g \
		-fblocks \
		-lBlocksRuntime \
		-lgc \
		-lm \
		-D 'uthash_malloc=GC_MALLOC'\
		-D 'uthash_free(ptr, sz)=NULL'\
		-D 'vec_realloc=GC_realloc'\
		-D 'vec_free(x)=NULL'\
		-D 'vec_malloc=GC_MALLOC'\
		$(VENDOR_C_FILES) \
		$(CCAN_C_FILES) \
		-I . \
		runtime/purescript.c \
		\
		-I .tmp/sources/Example1 \
		$(EXAMPLE1_OUTPUT_C_FILES) \
		-Wall \
		-Wno-unused-variable \
		-Wno-unused-value
