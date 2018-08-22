.PHONY: reference example1

CCAN_C_FILES = $(shell find ccan -type f -name '*.c')
REFERENCE_OUTPUT_C_FILES = $(shell find reference/output -type f -name '*.c')

reference:
	clang \
		-o a.out \
		-fblocks \
		-lBlocksRuntime \
		-lgc \
		$(CCAN_C_FILES) \
		-I . \
		runtime/purescript.c \
		\
		-I reference/output \
		$(REFERENCE_OUTPUT_C_FILES) \
		reference/main.c

EXAMPLE1_OUTPUT_C_FILES = $(shell find .output/Example1 -type f -name '*.c')

example1:
	clang \
		-o a.out \
		-g \
		-fblocks \
		-lBlocksRuntime \
		-lgc \
		$(CCAN_C_FILES) \
		-I . \
		runtime/purescript.c \
		\
		-I .output/Example1/Example1 \
		-Wall \
		$(EXAMPLE1_OUTPUT_C_FILES)
