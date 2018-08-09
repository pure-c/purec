#include <gc/gc.h>
#include <unistd.h>
#include <stdio.h>
#include "Block.h"

typedef void* (^abs)(void*);

// -----------------------------------------------------------------------------
// Any
// -----------------------------------------------------------------------------

#define ANY(v) (void *)(v)

enum tag_t {
	INT,
	FLOAT,
	BLOCK,
};

union value_t {
	int int_value;
	float float_value;
	struct managed_block_t * managed_block;
};

struct any_t {
	enum tag_t tag;
	union value_t value;
};

struct managed_block_t {
	void * block;
};

struct managed_block_t * any_get_block (struct any_t * x) {
	if (x->tag == BLOCK) {
		return x->value.managed_block;
	} else {
		return 0;
	}
}

struct any_t * any_from_managed_block (struct managed_block_t * managed_block) {
	struct any_t * x = GC_NEW(struct any_t);
	x->tag = BLOCK;
	x->value.managed_block = managed_block;
	return x;
}

#define MANAGED_BLOCK_CALL(x, arg) \
	(struct any_t *)(((abs) x->block)(arg));

void managed_block_release (struct managed_block_t * block) {
	Block_release(block->block);
}

struct managed_block_t * managed_block_new (void * block) {
	struct managed_block_t * managed_block = GC_NEW(struct managed_block_t);
	managed_block->block = block;
	GC_register_finalizer(
		managed_block,
		(GC_finalization_proc) managed_block_release,
		0, 0, 0);
	return managed_block;
}

// -----------------------------------------------------------------------------

struct any_t * go (char* x) {
	return any_from_managed_block(managed_block_new(
		Block_copy(^ () {
			return any_from_managed_block(managed_block_new(
				Block_copy(^ (int y) {
					printf ("%s: %d\n", x, y);
					return 0;
				})
			));
		})
	));
}

// -----------------------------------------------------------------------------

void main_1 () {
	struct any_t * x0 = go ("foobar");
	struct any_t * x1 = MANAGED_BLOCK_CALL(any_get_block(x0), 0);
	struct any_t * x2 = MANAGED_BLOCK_CALL(any_get_block(x1), 100);
}

int main () {
	GC_INIT();
	while (1) {
		main_1();
		usleep(100);
	}
	return 0;
}
