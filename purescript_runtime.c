#include <gc/gc.h>
#include "Block.h"
#include "purescript_runtime.h"

// -----------------------------------------------------------------------------
// managed blocks: automatically finalized Blocks
// -----------------------------------------------------------------------------

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
// any: dynamically typed values
// -----------------------------------------------------------------------------

struct managed_block_t * any_get_managed_block (struct purs_any_t * x) {
	if (x->tag == BLOCK) {
		return x->value.managed_block;
	} else {
		return 0;
	}
}

struct purs_any_t * any_from_managed_block (struct managed_block_t * managed_block) {
	struct purs_any_t * x = GC_NEW(struct purs_any_t);
	x->tag = BLOCK;
	x->value.managed_block = managed_block;
	return x;
}
