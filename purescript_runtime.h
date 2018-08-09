#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

// -----------------------------------------------------------------------------
// managed blocks: automatically finalized Blocks
// -----------------------------------------------------------------------------

struct managed_block_t {
	void * block;
};

struct managed_block_t * managed_block_new (void * block);

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

enum purs_any_tag_t {
	INT,
	FLOAT,
	BLOCK,
};

union purs_any_value_t {
	int int_value;
	float float_value;
	struct managed_block_t * managed_block;
};

struct purs_any_t {
	enum purs_any_tag_t tag;
	union purs_any_value_t value;
};

struct managed_block_t * purs_any_get_managed_block (struct purs_any_t * x);
struct purs_any_t * purs_any_from_managed_block (struct managed_block_t * managed_block);

#endif // PURESCRIPT_RUNTIME_H
