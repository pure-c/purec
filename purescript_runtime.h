#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

// -----------------------------------------------------------------------------
// managed blocks: automatically finalized Blocks
// -----------------------------------------------------------------------------

typedef struct managed_block managed_block_t;
struct managed_block {
	void * block;
};

managed_block_t * managed_block_new (void * block);

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

typedef struct purs_any purs_any_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef enum purs_any_tag purs_any_tag_t;
typedef void* (^abs_block_t)(void*);
typedef purs_any_t * (*abs_t) (purs_any_t*);

struct purs_cons {
	int tag;
	purs_any_t * value;
};

purs_cons_t * purs_cons_set(
	purs_cons_t * cons,
	int tag,
	purs_any_t * value);

enum purs_any_tag {
	INT = 0,       // integer
	FLOAT = 1,     // float
	ABS = 2,       // abstraction
	ABS_BLOCK = 3, // lambda abstraction
	CONS = 4,      // data constructor
	C_STRING = 5,  // NUL terminated C-String
};

union purs_any_value {
	int num_int;
	float num_float;
	abs_t * fn;
	char* c_string;
	managed_block_t * block;
	purs_cons_t * cons;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

void *            purs_any_get (purs_any_tag_t, purs_any_t *);
abs_t             purs_any_get_abs       (purs_any_t *);
int               purs_any_get_int       (purs_any_t *);
managed_block_t * purs_any_get_abs_block (purs_any_t *);
purs_cons_t *     purs_any_get_cons      (purs_any_t *);

purs_any_t * purs_any_set_abs       (purs_any_t *, abs_t *);
purs_any_t * purs_any_set_abs_block (purs_any_t *, managed_block_t *);
purs_any_t * purs_any_set_float     (purs_any_t *, float);
purs_any_t * purs_any_set_int       (purs_any_t *, int);
purs_any_t * purs_any_set_cons      (purs_any_t *, purs_cons_t *);
purs_any_t * purs_any_set_c_string  (purs_any_t *, char *);

#endif // PURESCRIPT_RUNTIME_H
