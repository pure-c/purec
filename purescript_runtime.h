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

struct purs_any_t;

struct purs_cons_t {
	int tag;
	struct purs_any_t * value;
};

struct purs_cons_t * purs_cons_set(
	struct purs_cons_t * cons,
	int tag,
	struct purs_any_t * value);

// EXAMPLE: Maybe a
// data Maybe a b = Just a | Nothing
// TODO: remove
#define _DATA_MAYBE__MAYBE__JUST 1
#define _DATA_MAYBE__MAYBE__NOTHING 2

enum purs_any_tag_t {
	INT = 0,       // integer
	FLOAT = 1,     // float
	ABS = 2,       // abstraction
	ABS_BLOCK = 3, // lambda abstraction
	CONS = 4,      // data constructor
	C_STRING = 5,  // NUL terminated C-String
};

typedef void* (^abs_block_t)(void*);
typedef struct purs_any_t * (*abs_t) (struct purs_any_t*);

union purs_any_value_t {
	int num_int;
	float num_float;
	abs_t * fn;
	char* c_string;
	struct managed_block_t * block;
	struct purs_cons_t * cons;
};

struct purs_any_t {
	enum purs_any_tag_t tag;
	union purs_any_value_t value;
};

void * purs_any_get (enum purs_any_tag_t, struct purs_any_t *);

abs_t                    purs_any_get_abs       (struct purs_any_t *);
int                      purs_any_get_int       (struct purs_any_t *);
struct managed_block_t * purs_any_get_abs_block (struct purs_any_t *);
struct purs_cons_t *     purs_any_get_cons      (struct purs_any_t *);

struct purs_any_t * purs_any_set_abs       (struct purs_any_t *, abs_t *);
struct purs_any_t * purs_any_set_abs_block (struct purs_any_t *, struct managed_block_t *);
struct purs_any_t * purs_any_set_float     (struct purs_any_t *, float);
struct purs_any_t * purs_any_set_int       (struct purs_any_t *, int);
struct purs_any_t * purs_any_set_cons      (struct purs_any_t *, struct purs_cons_t *);
struct purs_any_t * purs_any_set_c_string  (struct purs_any_t *, char *);

#endif // PURESCRIPT_RUNTIME_H
