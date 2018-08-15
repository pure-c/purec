#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#include "Block.h"
#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "vendor/utf8.h"
#include "ccan/asprintf/asprintf.h"

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

typedef struct managed managed_t;
struct managed {
	void * data;
};


typedef void (*managed_release_func)(managed_t * managed);
managed_t * managed_new(void * data, managed_release_func release);

// -----------------------------------------------------------------------------
// managed blocks
// -----------------------------------------------------------------------------

typedef managed_t managed_block_t;
managed_block_t * managed_block_new (void * block);

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
typedef managed_t managed_utf8str_t;
managed_utf8str_t * managed_utf8str_new (void *);

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
	purs_any_t ** values;
	int len;
};

enum purs_any_tag {
	INT = 0,       // integer
	FLOAT = 1,     // float
	ABS = 2,       // abstraction
	ABS_BLOCK = 3, // lambda abstraction
	CONS = 4,      // data constructor
	STRING = 6,    // UTF8 string
};

union purs_any_value {
	int num_int;
	float num_float;
	abs_t * fn;
	managed_utf8str_t * string;
	managed_block_t * block;
	purs_cons_t cons;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

abs_t               purs_any_get_abs       (purs_any_t *);
int *               purs_any_get_int       (purs_any_t *);
managed_block_t *   purs_any_get_abs_block (purs_any_t *);
purs_cons_t *       purs_any_get_cons      (purs_any_t *);
managed_utf8str_t * purs_any_get_string    (purs_any_t *);

purs_any_t * purs_any_set_abs       (purs_any_t *, abs_t *);
purs_any_t * purs_any_set_abs_block (purs_any_t *, managed_t *);
purs_any_t * purs_any_set_float     (purs_any_t *, float);
purs_any_t * purs_any_set_int       (purs_any_t *, int);
purs_any_t * purs_any_set_cons      (purs_any_t *, purs_cons_t);
purs_any_t * purs_any_set_string    (purs_any_t *, void *);

purs_any_t * purs_any_app (purs_any_t *, purs_any_t * arg);
purs_any_t * purs_any_concat(purs_any_t *, purs_any_t *);

#define PURS_ANY_BLOCK(x) \
	purs_any_set_abs_block( \
		GC_NEW(purs_any_t), \
		managed_block_new(Block_copy(^ x)) \
	)

#endif // PURESCRIPT_RUNTIME_H
