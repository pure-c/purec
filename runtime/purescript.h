#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#include "Block.h"
#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "vendor/utf8.h"
#include "ccan/asprintf/asprintf.h"
#include "uthash.h"

/* undefine the defaults */
#undef uthash_malloc
#undef uthash_free

/* re-define, specifying alternate functions */
#define uthash_malloc(sz) GC_MALLOC(sz)
#define uthash_free(ptr, sz)

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

typedef struct managed managed_t;
struct managed {
	const void * data;
};


typedef void (*managed_release_func)(managed_t * managed);
const managed_t * managed_new(const void * data, managed_release_func release);

// -----------------------------------------------------------------------------
// managed blocks
// -----------------------------------------------------------------------------

typedef managed_t managed_block_t;
const managed_block_t * managed_block_new (const void * block);

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
typedef managed_t managed_utf8str_t;
const managed_utf8str_t * managed_utf8str_new (const void *);

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

typedef struct purs_any purs_any_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef enum purs_any_tag purs_any_tag_t;
typedef const void * (^abs_block_t)(const void *);
typedef const purs_any_t * (*abs_t) (const purs_any_t*);

struct purs_cons {
	int tag;
	const purs_any_t ** values;
};

enum purs_any_tag {
	INT = 0,       // integer
	FLOAT = 1,     // float
	ABS = 2,       // abstraction
	ABS_BLOCK = 3, // lambda abstraction
	CONS = 4,      // data constructor
	STRING = 5,    // UTF8 string
	THUNK = 6,     // thunk
};

union purs_any_value {
	int num_int;
	float num_float;
	abs_t fn;
	const managed_utf8str_t * string;
	const managed_block_t * block;
	purs_cons_t cons;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

const purs_any_t * purs_any_unthunk (const purs_any_t * x);

const abs_t               purs_any_get_abs       (const purs_any_t *);
const int *               purs_any_get_int       (const purs_any_t *);
const float *             purs_any_get_float     (const purs_any_t *);
const managed_block_t *   purs_any_get_abs_block (const purs_any_t *);
const purs_cons_t *       purs_any_get_cons      (const purs_any_t *);
const managed_utf8str_t * purs_any_get_string    (const purs_any_t *);

purs_any_t * purs_any_set_abs       (purs_any_t *, const abs_t);
purs_any_t * purs_any_set_abs_block (purs_any_t *, const managed_t *);
purs_any_t * purs_any_set_float     (purs_any_t *, const float);
purs_any_t * purs_any_set_int       (purs_any_t *, const int);
purs_any_t * purs_any_set_cons      (purs_any_t *, const purs_cons_t);
purs_any_t * purs_any_set_string    (purs_any_t *, const managed_utf8str_t *);

const purs_any_t * purs_any_app (const purs_any_t *, const purs_any_t * arg);
const purs_any_t * purs_any_concat(const purs_any_t *, const purs_any_t *);

int purs_any_eq_string (const purs_any_t *, const void *);
int purs_any_eq_int    (const purs_any_t *, int);
int purs_any_eq_float  (const purs_any_t *, float);

/**
 * Create a lazily evaluated top-level value.
 */
#define PURS_ANY_THUNK_DECL($name, $init) \
	const purs_any_t * $name##____thunk_fn____ (const purs_any_t * ____unused____) { \
		static const purs_any_t * $name##____thunk_val____ = NULL; \
		if ($name##____thunk_val____ == NULL) { \
			$name##____thunk_val____ = $init; \
		} \
		return $name##____thunk_val____; \
	} \
	\
	const purs_any_t $name##____thunk____ = { \
		.tag = THUNK, \
		.value = { \
			.fn = $name##____thunk_fn____ \
		} \
	}; \
	\
	const purs_any_t * $name = & $name##____thunk____; \

#define PURS_ANY_NEW(n, x) \
	purs_any_set_##n( \
		GC_NEW(purs_any_t), \
		x \
	)

#define PURS_ANY_BLOCK(x) \
	PURS_ANY_NEW(abs_block, managed_block_new(Block_copy(^ x)))

#define PURS_ANY_INT(x) \
	PURS_ANY_NEW(int, x)

#define PURS_ANY_FLOAT(x) \
	PURS_ANY_NEW(float, x)

#define PURS_ANY_CONS(x) \
	PURS_ANY_NEW(cons, x)

#define PURS_ANY_STRING(x) \
	PURS_ANY_NEW(string, managed_utf8str_new(x))

/**
 * Helper to allocate a cons' 'value' field large enough to fit 'n' amount of
 * 'purs_any_t' pointers.
 *
 * XXX: this macro only exist for easy code emission from PureScript.
 */
#define PURS_CONS_VALUES_NEW(n) \
	GC_MALLOC(sizeof (purs_any_t *) * n)

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

typedef struct purs_record {
	const purs_any_t * key;
	const purs_any_t * value;
	UT_hash_handle hh;
} purs_record_t;

/**
 * Add given value under given key to given record.
 * Note: this function mutates the given record.
 */
#define PURS_RECORD_ADD_MUT($record, $key, $value) \
	do { \
		purs_record_t * entry = GC_NEW(purs_record_t); \
		entry->key = PURS_ANY_STRING($key); \
		entry->value = $value; \
		HASH_ADD_PTR($record, key, entry); \
	} while (0)

/**
 * Create a shallow copy of the given record.
 * Copies only the uthash structure
 */
const purs_record_t ** purs_record_copy_shallow(const purs_record_t *);

/**
 * Add a given key to the given record.
 */
const purs_record_t ** purs_record_add(const purs_record_t *, const void * key, const purs_any_t * value);

/**
 * Find an entry by it's key.
 * TODO: do this properly in amortized O(1)
 */
const purs_any_t * purs_record_find_by_key(const purs_record_t * record, const void * key);

#endif // PURESCRIPT_RUNTIME_H
