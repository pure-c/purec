#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <Block.h>
#include <gc.h>
#include <uthash.h>
#include "vendor/utf8.h"
#include "vendor/vec.h"
#include "ccan/asprintf/asprintf.h"

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
// misc
// -----------------------------------------------------------------------------

const void * purs_assert_not_null(const void *, const char * message);

#define purs_log_error(M)\
	fprintf(stderr, "[ERROR] (%s:%d) %s\n", __FILE__, __LINE__, M)

#define purs_assertf(A, M)\
	do {\
		if (!(A)) {\
			purs_log_error(M);\
			assert(A);\
		}\
	} while (0)

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

typedef struct purs_any purs_any_t;
typedef vec_t(const purs_any_t*) purs_vec_t;
typedef struct purs_record purs_record_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef enum purs_any_tag purs_any_tag_t;
typedef const void * (^abs_block_t)(const void *);
typedef const purs_any_t * (*abs_t) (const purs_any_t*);

struct purs_cons {
	int tag;
	const purs_any_t * const * values;
};

enum purs_any_tag {
	INT = 0,       // integer
	FLOAT = 1,     // float
	ABS = 2,       // abstraction
	ABS_BLOCK = 3, // lambda abstraction
	CONS = 4,      // data constructor
	RECORD = 5,    // a record (hash table)
	STRING = 6,    // UTF8 string
	THUNK = 7,     // thunk
};

union purs_any_value {
	int num_int;
	float num_float;
	abs_t fn;
	const managed_utf8str_t * string;
	const managed_block_t * block;
	const purs_record_t * record;
	purs_cons_t cons;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

const purs_any_t *     purs_any_unthunk       (const purs_any_t *);
const purs_any_tag_t * purs_any_get_tag_maybe (const purs_any_t *);

const abs_t               purs_any_get_abs_maybe       (const purs_any_t *);
const int *               purs_any_get_int_maybe       (const purs_any_t *);
const float *             purs_any_get_float_maybe     (const purs_any_t *);
const managed_block_t *   purs_any_get_abs_block_maybe (const purs_any_t *);
const purs_cons_t *       purs_any_get_cons_maybe      (const purs_any_t *);
const managed_utf8str_t * purs_any_get_string_maybe    (const purs_any_t *);
const purs_record_t *     purs_any_get_record_maybe    (const purs_any_t *);

const abs_t               purs_any_get_abs       (const purs_any_t *);
const int *               purs_any_get_int       (const purs_any_t *);
const float *             purs_any_get_float     (const purs_any_t *);
const managed_block_t *   purs_any_get_abs_block (const purs_any_t *);
const purs_cons_t *       purs_any_get_cons      (const purs_any_t *);
const managed_utf8str_t * purs_any_get_string    (const purs_any_t *);
const purs_record_t *     purs_any_get_record    (const purs_any_t *);

// XXX: caution, these functions mutate the input!
purs_any_t * purs_any_set_abs       (purs_any_t *, const abs_t);
purs_any_t * purs_any_set_abs_block (purs_any_t *, const managed_t *);
purs_any_t * purs_any_set_float     (purs_any_t *, const float);
purs_any_t * purs_any_set_int       (purs_any_t *, const int);
purs_any_t * purs_any_set_cons      (purs_any_t *, const purs_cons_t);
purs_any_t * purs_any_set_string    (purs_any_t *, const managed_utf8str_t *);
purs_any_t * purs_any_set_record    (purs_any_t *, const purs_record_t *);

// XXX: for convenient emitting only (might be removed)
int purs_cons_get_tag (const purs_cons_t * cons);

const purs_any_t * purs_any_app (const purs_any_t *, const purs_any_t * arg);
const purs_any_t * purs_any_concat(const purs_any_t *, const purs_any_t *);

int purs_any_eq_string (const purs_any_t *, const void *);
int purs_any_eq_int    (const purs_any_t *, int);
int purs_any_eq_float  (const purs_any_t *, float);

/**
 * Declare a lazily evaluated top-level value.
 */
#define PURS_ANY_THUNK_DECL(NAME)\
	const purs_any_t * NAME##____thunk_fn____ (const purs_any_t *);\
	const purs_any_t NAME##____thunk____;\
	const purs_any_t * NAME;

/**
 * Create a lazily evaluated top-level value.
 */
#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	const purs_any_t * NAME##____thunk_fn____ (const purs_any_t * ____unused____) {\
		static const purs_any_t * NAME##____thunk_val____ = NULL;\
		if (NAME##____thunk_val____ == NULL) {\
			NAME##____thunk_val____ = INIT;\
		}\
		return NAME##____thunk_val____;\
	}\
	\
	const purs_any_t NAME##____thunk____ = {\
		.tag = THUNK,\
		.value = {\
			.fn = NAME##____thunk_fn____\
		}\
	};\
	\
	const purs_any_t * NAME = & NAME##____thunk____;\

#define PURS_ANY_NEW(n, x)\
	purs_any_set_##n(\
		GC_NEW(purs_any_t),\
		x\
	)

#define PURS_ANY_BLOCK(x)\
	PURS_ANY_NEW(abs_block, managed_block_new(Block_copy(^ x)))

#define PURS_ANY_INT(x)\
	PURS_ANY_NEW(int, x)

#define PURS_ANY_FLOAT(x)\
	PURS_ANY_NEW(float, x)

#define PURS_ANY_CONS(x)\
	PURS_ANY_NEW(cons, x)

#define PURS_ANY_STRING_FROM_LIT(x)\
	PURS_ANY_NEW(string, managed_utf8str_new(afmt("%s", x)))

#define PURS_ANY_STRING(x)\
	PURS_ANY_NEW(string, managed_utf8str_new(x))

#define PURS_ANY_RECORD(n, ...)\
	PURS_ANY_NEW(record, purs_record_add_multi(NULL, n, __VA_ARGS__))

/**
 * Helper to allocate a cons' 'value' field large enough to fit 'n' amount of
 * 'purs_any_t' pointers.
 *
 * XXX: this macro only exist for easy code emission from PureScript.
 */
#define PURS_CONS_VALUES_NEW(n)\
	GC_MALLOC(sizeof (purs_any_t *) * n)
#define PURS_CONS_LIT(TAG, VALUES)\
	((purs_cons_t) { .tag = TAG, .values = VALUES })

// -----------------------------------------------------------------------------
// arrays (via vectors)
// -----------------------------------------------------------------------------

void purs_vec_release (purs_vec_t *);
const purs_vec_t * purs_vec_new (const purs_any_t ** items, int count);
const purs_vec_t * purs_vec_copy (const purs_vec_t *);

// -----------------------------------------------------------------------------
// records (via hash table)
// -----------------------------------------------------------------------------

typedef struct purs_record {
	const managed_utf8str_t * key;
	const purs_any_t * value;
	UT_hash_handle hh;
} purs_record_t;

/**
 * Create a shallow copy of the given record.
 * Copies only the uthash structure
 */
const purs_record_t * purs_record_copy_shallow(const purs_record_t *);

/**
 * Add a given set of key/value pairs to the given record.
 */
const purs_record_t * purs_record_add_multi(const purs_record_t *,
					    size_t count,
					    ...);

/**
 * Add a given key/value pair to the given record.
 */
#define purs_record_add(record, k, v) purs_record_add_multi(record, 1, k, v)

/**
 * Find an entry by it's key.
 */
purs_record_t * purs_record_find_by_key(const purs_record_t *,
					const void * key);

/**
 * Remove an entry by it's key.
 */
const purs_record_t * purs_record_remove(const purs_record_t *,
					 const void * key);

// -----------------------------------------------------------------------------
// FFI Helpers
// -----------------------------------------------------------------------------

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_FUNC_DECL(NAME)\
	PURS_ANY_THUNK_DECL(NAME##$)

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_FUNC_DEF(NAME, ARG_VARNAME, BODY)\
	PURS_ANY_THUNK_DEF(\
		NAME##$,\
		PURS_ANY_BLOCK((const purs_any_t * ARG_VARNAME) BODY))

#define PURS_FFI_LAMBDA(ARG_VARNAME, BODY)\
	PURS_ANY_BLOCK((const purs_any_t * ARG_VARNAME) BODY)

#endif // PURESCRIPT_RUNTIME_H
