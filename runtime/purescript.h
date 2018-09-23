#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

/* #define PURS_DEBUG_FINALIZATION */
/* #define PURS_DEBUG_SCOPES */

#define purs_malloc(sz) GC_MALLOC(sz)
#define purs_new(exp) GC_NEW(exp)

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "deps/bwdgc/include/gc.h"
#include "deps/blocksruntime/BlocksRuntime/Block.h"
#include "vendor/uthash.h"
#include "vendor/utf8.h"
#include "vendor/vec.h"
#include "ccan/asprintf/asprintf.h"

typedef struct purs_any purs_any_t;
typedef vec_t(const purs_any_t*) purs_vec_t;
typedef struct purs_record purs_record_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef enum purs_any_tag purs_any_tag_t;
typedef const purs_any_t * (^abs_block_t)(const purs_any_t *, va_list args);
typedef const purs_any_t * (*abs_t) (const purs_any_t*);

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

typedef struct managed managed_t;

typedef vec_t(void*) ptr_vec_t;
struct managed {
	const void * data;
	void * ctx;
	char * label;
};

typedef void (*managed_release_func)(managed_t * managed);
const managed_t * managed_new(const void * data, void * ctx, char * label, managed_release_func release);

// -----------------------------------------------------------------------------
// scopes
// -----------------------------------------------------------------------------

typedef struct purs_scope_entries {
	const purs_any_t * key;
	UT_hash_handle hh;
} purs_scope_entries_t;

typedef struct purs_scope purs_scope_t;
typedef struct purs_scope {
	const purs_scope_t * parent;
	purs_scope_entries_t * objects;
} purs_scope_t;

purs_scope_t * purs_scope_new (const purs_scope_t * parent);

purs_scope_t * __scope__;

const purs_any_t * _purs_scope_capture(purs_scope_t *, const purs_any_t *);
#define purs_scope_capture(PTR)\
	(const purs_any_t *) _purs_scope_capture(__scope__, PTR)

// -----------------------------------------------------------------------------
// managed blocks
// -----------------------------------------------------------------------------

typedef managed_t managed_block_t;
const managed_block_t * managed_block_new (char * label, purs_scope_t *, const void * block);

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
typedef managed_t managed_utf8str_t;
const managed_utf8str_t * managed_utf8str_new (const void *);

// -----------------------------------------------------------------------------
// misc
// -----------------------------------------------------------------------------

const void * purs_assert_not_null(const void *, const char * message);

#define purs_log_error(FMT, ...)\
	do {\
		fprintf(stderr, "[ERROR] (%s:%d) " #FMT "\n", __FILE__, __LINE__, ##__VA_ARGS__);\
	} while (0)

#define purs_assert(A, FMT, ...)\
	do {\
		if (!(A)) {\
			purs_log_error(FMT, ##__VA_ARGS__);\
			assert(A);\
		}\
	} while (0)

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

#define purs_any_int_t int32_t

struct purs_cons {
	int tag;
	const purs_any_t ** values;
};

enum purs_any_tag {
	PURS_ANY_TAG_UNKNOWN = 0,   // unknown (unused)
	PURS_ANY_TAG_INT = 1,       // integer
	PURS_ANY_TAG_NUMBER = 2,    // number
	PURS_ANY_TAG_ABS = 3,       // abstraction
	PURS_ANY_TAG_ABS_BLOCK = 4, // lambda abstraction
	PURS_ANY_TAG_CONS = 5,      // data constructor
	PURS_ANY_TAG_RECORD = 6,    // a record (hash table)
	PURS_ANY_TAG_STRING = 7,    // UTF8 string
	PURS_ANY_TAG_CHAR = 8,      // UTF8 string
	PURS_ANY_TAG_ARRAY = 9,     // array
	PURS_ANY_TAG_THUNK = 10,    // thunk
	PURS_ANY_TAG_FOREIGN = 11,  // a wrapped foreign value
};

const char * purs_any_tag_str (const purs_any_tag_t);

typedef struct purs_foreign {
	void * tag;
	void * data;
} purs_foreign_t;

union purs_any_value {
	purs_any_int_t integer;
	double number;
	abs_t fn;
	utf8_int32_t _char;
	const managed_utf8str_t * string;
	const managed_block_t * block;
	const purs_record_t * record;
	const purs_vec_t * array;
	purs_cons_t cons;
	purs_foreign_t foreign;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

const purs_any_t *     purs_any_unthunk       (const purs_any_t *);
const purs_any_tag_t * purs_any_get_tag_maybe (const purs_any_t *);

const abs_t               purs_any_get_abs_maybe       (const purs_any_t *);
const purs_any_int_t *    purs_any_get_int_maybe       (const purs_any_t *);
const double *            purs_any_get_number_maybe    (const purs_any_t *);
const managed_block_t *   purs_any_get_abs_block_maybe (const purs_any_t *);
const purs_cons_t *       purs_any_get_cons_maybe      (const purs_any_t *);
const managed_utf8str_t * purs_any_get_string_maybe    (const purs_any_t *);
const utf8_int32_t *      purs_any_get_char_maybe      (const purs_any_t *);
const purs_record_t *     purs_any_get_record_maybe    (const purs_any_t *);
const purs_vec_t *        purs_any_get_array_maybe     (const purs_any_t *);
const purs_foreign_t *    purs_any_get_foreign_maybe   (const purs_any_t *);

const abs_t               purs_any_get_abs       (const purs_any_t *);
const purs_any_int_t *    purs_any_get_int       (const purs_any_t *);
const double *            purs_any_get_number    (const purs_any_t *);
const managed_block_t *   purs_any_get_abs_block (const purs_any_t *);
const purs_cons_t *       purs_any_get_cons      (const purs_any_t *);
const managed_utf8str_t * purs_any_get_string    (const purs_any_t *);
const utf8_int32_t *      purs_any_get_char      (const purs_any_t *);
const purs_record_t *     purs_any_get_record    (const purs_any_t *);
const purs_vec_t *        purs_any_get_array     (const purs_any_t *);
const purs_foreign_t *    purs_any_get_foreign   (const purs_any_t *);

// XXX: caution, these functions mutate the input!
purs_any_t * purs_any_init_abs       (purs_any_t *, const abs_t);
purs_any_t * purs_any_init_abs_block (purs_any_t *, const managed_t *);
purs_any_t * purs_any_init_number    (purs_any_t *, const double);
purs_any_t * purs_any_init_int       (purs_any_t *, const purs_any_int_t);
purs_any_t * purs_any_init_char      (purs_any_t *, const utf8_int32_t);
purs_any_t * purs_any_init_cons      (purs_any_t *, const purs_cons_t);
purs_any_t * purs_any_init_string    (purs_any_t *, const managed_utf8str_t *);
purs_any_t * purs_any_init_record    (purs_any_t *, const purs_record_t *);
purs_any_t * purs_any_init_array     (purs_any_t *, const purs_vec_t *);
purs_any_t * purs_any_init_foreign   (purs_any_t *, const purs_foreign_t);

// XXX: for convenient emitting only (might be removed)
int purs_cons_get_tag (const purs_cons_t * cons);

const purs_any_t * purs_any_app (const purs_any_t *, const purs_any_t * arg, ...);
const purs_any_t * purs_any_concat(const purs_any_t *, const purs_any_t *);

int purs_any_eq_string (const purs_any_t *, const void *);
int purs_any_eq_char   (const purs_any_t *, utf8_int32_t);
int purs_any_eq_int    (const purs_any_t *, purs_any_int_t);
int purs_any_eq_number (const purs_any_t *, double);

/**
 * Create a lazily evaluated top-level value.
 */
#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	static const purs_any_t * NAME ## ____thunk_fn____ (const purs_any_t * ____unused____) {\
		static const purs_any_t * NAME ## ____thunk_val____ = NULL;\
		if (NAME ## ____thunk_val____ == NULL) {\
			NAME ## ____thunk_val____ = INIT;\
		}\
		return NAME ## ____thunk_val____;\
	}\
	\
	static const purs_any_t NAME ## ____thunk____ = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = {\
			.fn = NAME ## ____thunk_fn____\
		}\
	};\
	\
	const purs_any_t * NAME = & NAME ## ____thunk____;\

#define PURS_ANY_NEW(n, x)\
	(const purs_any_t *) purs_any_init_##n(\
		purs_new(purs_any_t),\
		x\
	)

#define PURS_ANY_INT_NEW(x)\
	PURS_ANY_NEW(int, x)

#define PURS_ANY_NUMBER_NEW(x)\
	PURS_ANY_NEW(number, x)

#define PURS_ANY_CONS_NEW(x)\
	PURS_ANY_NEW(cons, x)

/* TODO: remove this macro */
#define PURS_ANY_STRING_NEW_FROM_LIT(x)\
	PURS_ANY_NEW(string, managed_utf8str_new(afmt("%s", x)))

#define PURS_ANY_STRING_NEW(x)\
	PURS_ANY_NEW(string, managed_utf8str_new(x))

#define PURS_ANY_CHAR_NEW(x)\
	PURS_ANY_NEW(char, x)

#define PURS_ANY_RECORD_NEW(x)\
	PURS_ANY_NEW(record, x)

#define PURS_ANY_ARRAY_NEW(x)\
	PURS_ANY_NEW(array, x)

#define _PURS_FOREIGN(TAG, DATA)\
	{\
		.tag = (TAG),\
		.data = (DATA)\
	}\

#define PURS_ANY_FOREIGN_NEW(TAG, DATA)\
	PURS_ANY_NEW(foreign, ((purs_foreign_t) _PURS_FOREIGN(TAG, (void *) DATA)))

/*
 * purs_any_t initializers
 */

#define PURS_ANY_INT(x)\
	{ .tag = PURS_ANY_TAG_INT, .value = { .integer = x } }

#define PURS_ANY_NUMBER(x)\
	{ .tag = PURS_ANY_TAG_NUMBER, .value = { .number = x } }

#define PURS_ANY_CHAR(x)\
	{ .tag = PURS_ANY_TAG_CHAR, .value = { ._char = x } }

#define PURS_ANY_FOREIGN(TAG, DATA)\
	{\
		.tag = PURS_ANY_TAG_FOREIGN,\
		.value = {\
			.foreign = {\
				.tag = (TAG),\
				.data = (DATA)\
			}\
		}\
	}

/**
 * Helper to allocate a cons' 'value' field large enough to fit 'n' amount of
 * 'purs_any_t' pointers.
 *
 * XXX: this macro only exist for easy code emission from PureScript.
 */
#define PURS_CONS_VALUES_NEW(n)\
	purs_malloc(sizeof (const purs_any_t *) * n)

#define PURS_CONS_LIT(TAG, VALUES)\
	((purs_cons_t) { .tag = TAG, .values = VALUES })

// -----------------------------------------------------------------------------
// strings (via managed_utf8str_t)
// -----------------------------------------------------------------------------

const void * purs_string_copy (const void * string);

// -----------------------------------------------------------------------------
// arrays (via vectors)
// -----------------------------------------------------------------------------

void purs_vec_release (purs_vec_t *);
const purs_vec_t * purs_vec_new ();
const purs_vec_t * purs_vec_new_va (int count, ...);
const purs_vec_t * purs_vec_copy (const purs_vec_t *);
const purs_vec_t * purs_vec_slice (const purs_vec_t *, int begin);

#define purs_vec_new_from_array(count, ...)\
	purs_vec_new_va(count, __VA_ARGS__)

#define purs_vec_foreach(v, var, iter)\
	vec_foreach(v, var, iter)

#define purs_vec_reserve(v, n)\
	vec_reserve(v, n)

#define purs_vec_push_mut(v, x)\
	vec_push(v, x)

#define purs_vec_pusharr_mut(v, arr, count)\
	vec_pusharr(v, arr, count)

/**
 * Insert the value val at index idx shifting the elements after the index to
 * make room for the new value.
 */
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, const purs_any_t * val);

// -----------------------------------------------------------------------------
// records (via hash table)
// -----------------------------------------------------------------------------

typedef struct purs_record {
	const managed_utf8str_t * key;
	const purs_any_t * value;
	UT_hash_handle hh;
} purs_record_t;

const purs_any_t * purs_record_empty;

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
const purs_record_t * purs_record_find_by_key(const purs_record_t *,
					      const void * key);

/**
 * Remove an entry by it's key.
 */
const purs_record_t * purs_record_remove(const purs_record_t *,
					 const void * key);

/**
 * Create a new record from a bunch of key value pairs.
 * The 'count' is the count of pairs, not elements in the va_list.
 */
#define purs_record_new_from_kvps(count, ...)\
    purs_record_add_multi(NULL, count, __VA_ARGS__)

// -----------------------------------------------------------------------------
// FFI Helpers
// -----------------------------------------------------------------------------

#define managed_block_capture(MANAGED_BLOCK, PTR)\
	vec_push(managed->ptrs, ptr)

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_EXPORT(NAME)\
	const purs_any_t * NAME ## $

#define PURS_LAMBDA(A1, BODY)\
	PURS_ANY_NEW(\
		abs_block,\
		managed_block_new(\
			NULL,\
			__scope__,\
			Block_copy(^\
				(const purs_any_t * A1) \
				{\
					purs_scope_t * __parent_scope__ = __scope__;\
					purs_scope_t * __scope__ = purs_scope_new(__parent_scope__);\
					purs_scope_capture(A1);\
					BODY\
				}\
			)\
		)\
	)

/* TODO: generate these (using m4?) */
#define __MK_PURS_LAMBDA_UNCURRIED(A1, BODY, VA_ARGS_DECLS, VA_ARGS_DEFS, VA_ARGS_CAPTURE)\
	PURS_ANY_NEW(\
		abs_block,\
		managed_block_new(\
			NULL,\
			__scope__,\
			Block_copy(^\
				(const purs_any_t * A1, va_list args)\
				{\
					purs_scope_t * __parent_scope__ = __scope__;\
					purs_scope_t * __scope__ = purs_scope_new(__parent_scope__);\
					purs_scope_capture(A1);\
					VA_ARGS_DECLS;\
					{\
						VA_ARGS_DEFS;\
					}\
					VA_ARGS_CAPTURE;\
					BODY\
				}\
			)\
		)\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_2(A1, A2, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;,\
		A2 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_3(A1, A2, A3, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_4(A1, A2, A3, A4, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_5(A1, A2, A3, A4, A5, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_6(A1, A2, A3, A4, A5, A6, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_7(A1, A2, A3, A4, A5, A6, A7, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_8(A1, A2, A3, A4, A5, A6, A7, A8, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;\
		const purs_any_t * A8;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);\
		A8 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
		purs_scope_capture(A8);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_9(A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;\
		const purs_any_t * A8;\
		const purs_any_t * A9;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);\
		A8 = va_arg(args, const purs_any_t *);\
		A9 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
		purs_scope_capture(A8);\
		purs_scope_capture(A9);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;\
		const purs_any_t * A8;\
		const purs_any_t * A9;\
		const purs_any_t * A10;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);\
		A8 = va_arg(args, const purs_any_t *);\
		A9 = va_arg(args, const purs_any_t *);\
		A10 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
		purs_scope_capture(A8);\
		purs_scope_capture(A9);\
		purs_scope_capture(A10);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;\
		const purs_any_t * A8;\
		const purs_any_t * A9;\
		const purs_any_t * A10;\
		const purs_any_t * A11;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);\
		A8 = va_arg(args, const purs_any_t *);\
		A9 = va_arg(args, const purs_any_t *);\
		A10 = va_arg(args, const purs_any_t *);\
		A11 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
		purs_scope_capture(A8);\
		purs_scope_capture(A9);\
		purs_scope_capture(A10);\
		purs_scope_capture(A11);\
	)

/* TODO: generate these (using m4?) */
#define PURS_LAMBDA_UNCURRIED_12(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY)\
	__MK_PURS_LAMBDA_UNCURRIED(A1, BODY,\
		const purs_any_t * A2;\
		const purs_any_t * A3;\
		const purs_any_t * A4;\
		const purs_any_t * A5;\
		const purs_any_t * A6;\
		const purs_any_t * A7;\
		const purs_any_t * A8;\
		const purs_any_t * A9;\
		const purs_any_t * A10;\
		const purs_any_t * A11;,\
		const purs_any_t * A12;,\
		A2 = va_arg(args, const purs_any_t *);\
		A3 = va_arg(args, const purs_any_t *);\
		A4 = va_arg(args, const purs_any_t *);\
		A5 = va_arg(args, const purs_any_t *);\
		A6 = va_arg(args, const purs_any_t *);\
		A7 = va_arg(args, const purs_any_t *);\
		A8 = va_arg(args, const purs_any_t *);\
		A9 = va_arg(args, const purs_any_t *);\
		A10 = va_arg(args, const purs_any_t *);\
		A11 = va_arg(args, const purs_any_t *);,\
		A12 = va_arg(args, const purs_any_t *);,\
		purs_scope_capture(A2);\
		purs_scope_capture(A3);\
		purs_scope_capture(A4);\
		purs_scope_capture(A5);\
		purs_scope_capture(A6);\
		purs_scope_capture(A7);\
		purs_scope_capture(A8);\
		purs_scope_capture(A9);\
		purs_scope_capture(A10);\
		purs_scope_capture(A11);\
		purs_scope_capture(A12);\
	)


#define PURS_LAMBDA_1(A1, BODY)\
	PURS_LAMBDA(A1, BODY)

#define PURS_LAMBDA_2(A1, A2, BODY)\
	PURS_LAMBDA_1(A1, {\
		return PURS_LAMBDA(A2, BODY);\
	})

#define PURS_LAMBDA_3(A1, A2, A3, BODY)\
	PURS_LAMBDA_2(A1, A2, {\
		return PURS_LAMBDA(A3, BODY);\
	})

#define PURS_LAMBDA_4(A1, A2, A3, A4, BODY)\
	PURS_LAMBDA_3(A1, A2, A3, {\
		return PURS_LAMBDA(A4, BODY);\
	})

#define PURS_LAMBDA_5(A1, A2, A3, A4, A5, BODY)\
	PURS_LAMBDA_4(A1, A2, A3, A4, {\
		return PURS_LAMBDA(A5, BODY);\
	})

#define PURS_LAMBDA_6(A1, A2, A3, A4, A5, A6, BODY)\
	PURS_LAMBDA_5(A1, A2, A3, A4, A5, {\
		return PURS_LAMBDA(A6, BODY);\
	})

#define PURS_LAMBDA_7(A1, A2, A3, A4, A5, A6, A7, BODY)\
	PURS_LAMBDA_6(A1, A2, A3, A4, A5, A6, {\
		return PURS_LAMBDA(A7, BODY);\
	})

#define PURS_LAMBDA_8(A1, A2, A3, A4, A5, A6, A7, A8, BODY)\
	PURS_LAMBDA_7(A1, A2, A3, A4, A5, A6, A7, {\
		return PURS_LAMBDA(A8, BODY);\
	})

#define PURS_LAMBDA_9(A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\
	PURS_LAMBDA_8(A1, A2, A3, A4, A5, A6, A7, A8, {\
		return PURS_LAMBDA(A9, BODY);\
	})

#define PURS_LAMBDA_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\
	PURS_LAMBDA_9(A1, A2, A3, A4, A5, A6, A7, A8, A9, {\
		return PURS_LAMBDA(A10, BODY);\
	})

#define PURS_LAMBDA_11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY) \
	PURS_LAMBDA_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, {\
		return PURS_LAMBDA(A11, BODY);\
	})

#define PURS_LAMBDA_12(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY) \
	PURS_LAMBDA_11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, {\
		return PURS_LAMBDA(A12, BODY);\
	})

#define PURS_FFI_VALUE(NAME, INIT)\
	static const purs_any_t _ ## NAME ## $ = INIT;\
	const purs_any_t * NAME ## $ = & _ ## NAME ## $

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_VALUE_THUNKED(NAME, INIT)\
	PURS_ANY_THUNK_DEF(NAME ## $, INIT)

#define PURS_FFI_FUNC_1(NAME, A1, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_1(A1, BODY))

#define PURS_FFI_FUNC_2(NAME, A1, A2, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_2(A1, A2, BODY))

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_3(A1, A2, A3, BODY))

#define PURS_FFI_FUNC_4(NAME, A1, A2, A3, A4, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_4(A1, A2, A3, A4, BODY))

#define PURS_FFI_FUNC_5(NAME, A1, A2, A3, A4, A5, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_5(A1, A2, A3, A4, A5, BODY))

#define PURS_FFI_FUNC_6(NAME, A1, A2, A3, A4, A5, A6, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_6(A1, A2, A3, A4, A5, A6, BODY))

#define PURS_FFI_FUNC_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_7(A1, A2, A3, A4, A5, A6, A7, BODY))

#define PURS_FFI_FUNC_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_8(A1, A2, A3, A4, A5, A6, A7, A8, BODY))

#define PURS_FFI_FUNC_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_9(A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY))

#define PURS_FFI_FUNC_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY))

#define PURS_FFI_FUNC_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY))

#define PURS_FFI_FUNC_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_12(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY))

#define PURS_FFI_FUNC_UNCURRIED_1(NAME, A1, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_1(A1, BODY))

#define PURS_FFI_FUNC_UNCURRIED_2(NAME, A1, A2, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_2(A1, A2, BODY))

#define PURS_FFI_FUNC_UNCURRIED_3(NAME, A1, A2, A3, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_3(A1, A2, A3, BODY))

#define PURS_FFI_FUNC_UNCURRIED_4(NAME, A1, A2, A3, A4, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_4(A1, A2, A3, A4, BODY))

#define PURS_FFI_FUNC_UNCURRIED_5(NAME, A1, A2, A3, A4, A5, BODY)\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_5(A1, A2, A3, A4, A5, BODY))

#define PURS_FFI_FUNC_UNCURRIED_6(NAME, A1, A2, A3, A4, A5, A6, BODY)	\
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_6(A1, A2, A3, A4, A5, A6, BODY))

#define PURS_FFI_FUNC_UNCURRIED_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_7(A1, A2, A3, A4, A5, A6, A7, BODY))

#define PURS_FFI_FUNC_UNCURRIED_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_8(A1, A2, A3, A4, A5, A6, A7, A8, BODY))

#define PURS_FFI_FUNC_UNCURRIED_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_9(A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY))

#define PURS_FFI_FUNC_UNCURRIED_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY))

#define PURS_FFI_FUNC_UNCURRIED_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY))

#define PURS_FFI_FUNC_UNCURRIED_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY) \
	PURS_FFI_VALUE_THUNKED(\
		NAME,\
		PURS_LAMBDA_UNCURRIED_12(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY))

// -----------------------------------------------------------------------------
// Prim shims
// -----------------------------------------------------------------------------

#define Prim_undefined$ NULL

// -----------------------------------------------------------------------------
// Built-ins
// -----------------------------------------------------------------------------

const purs_any_t * purs_any_true;
const purs_any_t * purs_any_false;

const purs_any_t * purs_any_eq(const purs_any_t *, const purs_any_t *);

#endif // PURESCRIPT_RUNTIME_H
