#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

/* #define PURS_DEBUG_FINALIZATION */
/* #define PURS_DEBUG_SCOPES */

#define purs_malloc(sz) GC_MALLOC(sz)
#define purs_new(exp) GC_NEW(exp)

#define purs_log_error(FMT, ...)\
	do {\
		fprintf(stderr,\
			"[ERROR] (%s:%d) " # FMT "\n",\
			__FILE__,\
			__LINE__,\
			##__VA_ARGS__);\
	} while (0)

#define purs_assert(A, FMT, ...)\
	do {\
		if (!(A)) {\
			purs_log_error(FMT, ##__VA_ARGS__);\
			assert(A);\
		}\
} while (0)

#ifdef ANY
#error macro 'ANY' already defined
#endif

#ifdef APP
#error macro 'APP' already defined
#endif

#define ANY purs_any_t
#define APP purs_any_app

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "deps/bwdgc/include/gc.h"
#include "ccan/asprintf/asprintf.h"
#include "vendor/uthash.h"
#include "vendor/utf8.h"
#include "vendor/vec.h"

#define purs_any_int_t int32_t
#define purs_any_num_t double
typedef struct managed managed_t;
#define managed_utf8str_t managed_t
typedef struct purs_any purs_any_t;
typedef vec_t(const purs_any_t*) purs_vec_t;
typedef struct purs_record purs_record_t;
typedef struct purs_cons purs_cons_t;
typedef struct purs_cont purs_cont_t;
typedef struct purs_any_cont purs_any_cont_t;
typedef struct purs_any_thunk purs_any_thunk_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef const ANY * (purs_any_thunk_fun_t)(const void * ctx);
typedef const ANY * (purs_any_fun_t)(const void * ctx, const ANY *, va_list);
typedef struct purs_foreign purs_foreign_t;

struct managed { const void * data; };
void managed_default_release (managed_t * managed);
typedef void (*managed_release_func)(managed_t * managed);
const managed_t * managed_new(const void * data, managed_release_func release);

typedef enum {
	PURS_ANY_TAG_UNKNOWN = 0,
	PURS_ANY_TAG_INT = 1,
	PURS_ANY_TAG_NUM = 2,
	PURS_ANY_TAG_CONT = 3,
	PURS_ANY_TAG_THUNK = 4,
	PURS_ANY_TAG_CONS = 5,
	PURS_ANY_TAG_RECORD = 6,
	PURS_ANY_TAG_STRING = 7,
	PURS_ANY_TAG_CHAR = 8,
	PURS_ANY_TAG_ARRAY = 9,
	PURS_ANY_TAG_FOREIGN = 11,
} purs_any_tag_t;

struct purs_any_cont {
	purs_any_fun_t * fn;
	const void * ctx;
};

struct purs_any_thunk {
	purs_any_thunk_fun_t * fn;
	const void * ctx;
};

struct purs_foreign {
	void * tag;
	void * data;
};

struct purs_cons {
	int tag;
	const ANY ** values;
};

union purs_any_value {
	purs_any_int_t i;
	purs_any_num_t n;
	purs_any_cont_t cont;
	purs_any_thunk_t thunk;
	purs_cons_t cons;
	const purs_record_t * record;
	const managed_t * str;
	utf8_int32_t chr;
	const purs_vec_t * array;
	purs_foreign_t foreign;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

const ANY * purs_any_app(const ANY * f, const ANY * v, ...);
const ANY * purs_any_unthunk (const ANY *);
const purs_any_tag_t purs_any_get_tag (const ANY *);
const char * purs_any_tag_str (const purs_any_tag_t);

const ANY * purs_any_int_new(const purs_any_int_t);
const ANY * purs_any_num_new(const purs_any_num_t);
const ANY * purs_any_cont_new(const void * ctx, purs_any_fun_t *);
const ANY * purs_any_thunk_new(const void * ctx, purs_any_thunk_fun_t *);
const ANY * purs_any_cons_new(int tag, const ANY ** values);
const ANY * purs_any_record_new(const purs_record_t *);
const ANY * purs_any_string_new(const char * fmt, ...);
const ANY * purs_any_char_new(utf8_int32_t);
const ANY * purs_any_array_new(const purs_vec_t *);
const ANY * purs_any_foreign_new(void * tag, void * data);

const purs_any_int_t     purs_any_get_int       (const ANY *);
const purs_any_num_t     purs_any_get_num       (const ANY *);
const purs_cont_t *      purs_any_get_cont      (const ANY *);
const purs_cons_t *      purs_any_get_cons      (const ANY *);
const purs_record_t *    purs_any_get_record    (const ANY *);
const void *             purs_any_get_string    (const ANY *);
const utf8_int32_t       purs_any_get_char      (const ANY *);
const purs_vec_t *       purs_any_get_array     (const ANY *);
const purs_foreign_t *   purs_any_get_foreign   (const ANY *);

// -----------------------------------------------------------------------------
// Any: built-in functions
// -----------------------------------------------------------------------------

int purs_any_eq_string (const ANY *, const void *);
int purs_any_eq_char   (const ANY *, utf8_int32_t);
int purs_any_eq_int    (const ANY *, purs_any_int_t);
int purs_any_eq_num    (const ANY *, double);

int purs_any_eq(const ANY *, const ANY *);
const ANY * purs_any_concat(const ANY *, const ANY *);
const ANY * purs_any_copy(const ANY *);

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

const void * purs_string_copy (const void *);

// -----------------------------------------------------------------------------
// arrays
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
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, const ANY * val);

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

typedef struct purs_record {
	const managed_utf8str_t * key;
	const ANY * value;
	UT_hash_handle hh;
} purs_record_t;

// TODO: rename to 'purs_any_record_empty'
const ANY * purs_record_empty;

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
// Code-gen helpers
// -----------------------------------------------------------------------------

/* thunked pointer dereference. useful for recursive bindings */
const ANY ** purs_indirect_value_new();
void purs_indirect_value_assign(const ANY **, const ANY *);
const ANY * purs_indirect_thunk_new(const ANY **);
const ANY * purs_thunked_deref(const void * data);

#define purs_any_int_neg(X) purs_any_int_new(-purs_any_get_int(X))
#define purs_any_int_set_mut(X, V) do { X->value.i = V; } while (0)
#define purs_any_assign_mut(V1, V2)\
	do {\
		((ANY*) V1)->tag = V2->tag;\
		((ANY*) V1)->value = V2->value;\
	} while (0)

/* code-gen helper to allocate and fill a scope.
 * assumes scope to consist only of (const ANY *) pointers, the count of which
 * is known.
 */
const ANY ** _purs_scope_alloc(int num_bindings);
const ANY ** _purs_scope_new(int num_bindings, const ANY * binding, ...);

/* declare a thunked top-level value.
 */
#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	static const ANY * NAME ## __thunk_fn__ (const void * __unused__1) { \
		static const ANY * NAME ## __thunk_val__ = NULL;\
		if (NAME ## __thunk_val__ == NULL) {\
			NAME ## __thunk_val__ = INIT;\
		}\
		return NAME ## __thunk_val__;\
	}\
	static const ANY NAME ## __thunk__ = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = {\
			.thunk = {\
				.fn = NAME ## __thunk_fn__,\
				.ctx = NULL\
			}\
		}\
	};\
	const ANY * NAME = & NAME ## __thunk__;\

/* allocate a cons 'value' field large enough to fit 'n' amount of 'ANY *'
 */
#define PURS_CONS_VALUES_NEW(n)\
	purs_malloc(sizeof (const ANY *) * n)

/* simply return the 'tag' of a 'purs_cons_t'.
 */
int purs_cons_get_tag (const purs_cons_t * cons);

// -----------------------------------------------------------------------------
// Any: initializers
// -----------------------------------------------------------------------------

#define PURS_ANY_INT(x)\
	{ .tag = PURS_ANY_TAG_INT, .value = { .i = x } }

#define PURS_ANY_NUM(x)\
	{ .tag = PURS_ANY_TAG_NUM, .value = { .n = x } }

#define PURS_ANY_CHAR(x)\
	{ .tag = PURS_ANY_TAG_CHAR, .value = { .chr = x } }

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

// -----------------------------------------------------------------------------
// FFI helpers
// -----------------------------------------------------------------------------

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_EXPORT(NAME)\
	const ANY * NAME ## $

#define PURS_SCOPE_T(NAME, DECLS)\
	typedef struct NAME {\
		struct DECLS;\
	} NAME

#define PURS_FFI_VALUE(NAME, INIT)\
	static const purs_any_t _ ## NAME ## $ = INIT;\
	const purs_any_t * NAME ## $ = & _ ## NAME ## $

// -----------------------------------------------------------------------------
// FFI: fixed-arity curried functions
// -----------------------------------------------------------------------------

#define _PURS_FFI_FUNC_ENTRY(NAME)\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME##__1, .ctx = NULL } }\
	};\
	const ANY * NAME ## $ = & NAME##__1_

#define _PURS_FFI_FUNC_CONT(NAME, CUR, NEXT)\
	const ANY * NAME##__##CUR (const void * super, const ANY * a, va_list $__unused__) {\
		const ANY ** ctx = _purs_scope_alloc(CUR);\
		if (super != NULL) {\
			memcpy(ctx, super, CUR * sizeof (const ANY *));\
		}\
		if (ctx != NULL) {\
			ctx[CUR - 1] = a;\
		}\
		return purs_any_cont_new(ctx, NAME##__##NEXT);\
	}

#define _PURS_FFI_FUNC_CONT_1_TO_2(NAME)   _PURS_FFI_FUNC_CONT(NAME,  1,  2)
#define _PURS_FFI_FUNC_CONT_2_TO_3(NAME)   _PURS_FFI_FUNC_CONT(NAME,  2,  3)
#define _PURS_FFI_FUNC_CONT_3_TO_4(NAME)   _PURS_FFI_FUNC_CONT(NAME,  3,  4)
#define _PURS_FFI_FUNC_CONT_4_TO_5(NAME)   _PURS_FFI_FUNC_CONT(NAME,  4,  5)
#define _PURS_FFI_FUNC_CONT_5_TO_6(NAME)   _PURS_FFI_FUNC_CONT(NAME,  5,  6)
#define _PURS_FFI_FUNC_CONT_6_TO_7(NAME)   _PURS_FFI_FUNC_CONT(NAME,  6,  7)
#define _PURS_FFI_FUNC_CONT_7_TO_8(NAME)   _PURS_FFI_FUNC_CONT(NAME,  7,  8)
#define _PURS_FFI_FUNC_CONT_8_TO_9(NAME)   _PURS_FFI_FUNC_CONT(NAME,  8,  9)
#define _PURS_FFI_FUNC_CONT_9_TO_10(NAME)  _PURS_FFI_FUNC_CONT(NAME,  9, 10)
#define _PURS_FFI_FUNC_CONT_10_TO_11(NAME) _PURS_FFI_FUNC_CONT(NAME, 10, 11)
#define _PURS_FFI_FUNC_CONT_11_TO_12(NAME) _PURS_FFI_FUNC_CONT(NAME, 11, 12)

#define PURS_FFI_FUNC_1(NAME, A1, BODY)\
	const ANY * NAME##__1 (const void * super, const ANY * A1, va_list $__unused__) {\
		BODY;\
	}\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_2(NAME, A1, A2, BODY)\
	const ANY * NAME##__2 (const void * super, const ANY * A2, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3, BODY)\
	const ANY * NAME##__3 (const void * super, const ANY * A3, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_4(NAME, A1, A2, A3, A4, BODY)\
	const ANY * NAME##__4 (const void * super, const ANY * A4, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_5(NAME, A1, A2, A3, A4, A5, BODY)\
	const ANY * NAME##__5 (const void * super, const ANY * A5, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_6(NAME, A1, A2, A3, A4, A5, A6, BODY)\
	const ANY * NAME##__6 (const void * super, const ANY * A6, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY)\
	const ANY * NAME##__7 (const void * super, const ANY * A7, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8, BODY)\
	const ANY * NAME##__8 (const void * super, const ANY * A8, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		const ANY * A7 = ((const ANY **)super)[6];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\
	const ANY * NAME##__9 (const void * super, const ANY * A9, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		const ANY * A7 = ((const ANY **)super)[6];\
		const ANY * A8 = ((const ANY **)super)[7];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_8_TO_9(NAME);\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\
	const ANY * NAME##__10 (const void * super, const ANY * A10, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		const ANY * A7 = ((const ANY **)super)[6];\
		const ANY * A8 = ((const ANY **)super)[7];\
		const ANY * A9 = ((const ANY **)super)[8];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_9_TO_10(NAME);\
	_PURS_FFI_FUNC_CONT_8_TO_9(NAME);\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY)\
	const ANY * NAME##__11 (const void * super, const ANY * A11, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		const ANY * A7 = ((const ANY **)super)[6];\
		const ANY * A8 = ((const ANY **)super)[7];\
		const ANY * A9 = ((const ANY **)super)[8];\
		const ANY * A10 = ((const ANY **)super)[9];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_10_TO_11(NAME);\
	_PURS_FFI_FUNC_CONT_9_TO_10(NAME);\
	_PURS_FFI_FUNC_CONT_8_TO_9(NAME);\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY)\
	const ANY * NAME##__12 (const void * super, const ANY * A12, va_list $__unused__) {\
		const ANY * A1 = ((const ANY **)super)[0];\
		const ANY * A2 = ((const ANY **)super)[1];\
		const ANY * A3 = ((const ANY **)super)[2];\
		const ANY * A4 = ((const ANY **)super)[3];\
		const ANY * A5 = ((const ANY **)super)[4];\
		const ANY * A6 = ((const ANY **)super)[5];\
		const ANY * A7 = ((const ANY **)super)[6];\
		const ANY * A8 = ((const ANY **)super)[7];\
		const ANY * A9 = ((const ANY **)super)[8];\
		const ANY * A10 = ((const ANY **)super)[9];\
		const ANY * A11 = ((const ANY **)super)[10];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_11_TO_12(NAME);\
	_PURS_FFI_FUNC_CONT_10_TO_11(NAME);\
	_PURS_FFI_FUNC_CONT_9_TO_10(NAME);\
	_PURS_FFI_FUNC_CONT_8_TO_9(NAME);\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

// -----------------------------------------------------------------------------
// FFI: fixed-arity uncurried functions
// -----------------------------------------------------------------------------

#define _PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME, .ctx = NULL } }\
	};\
	const ANY * NAME ## $ = & NAME##__1_

#define PURS_FFI_FUNC_UNCURRIED_1(NAME, A1, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list $__unused__) {\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_2(NAME, A1, A2, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_3(NAME, A1, A2, A3, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_4(NAME, A1, A2, A3, A4, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_5(NAME, A1, A2, A3, A4, A5, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_6(NAME, A1, A2, A3, A4, A5, A6, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		const ANY * A8 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		const ANY * A8 = va_arg(vl, const ANY *);\
		const ANY * A9 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		const ANY * A8 = va_arg(vl, const ANY *);\
		const ANY * A9 = va_arg(vl, const ANY *);\
		const ANY * A10 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		const ANY * A8 = va_arg(vl, const ANY *);\
		const ANY * A9 = va_arg(vl, const ANY *);\
		const ANY * A10 = va_arg(vl, const ANY *);\
		const ANY * A11 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

#define PURS_FFI_FUNC_UNCURRIED_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY)\
	const ANY * NAME (const void * super, const ANY * A1, va_list vl) {\
		const ANY * A2 = va_arg(vl, const ANY *);\
		const ANY * A3 = va_arg(vl, const ANY *);\
		const ANY * A4 = va_arg(vl, const ANY *);\
		const ANY * A5 = va_arg(vl, const ANY *);\
		const ANY * A6 = va_arg(vl, const ANY *);\
		const ANY * A7 = va_arg(vl, const ANY *);\
		const ANY * A8 = va_arg(vl, const ANY *);\
		const ANY * A9 = va_arg(vl, const ANY *);\
		const ANY * A10 = va_arg(vl, const ANY *);\
		const ANY * A11 = va_arg(vl, const ANY *);\
		const ANY * A12 = va_arg(vl, const ANY *);\
		BODY;\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)

// -----------------------------------------------------------------------------
// Prim shims
// -----------------------------------------------------------------------------

#define Prim_undefined$ NULL

// -----------------------------------------------------------------------------
// Built-ins
// -----------------------------------------------------------------------------

const ANY * purs_any_true;
const ANY * purs_any_false;
const ANY * purs_any_NaN;
const ANY * purs_any_int_one;
const ANY * purs_any_num_one;
const ANY * purs_any_int_zero;
const ANY * purs_any_num_zero;

#define purs_any_bool(V) \
	(V == 1) \
		? purs_any_true \
		: purs_any_false
#define purs_any_not(V) \
	purs_any_is_true(V) \
		? purs_any_false \
		: purs_any_false
#define purs_any_is_true(V) purs_any_eq(V, purs_any_true)
#define purs_any_is_false(V) purs_any_eq(V, purs_any_false)
#define purs_any_while(COND) while(purs_any_is_true(COND))

/* check for NaN: https://stackoverflow.com/a/570694 */
#define purs_any_is_NaN(V) (purs_any_get_tag(V) == PURS_ANY_TAG_NUM && \
			    purs_any_get_num(V) != purs_any_get_num(V))

#define PURS_NAN NAN
#define PURS_INFINITY INFINITY

#endif // PURESCRIPT_RUNTIME_H
