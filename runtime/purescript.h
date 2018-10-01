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
typedef struct purs_any_cont purs_any_cont_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef const ANY * (purs_any_thunk_t)();
typedef const ANY * (purs_any_fun_t)(const void * ctx, const ANY *);
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

struct purs_foreign {
	void * tag;
	void * data;
};

struct purs_cons {
	int tag;
	const purs_any_t ** values;
};

union purs_any_value {
	purs_any_int_t i;
	purs_any_num_t n;
	purs_any_cont_t cont;
	purs_any_thunk_t * thunk;
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

const ANY * purs_any_app(const ANY * f, const ANY * v);
const ANY * purs_any_unthunk (const ANY *);
const char * purs_any_tag_str (const purs_any_tag_t);

const ANY * purs_any_int_new(const purs_any_int_t);
const ANY * purs_any_num_new(const purs_any_num_t);
const ANY * purs_any_cont_new(const void * ctx, purs_any_fun_t *);
const ANY * purs_any_thunk_new(purs_any_thunk_t *);
const ANY * purs_any_cons_new(int tag, const purs_any_t ** values);
const ANY * purs_any_record_new(const purs_record_t *);
const ANY * purs_any_string_new(const char *);
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
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, const purs_any_t * val);

// -----------------------------------------------------------------------------
// records
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
// Code-gen helpers
// -----------------------------------------------------------------------------

/* code-gen helper to allocate and fill a scope.
 * assumes scope to consist only of (const ANY *) pointers, the count of which
 * is known.
 */
const ANY ** _purs_scope_new(int num_bindings, const ANY * binding, ...);

/* declare a thunked top-level value.
 */
#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	static const ANY * NAME ## __thunk_fn__ (const void * __unused__1, const ANY * __unused__2) { \
		static const purs_any_t * NAME ## __thunk_val__ = NULL;\
		if (NAME ## __thunk_val__ == NULL) {\
			NAME ## __thunk_val__ = INIT;\
		}\
		return NAME ## __thunk_val__;\
	}\
	static const purs_any_t NAME ## __thunk__ = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = {\
			.cont = {\
				.fn = NAME ## __thunk_fn__,\
				.ctx = NULL\
			}\
		}\
	};\
	const purs_any_t * NAME = & NAME ## __thunk__;\

/* allocate a cons 'value' field large enough to fit 'n' amount of 'ANY *'
 */
#define PURS_CONS_VALUES_NEW(n)\
	purs_malloc(sizeof (const ANY *) * n)

// -----------------------------------------------------------------------------
// FFI helpers
// -----------------------------------------------------------------------------

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_EXPORT(NAME)\
	const purs_any_t * NAME ## $

#define PURS_SCOPE_T(NAME, DECLS)\
	typedef struct NAME {\
		struct DECLS;\
	} NAME

#define PURS_FFI_FUNC_0(NAME, BODY)\
	const ANY * NAME (void * super) BODY

#define PURS_FFI_FUNC_1(NAME, A1, BODY)\
	const ANY * NAME (const void * super, const ANY * A1) BODY

#define PURS_FFI_FUNC_2(NAME, A1, A2, BODY)\
	PURS_SCOPE_T(NAME##__ctx__1, { const ANY * A1; });\
	PURS_SCOPE_T(NAME##__ctx__2, { const ANY * A1; const ANY * A2; }); \
	const ANY * NAME##__2 (const void * super, const ANY * A2) {\
		NAME##__ctx__2 * ctx = purs_new(NAME##__ctx__2);\
		ctx->A1 = ((const NAME##__ctx__1*)super)->A1;\
		ctx->A2 = A2;\
		const ANY * A1 = ctx->A1;\
		BODY;\
	}\
	const ANY * NAME##__1 (const void * super, const ANY * A1) {\
		NAME##__ctx__1 * ctx = purs_new(NAME##__ctx__1);\
		ctx->A1 = A1;\
		return purs_any_cont_new(ctx, NAME##__2);\
	}\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME##__1, .ctx = NULL } }\
	};\
	const ANY * NAME = & NAME##__1_

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3, BODY)\
	PURS_SCOPE_T(NAME##__ctx__1, { const ANY * A1; });\
	PURS_SCOPE_T(NAME##__ctx__2, { const ANY * A1; const ANY * A2; }); \
	PURS_SCOPE_T(NAME##__ctx__3, { const ANY * A1; const ANY * A2; const ANY * A3; }); \
	const ANY * NAME##__3 (const void * super, const ANY * A3) {\
		NAME##__ctx__3 * ctx = purs_new(NAME##__ctx__3);\
		ctx->A1 = ((const NAME##__ctx__2*)super)->A1;\
		ctx->A2 = ((const NAME##__ctx__2*)super)->A2;\
		ctx->A3 = A3;\
		const ANY * A1 = ctx->A1;\
		const ANY * A2 = ctx->A2;\
		BODY;\
	}\
	const ANY * NAME##__2 (const void * super, const ANY * A2) {\
		NAME##__ctx__2 * ctx = purs_new(NAME##__ctx__2);\
		ctx->A1 = ((const NAME##__ctx__1*)super)->A1;\
		ctx->A2 = A2;\
		return purs_any_cont_new(ctx, NAME##__3);\
	}\
	const ANY * NAME##__1 (const void * super, const ANY * A1) {\
		NAME##__ctx__1 * ctx = purs_new(NAME##__ctx__1);\
		ctx->A1 = A1;\
		return purs_any_cont_new(ctx, NAME##__2);\
	}\
	const ANY NAME##__1_ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = { .fn = NAME##__1, .ctx = NULL } }\
	};\
	const ANY * NAME = & NAME##__1_

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
