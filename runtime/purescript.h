#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#define purs_malloc(SZ) GC_MALLOC(SZ)
#define purs_realloc(PTR, SZ) GC_REALLOC(PTR, SZ)
#define purs_new(EXP) GC_NEW(EXP)

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
#define managed_utf8str_t managed_t

typedef struct managed managed_t;
typedef struct purs_any purs_any_t;
typedef vec_t(purs_any_t) purs_vec_t;
typedef struct purs_record purs_record_t;
typedef struct purs_any_cont purs_any_cont_t;
typedef struct purs_any_thunk purs_any_thunk_t;
typedef struct purs_any_cons purs_any_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef ANY (purs_any_thunk_fun_t)(ANY ctx);
typedef ANY (purs_any_cont_fun_t)(ANY * ctx, ANY, va_list);
typedef struct purs_foreign purs_foreign_t;

struct managed { const void * data; };
void managed_default_release (managed_t * managed);
typedef void (*managed_release_func)(managed_t * managed);
const managed_t * managed_new(const void * data, managed_release_func release);

typedef enum {
	PURS_ANY_TAG_NULL = 0,
	PURS_ANY_TAG_INT = 1,
	PURS_ANY_TAG_NUM = 2,
	PURS_ANY_TAG_CONT = 3,
	PURS_ANY_TAG_THUNK = 4,
	PURS_ANY_TAG_CONS = 5,
	PURS_ANY_TAG_RECORD = 6,
	PURS_ANY_TAG_STRING = 7,
	PURS_ANY_TAG_CHAR = 8,
	PURS_ANY_TAG_ARRAY = 9,
	PURS_ANY_TAG_FOREIGN = 10,
} purs_any_tag_t;

struct purs_foreign {
	void * tag;
	void * data;
};

union purs_any_value {

	/* inline values */
	purs_any_int_t i;
	purs_any_num_t n;
	utf8_int32_t chr;
	purs_foreign_t foreign;

	/* self-referential, and other values */
	purs_any_cont_t * cont;
	purs_any_cons_t * cons;
	purs_any_thunk_t * thunk;
	const purs_record_t * record;
	const managed_t * str;
	const purs_vec_t * array;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

struct purs_any_thunk {
	purs_any_thunk_fun_t * fn;
	ANY ctx;
};

struct purs_any_cont {
	purs_any_cont_fun_t * fn;
	int len;
	ANY * ctx;
};

/* todo: track len values, for clean up */
struct purs_any_cons {
	int tag;
	ANY * values;
};

ANY purs_any_null;
#define purs_any_is_null(x) (x.tag == PURS_ANY_TAG_NULL)

ANY purs_any_app(ANY, ANY, ...);
ANY purs_any_unthunk (ANY);
const purs_any_tag_t purs_any_get_tag (ANY);
const char * purs_any_tag_str (const purs_any_tag_t);

/* note: two versions for compat/historical reasons */
#define purs_any_int PURS_ANY_INT
#define purs_any_num PURS_ANY_NUM
#define purs_any_char PURS_ANY_CHAR
#define purs_any_foreign PURS_ANY_FOREIGN
#define purs_any_array PURS_ANY_ARRAY
#define purs_any_record PURS_ANY_RECORD

ANY purs_any_cont(ANY * ctx, int len, purs_any_cont_fun_t *);
ANY purs_any_thunk(ANY ctx, purs_any_thunk_fun_t *);
ANY purs_any_cons(int tag, ANY* values);
ANY purs_any_string(const char * fmt, ...);

/* allocate a new string box with existing, *GC-allocated* data */
ANY purs_any_string_new_mv(const char *);

const purs_any_int_t     purs_any_get_int       (ANY);
const purs_any_num_t     purs_any_get_num       (ANY);
const utf8_int32_t       purs_any_get_char      (ANY);
purs_foreign_t           purs_any_get_foreign   (ANY);
purs_any_cont_t *        purs_any_get_cont      (ANY);
purs_any_cons_t *        purs_any_get_cons      (ANY);
const purs_record_t *    purs_any_get_record    (ANY);
const void *             purs_any_get_string    (ANY);
const purs_vec_t *       purs_any_get_array     (ANY);

// -----------------------------------------------------------------------------
// Any: built-in functions
// -----------------------------------------------------------------------------

int purs_any_eq_string (ANY, const void *);
int purs_any_eq_char   (ANY, utf8_int32_t);
int purs_any_eq_int    (ANY, purs_any_int_t);
int purs_any_eq_num    (ANY, double);

int purs_any_eq(ANY, ANY);
ANY purs_any_concat(ANY, ANY);

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

const void * purs_string_copy (const void *);

#define purs_string_size(STR) utf8size(STR)
#define purs_string_len(STR) utf8len(STR)

// -----------------------------------------------------------------------------
// arrays
// -----------------------------------------------------------------------------

void purs_vec_release (purs_vec_t *);
const purs_vec_t * purs_vec_new ();
const purs_vec_t * purs_vec_new_va (int count, ...);
const purs_vec_t * purs_vec_copy (const purs_vec_t *);
const purs_vec_t * purs_vec_slice (const purs_vec_t *, int begin);

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
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, ANY val);

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

typedef struct purs_record {
	const managed_utf8str_t * key;
	ANY value;
	UT_hash_handle hh;
} purs_record_t;

// TODO: rename to 'purs_any_record_empty'
ANY purs_record_empty;

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
 * Add a given set of key/value pairs to the given record (by mutation)
 */
purs_record_t * purs_record_add_multi_mut(purs_record_t *,
					  size_t count,
					  ...);

/**
 * Merge two records. The right record overwrites any labels in the left record.
 */
const purs_record_t * purs_record_merge(const purs_record_t *,
					const purs_record_t *);


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
 * Remove an entry by it's key (by mutation)
 */
purs_record_t * purs_record_remove_mut(purs_record_t * source,
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

struct tco_state {
	int done;
	purs_any_t * args;
};

#define purs_tco_state_new(N)\
	({\
		struct tco_state x;\
		x.done = 0;\
		x.args = purs_malloc(sizeof (ANY) * N);\
		x;\
	})
#define purs_tco_is_done(X) (X.done == 1)
#define purs_tco_set_done(X) (((struct tco_state *) X)->done = 1)
#define purs_tco_get_arg(X, I) (((struct tco_state *) X)->args[I])
#define purs_tco_set_arg(X, I, V) (X.args[I] = V)
#define purs_tco_mut_arg(X, I, V) (((struct tco_state *) X)->args[I] = V)

#define purs_foreign_get_data(X)\
	(X.data)

/* emit a scope struct */
#define PURS_SCOPE_T(NAME, DECLS)\
	typedef struct NAME {\
		struct DECLS;\
	} NAME

/* todo: remove this! */
#define purs_cons_get_tag(V) V->tag
#define purs_address_of(V) &V
#define purs_derefence(V) *V

/* thunked pointer dereference. useful for recursive bindings */
ANY * purs_indirect_value_new();
void purs_indirect_value_assign(ANY *, ANY);
ANY purs_indirect_thunk_new(ANY *);
ANY purs_thunked_deref(ANY);

/* allocate a buffer to fit 'N' 'ANY's */
#define purs_malloc_any_buf(N) purs_malloc(sizeof (ANY) * N)

/* code-gen helper to allocate and fill a scope. */
ANY* purs_malloc_many(int num_bindings);

/* declare a thunked top-level value. */
#define PURS_ANY_THUNK_DEF(NAME, INIT)\
	static ANY NAME ## __thunk_fn__ (ANY __unused__1) { \
		static ANY v;\
		static int x = 0;\
		if (x == 0) {\
			x = 1;\
			v = INIT;\
		}\
		return v;\
	};\
	purs_any_thunk_t NAME ## __thunk__ = {\
		.fn = NAME ## __thunk_fn__,\
		.ctx = { .tag = PURS_ANY_TAG_NULL }\
	};\
	ANY NAME = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = { .thunk = & NAME ## __thunk__ }\
	};

#define purs_any_int_neg(X) purs_any_int_new(-purs_any_get_int(X))

// -----------------------------------------------------------------------------
// Any: initializers
// -----------------------------------------------------------------------------

#define PURS_ANY_INT(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_INT, .value = { .i = X } })

#define PURS_ANY_NUM(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_NUM, .value = { .n = X } })

#define PURS_ANY_CHAR(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_CHAR, .value = { .chr = X } })

#define PURS_ANY_FOREIGN(TAG, DATA)\
	((purs_any_t){\
		.tag = PURS_ANY_TAG_FOREIGN,\
		.value = {\
			.foreign = {\
				.tag = (TAG),\
				.data = (DATA)\
			}\
		}\
	})

#define PURS_ANY_RECORD(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_RECORD, .value = { .record = X } })

#define PURS_ANY_ARRAY(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_ARRAY, .value = { .array = X } })

// -----------------------------------------------------------------------------
// FFI helpers
// -----------------------------------------------------------------------------

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_EXPORT(NAME)\
	ANY NAME ## _$

#define PURS_FFI_VALUE(NAME, INIT)\
	static const purs_any_t NAME ## _$ = INIT

// -----------------------------------------------------------------------------
// FFI: fixed-arity curried functions
// -----------------------------------------------------------------------------

#define _PURS_FFI_FUNC_ENTRY(NAME)\
	purs_any_cont_t NAME ## __cont__ = {\
		.fn = NAME ## __1,\
		.len = 0,\
		.ctx = NULL\
	};\
	ANY NAME = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	};\
	/* for code-gen use. todo: remove? */\
	ANY NAME ## _$ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	}

#define _PURS_FFI_FUNC_CONT(NAME, CUR, NEXT)\
	ANY NAME##__##CUR (ANY * $__super__, ANY a, va_list $__unused__) {\
		ANY* ctx = purs_malloc_many(CUR);\
		if ($__super__ != NULL) {\
			memcpy(ctx, $__super__, CUR * sizeof (ANY));\
		}\
		if (ctx != NULL) {\
			ctx[CUR - 1] = a;\
		}\
		return purs_any_cont(ctx, CUR, NAME##__##NEXT);\
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

#define PURS_FFI_FUNC_CONTEXT $__super__

#define PURS_FFI_FUNC_1(NAME, A1, BODY)\
	ANY NAME##__1 (ANY * $__super__, ANY A1, va_list $__unused__) {\
		BODY;\
	}\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_2(NAME, A1, A2, BODY)\
	ANY NAME##__2 (ANY * $__super__, ANY A2, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3, BODY)\
	ANY NAME##__3 (ANY * $__super__, ANY A3, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_4(NAME, A1, A2, A3, A4, BODY)\
	ANY NAME##__4 (ANY * $__super__, ANY A4, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_5(NAME, A1, A2, A3, A4, A5, BODY)\
	ANY NAME##__5 (ANY * $__super__, ANY A5, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_6(NAME, A1, A2, A3, A4, A5, A6, BODY)\
	ANY NAME##__6 (ANY * $__super__, ANY A6, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		BODY;\
	}\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME)

#define PURS_FFI_FUNC_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY)\
	ANY NAME##__7 (ANY * $__super__, ANY A7, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
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
	ANY NAME##__8 (ANY * $__super__, ANY A8, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
		ANY A7 = ((ANY*)$__super__)[6];\
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
	ANY NAME##__9 (ANY * $__super__, ANY A9, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
		ANY A7 = ((ANY*)$__super__)[6];\
		ANY A8 = ((ANY*)$__super__)[7];\
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
	ANY NAME##__10 (ANY * $__super__, ANY A10, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
		ANY A7 = ((ANY*)$__super__)[6];\
		ANY A8 = ((ANY*)$__super__)[7];\
		ANY A9 = ((ANY*)$__super__)[8];\
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
	ANY NAME##__11 (ANY * $__super__, ANY A11, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
		ANY A7 = ((ANY*)$__super__)[6];\
		ANY A8 = ((ANY*)$__super__)[7];\
		ANY A9 = ((ANY*)$__super__)[8];\
		ANY A10 = ((ANY*)$__super__)[9];\
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
	ANY NAME##__12 (ANY * $__super__, ANY A12, va_list $__unused__) {\
		ANY A1 = ((ANY*)$__super__)[0];\
		ANY A2 = ((ANY*)$__super__)[1];\
		ANY A3 = ((ANY*)$__super__)[2];\
		ANY A4 = ((ANY*)$__super__)[3];\
		ANY A5 = ((ANY*)$__super__)[4];\
		ANY A6 = ((ANY*)$__super__)[5];\
		ANY A7 = ((ANY*)$__super__)[6];\
		ANY A8 = ((ANY*)$__super__)[7];\
		ANY A9 = ((ANY*)$__super__)[8];\
		ANY A10 = ((ANY*)$__super__)[9];\
		ANY A11 = ((ANY*)$__super__)[10];\
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

/* // ----------------------------------------------------------------------------- */
/* // FFI: fixed-arity uncurried functions */
/* // ----------------------------------------------------------------------------- */

/* #define _PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)\ */
/* 	ANY NAME##__1_ = {\ */
/* 		.tag = PURS_ANY_TAG_CONT,\ */
/* 		.value = { .cont = { .fn = NAME, .ctx = purs_any_null } }\ */
/* 	};\ */
/* 	ANY NAME ## _$ = & NAME##__1_ */

/* #define PURS_FFI_FUNC_UNCURRIED_1(NAME, A1, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list $__unused__) {\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_2(NAME, A1, A2, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_3(NAME, A1, A2, A3, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_4(NAME, A1, A2, A3, A4, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_5(NAME, A1, A2, A3, A4, A5, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_6(NAME, A1, A2, A3, A4, A5, A6, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_7(NAME, A1, A2, A3, A4, A5, A6, A7, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		ANY A8 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		ANY A8 = va_arg(vl, ANY);\ */
/* 		ANY A9 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		ANY A8 = va_arg(vl, ANY);\ */
/* 		ANY A9 = va_arg(vl, ANY);\ */
/* 		ANY A10 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		ANY A8 = va_arg(vl, ANY);\ */
/* 		ANY A9 = va_arg(vl, ANY);\ */
/* 		ANY A10 = va_arg(vl, ANY);\ */
/* 		ANY A11 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

/* #define PURS_FFI_FUNC_UNCURRIED_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, BODY)\ */
/* 	ANY NAME (const void * $__super__, ANY A1, va_list vl) {\ */
/* 		ANY A2 = va_arg(vl, ANY);\ */
/* 		ANY A3 = va_arg(vl, ANY);\ */
/* 		ANY A4 = va_arg(vl, ANY);\ */
/* 		ANY A5 = va_arg(vl, ANY);\ */
/* 		ANY A6 = va_arg(vl, ANY);\ */
/* 		ANY A7 = va_arg(vl, ANY);\ */
/* 		ANY A8 = va_arg(vl, ANY);\ */
/* 		ANY A9 = va_arg(vl, ANY);\ */
/* 		ANY A10 = va_arg(vl, ANY);\ */
/* 		ANY A11 = va_arg(vl, ANY);\ */
/* 		ANY A12 = va_arg(vl, ANY);\ */
/* 		BODY;\ */
/* 	}\ */
/* 	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME) */

// -----------------------------------------------------------------------------
// Prim shims
// note: See codegen notes about '_$' suffix
// -----------------------------------------------------------------------------

#define Prim_undefined_$ purs_any_null

// -----------------------------------------------------------------------------
// Built-ins
// -----------------------------------------------------------------------------

ANY purs_any_true;
ANY purs_any_false;
ANY purs_any_NaN;
ANY purs_any_int_one;
ANY purs_any_num_one;
ANY purs_any_int_zero;
ANY purs_any_num_zero;

#define purs_any_bool(V) \
	(V == 1) \
		? purs_any_true \
		: purs_any_false

#define purs_any_not(V) \
	purs_any_is_true(V) \
		? purs_any_false \
		: purs_any_true

/* todo: inline definition */
#define purs_any_is_true(V) purs_any_eq(V, purs_any_true)

/* todo: inline definition */
#define purs_any_is_false(V) purs_any_eq(V, purs_any_false)

/* check for NaN: https://stackoverflow.com/a/570694 */
#define purs_any_is_NaN(V) (purs_any_get_tag(V) == PURS_ANY_TAG_NUM && \
			    purs_any_get_num(V) != purs_any_get_num(V))

#define PURS_NAN NAN
#define PURS_INFINITY INFINITY

#endif // PURESCRIPT_RUNTIME_H
