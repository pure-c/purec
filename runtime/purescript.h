#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#ifdef WITH_GC
#include "deps/bwdgc/include/gc.h"
#define purs_malloc(SZ) GC_MALLOC(SZ)
#define purs_realloc(PTR, SZ) GC_REALLOC(PTR, SZ)
#define purs_new(EXP) GC_NEW(sizeof (EXP))
#define purs_free(X)
#else
#ifdef UNIT_TESTING
extern void mock_assert(const int result, const char *const expression, const char *const file, const int line);
#undef assert
#define assert(A) mock_assert((A), #A, __FILE__, __LINE__)
#define purs_assert(A, FMT, ...)\
	do {\
		if (!(A)) {\
			/*char buf[1024];*/\
			/*sprintf(buf, FMT, ##__VA_ARGS__);*/\
			mock_assert((A), #A, __FILE__, __LINE__);\
		}\
	} while (0)
extern void* _test_malloc(const size_t size, const char* file, const int line);
extern void* _test_calloc(const size_t number_of_elements, const size_t size, const char* file, const int line);
extern void _test_free(void* const ptr, const char* file, const int line);
#define purs_malloc(SZ) _test_malloc(SZ, __FILE__, __LINE__)
#define purs_realloc(PTR, SZ) _test_malloc(PTR, SZ, __FILE__, __LINE__)
#define purs_free(PTR) _test_free(PTR, __FILE__, __LINE__)
#define purs_new(EXP) purs_malloc(sizeof (EXP))
#else // UNIT_TESTING
#define purs_malloc(SZ) malloc(SZ)
#define purs_realloc(PTR, SZ) realloc(PTR, SZ)
#define purs_new(EXP) purs_malloc(sizeof (EXP))
#define purs_free(X) free(X)
#define purs_assert(A, FMT, ...)\
	do {\
		if (!(A)) {\
			purs_log_error(FMT, ##__VA_ARGS__);\
			assert(A);\
		}\
	} while (0)
#endif
#endif

#include "ccan/asprintf/asprintf.h"
#include "vendor/uthash.h"
#include "vendor/utf8.h"
#include "vendor/vec.h"

#define purs_log_error(FMT, ...)\
	do {\
		fprintf(stderr,\
			"[ERROR] (%s:%d) " # FMT "\n",\
			__FILE__,\
			__LINE__,\
			##__VA_ARGS__);\
	} while (0)

#ifdef ANY
#error macro 'ANY' already defined
#endif

#ifdef APP
#error macro 'APP' already defined
#endif

#define ANY purs_any_t
#define APP purs_any_app

#define purs_any_int_t int32_t
#define purs_any_num_t double

typedef struct purs_vec purs_vec_t;
typedef struct purs_any purs_any_t;
typedef struct purs_record purs_record_t;
typedef struct purs_cont purs_cont_t;
typedef struct purs_any_thunk purs_any_thunk_t;
typedef struct purs_any_cons purs_any_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef struct purs_scope purs_scope_t;
typedef ANY (purs_any_thunk_fun_t)(ANY ctx);
typedef ANY (purs_cont_fun_t)(const struct purs_scope *, ANY, va_list);
typedef struct purs_foreign purs_foreign_t;
typedef struct purs_str purs_str_t;

/* tag numbers are strictly sequential (for lookups, etc.)! */
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
#define PURS_ANY_TAGS_TOT 10 /* Keep this in Sync! */

struct purs_foreign {
	void * tag;
	void * data;
};

/* a reference-counted structure */
struct purs_rc {
	void (*free_fn)(const struct purs_rc *);
	int count;
};

#define container_of(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))

static inline void purs_rc_retain(const struct purs_rc *ref) {
	((struct purs_rc *)ref)->count++;
}

static inline void purs_rc_release(const struct purs_rc *ref) {
	if (--((struct purs_rc *)ref)->count == 0) {
		ref->free_fn(ref);
	}
}

/* by convetion, the rc is embedded as 'rc', making these macros possible */
#define PURS_RC_RELEASE(X) purs_rc_release(&(X)->rc)
#define PURS_RC_RETAIN(X)  purs_rc_retain(&(X)->rc)

union purs_any_value {
	/* inline values */
	purs_any_int_t i;
	purs_any_num_t n;
	utf8_int32_t chr;
	purs_foreign_t foreign;

	/* self-referential, and other values */
	const purs_cont_t * cont;
	purs_any_cons_t * cons;
	purs_any_thunk_t * thunk;
	const purs_record_t * record;
	const purs_str_t * str;
	const purs_vec_t * array;
};

struct purs_any {
	purs_any_tag_t tag;
	purs_any_value_t value;
};

struct purs_any_thunk {
	purs_any_thunk_fun_t * fn;
	ANY ctx;
	struct purs_rc rc;
};

struct purs_cont {
	purs_cont_fun_t * fn;
	const struct purs_scope * scope; /* todo: inline? */
	struct purs_rc rc;
};

struct purs_any_cons {
	int tag;
	int size;
	ANY * values;
	struct purs_rc rc;
};

struct purs_str {
	char * data;
	struct purs_rc rc;
};

/* a reference-counted vec_t(...) */
struct purs_vec {
	ANY * data;
	int length;
	int capacity;
	struct purs_rc rc;
};

ANY purs_any_null;
#define purs_any_is_null(x) (x.tag == PURS_ANY_TAG_NULL)

ANY purs_any_app(ANY, ANY, ...);
ANY purs_any_unthunk (ANY);
const purs_any_tag_t purs_any_get_tag (ANY);
const char * purs_any_tag_str (const purs_any_tag_t);

/* XXX these functions heap-allocate. maybe rename? */
ANY purs_any_cons(int tag, int size, ANY* values);

#define __PURS_ANY_GETTER(N, A, R, TAG)\
	static inline R _purs_any_get_ ## N (ANY v, char * file, int line) {\
		v = purs_any_unthunk(v);\
		purs_assert(v.tag == TAG,\
			    "expected tag: %s, but got: %s. at %s:%d",\
			    purs_any_tag_str(TAG),\
			    purs_any_tag_str(v.tag),\
			    file,\
			    line);\
		return v.value.A;\
	}

__PURS_ANY_GETTER(int, i, purs_any_int_t, PURS_ANY_TAG_INT)
__PURS_ANY_GETTER(num, n, purs_any_num_t, PURS_ANY_TAG_NUM)
__PURS_ANY_GETTER(char, chr, utf8_int32_t, PURS_ANY_TAG_CHAR)
__PURS_ANY_GETTER(foreign, foreign, purs_foreign_t, PURS_ANY_TAG_FOREIGN)
__PURS_ANY_GETTER(cont, cont, const purs_cont_t *, PURS_ANY_TAG_CONT)
__PURS_ANY_GETTER(cons, cons, const purs_any_cons_t *, PURS_ANY_TAG_CONS)
__PURS_ANY_GETTER(thunk, thunk, const purs_any_thunk_t *, PURS_ANY_TAG_THUNK)
__PURS_ANY_GETTER(record, record, const purs_record_t *, PURS_ANY_TAG_RECORD)
__PURS_ANY_GETTER(string, str, const purs_str_t *, PURS_ANY_TAG_STRING)
__PURS_ANY_GETTER(array, array, const purs_vec_t *, PURS_ANY_TAG_ARRAY)

/* todo: generate faster, unsafe variants */
#define purs_any_get_int(A) _purs_any_get_int((A), __FILE__, __LINE__)
#define purs_any_get_num(A) _purs_any_get_num((A), __FILE__, __LINE__)
#define purs_any_get_char(A) _purs_any_get_char((A), __FILE__, __LINE__)
#define purs_any_get_foreign(A) _purs_any_get_foreign((A), __FILE__, __LINE__)
#define purs_any_get_cont(A) _purs_any_get_cont((A), __FILE__, __LINE__)
#define purs_any_get_cons(A) _purs_any_get_cons((A), __FILE__, __LINE__)
#define purs_any_get_thunk(A) _purs_any_get_thunk((A), __FILE__, __LINE__)
#define purs_any_get_record(A) _purs_any_get_record((A), __FILE__, __LINE__)
#define purs_any_get_string(A) _purs_any_get_string((A), __FILE__, __LINE__)
#define purs_any_get_array(A) _purs_any_get_array((A), __FILE__, __LINE__)

// -----------------------------------------------------------------------------
// Any: built-in functions
// -----------------------------------------------------------------------------

static inline int purs_any_eq_char (ANY x, utf8_int32_t y) {
	return purs_any_get_char(x) == y;
}

static inline int purs_any_eq_string (ANY x, const void * str) {
	return utf8cmp(purs_any_get_string(x)->data, str) == 0;
}

static inline int purs_any_eq_int (ANY x, purs_any_int_t y) {
	return purs_any_get_int(x) == y;
}

static inline int purs_any_eq_num (ANY x, double y) {
	return purs_any_get_num(x) == y;
}

int purs_any_eq(ANY, ANY);
ANY purs_any_concat(ANY, ANY);

// -----------------------------------------------------------------------------
// continuations
// -----------------------------------------------------------------------------

const purs_cont_t * purs_cont_new(const struct purs_scope *, purs_cont_fun_t *);

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

const purs_str_t * purs_str_new(const char * fmt, ...);
const void * purs_string_copy (const void *);

#define purs_string_size(STR) utf8size(STR)
#define purs_string_len(STR) utf8len(STR)

// -----------------------------------------------------------------------------
// arrays
// -----------------------------------------------------------------------------

const purs_vec_t * purs_vec_new_va (int count, ...);
const purs_vec_t * purs_vec_copy (const purs_vec_t *);
const purs_vec_t * purs_vec_splice (const purs_vec_t *, int start, int count);
const purs_vec_t * purs_vec_concat(const purs_vec_t * lhs, const purs_vec_t * rhs);

#define purs_vec_foreach(v, var, iter) vec_foreach(v, var, iter)
#define purs_vec_reserve(v, n) vec_reserve(v, n)
#define purs_vec_push_mut(v, x) vec_push(v, x)
#define purs_vec_pusharr_mut(v, arr, count) vec_pusharr(v, arr, count)

/**
 * Insert the value val at index idx shifting the elements after the index to
 * make room for the new value.
 */
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, ANY val);

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

typedef struct purs_node_record {
	const void * key;
	ANY value;
	UT_hash_handle hh;
} purs_record_node_t;

typedef struct purs_record {
	const purs_record_node_t * root;
	struct purs_rc rc;
} purs_record_t;

// TODO: rename to 'purs_any_record_empty'
ANY purs_record_empty;

const purs_record_t * purs_record_new_va(int count, ...);

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
ANY * purs_record_find_by_key(const purs_record_t *,
			      const void * key);

// -----------------------------------------------------------------------------
// Code-gen helpers
// -----------------------------------------------------------------------------

#define purs_address_of(V) &V
#define purs_derefence(V) *V

/* Tail-call optimization generation */
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
#define purs_foreign_get_data(X) (X.data)

/* Captured scope generation */
struct purs_scope {
	int size;
	ANY* bindings;
	struct purs_rc rc;
};

#define purs_scope_binding_at(S, N) ((S)->bindings[(N)])
#define purs_scope_capture_at(S, N, B) { (S)->bindings[(N)] = (B); }

struct purs_scope * purs_scope_new(int size, ...);
struct purs_scope * purs_scope_new1(int size);

/* todo: remove this! */
#define purs_cons_get_tag(V) V->tag

/* Thunked pointer dereference: Recursive bindings support */
#define purs_indirect_value_new() purs_new(ANY)
#define purs_indirect_value_assign(I, V) *(I) = (V)
#define purs_indirect_thunk_new(X) \
	({\
		purs_any_thunk_t * thunk = purs_malloc(sizeof (purs_any_thunk_t));\
		thunk->ctx = ((purs_any_t){ .value = { .foreign = { .data = (X) } } }); \
		thunk->fn = purs_thunked_deref;\
		PURS_ANY_THUNK(thunk);\
	})
ANY purs_thunked_deref(ANY);

/* allocate a buffer to fit 'N' 'ANY's */
#define purs_malloc_any_buf(N) purs_malloc(sizeof (ANY) * N)

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

#define PURS_ANY_RETAIN(X) {\
		switch ((X)->tag) {\
		case PURS_ANY_TAG_STRING:\
			purs_rc_retain(&((X)->value.str->rc));\
			break;\
		default:\
			break;\
		}\
	}

#define PURS_ANY_RELEASE(X) {\
		switch ((X)->tag) {\
		case PURS_ANY_TAG_NULL:\
		case PURS_ANY_TAG_INT:\
		case PURS_ANY_TAG_NUM:\
		case PURS_ANY_TAG_CHAR:\
			break;\
		case PURS_ANY_TAG_THUNK:\
		case PURS_ANY_TAG_CONS:\
		case PURS_ANY_TAG_FOREIGN:\
			assert(0);\
			break;\
		case PURS_ANY_TAG_ARRAY:\
			purs_rc_release(&((X)->value.array->rc));\
			break;\
		case PURS_ANY_TAG_RECORD:\
			purs_rc_release(&((X)->value.record->rc));\
			break;\
		case PURS_ANY_TAG_CONT:\
			purs_rc_release(&((X)->value.cont->rc));\
			break;\
		case PURS_ANY_TAG_STRING:\
			purs_rc_release(&((X)->value.str->rc));\
			break;\
		}\
	}

#define PURS_ANY_INT(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_INT, .value = { .i = (X) } })

#define PURS_ANY_NUM(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_NUM, .value = { .n = (X) } })

#define PURS_ANY_CHAR(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_CHAR, .value = { .chr = (X) } })

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

#define PURS_ANY_STRING(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_STRING, .value = { .str = (X) } })

#define PURS_ANY_RECORD(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_RECORD, .value = { .record = (X) } })

#define PURS_ANY_ARRAY(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_ARRAY, .value = { .array = (X) } })

#define PURS_ANY_THUNK(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_THUNK, .value = { .thunk = (X) } })

#define PURS_ANY_CONT(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_CONT, .value = { .cont = (X) } })

/* DEPRECATED: two versions for compat/historical reasons only */
#define purs_any_int PURS_ANY_INT
#define purs_any_num PURS_ANY_NUM
#define purs_any_char PURS_ANY_CHAR
#define purs_any_foreign PURS_ANY_FOREIGN
#define purs_any_array PURS_ANY_ARRAY
#define purs_any_record PURS_ANY_RECORD
#define purs_any_cont PURS_ANY_CONT
#define purs_any_string PURS_ANY_STRING

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
	purs_cont_t NAME ## __cont__ = {\
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
		ANY* ctx = purs_malloc_any_buf(CUR);\
		if ($__super__ != NULL) {\
			memcpy(ctx, $__super__, CUR * sizeof (ANY));\
		}\
		if (ctx != NULL) {\
			ctx[CUR - 1] = a;\
		}\
		return purs_cont(ctx, CUR, NAME##__##NEXT);\
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
