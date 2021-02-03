/** @file */
#ifndef PURESCRIPT_RUNTIME_H
#define PURESCRIPT_RUNTIME_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

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

#include "ccan/asprintf/asprintf.h"
#include "vendor/uthash.h"
#include "vendor/utf8.h"
#include "vendor/vec.h"

#define purs_trace_any(A)\
	printf("%s:%d: TAG=%s\n", __FILE__, __LINE__, purs_any_tag_str((A).tag))

#define purs_log_error(FMT, ...)\
	do {\
		fprintf(stderr,\
			"[ERROR] (%s:%d) " # FMT "\n",\
			__FILE__,\
			__LINE__,\
			##__VA_ARGS__);\
	} while (0)

#define purs_int_t int32_t
#define purs_num_t double

typedef utf8_int32_t purs_char_t;
typedef struct purs_vec purs_vec_t;
typedef struct purs_any purs_any_t;
typedef struct purs_record purs_record_t;
typedef struct purs_cont purs_cont_t;
typedef struct purs_thunk purs_thunk_t;
typedef struct purs_cons purs_cons_t;
typedef union purs_any_value purs_any_value_t;
typedef struct purs_scope purs_scope_t;
typedef purs_any_t (purs_thunk_fun_t)(void * ctx);
typedef purs_any_t (purs_cont_fun_t)(const struct purs_scope *, purs_any_t, va_list);
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

/** @private
    @internal Keep this in sync with tag enum! */
#define PURS_ANY_TAGS_TOT 10

/// @brief a reference-counted structure
struct purs_rc {
	void (*free_fn)(const struct purs_rc *);
	int count;
};

#define container_of(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))

/// @private
static inline void purs_rc_retain(const struct purs_rc *ref) {
	if (((struct purs_rc *)ref)->count != -1 /* stack */) {
		((struct purs_rc *)ref)->count++;
	}
}

/// @private
static inline void purs_rc_release(const struct purs_rc *ref) {
	if (((struct purs_rc *)ref)->count != -1 /* stack */) {
		if (--((struct purs_rc *)ref)->count == 0) {
			ref->free_fn(ref);
		}
	}
}

/* by convetion, the rc is embedded as 'rc', making these macros possible */
#define PURS_RC_RELEASE(X) do { if ((X) != NULL) purs_rc_release(&((X)->rc)); } while (0)
#define PURS_RC_RETAIN(X) do  { if ((X) != NULL)  purs_rc_retain(&((X)->rc)); } while (0)

/* all "base"-compatible structures must have their "rc" field in the same
   position as "_purs_rc_base." */
struct _purs_rc_base { struct purs_rc rc; };
#define PURS_RC_BASE_FIELDS struct purs_rc rc;
#define PURS_RC_BASE_RELEASE(X) PURS_RC_RELEASE((struct _purs_rc_base *) X)
#define PURS_RC_BASE_RETAIN(X) PURS_RC_RETAIN((struct _purs_rc_base *) X)

/// Union of all possible types of values 'purs_any' can assume.
union purs_any_value {
	/* inline values */
	purs_int_t i;
	purs_num_t n;
	purs_char_t chr;

	/* self-referential, and other values */
	const purs_foreign_t * foreign;
	const purs_cont_t * cont;
	const purs_cons_t * cons;
	const purs_thunk_t * thunk;
	const purs_record_t * record;
	const purs_str_t * str;
	const purs_vec_t * array;
};

/// A dynamic value.
struct purs_any {
	/// tag identifying the type of value
	purs_any_tag_t tag;
	purs_any_value_t value;
};

/// A thunk; A 0-ary function that evaluates to a value.
struct purs_thunk {
	PURS_RC_BASE_FIELDS
	purs_thunk_fun_t * fn;
	void * ctx;
};

/// A continuation.
struct purs_cont {
	PURS_RC_BASE_FIELDS
	purs_cont_fun_t * fn;
	const struct purs_scope * scope; /* todo: inline? */
};

/// A PureScript ADT constructor.
struct purs_cons {
	PURS_RC_BASE_FIELDS
	int tag;
	int size;
	purs_any_t * values;
};

/// A PureScript String
struct purs_str {
	PURS_RC_BASE_FIELDS
	char * data;
};

typedef void (*purs_foreign_finalizer)(void* tag, void* data);

/// A PureScript foreign value
struct purs_foreign {
	PURS_RC_BASE_FIELDS
	/// user-defined, arbitrary tag identifying the type of foreign value.
	void *tag;
	/// user-defined, arbitrary data.
	void *data;
	/// callback to finalize the foreign value when collected.
	/// TODO: tie in with tracing GC?
	purs_foreign_finalizer finalize_cb;
};

/// A PureScript array
struct purs_vec {
	PURS_RC_BASE_FIELDS
	purs_any_t * data;
	int length;
	int capacity;
};

typedef struct purs_node_record {
	const void * key;
	purs_any_t value;
	UT_hash_handle hh;
} purs_record_node_t;

/// A PureScript record
typedef struct purs_record {
	struct purs_rc rc;
	const purs_record_node_t * root;
} purs_record_t;

/// The empty PureScript record
purs_any_t purs_any_record_empty;

/** @brief assert type equality between expected and actual tags. */
#define purs_any_assert_tag_eq(EXPECTED, ACTUAL)\
	purs_assert((EXPECTED) == (ACTUAL),\
		    "expected tag: %s, but got: %s",\
		    purs_any_tag_str((EXPECTED)),\
		    purs_any_tag_str((ACTUAL)))

/// Sentinel value acting as 'NULL'
purs_any_t purs_any_null;

/// Test if dynamic value is 'NULL'
#define purs_any_is_null(x) (x.tag == PURS_ANY_TAG_NULL)

/// Render a dynamic value's tag as a string; For debugging and error reporting.
const char * purs_any_tag_str (const purs_any_tag_t);

/// @private Used by code-gen only.
static inline void purs_debug(purs_any_t v, char** out) {
	asprintf(out, "tag=%s", purs_any_tag_str(v.tag));
}

/** @brief Retain the given dynamic value, if it makes sense. Primitive values such
    as integers, numbers, and chars are passed by value and need not be
    retained. */
#define PURS_ANY_RETAIN(V) {\
		switch ((V).tag) {\
		case PURS_ANY_TAG_STRING:\
			PURS_RC_RETAIN(((V).value.str));\
			break;\
		case PURS_ANY_TAG_ARRAY:\
			PURS_RC_RETAIN((V).value.array);\
			break;\
		case PURS_ANY_TAG_RECORD:\
			PURS_RC_RETAIN((V).value.record);\
			break;\
		case PURS_ANY_TAG_CONT:\
			PURS_RC_RETAIN((V).value.cont);\
			break;\
		case PURS_ANY_TAG_THUNK:\
			PURS_RC_RETAIN((V).value.thunk);\
			break;\
		case PURS_ANY_TAG_CONS:\
			PURS_RC_RETAIN((V).value.cons);\
			break;\
		case PURS_ANY_TAG_FOREIGN:\
			PURS_RC_RETAIN((V).value.foreign);\
			break;\
		default:\
			break;\
		}\
	}

/** @brief Release the given dynamic value, if it makes sense. Primitive values
    such as integers, numbers, and chars are passed by value and need not be
    retained or released. */
#define PURS_ANY_RELEASE(V) {\
		switch ((V).tag) {\
		case PURS_ANY_TAG_FOREIGN:\
			PURS_RC_RELEASE((V).value.foreign);\
			break;\
		case PURS_ANY_TAG_CONS:\
			PURS_RC_RELEASE((V).value.cons);\
			break;\
		case PURS_ANY_TAG_THUNK:\
			PURS_RC_RELEASE((V).value.thunk);\
			break;\
		case PURS_ANY_TAG_STRING:\
			PURS_RC_RELEASE((V).value.str);\
			break;\
		case PURS_ANY_TAG_ARRAY:\
			PURS_RC_RELEASE((V).value.array);\
			break;\
		case PURS_ANY_TAG_RECORD:\
			PURS_RC_RELEASE((V).value.record);\
			break;\
		case PURS_ANY_TAG_CONT:\
			PURS_RC_RELEASE((V).value.cont);\
			break;\
		default:\
			break;\
		}\
	}

/**
   @brief Evaluate the given thunk. If passed in value is not a thunk, returns
          value.
   @param[in] x The value to unthunk.
   @param[in, out] has_changed Set to 1 if value was a thunk and forced, else
                               set to 0. If it is set to 1, this implies the
                               the returned value has been retained and the user
                               must release it. */
static inline purs_any_t purs_any_unthunk(purs_any_t x, int *has_changed) {
	purs_any_t out = x;
	if (has_changed != NULL) {
		*has_changed = 0;
	}
	purs_any_t last = purs_any_null;
	while (out.tag == PURS_ANY_TAG_THUNK) {
		out = out.value.thunk->fn(out.value.thunk->ctx);
		PURS_ANY_RELEASE(last);
		last = out;
		if (has_changed != NULL) {
			*has_changed = 1;
		}
	}
	return out;
}

/**
   @brief Apply the given function to the given arguments.
   @param[in] _f The function value to apply. Must be a continuation or a thunk
                 evaluating to a continuation.
   @param[in] v The argument(s) to supply the function with.
  */
static inline purs_any_t purs_any_app(purs_any_t _f, purs_any_t v, ...) {

	/* unthunk, if necessary */
	int has_changed;
	purs_any_t f = purs_any_unthunk(_f, &has_changed);
	purs_any_assert_tag_eq(PURS_ANY_TAG_CONT, f.tag);

	/* apply the function */
	va_list args;
	va_start(args, v);
	purs_any_t r = f.value.cont->fn(f.value.cont->scope, v, args);
	va_end(args);

	/* release the intermediate result */
	if (has_changed) {
		PURS_ANY_RELEASE(f);
	}

	return r;
}

/* todo: remove this! */
static inline const purs_any_tag_t purs_any_get_tag (purs_any_t v) {
	return v.tag;
}

/// @private
#define __PURS_ANY_GET(N, A, R, TAG)\
	static inline R _purs_any_get_ ## N (purs_any_t v, char * file, int line) {\
		purs_any_assert_tag_eq(TAG, v.tag);\
		return v.value.A;\
	}

__PURS_ANY_GET(int, i, purs_int_t, PURS_ANY_TAG_INT)
__PURS_ANY_GET(num, n, purs_num_t, PURS_ANY_TAG_NUM)
__PURS_ANY_GET(char, chr, purs_char_t, PURS_ANY_TAG_CHAR)
__PURS_ANY_GET(foreign, foreign, const purs_foreign_t *, PURS_ANY_TAG_FOREIGN)
__PURS_ANY_GET(cont, cont, const purs_cont_t *, PURS_ANY_TAG_CONT)
__PURS_ANY_GET(cons, cons, const purs_cons_t *, PURS_ANY_TAG_CONS)
__PURS_ANY_GET(thunk, thunk, const purs_thunk_t *, PURS_ANY_TAG_THUNK)
__PURS_ANY_GET(record, record, const purs_record_t *, PURS_ANY_TAG_RECORD)
__PURS_ANY_GET(string, str, const purs_str_t *, PURS_ANY_TAG_STRING)
__PURS_ANY_GET(array, array, const purs_vec_t *, PURS_ANY_TAG_ARRAY)

/* todo: generate faster, unsafe variants */

/** @brief Retrieve underlying integer or crash. */
#define purs_any_get_int(A) _purs_any_get_int((A), __FILE__, __LINE__)

/** @brief Retrieve underlying number or crash. */
#define purs_any_get_num(A) _purs_any_get_num((A), __FILE__, __LINE__)

/** @brief Retrieve underlying char or crash. */
#define purs_any_get_char(A) _purs_any_get_char((A), __FILE__, __LINE__)

/** @brief Retrieve underlying foreign value or crash.
    The returned value is **not retained.** */
#define purs_any_get_foreign(A) _purs_any_get_foreign((A), __FILE__, __LINE__)

/** @brief Retrieve underlying continuation or crash.
    The returned value is **not retained.** */
#define purs_any_get_cont(A) _purs_any_get_cont((A), __FILE__, __LINE__)

/** @brief Retrieve underlying data constructor or crash.
    The returned value is **not retained.** */
#define purs_any_get_cons(A) _purs_any_get_cons((A), __FILE__, __LINE__)

/** @brief Retrieve underlying thunk or crash.
    The returned value is **not retained.** */
#define purs_any_get_thunk(A) _purs_any_get_thunk((A), __FILE__, __LINE__)

/** @brief Retrieve underlying record or crash.
    The returned value is **not retained.** */
#define purs_any_get_record(A) _purs_any_get_record((A), __FILE__, __LINE__)

/** @brief Retrieve underlying string or crash.
    The returned value is **not retained.** */
#define purs_any_get_string(A) _purs_any_get_string((A), __FILE__, __LINE__)

/** @brief Retrieve underlying array or crash.
    The returned value is **not retained.** */
#define purs_any_get_array(A) _purs_any_get_array((A), __FILE__, __LINE__)

/// @private
#define __PURS_ANY_FORCE_COPY(N, A, R, TAG)\
	static inline R _purs_any_force_ ## N (purs_any_t v, char * file, int line) {\
		int was_forced;\
		v = purs_any_unthunk(v, &was_forced);\
		purs_any_assert_tag_eq(TAG, v.tag);\
		R r = v.value.A;\
		if (was_forced) PURS_ANY_RELEASE(v);\
		return r;\
	}

/// @private
#define __PURS_ANY_FORCE_RETAIN(N, A, R, TAG)\
	static inline R _purs_any_force_ ## N (purs_any_t v, char * file, int line) {\
		int was_forced;\
		v = purs_any_unthunk(v, &was_forced);\
		purs_any_assert_tag_eq(TAG, v.tag);\
		R r = v.value.A;\
		PURS_RC_BASE_RETAIN(r);\
		if (was_forced) PURS_ANY_RELEASE(v);\
		return r;\
	}

__PURS_ANY_FORCE_COPY(int, i, purs_int_t, PURS_ANY_TAG_INT)
__PURS_ANY_FORCE_COPY(num, n, purs_num_t, PURS_ANY_TAG_NUM)
__PURS_ANY_FORCE_COPY(char, chr, purs_char_t, PURS_ANY_TAG_CHAR)
__PURS_ANY_FORCE_RETAIN(cont, cont, const purs_cont_t *, PURS_ANY_TAG_CONT)
__PURS_ANY_FORCE_RETAIN(cons, cons, const purs_cons_t *, PURS_ANY_TAG_CONS)
__PURS_ANY_FORCE_RETAIN(thunk, thunk, const purs_thunk_t *, PURS_ANY_TAG_THUNK)
__PURS_ANY_FORCE_RETAIN(record, record, const purs_record_t *, PURS_ANY_TAG_RECORD)
__PURS_ANY_FORCE_RETAIN(string, str, const purs_str_t *, PURS_ANY_TAG_STRING)
__PURS_ANY_FORCE_RETAIN(array, array, const purs_vec_t *, PURS_ANY_TAG_ARRAY)
__PURS_ANY_FORCE_RETAIN(foreign, foreign, const purs_foreign_t *, PURS_ANY_TAG_FOREIGN)

/** @brief Force-evaluate value to an int. */
#define purs_any_force_int(A) _purs_any_force_int((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a char. */
#define purs_any_force_char(A) _purs_any_force_char((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a number. */
#define purs_any_force_num(A) _purs_any_force_num((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a foreign.
  * The returned value is retained and must be released.
  */
#define purs_any_force_foreign(A) _purs_any_force_foreign((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a continuation.
  * The returned value is retained and must be released.
  */
#define purs_any_force_cont(A) _purs_any_force_cont((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a data constructor.
  * The returned value is retained and must be released.
  */
#define purs_any_force_cons(A) _purs_any_force_cons((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a thunk.
  * The returned value is retained and must be released.
  */
#define purs_any_force_thunk(A) _purs_any_force_thunk((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a record.
  * The returned value is retained and must be released.
  */
#define purs_any_force_record(A) _purs_any_force_record((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to a string.
  * The returned value is retained and must be released.
  */
#define purs_any_force_string(A) _purs_any_force_string((A), __FILE__, __LINE__)

/** @brief Force-evaluate value to an array.
  * The returned value is retained and must be released.
  */
#define purs_any_force_array(A) _purs_any_force_array((A), __FILE__, __LINE__)

/// @private
__PURS_ANY_FORCE_COPY(cons_tag, cons->tag, int, PURS_ANY_TAG_CONS)

/// Force-evaluate to the tag of a contained constructor value.
#define purs_any_force_cons_tag(A) \
	_purs_any_force_cons_tag((A), __FILE__, __LINE__)

/// @private
__PURS_ANY_FORCE_COPY(array_length,\
		      array == NULL\
			? 0\
			: v.value.array->length,\
		      int,\
		      PURS_ANY_TAG_ARRAY)

/** @brief Force-evaluate to the length of an array.
           The array is **not retained.** */
#define purs_any_force_array_length(A) \
	_purs_any_force_array_length((A), __FILE__, __LINE__)

// -----------------------------------------------------------------------------
// Any: built-in functions
// -----------------------------------------------------------------------------

/** @brief check that the given char value equals the given char.
    @param[in] x The char value. Must be an evaluated char value, or else will
    abort program.
    @param[in] y The char to compare against */
static inline int purs_any_eq_char (purs_any_t x, purs_char_t y) {
	return purs_any_get_char(x) == y;
}

/** @brief check that the given string value equals the given UTF-8 string.
    @param[in] x The string value. Must be an evaluated string value, or else
    will abort program.
    @param[in] y The UTF-8 string to compare against. */
static inline int purs_any_eq_string (purs_any_t x, const void * str) {
	return utf8cmp(purs_any_get_string(x)->data, str) == 0;
}

/** @brief check that the given integer value equals the given integer.
    @param[in] x The integer value. Must be an evaluated integer value, or else
    will abort program.
    @param[in] y The integer to compare against */
static inline int purs_any_eq_int (purs_any_t x, purs_int_t y) {
	return purs_any_get_int(x) == y;
}

/** @brief check that the given number value equals the given number.
    @param[in] x The number value. Must be an evaluated number value, or else
    will abort program.
    @param[in] y The number to compare against */
static inline int purs_any_eq_num (purs_any_t x, purs_num_t y) {
	return purs_any_get_num(x) == y;
}

int purs_any_eq(purs_any_t, purs_any_t);
purs_any_t purs_any_concat(purs_any_t, purs_any_t);

// -----------------------------------------------------------------------------
// continuations
// -----------------------------------------------------------------------------

const purs_cont_t * purs_cont_new(const struct purs_scope *, purs_cont_fun_t *);

// -----------------------------------------------------------------------------
// foreign
// -----------------------------------------------------------------------------

const purs_foreign_t * purs_foreign_new(void* tag,
					void* data,
					purs_foreign_finalizer finalize_cb);

// -----------------------------------------------------------------------------
// data constructors
// -----------------------------------------------------------------------------

const purs_cons_t * purs_cons_new(int tag, int size, ...);

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

const purs_vec_t * purs_vec_new ();
const purs_vec_t * purs_vec_new_va (int count, ...);
const purs_vec_t * purs_vec_copy (const purs_vec_t *);
const purs_vec_t * purs_vec_splice (const purs_vec_t *, int start, int count);
const purs_vec_t * purs_vec_concat(const purs_vec_t * lhs, const purs_vec_t * rhs);

#define purs_vec_length(v) ((v == NULL) ? 0 : v->length)
#define purs_vec_foreach(v, var, iter) if (v != NULL) vec_foreach(v, var, iter)
#define purs_vec_reserve(v, n) vec_reserve(v, n)
#define purs_vec_push_mut(v, x) vec_push(v, x)
#define purs_vec_pusharr_mut(v, arr, count) vec_pusharr(v, arr, count)

/**
 * Insert the value val at index idx shifting the elements after the index to
 * make room for the new value.
 */
const purs_vec_t * purs_vec_insert(const purs_vec_t *, int idx, purs_any_t val);

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

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
purs_any_t * purs_record_find_by_key(const purs_record_t *,
				     const void * key);

// -----------------------------------------------------------------------------
// Code-gen helpers
// -----------------------------------------------------------------------------

static inline int purs_any_get_main_rc_compat(purs_any_t v) {
	switch(v.tag) {
	case PURS_ANY_TAG_NULL:
		return 0;
	case PURS_ANY_TAG_INT:
		return purs_any_force_int(v);
	default: {
		char* s;
		purs_debug(v, &s);
		purs_assert(0,
			    "program did not return unit or int, value=(%s)\n",
			    s);
		free(s); /* for good measure */
		return -1; /* silence warning */
	}
	}
}

#define purs_address_of(V) &V
#define purs_derefence(V) *V

/* Tail-call optimization generation */
struct tco_state {
	int done;
	int size;
	purs_any_t * args;
};

#define purs_tco_state_new(N)\
	({\
		struct tco_state x;\
		x.done = 0;\
		x.size = N;\
		x.args = purs_malloc(sizeof (purs_any_t) * N);\
		x;\
	})
#define purs_tco_state_free(S) do {\
	for (int i = 0; i < S.size; i++) {\
		PURS_ANY_RELEASE(S.args[i]);\
	}\
	purs_free(S.args);\
} while (0)
#define purs_tco_is_done(X) (X.done == 1)
#define purs_tco_set_done(X) (((struct tco_state *) X)->done = 1)
#define purs_tco_get_arg(X, I) (((struct tco_state *) X)->args[I])
#define purs_tco_set_arg(X, I, V) do {\
		purs_any_t __v__ = (V);\
		PURS_ANY_RETAIN(__v__);\
		X.args[I] = __v__;\
	} while (0)
#define purs_tco_mut_arg(X, I, V) do {\
		purs_any_t __v__ = (V);\
		PURS_ANY_RELEASE(((struct tco_state *) X)->args[I]);\
		PURS_ANY_RETAIN(__v__);\
		((struct tco_state *) X)->args[I] = __v__;\
	} while (0)
#define purs_foreign_get_data(X) (X->data)

/* Captured scope generation */
struct purs_scope {
	struct purs_rc rc;
	int size;
	purs_any_t *bindings;
};

#define purs_scope_binding_at(S, N) ((S)->bindings[(N)])
#define purs_scope_capture_at(S, N, B) { (S)->bindings[(N)] = (B); }

struct purs_scope * purs_scope_new(int size, ...);
struct purs_scope * purs_scope_new1(int size);

/* todo: remove this! */
#define purs_cons_get_tag(V) V->tag

/* allocate a buffer to fit 'N' 'purs_any_t's */
#define purs_malloc_any_buf(N) purs_malloc(sizeof (purs_any_t) * N)

/* declare a thunked top-level value.
   todo: consider caching top-level thunks once forced.
 */
#define PURS_ANY_THUNK_DEF(NAME)\
	static purs_any_t NAME ## __thunk_fn__init();\
	static purs_any_t NAME ## __thunk_fn__ (void * __unused__1) { \
	    return NAME ## __thunk_fn__init();\
	};\
	purs_thunk_t NAME ## __thunk__ = {\
		.fn = NAME ## __thunk_fn__,\
		.ctx = NULL,\
		.rc = { NULL, -1 }\
	};\
	purs_any_t NAME = {\
		.tag = PURS_ANY_TAG_THUNK,\
		.value = { .thunk = & NAME ## __thunk__ }\
	};\
	purs_any_t NAME ## __thunk_fn__init()

#define purs_any_int_neg(X) purs_any_int_new(-purs_any_get_int(X))

// -----------------------------------------------------------------------------
// Any: initializers
// -----------------------------------------------------------------------------

#define PURS_ANY_NULL\
	((purs_any_t){ .tag = PURS_ANY_TAG_NULL })

#define PURS_ANY_INT(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_INT, .value = { .i = (X) } })

#define PURS_ANY_NUM(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_NUM, .value = { .n = (X) } })

#define PURS_ANY_CHAR(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_CHAR, .value = { .chr = (X) } })

#define PURS_ANY_FOREIGN(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_FOREIGN, .value = { .foreign = (X) } })

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

#define PURS_ANY_CONS(X)\
	((purs_any_t){ .tag = PURS_ANY_TAG_CONS, .value = { .cons = (X) } })

/* DEPRECATED: two versions for compat/historical reasons only */
#define purs_any_int PURS_ANY_INT
#define purs_any_num PURS_ANY_NUM
#define purs_any_char PURS_ANY_CHAR
#define purs_any_foreign PURS_ANY_FOREIGN
#define purs_any_array PURS_ANY_ARRAY
#define purs_any_record PURS_ANY_RECORD
#define purs_any_cont PURS_ANY_CONT
#define purs_any_cons PURS_ANY_CONS
#define purs_any_string PURS_ANY_STRING
#define purs_any_thunk PURS_ANY_THUNK

// -----------------------------------------------------------------------------
// Code-gen helpers (pt. 2)
// -----------------------------------------------------------------------------

/* Thunked pointer dereference: Recursive bindings support */
static inline void purs_indirect_value_assign(purs_any_t * x, purs_any_t v) {
	*x = v;
	PURS_ANY_RETAIN(v);
}

static inline void purs_indirect_value_free(purs_any_t * x) {
	if (x != NULL) {
		purs_any_t y = *x;
		PURS_ANY_RELEASE(y);
	}
	purs_free(x);
}

static inline purs_any_t * purs_indirect_value_new() {
	purs_any_t *x = purs_new(purs_any_t);
	x->tag = PURS_ANY_TAG_NULL;
	x->value = (purs_any_value_t){};
	return x;
}

static inline purs_any_t purs_indirect_thunk_deref(void * ctx) {
	purs_any_t any = *((purs_any_t*)(ctx));
	PURS_ANY_RETAIN(any); /* user will release! */
	return any;
}

static void purs_indirect_thunk_free(const struct purs_rc *ref) {
	purs_thunk_t * thunk = container_of(ref, purs_thunk_t, rc);
	purs_indirect_value_free((purs_any_t *) thunk->ctx);
	purs_free(thunk);
}

/* takes ownership of 'x' */
static inline purs_any_t purs_indirect_thunk_new(purs_any_t * x) {
	purs_thunk_t * thunk = purs_malloc(sizeof (purs_thunk_t));
	thunk->ctx = x;
	thunk->fn = purs_indirect_thunk_deref;
	thunk->rc = ((struct purs_rc) { purs_indirect_thunk_free, 1 });
	return PURS_ANY_THUNK(thunk);
}

// -----------------------------------------------------------------------------
// FFI helpers
// -----------------------------------------------------------------------------

/* note: The '$' is currently appended to all names (see code generation) */
#define PURS_FFI_EXPORT(NAME)\
	purs_any_t NAME ## _$

#define PURS_FFI_VALUE(NAME, INIT)\
	purs_any_t NAME ## _$ = INIT

// -----------------------------------------------------------------------------
// FFI: fixed-arity curried functions
// -----------------------------------------------------------------------------

#define _PURS_FFI_FUNC_ENTRY(NAME)\
	purs_cont_t NAME ## __cont__ = {\
		.fn = NAME ## __1,\
		.scope = NULL,\
		.rc = { .count = -1 }\
	};\
	purs_any_t NAME = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	};\
	/* for code-gen use. todo: remove? */\
	purs_any_t NAME ## _$ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	}

#define _PURS_FFI_FUNC_CONT(NAME, CUR, NEXT)\
	purs_any_t NAME##__##CUR (const purs_scope_t * $__super__, purs_any_t a, va_list $__unused__) {\
		purs_scope_t * scope = purs_scope_new1(CUR);\
		if ($__super__ != NULL) {\
			memcpy(scope->bindings,\
			       $__super__->bindings,\
			       $__super__->size * sizeof (purs_any_t));\
			for (int i = 0; i < $__super__->size; i++) {\
				PURS_ANY_RETAIN(scope->bindings[i]);\
			}\
		}\
		scope->bindings[CUR - 1] = a;\
		PURS_ANY_RETAIN(scope->bindings[CUR - 1]);\
		const purs_cont_t * cont = purs_cont_new(scope, NAME##__##NEXT);\
		PURS_RC_RELEASE(scope);\
		return purs_any_cont(cont);\
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

#define PURS_FFI_FUNC_1(NAME, A1)\
	purs_any_t NAME##__1_impl (purs_any_t);\
	purs_any_t NAME##__1 (const purs_scope_t * $__super__, purs_any_t A1, va_list $__unused__) {\
		return NAME##__1_impl(A1);\
	}\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__1_impl (purs_any_t A1)

#define PURS_FFI_FUNC_2(NAME, A1, A2)\
	purs_any_t NAME##__2_impl (purs_any_t, purs_any_t);\
	purs_any_t NAME##__2 (const purs_scope_t * $__super__, purs_any_t A2, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		return NAME##__2_impl(A1, A2);\
	}\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__2_impl (purs_any_t A1, purs_any_t A2)

#define PURS_FFI_FUNC_3(NAME, A1, A2, A3)\
	purs_any_t NAME##__3_impl (purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__3 (const purs_scope_t * $__super__, purs_any_t A3, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		return NAME##__3_impl(A1, A2, A3);\
	}\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__3_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3)

#define PURS_FFI_FUNC_4(NAME, A1, A2, A3, A4)\
	purs_any_t NAME##__4_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__4 (const purs_scope_t * $__super__, purs_any_t A4, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		return NAME##__4_impl(A1, A2, A3, A4);\
	}\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__4_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4)

#define PURS_FFI_FUNC_5(NAME, A1, A2, A3, A4, A5)\
	purs_any_t NAME##__5_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__5 (const purs_scope_t * $__super__, purs_any_t A5, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		return NAME##__5_impl(A1, A2, A3, A4, A5);\
	}\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__5_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5)

#define PURS_FFI_FUNC_6(NAME, A1, A2, A3, A4, A5, A6)\
	purs_any_t NAME##__6_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__6 (const purs_scope_t * $__super__, purs_any_t A6, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		return NAME##__6_impl(A1, A2, A3, A4, A5, A6);\
	}\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__6_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6)

#define PURS_FFI_FUNC_7(NAME, A1, A2, A3, A4, A5, A6, A7)\
	purs_any_t NAME##__7_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__7 (const purs_scope_t * $__super__, purs_any_t A7, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		return NAME##__7_impl(A1, A2, A3, A4, A5, A6, A7);\
	}\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__7_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7)

#define PURS_FFI_FUNC_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8)\
	purs_any_t NAME##__8_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__8 (const purs_scope_t * $__super__, purs_any_t A8, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		purs_any_t A7 = $__super__->bindings[6];\
		return NAME##__8_impl(A1, A2, A3, A4, A5, A6, A7, A8);\
	}\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__8_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8)

#define PURS_FFI_FUNC_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9)\
	purs_any_t NAME##__9_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__9 (const purs_scope_t * $__super__, purs_any_t A9, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		purs_any_t A7 = $__super__->bindings[6];\
		purs_any_t A8 = $__super__->bindings[7];\
		return NAME##__9_impl(A1, A2, A3, A4, A5, A6, A7, A8, A9);\
	}\
	_PURS_FFI_FUNC_CONT_8_TO_9(NAME);\
	_PURS_FFI_FUNC_CONT_7_TO_8(NAME);\
	_PURS_FFI_FUNC_CONT_6_TO_7(NAME);\
	_PURS_FFI_FUNC_CONT_5_TO_6(NAME);\
	_PURS_FFI_FUNC_CONT_4_TO_5(NAME);\
	_PURS_FFI_FUNC_CONT_3_TO_4(NAME);\
	_PURS_FFI_FUNC_CONT_2_TO_3(NAME);\
	_PURS_FFI_FUNC_CONT_1_TO_2(NAME);\
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__9_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9)

#define PURS_FFI_FUNC_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)\
	purs_any_t NAME##__10_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t); \
	purs_any_t NAME##__10 (const purs_scope_t * $__super__, purs_any_t A10, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		purs_any_t A7 = $__super__->bindings[6];\
		purs_any_t A8 = $__super__->bindings[7];\
		purs_any_t A9 = $__super__->bindings[8];\
		return NAME##__10_impl(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10);\
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
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__10_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10)

#define PURS_FFI_FUNC_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)\
	purs_any_t NAME##__11_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t); \
	purs_any_t NAME##__11 (const purs_scope_t * $__super__, purs_any_t A11, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		purs_any_t A7 = $__super__->bindings[6];\
		purs_any_t A8 = $__super__->bindings[7];\
		purs_any_t A9 = $__super__->bindings[8];\
		purs_any_t A10 = $__super__->bindings[9];\
		return NAME##__11_impl(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11); \
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
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__11_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10, purs_any_t A11)

#define PURS_FFI_FUNC_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)\
	purs_any_t NAME##__12_impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##__12 (const purs_scope_t * $__super__, purs_any_t A12, va_list $__unused__) {\
		purs_any_t A1 = $__super__->bindings[0];\
		purs_any_t A2 = $__super__->bindings[1];\
		purs_any_t A3 = $__super__->bindings[2];\
		purs_any_t A4 = $__super__->bindings[3];\
		purs_any_t A5 = $__super__->bindings[4];\
		purs_any_t A6 = $__super__->bindings[5];\
		purs_any_t A7 = $__super__->bindings[6];\
		purs_any_t A8 = $__super__->bindings[7];\
		purs_any_t A9 = $__super__->bindings[8];\
		purs_any_t A10 = $__super__->bindings[9];\
		purs_any_t A11 = $__super__->bindings[10];\
		return NAME##__12_impl(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12);\
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
	_PURS_FFI_FUNC_ENTRY(NAME);\
	purs_any_t NAME##__12_impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10, purs_any_t A11, purs_any_t A12)

// -----------------------------------------------------------------------------
// FFI: fixed-arity uncurried functions
// -----------------------------------------------------------------------------

#define _PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME)\
	purs_cont_t NAME ## __cont__ = {\
		.fn = NAME ## _fn,\
		.scope = NULL,\
		.rc = { .count = -1 }\
	};\
	purs_any_t NAME = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	};\
	/* for code-gen use. todo: remove? */\
	purs_any_t NAME ## _$ = {\
		.tag = PURS_ANY_TAG_CONT,\
		.value = { .cont = & NAME ## __cont__ }\
	}

#define PURS_FFI_FUNC_UNCURRIED_1(NAME, A1)\
	purs_any_t NAME##__impl (purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list $__unused__) {\
		return NAME##__impl (A1);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1)

#define PURS_FFI_FUNC_UNCURRIED_2(NAME, A1, A2)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2)

#define PURS_FFI_FUNC_UNCURRIED_3(NAME, A1, A2, A3)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3)

#define PURS_FFI_FUNC_UNCURRIED_4(NAME, A1, A2, A3, A4)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4)

#define PURS_FFI_FUNC_UNCURRIED_5(NAME, A1, A2, A3, A4, A5)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5)

#define PURS_FFI_FUNC_UNCURRIED_6(NAME, A1, A2, A3, A4, A5, A6)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6)

#define PURS_FFI_FUNC_UNCURRIED_7(NAME, A1, A2, A3, A4, A5, A6, A7)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7)

#define PURS_FFI_FUNC_UNCURRIED_8(NAME, A1, A2, A3, A4, A5, A6, A7, A8)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		purs_any_t A8 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7, A8);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8)

#define PURS_FFI_FUNC_UNCURRIED_9(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9) \
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		purs_any_t A8 = va_arg(vl, purs_any_t);\
		purs_any_t A9 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7, A8, A9);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9)

#define PURS_FFI_FUNC_UNCURRIED_10(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) \
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		purs_any_t A8 = va_arg(vl, purs_any_t);\
		purs_any_t A9 = va_arg(vl, purs_any_t);\
		purs_any_t A10 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10)

#define PURS_FFI_FUNC_UNCURRIED_11(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t);\
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		purs_any_t A8 = va_arg(vl, purs_any_t);\
		purs_any_t A9 = va_arg(vl, purs_any_t);\
		purs_any_t A10 = va_arg(vl, purs_any_t);\
		purs_any_t A11 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10, purs_any_t A11)

#define PURS_FFI_FUNC_UNCURRIED_12(NAME, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)\
	purs_any_t NAME##__impl (purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t, purs_any_t); \
	purs_any_t NAME##_fn (const purs_scope_t * $__super__, purs_any_t A1, va_list vl) {\
		purs_any_t A2 = va_arg(vl, purs_any_t);\
		purs_any_t A3 = va_arg(vl, purs_any_t);\
		purs_any_t A4 = va_arg(vl, purs_any_t);\
		purs_any_t A5 = va_arg(vl, purs_any_t);\
		purs_any_t A6 = va_arg(vl, purs_any_t);\
		purs_any_t A7 = va_arg(vl, purs_any_t);\
		purs_any_t A8 = va_arg(vl, purs_any_t);\
		purs_any_t A9 = va_arg(vl, purs_any_t);\
		purs_any_t A10 = va_arg(vl, purs_any_t);\
		purs_any_t A11 = va_arg(vl, purs_any_t);\
		purs_any_t A12 = va_arg(vl, purs_any_t);\
		return NAME##__impl (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12);\
	}\
	_PURS_FFI_FUNC_UNCURRIED_ENTRY(NAME);\
	purs_any_t NAME##__impl (purs_any_t A1, purs_any_t A2, purs_any_t A3, purs_any_t A4, purs_any_t A5, purs_any_t A6, purs_any_t A7, purs_any_t A8, purs_any_t A9, purs_any_t A10, purs_any_t A11, purs_any_t A12)

// -----------------------------------------------------------------------------
// Prim shims
// note: See codegen notes about '_$' suffix
// -----------------------------------------------------------------------------

#define Prim_undefined_$ purs_any_null

// -----------------------------------------------------------------------------
// Built-ins
// -----------------------------------------------------------------------------

purs_any_t purs_any_true;
purs_any_t purs_any_false;
purs_any_t purs_any_NaN;
purs_any_t purs_any_int_one;
purs_any_t purs_any_num_one;
purs_any_t purs_any_int_zero;
purs_any_t purs_any_num_zero;

/// turn 1/0 to a bool value
#define purs_any_bool(V) \
	(V == 1) \
		? purs_any_true \
		: purs_any_false

/// flip bool value
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

#ifdef ANY
#error macro 'ANY' already defined
#endif

#ifdef APP
#error macro 'APP' already defined
#endif

/// Convenience macro for the type of dynamic values.
#define ANY purs_any_t

/// Convenience macro to apply functions.
#define APP purs_any_app

#endif // PURESCRIPT_RUNTIME_H
