#include <gc/gc.h>
#include "Block.h"
#include "runtime/purescript.h"

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

const managed_t * managed_new (const void * data, const managed_release_func release) {
	managed_t * managed = GC_NEW(managed_block_t);
	managed->data = data;
	GC_register_finalizer(
		managed,
		(GC_finalization_proc) release,
		0, 0, 0);
	return managed;
}

// -----------------------------------------------------------------------------
// managed blocks
// -----------------------------------------------------------------------------

void managed_block_release (managed_t * managed) {
	Block_release(managed->data);
}

const managed_block_t * managed_block_new (const void * block) {
	return managed_new(block, managed_block_release);
}

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
void managed_utf8str_release (managed_t * managed) {
	free((void *) managed->data);
}

const managed_utf8str_t * managed_utf8str_new (const void * data) {
	return managed_new(data, managed_utf8str_release);
}

// -----------------------------------------------------------------------------
// misc
// -----------------------------------------------------------------------------

inline const void * purs_assert_not_null(const void * data, const char * message) {
	char * message_ = afmt(message);
	purs_assertf(data != NULL, message_);
	free (message_);
	return data;
}

// -----------------------------------------------------------------------------
// any: dynamically typed values
// -----------------------------------------------------------------------------

inline const purs_any_t * purs_any_unthunk (const purs_any_t * x) {
	while (x->tag == THUNK) {
		x = x->value.fn(NULL);
	}
	return x;
}

inline const purs_any_tag_t * purs_any_get_tag_maybe (const purs_any_t * x) {
	if      (x->tag == INT)		return &x->tag;
	else if (x->tag == FLOAT)		return &x->tag;
	else if (x->tag == ABS)		return &x->tag;
	else if (x->tag == ABS_BLOCK)	return &x->tag;
	else if (x->tag == CONS)		return &x->tag;
	else if (x->tag == RECORD)		return &x->tag;
	else if (x->tag == STRING)		return &x->tag;
	else if (x->tag == THUNK)		return &x->tag;
	else return NULL;
}

const purs_cons_t * purs_any_get_cons_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == CONS) {
		return & x->value.cons;
	} else {
		return NULL;
	}
}

const abs_t purs_any_get_abs_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS) {
		return (abs_t)(x->value.fn);
	} else {
		return NULL;
	}
}

const int * purs_any_get_int_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == INT) {
		return &x->value.num_int;
	} else {
		return NULL;
	}
}

const float * purs_any_get_float_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == FLOAT) {
		return &x->value.num_float;
	} else {
		return NULL;
	}
}

const managed_block_t * purs_any_get_abs_block_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS_BLOCK) {
		return (managed_block_t *) x->value.block;
	} else {
		return NULL;
	}
}

const managed_utf8str_t * purs_any_get_string_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == STRING) {
		return (const managed_block_t *) x->value.string;
	} else {
		return NULL;
	}
}

const purs_record_t * purs_any_get_record_maybe (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == RECORD) {
		return (const purs_record_t *) x->value.record;
	} else {
		return NULL;
	}
}

#define PURS_ANY_GET_IMPL(T, X) \
	const T purs_any_get_##X (const purs_any_t * x) { \
		purs_assert_not_null(x, "(purs_any_get_" #X ") expected: " #X); \
		char * msg = afmt( \
			"(purs_any_get_" #X ") invalid type: %d", \
			* (const int *) purs_assert_not_null( \
				purs_any_get_tag_maybe(x), \
				"(purs_any_get_" #X ") expected purs_any_t" \
			) \
		); \
		const T r = purs_assert_not_null( \
			purs_any_get_##X##_maybe(x), \
			msg \
		); \
		free(msg); \
		return r; \
	}

PURS_ANY_GET_IMPL(purs_cons_t *, cons);
PURS_ANY_GET_IMPL(abs_t, abs);
PURS_ANY_GET_IMPL(int *, int);
PURS_ANY_GET_IMPL(float *, float);
PURS_ANY_GET_IMPL(managed_block_t *, abs_block);
PURS_ANY_GET_IMPL(managed_utf8str_t *, string);
PURS_ANY_GET_IMPL(purs_record_t *, record);

#define PURS_ANY_SET_IMPL(NAME, TYPE, TAG, KEY) \
	purs_any_t * NAME (purs_any_t * any, TYPE val) { \
		any->tag = TAG; \
		any->value.KEY = val; \
		return any; \
	}

PURS_ANY_SET_IMPL(purs_any_set_abs, const abs_t, ABS, fn)
PURS_ANY_SET_IMPL(purs_any_set_abs_block, const managed_block_t *, ABS_BLOCK, block)
PURS_ANY_SET_IMPL(purs_any_set_float, float, FLOAT, num_float)
PURS_ANY_SET_IMPL(purs_any_set_int, int, INT, num_int)
PURS_ANY_SET_IMPL(purs_any_set_cons, purs_cons_t, CONS, cons)
PURS_ANY_SET_IMPL(purs_any_set_string, const managed_utf8str_t *, STRING, string)
PURS_ANY_SET_IMPL(purs_any_set_record, const purs_record_t *, RECORD, record)

// XXX: for convenient emitting only (might be removed)
int purs_cons_get_tag (const purs_cons_t * cons) {
	return cons->tag;
}

const purs_any_t * purs_any_app (const purs_any_t * x, const purs_any_t * arg) {
	const void * f;
	const managed_block_t * b;

	x = purs_any_unthunk(x);

	b = purs_any_get_abs_block_maybe(x);
	if (b != NULL) {
		return ((abs_block_t) b->data)(arg);
	}

	f = purs_any_get_abs_maybe(x);
	if (f != NULL) {
		return ((abs_t) f)(arg);
	}

	purs_assertf(0, "expected function");
}

int purs_any_eq_string (const purs_any_t * x, const void * str) {
	const managed_utf8str_t * a = purs_any_get_string(x);
	return utf8cmp(a->data, str) == 0;

}

int purs_any_eq_int (const purs_any_t * x, int y) {
	const int * a = purs_any_get_int(x);
	return *a == y;
}

int purs_any_eq_float (const purs_any_t * x, float y) {
	const float * a = purs_any_get_float(x);
	return *a == y;
}

/**
 Concatenate two dyanmic values into a new dynamic value
 TODO: define for all types encapsulated by purs_any_t
*/
const purs_any_t * purs_any_concat(const purs_any_t * a, const purs_any_t * b) {
	const managed_utf8str_t * a_utf8str = purs_any_get_string(a);
	const managed_utf8str_t * b_utf8str = purs_any_get_string(b);
	return purs_any_set_string(
		GC_NEW(purs_any_t),
		managed_utf8str_new(afmt("%s%s", a_utf8str->data, b_utf8str->data))
	);
}

// -----------------------------------------------------------------------------
// arrays (via vectors)
// -----------------------------------------------------------------------------

void purs_vec_release (purs_vec_t * vec) {
	vec_deinit(vec);
}

const purs_vec_t * purs_vec_new (const purs_any_t ** items, int count) {
	purs_vec_t * v = GC_NEW(purs_vec_t);
	GC_register_finalizer(v,
						  (GC_finalization_proc) purs_vec_release,
						  0, 0, 0);
	vec_init(v);
	vec_pusharr(v, items, count);
	return (const purs_vec_t *) v;
}

const purs_vec_t * purs_vec_copy (const purs_vec_t * vec) {
	purs_vec_t * copy = (purs_vec_t *) purs_vec_new(NULL, 0);
	vec_expand_((char**)&copy->data,
				(int *)&vec->length,
				(int *)&vec->capacity,
				sizeof(*copy->data));
	memcpy(copy->data,
		   vec->data,
		   sizeof (*copy->data) * vec->capacity);
	return (const purs_vec_t *) copy;
}


// -----------------------------------------------------------------------------
// records (via hash tables)
// -----------------------------------------------------------------------------

const purs_record_t * purs_record_copy_shallow(const purs_record_t * source) {
	const purs_record_t * current_entry, * tmp;
	purs_record_t * entry_copy;
	purs_record_t * record = NULL;
	HASH_ITER(hh, source, current_entry, tmp) {
		entry_copy = GC_NEW(purs_record_t);
		memcpy(entry_copy, current_entry, sizeof(purs_record_t));
		HASH_ADD_KEYPTR(
			hh,
			record,
			entry_copy->key->data,
			utf8size(entry_copy->key->data),
			entry_copy
		);
	}
	return (const purs_record_t *) record;
}

const purs_record_t * purs_record_add_multi(const purs_record_t * source, size_t count, ...) {
	if (count == 0) {
		return source;
	}

	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);

	va_list args;
	va_start(args, count);

	for (size_t i = 0; i < count; i++) {
		const void * key = va_arg(args, const void *);
		const purs_any_t * value = va_arg(args, const purs_any_t *);
		purs_record_t * entry = GC_NEW(purs_record_t);
		entry->key = managed_utf8str_new(key);
		entry->value = value;
		HASH_ADD_KEYPTR(
			hh,
			copy,
			entry->key->data,
			utf8size(entry->key->data),
			entry
		);
	}

	va_end(args);

	return (const purs_record_t *) copy;
}

/**
 * Remove a value at a given key
 */
const purs_record_t * purs_record_remove(const purs_record_t * source,
										 const void * key) {
	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
	purs_record_t * v = purs_record_find_by_key(source, key);
	if (v != NULL) {
		HASH_DEL(copy, (purs_record_t *) v);
	}
	return (const purs_record_t *) copy;
}

/**
 * Find a value at a given key
 */
purs_record_t * purs_record_find_by_key(const purs_record_t * record,
										const void * key) {
	purs_record_t * result;
    size_t len = utf8size(key);
	HASH_FIND(hh, record, key, len, result);
	return result;
}
