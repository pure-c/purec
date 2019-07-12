#include "runtime/purescript.h"

static inline void managed_noop_release (managed_t * managed) {}

inline void managed_default_release (managed_t * managed) {
	free((void *) managed->data);
}

const managed_t * managed_new (const void * data,
			       const managed_release_func release) {
	managed_t * managed = purs_new(managed_t);
	managed->data = data;
	if (release != NULL) {
		GC_register_finalizer(
			(void *) managed,
			(GC_finalization_proc) release,
			0, 0, 0);
	} else {
		GC_register_finalizer(
			(void *) managed,
			(GC_finalization_proc) managed_default_release,
			0, 0, 0);
	}
	return managed;
}

// -----------------------------------------------------------------------------
// Any: allocate
// -----------------------------------------------------------------------------

/* todo: turn into macro */
ANY purs_any_cont(ANY * ctx, int len, purs_any_cont_fun_t * fn) {
	ANY v;
	v.tag = PURS_ANY_TAG_CONT;
	v.value.cont = purs_malloc(sizeof (purs_any_cont_t));
	v.value.cont->fn = fn;
	v.value.cont->ctx = ctx;
	v.value.cont->len = len;
	return v;
}

/* todo: turn into macro */
ANY purs_any_cons(int tag, int size, ANY* values) {
	ANY v;
	v.tag = PURS_ANY_TAG_CONS;
	v.value.cons = purs_malloc(sizeof (purs_any_cons_t));
	v.value.cons->tag = tag;
	v.value.cons->size = size;
	v.value.cons->values = values;
	return v;
}

/* todo: turn into macro */
ANY purs_any_string_mv(const char * ptr) {
	ANY v;
	v.tag = PURS_ANY_TAG_STRING;
	v.value.str = managed_new(ptr, managed_noop_release);
	return v;
}

/* todo: turn into macro */
ANY purs_any_string(const char * fmt, ...) {
	ANY v;
	v.tag = PURS_ANY_TAG_STRING;
	va_list ap;
	char *ptr;
	va_start(ap, fmt);
	assert (vasprintf(&ptr, fmt, ap) >= 0);
	va_end(ap);
	v.value.str = managed_new(ptr, NULL);
	return v;
}

// -----------------------------------------------------------------------------
// Any: getters
// -----------------------------------------------------------------------------

inline const char * purs_any_tag_str (const purs_any_tag_t tag) {
	static const char * tags[] = {
		"NULL",
		"INT",
		"NUM",
		"CONT",
		"THUNK",
		"CONS",
		"RECORD",
		"STRING",
		"CHAR",
		"ARRAY",
		"FOREIGN",
	};
	return tags[tag];
}

#define _PURS_ASSERT_TAG(TAG)\
	do {\
		v = purs_any_unthunk(v);\
		purs_assert(v.tag == TAG, "expected tag: %s, but got: %s",\
			    purs_any_tag_str(TAG),\
			    purs_any_tag_str(v.tag));\
	} while (0)


inline const purs_any_int_t purs_any_get_int (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_INT);
	return v.value.i;
}

inline const purs_any_num_t purs_any_get_num (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_NUM);
	return v.value.n;
}

inline const utf8_int32_t purs_any_get_char (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CHAR);
	return v.value.chr;
}

inline purs_any_cont_t * purs_any_get_cont (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONT);
	return v.value.cont;
}

inline purs_any_cons_t * purs_any_get_cons (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONS);
	return v.value.cons;
}

inline const purs_record_t * purs_any_get_record (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_RECORD);
	return v.value.record;
}

inline const purs_vec_t * purs_any_get_array (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_ARRAY);
	return v.value.array;
}

inline purs_foreign_t purs_any_get_foreign (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_FOREIGN);
	return v.value.foreign;
}

inline const void * purs_any_get_string (ANY v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_STRING);
	return v.value.str->data;
}

// -----------------------------------------------------------------------------
// Any
// -----------------------------------------------------------------------------

inline ANY purs_any_unthunk (ANY x) {
	ANY out = x;
	while (out.tag == PURS_ANY_TAG_THUNK) {
		out = out.value.thunk->fn(out.value.thunk->ctx);
	}
	return out;
}

inline const purs_any_tag_t purs_any_get_tag (ANY v) {
	return v.tag;
}

inline ANY purs_any_app(ANY f, ANY v, ...) {
	f = purs_any_unthunk(f);
	assert(f.tag == PURS_ANY_TAG_CONT);
	va_list args;
	va_start(args, v);
	ANY r = f.value.cont->fn(f.value.cont->ctx, v, args);
	va_end(args);
	return r;
}

// -----------------------------------------------------------------------------
// Any: built-ins
// -----------------------------------------------------------------------------

ANY purs_any_null = { .tag = PURS_ANY_TAG_NULL };

ANY purs_any_true = PURS_ANY_INT(1);
ANY purs_any_false = PURS_ANY_INT(0);

ANY purs_any_int_zero = PURS_ANY_INT(0);
ANY purs_any_num_zero = PURS_ANY_NUM(0.0);

ANY purs_any_int_one = PURS_ANY_INT(1);
ANY purs_any_num_one = PURS_ANY_NUM(1.0);

ANY purs_any_NaN = PURS_ANY_NUM(PURS_NAN);
ANY purs_any_infinity = PURS_ANY_NUM(PURS_INFINITY);
ANY purs_any_neg_infinity = PURS_ANY_NUM(-PURS_INFINITY);

inline int purs_any_eq_char (ANY x, utf8_int32_t y) {
	return purs_any_get_char(x) == y;
}

inline int purs_any_eq_string (ANY x, const void * str) {
	return utf8cmp(purs_any_get_string(x), str) == 0;
}

inline int purs_any_eq_int (ANY x, purs_any_int_t y) {
	return purs_any_get_int(x) == y;
}

inline int purs_any_eq_num (ANY x, double y) {
	return purs_any_get_num(x) == y;
}

int purs_any_eq(ANY x, ANY y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	/* special treatment for NaN on LHS */
	if (purs_any_is_NaN(x) &&
		(y.tag == PURS_ANY_TAG_NUM || y.tag == PURS_ANY_TAG_INT)) {
		return 0;
	}

	/* special treatment for NaN on RHS */
	if (purs_any_is_NaN(y) &&
		(x.tag == PURS_ANY_TAG_NUM || x.tag == PURS_ANY_TAG_INT)) {
		return 0;
	}

	if (x.tag == PURS_ANY_TAG_NULL || y.tag == PURS_ANY_TAG_NULL) {
		return 0;
	} else {
		purs_assert(
			x.tag == y.tag,
			"Cannot eq %s with %s",
			purs_any_tag_str(x.tag),
			purs_any_tag_str(y.tag));

		switch (x.tag) {
		case PURS_ANY_TAG_INT:
			return purs_any_get_int(x) == purs_any_get_int(y);
		case PURS_ANY_TAG_NUM:
			return purs_any_get_num(x) == purs_any_get_num(y);
		case PURS_ANY_TAG_STRING:
			return (utf8cmp(purs_any_get_string(x), purs_any_get_string(y)) == 0);
		case PURS_ANY_TAG_CHAR:
			return purs_any_get_char(x) == purs_any_get_char(y);
		default:
			return 0;
		}
	}
}

/**
 Concatenate two dyanmic values into a new dynamic value
*/
ANY purs_any_concat(ANY x, ANY y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	assert(x.tag != PURS_ANY_TAG_NULL);
	assert(y.tag != PURS_ANY_TAG_NULL);

	if (x.tag != y.tag) {
		purs_assert(
			0,
			"cannot concat %s with %s",
			purs_any_tag_str(x.tag),
			purs_any_tag_str(y.tag));
	} else {
		switch(x.tag) {
		case PURS_ANY_TAG_STRING: {
			return purs_any_string(
				"%s%s",
				     purs_any_get_string(x),
				     purs_any_get_string(y));
		}
		case PURS_ANY_TAG_ARRAY: {
			const purs_vec_t * x_vec = purs_any_get_array(x);
			const purs_vec_t * y_vec = purs_any_get_array(y);
			if (x_vec->length == 0) {
				return y;
			} else if (y_vec->length == 0) {
				return x;
			} else {
				purs_vec_t * out_vec = (purs_vec_t *) purs_vec_copy(x_vec);
				vec_pusharr(out_vec, y_vec->data, y_vec->length);
				return purs_any_array((const purs_vec_t *) out_vec);
			}
		}
		default:
			purs_assert(0, "cannot concat %s", purs_any_tag_str(x.tag));
		}
	}
}

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

const void * purs_string_copy (const void * source) {
	size_t sz = utf8size(source);
	void * dest = malloc(sz);
	memcpy(dest, source, sz);
	return (const void*) dest;
}

// -----------------------------------------------------------------------------
// arrays
// -----------------------------------------------------------------------------

inline void purs_vec_release (purs_vec_t * vec) {
	vec_deinit(vec);
}

inline const purs_vec_t * purs_vec_new () {
	purs_vec_t * v = purs_new(purs_vec_t);
	vec_init(v);
	return (const purs_vec_t *) v;
}

const purs_vec_t * purs_vec_new_va (int count, ...) {
	int i;
	va_list args;
	ANY* xs = malloc(sizeof (ANY) * count);
	va_start(args, count);
	for (i = 0; i < count; i++) {
		xs[i] = va_arg(args, ANY);
	}
	purs_vec_t * o = (purs_vec_t *) purs_vec_new();
	vec_pusharr(o, xs, count);
	free(xs);
	return (const purs_vec_t *) o;
}

const purs_vec_t * purs_vec_copy (const purs_vec_t * vec) {
	if (vec == NULL || vec->data == NULL) {
		return (purs_vec_t *) purs_vec_new();
	} else {
		purs_vec_t * copy = (purs_vec_t *) purs_vec_new();
		copy->length = vec->length;
		copy->capacity = vec->capacity;
		copy->data = vec_malloc(sizeof (ANY*) * vec->capacity);
		memcpy(copy->data,
		       vec->data,
		       sizeof (*copy->data) * vec->capacity);
		return (const purs_vec_t *) copy;
	}
}

const purs_vec_t * purs_vec_slice (const purs_vec_t * vec, int begin) {
	purs_vec_t * copy = (purs_vec_t *) purs_vec_copy(vec);
	vec_splice(copy, 0, begin);
	return (const purs_vec_t *) copy;
}

const purs_vec_t * purs_vec_insert(const purs_vec_t * vec,
				   int idx,
				   ANY val) {
	if (vec == NULL) {
		return purs_vec_new_va(1, val);
	} else {
		purs_vec_t * out = (purs_vec_t *) purs_vec_copy(vec);
		vec_insert(out, idx, val);
		return (const purs_vec_t *) out;
	}
}

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

ANY purs_record_empty = PURS_ANY_RECORD(NULL);

const purs_record_t * purs_record_copy_shallow(const purs_record_t * source) {
	const purs_record_t * current_entry, * tmp;
	purs_record_t * entry_copy;
	purs_record_t * record = NULL;
	HASH_ITER(hh, source, current_entry, tmp) {
		entry_copy = purs_new(purs_record_t);
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

static purs_record_t * _purs_record_add_multi_mut(purs_record_t * source,
						  size_t count,
						  va_list args) {
	for (size_t i = 0; i < count; i++) {
		const void * key = va_arg(args, const void *);
		ANY value = va_arg(args, ANY);
		purs_record_t * entry = purs_new(purs_record_t);
		entry->key = managed_new(afmt("%s", key), NULL);
		entry->value = value;
		HASH_ADD_KEYPTR(
			hh,
			source,
			entry->key->data,
			utf8size(entry->key->data),
			entry
		);
	}
	return source;
}

purs_record_t * purs_record_add_multi_mut(purs_record_t * source,
					  size_t count,
					  ...) {
	va_list args;
	va_start(args, count);
	_purs_record_add_multi_mut(source, count, args);
	va_end(args);
	return source;
}

const purs_record_t * purs_record_add_multi(const purs_record_t * source,
					    size_t count,
					    ...) {
	if (count == 0) {
		return source;
	}

	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
	va_list args;
	va_start(args, count);
	copy = _purs_record_add_multi_mut(copy, count, args);
	va_end(args);
	return (const purs_record_t *) copy;
}

const purs_record_t * purs_record_merge(const purs_record_t * l,
					const purs_record_t * r) {
	const purs_record_t *rec, *tmp = NULL;
	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(l);
	HASH_ITER(hh, r, rec, tmp) {
		purs_record_t * entry = purs_new(purs_record_t);
		entry->key = rec->key;
		entry->value = rec->value;
		HASH_ADD_KEYPTR(
			hh,
			copy,
			entry->key->data,
			utf8size(entry->key->data),
			entry
		);
	}
	return (const purs_record_t *) copy;
}

const purs_record_t * purs_record_remove(const purs_record_t * source,
					 const void * key) {
	purs_record_t * v = (purs_record_t *) purs_record_find_by_key(source, key);
	if (v != NULL) {
		purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
		HASH_DEL(copy, (purs_record_t *) v);
		return (const purs_record_t *) copy;
	} else {
		return source;
	}
}

purs_record_t * purs_record_remove_mut(purs_record_t * source,
				       const void * key) {
	purs_record_t * v = (purs_record_t *) purs_record_find_by_key(source, key);
	if (v != NULL) {
		HASH_DEL(source, (purs_record_t *) v);
	}
	return source;
}

const purs_record_t * purs_record_find_by_key(const purs_record_t * record,
					      const void * key) {
	purs_record_t * result;
	size_t len = utf8size(key);
	HASH_FIND(hh, record, key, len, result);
	return result;
}

// -----------------------------------------------------------------------------
// Code-gen helpers
// -----------------------------------------------------------------------------

ANY purs_thunked_deref(ANY ctx) {
	return *((ANY*)(ctx.value.foreign.data));
}
