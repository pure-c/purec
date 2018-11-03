#include <purescript.h>

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

inline const ANY * purs_any_int_new(const purs_any_int_t i) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_INT;
	v->value.i = i;
	return v;
}

inline const ANY * purs_any_num_new(const purs_any_num_t n) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_NUM;
	v->value.n = n;
	return v;
}

inline const ANY * purs_any_cont_new(const void * ctx, purs_any_fun_t * fn) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_CONT;
	v->value.cont.fn = fn;
	v->value.cont.ctx = ctx;
	return v;
}

inline const ANY * purs_any_thunk_new(const void * ctx, purs_any_thunk_fun_t * fn) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_THUNK;
	v->value.thunk.ctx = ctx;
	v->value.thunk.fn = fn;
	return v;
}

inline const ANY * purs_any_cons_new(int tag, const ANY ** values) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_CONS;
	v->value.cons.tag = tag;
	v->value.cons.values = values;
	return v;
}

inline const ANY * purs_any_record_new(const purs_record_t * record) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_RECORD;
	v->value.record = record;
	return v;
}

inline const ANY * purs_any_string_new(const char * fmt, ...) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_STRING;
	va_list ap;
	char *ptr;
	va_start(ap, fmt);
	assert (vasprintf(&ptr, fmt, ap) >= 0);
	va_end(ap);
	v->value.str = managed_new(ptr, NULL);
	return v;
}

inline const ANY * purs_any_char_new(utf8_int32_t chr) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_CHAR;
	v->value.chr = chr;
	return v;
}

inline const ANY * purs_any_array_new(const purs_vec_t * array) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_ARRAY;
	v->value.array = array;
	return v;
}

inline const ANY * purs_any_foreign_new(void * tag, void * data) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_FOREIGN;
	v->value.foreign.tag = tag;
	v->value.foreign.data = data;
	return v;
}

// -----------------------------------------------------------------------------
// Any: getters
// -----------------------------------------------------------------------------

inline const char * purs_any_tag_str (const purs_any_tag_t tag) {
	static const char * tags[] = {
		"UNKNOWN",
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
		purs_assert(v != NULL, "expected tag: %s, but got: NULL", \
			    purs_any_tag_str(TAG));\
		v = purs_any_unthunk(v);\
		purs_assert(v->tag == TAG, "expected tag: %s, but got: %s",\
			    purs_any_tag_str(TAG),\
			    purs_any_tag_str(v->tag));\
	} while (0)


inline const purs_any_int_t purs_any_get_int (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_INT);
	return v->value.i;
}

inline const purs_any_num_t purs_any_get_num (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_NUM);
	return v->value.n;
}

inline const purs_cont_t * purs_any_get_cont (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONT);
	return (const purs_cont_t *) &v->value.cont;
}

inline const purs_cons_t * purs_any_get_cons (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONS);
	return (const purs_cons_t *) &v->value.cons;
}

inline const purs_record_t * purs_any_get_record (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_RECORD);
	return v->value.record;
}

inline const void * purs_any_get_string (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_STRING);
	return v->value.str->data;
}

inline const utf8_int32_t purs_any_get_char (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CHAR);
	return v->value.chr;
}

inline const purs_vec_t * purs_any_get_array (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_ARRAY);
	return v->value.array;
}

inline const purs_foreign_t * purs_any_get_foreign (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_FOREIGN);
	return (const purs_foreign_t *) &v->value.foreign;
}

// -----------------------------------------------------------------------------
// Any
// -----------------------------------------------------------------------------

inline const ANY * purs_any_unthunk (const ANY * x) {
	const ANY * tmp;
	const ANY * out = (ANY *) x;
	while (out != NULL && out->tag == PURS_ANY_TAG_THUNK) {
		tmp = out->value.thunk.fn(out->value.thunk.ctx);
		purs_assert(tmp != out, "infinite unthunk loop");
		out = tmp;
	}
	return (const ANY *) out;
}

inline const purs_any_tag_t purs_any_get_tag (const ANY * v) {
	return v->tag;
}

inline const ANY * purs_any_app(const ANY * f, const ANY * v, ...) {
	assert(f != NULL);
	f = purs_any_unthunk(f);
	assert(f != NULL);
	assert(f->tag == PURS_ANY_TAG_CONT);
	va_list args;
	va_start(args, v);
	const ANY * r = f->value.cont.fn(f->value.cont.ctx, v, args);
	va_end(args);
	return r;
}

// -----------------------------------------------------------------------------
// Any: built-ins
// -----------------------------------------------------------------------------

PURS_ANY_THUNK_DEF(purs_any_true, purs_any_int_new(1));
PURS_ANY_THUNK_DEF(purs_any_false, purs_any_int_new(0));
PURS_ANY_THUNK_DEF(purs_any_int_zero, purs_any_int_new(0));
PURS_ANY_THUNK_DEF(purs_any_num_zero, purs_any_num_new(0.0));
PURS_ANY_THUNK_DEF(purs_any_int_one, purs_any_int_new(1));
PURS_ANY_THUNK_DEF(purs_any_num_one, purs_any_num_new(1.0));
PURS_ANY_THUNK_DEF(purs_any_NaN, purs_any_num_new(PURS_NAN));
PURS_ANY_THUNK_DEF(purs_any_infinity, purs_any_num_new(PURS_INFINITY));
PURS_ANY_THUNK_DEF(purs_any_neg_infinity, purs_any_num_new(-PURS_INFINITY));

inline int purs_any_eq_char (const ANY * x, utf8_int32_t y) {
	return purs_any_get_char(x) == y;
}

inline int purs_any_eq_string (const ANY * x, const void * str) {
	return utf8cmp(purs_any_get_string(x), str) == 0;
}

inline int purs_any_eq_int (const ANY * x, purs_any_int_t y) {
	return purs_any_get_int(x) == y;
}

inline int purs_any_eq_num (const ANY * x, double y) {
	return purs_any_get_num(x) == y;
}

int purs_any_eq(const ANY * x, const ANY * y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	/* special treatment for NaN on LHS */
	if (x != NULL && purs_any_is_NaN(x) &&
		(y->tag == PURS_ANY_TAG_NUM || y->tag == PURS_ANY_TAG_INT)) {
		return 0;
	}

	/* special treatment for NaN on RHS */
	if (y != NULL && purs_any_is_NaN(y) &&
		(x->tag == PURS_ANY_TAG_NUM || x->tag == PURS_ANY_TAG_INT)) {
		return 0;
	}

	if (x == y) {
		return 1;
	} else if (x == NULL || y == NULL) {
		return 0;
	} else {
		purs_assert(
			x->tag == y->tag,
			"Cannot eq %s with %s",
			purs_any_tag_str(x->tag),
			purs_any_tag_str(y->tag));

		switch (x->tag) {
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
const ANY * purs_any_concat(const ANY * x, const ANY * y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

	assert(x != NULL);
	assert(y != NULL);

	if (x->tag != y->tag) {
		purs_assert(
			0,
			"cannot concat %s with %s",
			purs_any_tag_str(x->tag),
			purs_any_tag_str(y->tag));
	} else {
		switch(x->tag) {
		case PURS_ANY_TAG_STRING: {
			return purs_any_string_new(
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
				return purs_any_array_new((const purs_vec_t *) out_vec);
			}
		}
		default:
			purs_assert(0, "cannot concat %s", purs_any_tag_str(x->tag));
		}
	}
}

inline const ANY * purs_any_copy(const ANY * src) {
	ANY * copy = purs_new(ANY);
	memcpy(copy, src, sizeof (ANY));
	return (const ANY*) copy;
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
	const ANY ** xs = malloc(sizeof (ANY *) * count);
	va_start(args, count);
	for (i = 0; i < count; i++) {
		xs[i] = va_arg(args, const ANY *);
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
				   const ANY * val) {
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

PURS_ANY_THUNK_DEF(purs_record_empty, purs_any_record_new(NULL));

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

const purs_record_t * purs_record_add_multi(const purs_record_t * source, size_t count, ...) {
	if (count == 0) {
		return source;
	}

	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);

	va_list args;
	va_start(args, count);

	for (size_t i = 0; i < count; i++) {
		const void * key = va_arg(args, const void *);
		const ANY * value = va_arg(args, const ANY *);
		purs_record_t * entry = purs_new(purs_record_t);
		entry->key = managed_new(afmt("%s", key), NULL);
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
	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
	purs_record_t * v = (purs_record_t *) purs_record_find_by_key(source, key);
	if (v != NULL) {
		HASH_DEL(copy, (purs_record_t *) v);
	}
	return (const purs_record_t *) copy;
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

inline const ANY * purs_indirect_thunk_new(const ANY ** x) {
	return purs_any_thunk_new(x, purs_thunked_deref);
}

inline void purs_indirect_value_assign(const ANY ** i, const ANY * v) {
	*i = v;
}

inline const ANY ** purs_indirect_value_new() {
	return purs_new(const ANY *);
}

inline const ANY * purs_thunked_deref(const void * data) {
	const ANY ** _data = (const ANY **) data;
	return *_data;
}

inline int purs_cons_get_tag (const purs_cons_t * cons) {
	return cons->tag;
}

inline const ANY ** _purs_scope_alloc(int num_bindings) {
	if (num_bindings == 0) return NULL;
	return purs_malloc(num_bindings * sizeof (const ANY *));
}

inline const ANY ** _purs_scope_new(int num_bindings, const ANY * binding, ...) {
	if (num_bindings == 0) return NULL;
	const ANY ** mem = purs_malloc(num_bindings * sizeof (const ANY *));
	mem[0] = binding;
	va_list vl;
	va_start(vl, binding);
	for (int i = 1; i < num_bindings; i++) {
		mem[i] = va_arg(vl, const ANY *);
	}
	va_end(vl);
	return (const ANY **) mem;
}
