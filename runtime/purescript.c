#include "runtime/purescript.h"

// -----------------------------------------------------------------------------
// Any: allocate
// -----------------------------------------------------------------------------

static void purs_cont_free(const struct purs_rc *ref) {
	purs_cont_t * x = container_of(ref, purs_cont_t, rc);
	if (x->scope != NULL) PURS_RC_RELEASE(x->scope);
	purs_free(x);
}

const purs_cont_t * purs_cont_new(const struct purs_scope * scope,
				  purs_cont_fun_t * fn) {
	purs_cont_t * cont = purs_malloc(sizeof (purs_cont_t));
	cont->fn = fn;
	cont->scope = scope;
	cont->rc = ((struct purs_rc) { purs_cont_free, 1 });
	if (scope != NULL) PURS_RC_RETAIN(scope);
	return (const purs_cont_t *) cont;
}

/* todo: treat. */
ANY purs_any_cons(int tag, int size, ANY* values) {
	ANY v;
	v.tag = PURS_ANY_TAG_CONS;
	v.value.cons = purs_malloc(sizeof (purs_any_cons_t));
	v.value.cons->tag = tag;
	v.value.cons->size = size;
	v.value.cons->values = values;
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

// -----------------------------------------------------------------------------
// Scopes
// -----------------------------------------------------------------------------

static void purs_scope_free(const struct purs_rc *ref) {
	struct purs_scope * x = container_of(ref, struct purs_scope, rc);
	for (int i = 0; i < x->size; i++) {
		PURS_ANY_RELEASE(&(x->bindings[i]));
	}
	purs_free(x->bindings);
	purs_free(x);
}

struct purs_scope * purs_scope_new1(int size) {
	if (size == 0) return NULL;
	struct purs_scope * scope = purs_new(struct purs_scope);
	ANY* bindings = purs_malloc(sizeof (ANY) * size);
	scope->size = size;
	scope->bindings = bindings;
	memset(bindings, 0, sizeof (ANY) * size); /* todo: calloc? */
	scope->rc = ((struct purs_rc) { purs_scope_free, 1 });
	return scope;
}

struct purs_scope * purs_scope_new(int size, ...) {
	if (size == 0) return NULL;
	struct purs_scope * scope = purs_new(struct purs_scope);
	ANY* bindings = purs_malloc(sizeof (ANY) * size);
	scope->size = size;
	scope->bindings = bindings;
	int i;
	va_list ap;
	va_start(ap, size);
	for (i = 0; i < size; i++) {
		bindings[i] = va_arg(ap, ANY);
		PURS_ANY_RETAIN(&bindings[i]);
	}
	va_end(ap);
	scope->rc = ((struct purs_rc) { purs_scope_free, 1 });
	return scope;
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

int purs_any_eq(ANY x, ANY y) {
	x = purs_any_unthunk(x, NULL);
	y = purs_any_unthunk(y, NULL);

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
	x = purs_any_unthunk(x, NULL);
	y = purs_any_unthunk(y, NULL);

	assert(x.tag != PURS_ANY_TAG_NULL);
	assert(y.tag != PURS_ANY_TAG_NULL);
	assert(x.tag == y.tag);

	switch(x.tag) {
	case PURS_ANY_TAG_STRING: {
		return purs_any_string(purs_str_new("%s%s",
						    purs_any_get_string(x)->data,
						    purs_any_get_string(y)->data));
	}
	case PURS_ANY_TAG_ARRAY: {
		const purs_vec_t * x_vec = purs_any_get_array(x);
		const purs_vec_t * y_vec = purs_any_get_array(y);
		return purs_any_array(purs_vec_concat(x_vec, y_vec));
	}
	default:
		purs_assert(0, "cannot concat %s", purs_any_tag_str(x.tag));
	}
}

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

static void purs_str_free(const struct purs_rc *ref) {
	purs_str_t * x = container_of(ref, purs_str_t, rc);
	free(x->data); /* do not use 'purs_free' ! */
	purs_free(x);
}

const purs_str_t * purs_str_new(const char * fmt, ...) {
	va_list ap;
	purs_str_t * x = purs_new(purs_str_t);
	x->rc = (struct purs_rc) { purs_str_free, 1 };
	va_start(ap, fmt);
	assert (vasprintf(&x->data, fmt, ap) >= 0);
	va_end(ap);
	return (const purs_str_t *) x;
}

// -----------------------------------------------------------------------------
// arrays
// -----------------------------------------------------------------------------

static void purs_vec_free(const struct purs_rc *ref) {
	purs_vec_t * x = container_of(ref, purs_vec_t, rc);
	int i;
	ANY v;
	purs_vec_foreach(x, v, i) {
		PURS_ANY_RELEASE(&v);
	}
	vec_deinit(x);
	purs_free(x);
}

static inline purs_vec_t * purs_vec_new() {
	purs_vec_t * o = purs_new(purs_vec_t);
	o->data = NULL;
	o->length = 0;
	o->capacity = 0;
	o->rc = (struct purs_rc) { purs_vec_free, 1 };
	return o;
}

#define purs_vec_empty NULL
#define purs_vec_is_empty(V) (V == NULL || V->length == 0)

const purs_vec_t * purs_vec_concat(const purs_vec_t * lhs,
				   const purs_vec_t * rhs) {
	if (purs_vec_is_empty(lhs)) {
		if (rhs != NULL) {
			PURS_RC_RETAIN(rhs);
			return rhs;
		}
		return NULL;
	} else if (purs_vec_is_empty(rhs)) {
		if (lhs != NULL) {
			PURS_RC_RETAIN(lhs);
			return lhs;
		}
		return NULL;
	} else {
		int length = lhs->length + rhs->length;
		purs_vec_t * o = purs_vec_new();
		o->data = vec_malloc(sizeof (ANY) * length);
		o->length = length;
		o->capacity = length;
		memcpy(o->data, lhs->data, sizeof (ANY) * lhs->length);
		memcpy(o->data + lhs->length, rhs->data, sizeof (ANY) * rhs->length);
		for (int i = 0; i < o->length; i++) {
			PURS_ANY_RETAIN(&o->data[i]);
		}
		return o;
	}
}

const purs_vec_t * purs_vec_new_va (int count, ...) {
	if (count <= 0) {
		return NULL;
	}

	purs_vec_t * o = purs_vec_new();

	o->data = vec_malloc(sizeof (ANY) * count);
	o->length = count;
	o->capacity = count;

	va_list ap;
	va_start(ap, count);
	for (int i = 0; i < count; i++) {
		o->data[i] = va_arg(ap, ANY);
		PURS_ANY_RETAIN(&o->data[i]);
	}
	va_end(ap);

	return (const purs_vec_t *) o;
}

static const purs_vec_t * _purs_vec_copy (const purs_vec_t * vec) {
	if (purs_vec_is_empty(vec)) {
		return NULL;
	} else {
		purs_vec_t * o = purs_vec_new();
		o->length = vec->length;
		o->capacity = vec->capacity;
		o->data = vec_malloc(sizeof (ANY) * vec->capacity);
		memcpy(o->data, vec->data, sizeof (ANY) * vec->capacity);
		return (const purs_vec_t *) o;
	}
}

const purs_vec_t * purs_vec_copy (const purs_vec_t * vec) {
	const purs_vec_t * copy = _purs_vec_copy(vec);
	for (int i = 0; i < copy->length; i++) {
		PURS_ANY_RETAIN(&copy->data[i]);
	}
	return copy;
}

const purs_vec_t * purs_vec_splice (const purs_vec_t * vec,
				    int start,
				    int count) {
	/* todo: avoid copying input array */
	purs_vec_t * copy = (purs_vec_t *) _purs_vec_copy(vec);
	vec_splice(copy, start, count);
	for (int i = 0; i < copy->length; i++) {
		PURS_ANY_RETAIN(&copy->data[i]);
	}
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
		PURS_ANY_RETAIN(&val);
		return (const purs_vec_t *) out;
	}
}

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

static inline void _purs_record_add_multi_mut(purs_record_t * x, int count, va_list ap);

ANY purs_record_empty = PURS_ANY_RECORD(NULL);

static void purs_record_free(const struct purs_rc *ref) {
	purs_record_t * x = container_of(ref, purs_record_t, rc);
	const purs_record_node_t * e, * tmp;
	HASH_ITER(hh, x->root, e, tmp) {
		PURS_ANY_RELEASE(&e->value);
		HASH_DEL(x->root, (purs_record_node_t *) e);
		purs_free((purs_record_node_t *) e);
	}
	purs_free(x);
}

/* construct a new record from key/value pairs, e.g.:
 * > purs_record_new_va(2, "foo", foo, "bar", bar);
 */
const purs_record_t * purs_record_new_va(int count, ...) {
	purs_record_t * x = purs_new(purs_record_t);
	x->root = NULL;
	va_list ap;
	va_start(ap, count);
	_purs_record_add_multi_mut(x, count, ap);
	va_end(ap);
	x->rc = ((struct purs_rc) { purs_record_free, 1 });
	return (const purs_record_t *) x;
}

/* create a shallow copy of the record
 */
const purs_record_t * purs_record_copy_shallow(const purs_record_t * source) {
	const purs_record_node_t * src, * tmp;
	purs_record_t * x = purs_new(purs_record_t);
	x->root = NULL;
	HASH_ITER(hh, source->root, src, tmp) {
		purs_record_node_t * dst = purs_new(purs_record_node_t);
		dst->key = afmt("%s", src->key); /* todo: perf */
		dst->value = src->value;
		PURS_ANY_RETAIN(&dst->value);
		HASH_ADD_KEYPTR(
			hh,
			x->root,
			dst->key,
			utf8size(dst->key),
			dst
		);
	}
	x->rc = ((struct purs_rc) { purs_record_free, 1 });
	return (const purs_record_t *) x;
}

const purs_record_t * purs_record_add_multi(const purs_record_t * source,
					    size_t count,
					    ...) {
	if (count == 0) {
		PURS_RC_RETAIN(source);
		return source;
	}

	purs_record_t * copy = (purs_record_t *) purs_record_copy_shallow(source);
	va_list args;
	va_start(args, count);
	_purs_record_add_multi_mut(copy, count, args);
	va_end(args);
	return (const purs_record_t *) copy;
}

static inline
void _purs_record_add_multi_mut(purs_record_t * x,
				int count,
				va_list ap) {
	for (int i = 0; i < count; i++) {
		const char * key = va_arg(ap, const char *);
		ANY value = va_arg(ap, ANY);
		purs_record_node_t * entry = purs_new(purs_record_node_t);
		entry->key = afmt("%s", key); /* todo: perf */
		entry->value = value;
		PURS_ANY_RETAIN(&value);
		HASH_ADD_KEYPTR(
			hh,
			x->root,
			entry->key,
			utf8size(entry->key),
			entry
		);
	}
}

ANY * purs_record_find_by_key(const purs_record_t * record,
			      const void * key) {
	purs_record_node_t * result;
	size_t len = utf8size(key);
	HASH_FIND(hh, record->root, key, len, result);
	if (result == NULL) return NULL;
	return &result->value;
}
