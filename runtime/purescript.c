#include "runtime/purescript.h"

// -----------------------------------------------------------------------------
// Tail-call optimization support
// -----------------------------------------------------------------------------

void purs_tco_state_init(purs_tco_state_t *tco, int size, ...) {
	tco->result = purs_any_null;
	tco->done = 0;
	va_list ap;
	va_start(ap, size);
	tco->scope = purs_scope_new_va(size, ap);
	va_end(ap);
}

// -----------------------------------------------------------------------------
// Continuations
// -----------------------------------------------------------------------------

static void purs_cont_free(const struct purs_rc *ref) {
	purs_cont_t * x = container_of(ref, purs_cont_t, rc);
	if (x->scope != NULL) PURS_RC_RELEASE(x->scope);
	purs_free(x);
}

const purs_cont_t * purs_cont_new(const purs_scope_t *scope,
				  purs_cont_fun_t *fn) {
	purs_cont_t * cont = purs_new(purs_cont_t);
	cont->fn = fn;
	cont->scope = scope;
	cont->rc = ((struct purs_rc) { purs_cont_free, 1 });
	if (scope != NULL) PURS_RC_RETAIN(scope);
	return (const purs_cont_t *) cont;
}

// -----------------------------------------------------------------------------
// foreign
// -----------------------------------------------------------------------------

static void purs_foreign_free(const struct purs_rc *ref) {
	purs_foreign_t * x = container_of(ref, purs_foreign_t, rc);
	if (x->finalize_cb) x->finalize_cb(x->tag, x->data);
	purs_free(x);
}

const purs_foreign_t * purs_foreign_new(void * tag,
					void * data,
					purs_foreign_finalizer finalize_cb) {
	purs_foreign_t * foreign = purs_new(purs_foreign_t);
	foreign->tag = tag;
	foreign->data = data;
	foreign->finalize_cb = finalize_cb;
	foreign->rc = ((struct purs_rc) { purs_foreign_free, 1 });
	return (const purs_foreign_t*) foreign;
}

// -----------------------------------------------------------------------------
// data constructors
// -----------------------------------------------------------------------------

static void purs_cons_free(const struct purs_rc *ref) {
	purs_cons_t *x = container_of(ref, purs_cons_t, rc);
	for (int i = 0; i < x->size; i++) {
		PURS_ANY_RELEASE(x->values[i]);
	}
	purs_free(x->values);
	purs_free(x);
}

const purs_cons_t * purs_cons_new(int tag, int size, ...) {
	purs_cons_t * cons = purs_new(purs_cons_t);
	cons->tag = tag;
	cons->size = size;
	if (size <= 0) {
		cons->values = NULL;
	} else {
	    cons->values = purs_malloc(sizeof (purs_any_t) * size);
	}
	va_list ap;
	va_start(ap, size);
	for (int i = 0; i < size; i++) {
		cons->values[i] = va_arg(ap, purs_any_t);
		PURS_ANY_RETAIN(cons->values[i]);
	}
	va_end(ap);
	cons->rc = ((struct purs_rc) { purs_cons_free, 1 });
	return (const purs_cons_t *) cons;
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
	purs_scope_t *x = container_of(ref, purs_scope_t, rc);
	for (int i = 0; i < x->size; i++) {
		PURS_ANY_RELEASE(x->bindings[i]);
	}
	purs_free(x->bindings);
	purs_free(x);
}

purs_scope_t * purs_scope_new1(int size) {
	if (size == 0) return NULL;
	purs_scope_t *scope = purs_new(purs_scope_t);
	purs_any_t *bindings = purs_malloc(sizeof (purs_any_t) * size);
	scope->size = size;
	scope->bindings = bindings;
	memset(bindings, 0, sizeof (purs_any_t) * size); /* todo: calloc? */
	scope->rc = ((struct purs_rc) { purs_scope_free, 1 });
	return scope;
}

purs_scope_t* purs_scope_new_va(int size, va_list ap) {
	if (size == 0) return NULL;
	purs_scope_t *scope = purs_new(purs_scope_t);
	purs_any_t *bindings = purs_malloc(sizeof (purs_any_t) * size);
	scope->size = size;
	scope->bindings = bindings;
	for (int i = 0; i < size; i++) {
		bindings[i] = va_arg(ap, purs_any_t);
		PURS_ANY_RETAIN(bindings[i]);
	}
	scope->rc = ((struct purs_rc) { purs_scope_free, 1 });
	return scope;
}

purs_scope_t* purs_scope_new(int size, ...) {
	va_list ap;
	va_start(ap, size);
	purs_scope_t *scope = purs_scope_new_va(size, ap);
	va_end(ap);
	return scope;
}

const purs_scope_t* purs_scope_extend(const purs_scope_t* scope, int count, ...) {
	purs_scope_t *copy = purs_scope_new1(scope->size + count);\
	memcpy(copy->bindings,
	       scope->bindings,
	       scope->size * sizeof (purs_any_t));
	for (int i = 0; i < copy->size; i++) {
		PURS_ANY_RETAIN(copy->bindings[i]);
	}
	va_list ap;
	va_start(ap, count);
	for (int i = 0; i < count; i++) {
		copy->bindings[scope->size + i] = va_arg(ap, purs_any_t);
		PURS_ANY_RETAIN(copy->bindings[scope->size + i]);
	}
	va_end(ap);
	return copy;
}

// -----------------------------------------------------------------------------
// Any: built-ins
// -----------------------------------------------------------------------------

purs_any_t purs_any_null = { .tag = PURS_ANY_TAG_NULL };
purs_any_t purs_any_array_empty = {
	.tag = PURS_ANY_TAG_ARRAY,
	.value = { .array = NULL }
};

purs_any_t purs_any_true = PURS_ANY_INT(1);
purs_any_t purs_any_false = PURS_ANY_INT(0);

purs_any_t purs_any_int_zero = PURS_ANY_INT(0);
purs_any_t purs_any_num_zero = PURS_ANY_NUM(0.0);

purs_any_t purs_any_int_one = PURS_ANY_INT(1);
purs_any_t purs_any_num_one = PURS_ANY_NUM(1.0);

purs_any_t purs_any_NaN = PURS_ANY_NUM(PURS_NAN);
purs_any_t purs_any_infinity = PURS_ANY_NUM(PURS_INFINITY);
purs_any_t purs_any_neg_infinity = PURS_ANY_NUM(-PURS_INFINITY);

int purs_any_eq(purs_any_t x, purs_any_t y) {
	int ret = 0;
	int x_has_changed;
	int y_has_changed;
	x = purs_any_unthunk(x, &x_has_changed);
	y = purs_any_unthunk(y, &y_has_changed);

	/* special treatment for NaN on LHS */
	if (purs_any_is_NaN(x) &&
		(y.tag == PURS_ANY_TAG_NUM || y.tag == PURS_ANY_TAG_INT)) {
		ret = 0;
		goto end;
	}

	/* special treatment for NaN on RHS */
	if (purs_any_is_NaN(y) &&
		(x.tag == PURS_ANY_TAG_NUM || x.tag == PURS_ANY_TAG_INT)) {
		ret = 0;
		goto end;
	}

	if (x.tag == PURS_ANY_TAG_NULL || y.tag == PURS_ANY_TAG_NULL) {
		ret = 0;
		goto end;
	} else {
		purs_assert(
			x.tag == y.tag,
			"Cannot eq %s with %s",
			purs_any_tag_str(x.tag),
			purs_any_tag_str(y.tag));

		switch (x.tag) {
		case PURS_ANY_TAG_INT:
			ret = purs_any_unsafe_get_int(x) == purs_any_unsafe_get_int(y);
			goto end;
		case PURS_ANY_TAG_NUM:
			ret = purs_any_unsafe_get_num(x) == purs_any_unsafe_get_num(y);
			goto end;
		case PURS_ANY_TAG_STRING:
			ret = (utf8cmp(purs_any_unsafe_get_string(x)->data,
				       purs_any_unsafe_get_string(y)->data) == 0);
			goto end;
		case PURS_ANY_TAG_CHAR:
			ret = purs_any_unsafe_get_char(x) == purs_any_unsafe_get_char(y);
			goto end;
		default:
			ret = 0;
			goto end;
		}
	}
 end:
	if (x_has_changed) PURS_ANY_RELEASE(x);
	if (y_has_changed) PURS_ANY_RELEASE(y);
	return ret;
}

/**
 Concatenate two dynamic values into a new dynamic value
*/
purs_any_t purs_any_concat(purs_any_t x, purs_any_t y) {
	purs_any_t ret;
	int x_has_changed = 0;
	int y_has_changed = 0;
	x = purs_any_unthunk(x, &x_has_changed);
	y = purs_any_unthunk(y, &y_has_changed);

	assert(x.tag != PURS_ANY_TAG_NULL);
	assert(y.tag != PURS_ANY_TAG_NULL);
	assert(x.tag == y.tag);

	switch(x.tag) {
	case PURS_ANY_TAG_STRING: {
		ret = purs_any_string(purs_str_new("%s%s",
						   purs_any_unsafe_get_string(x)->data,
						   purs_any_unsafe_get_string(y)->data));
		goto end;
	}
	case PURS_ANY_TAG_ARRAY: {
		const purs_vec_t *x_vec = purs_any_unsafe_get_array(x);
		const purs_vec_t *y_vec = purs_any_unsafe_get_array(y);
		ret = purs_any_array(purs_vec_concat(x_vec, y_vec));
		goto end;
	}
	default:
		purs_assert(0, "cannot concat %s", purs_any_tag_str(x.tag));
	}

	return purs_any_null /* never reached */;

 end:
	if (x_has_changed) PURS_ANY_RELEASE(x);
	if (y_has_changed) PURS_ANY_RELEASE(y);
	return ret;
}

// -----------------------------------------------------------------------------
// strings
// -----------------------------------------------------------------------------

static void purs_str_static_free(const struct purs_rc *ref) {
	purs_str_t *x = container_of(ref, purs_str_t, rc);
	purs_free(x);
}

static void purs_str_free(const struct purs_rc *ref) {
	purs_str_t *x = container_of(ref, purs_str_t, rc);
	free((void*)x->data); /* do not use 'purs_free' ! */
	purs_free(x);
}

const purs_str_t * purs_str_static_new(const char *str) {
	va_list ap;
	purs_str_t *x = purs_new(purs_str_t);
	x->rc = (struct purs_rc) { purs_str_static_free, 1 };
	x->data = str;
	x->flags = 0;
	return (const purs_str_t *) x;
}

const purs_str_t * purs_str_new(const char *fmt, ...) {
	va_list ap;
	purs_str_t *x = purs_new(purs_str_t);
	x->rc = (struct purs_rc) { purs_str_free, 1 };
	va_start(ap, fmt);
	assert (vasprintf((char**)&x->data, fmt, ap) >= 0);
	va_end(ap);
	return (const purs_str_t *) x;
}

// -----------------------------------------------------------------------------
// arrays
// -----------------------------------------------------------------------------

static void purs_vec_free(const struct purs_rc *ref) {
	purs_vec_t * x = container_of(ref, purs_vec_t, rc);
	int i;
	purs_any_t v;
	purs_vec_foreach(x, v, i) {
		PURS_ANY_RELEASE(v);
	}
	vec_deinit(x);
	purs_free(x);
}

const purs_vec_t * purs_vec_new1(int capacity) {
	purs_vec_t * o = purs_new(purs_vec_t);
	o->data = vec_malloc(sizeof (purs_any_t) * capacity);
	o->length = 0;
	o->capacity = capacity;
	o->rc = (struct purs_rc) { purs_vec_free, 1 };
	return o;
}

const purs_vec_t * purs_vec_new() {
	purs_vec_t * o = purs_new(purs_vec_t);
	o->data = NULL;
	o->length = 0;
	o->capacity = 0;
	o->rc = (struct purs_rc) { purs_vec_free, 1 };
	return o;
}

const purs_vec_t * purs_vec_concat(const purs_vec_t *lhs,
				   const purs_vec_t *rhs) {
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
		purs_vec_t *o = (purs_vec_t *) purs_vec_new();
		o->data = vec_malloc(sizeof (purs_any_t) * length);
		o->length = length;
		o->capacity = length;
		memcpy(o->data, lhs->data, sizeof (purs_any_t) * lhs->length);
		memcpy(o->data + lhs->length, rhs->data, sizeof (purs_any_t) * rhs->length);
		for (int i = 0; i < o->length; i++) {
			PURS_ANY_RETAIN(o->data[i]);
		}
		return o;
	}
}

const purs_vec_t * purs_vec_new_va (int count, ...) {
	if (count <= 0) {
		return NULL;
	}

	purs_vec_t * o = (purs_vec_t *) purs_vec_new();

	o->data = vec_malloc(sizeof (purs_any_t) * count);
	o->length = count;
	o->capacity = count;

	va_list ap;
	va_start(ap, count);
	for (int i = 0; i < count; i++) {
		o->data[i] = va_arg(ap, purs_any_t);
		PURS_ANY_RETAIN(o->data[i]);
	}
	va_end(ap);

	return (const purs_vec_t *) o;
}

static const purs_vec_t * _purs_vec_copy (const purs_vec_t * vec) {
	if (purs_vec_is_empty(vec)) {
		return NULL;
	}

	purs_vec_t * o = (purs_vec_t *) purs_vec_new();
	o->length = vec->length;
	o->capacity = vec->capacity;
	o->data = vec_malloc(sizeof (purs_any_t) * vec->capacity);
	memcpy(o->data, vec->data, sizeof (purs_any_t) * vec->capacity);
	return (const purs_vec_t *) o;
}

const purs_vec_t * purs_vec_copy (const purs_vec_t * vec) {
	const purs_vec_t * copy = _purs_vec_copy(vec);
	if (copy == NULL) return NULL /* empty */;
	for (int i = 0; i < copy->length; i++) {
		PURS_ANY_RETAIN(copy->data[i]);
	}
	return copy;
}

const purs_vec_t * purs_vec_splice (const purs_vec_t * vec,
				    int start,
				    int count) {
	/* todo: avoid copying input array */
	purs_vec_t *copy = (purs_vec_t *) _purs_vec_copy(vec);
	vec_splice(copy, start, count);
	for (int i = 0; i < copy->length; i++) {
		PURS_ANY_RETAIN(copy->data[i]);
	}
	return (const purs_vec_t *) copy;
}

const purs_vec_t * purs_vec_insert(const purs_vec_t * vec,
				   int idx,
				   purs_any_t val) {
	if (vec == NULL) {
		return purs_vec_new_va(1, val);
	} else {
		purs_vec_t * out = (purs_vec_t *) purs_vec_copy(vec);
		vec_insert(out, idx, val);
		PURS_ANY_RETAIN(val);
		return (const purs_vec_t *) out;
	}
}

// -----------------------------------------------------------------------------
// records
// -----------------------------------------------------------------------------

#define purs_str_force_len(STR) do {\
	purs_str_t *__str0 = (STR);\
	if ((__str0->flags & PURS_STR_HAS_LEN) != PURS_STR_HAS_LEN) {\
		__str0->data_len = utf8size(__str0->data);\
		__str0->flags |= PURS_STR_HAS_LEN;\
	}\
} while (0)

inline unsigned purs_str_len(const purs_str_t *s) {
	purs_str_force_len((purs_str_t*)s);
	return s->data_len;
}

#define purs_str_force_hash(STR) do {\
	purs_str_t *__str1 = (STR);\
	if ((__str1->flags & PURS_STR_HASHED) != PURS_STR_HASHED) {\
		purs_str_force_len(__str1);\
		HASH_VALUE(__str1->data, __str1->data_len, __str1->hash);\
		__str1->flags |= PURS_STR_HASHED;\
	}\
} while (0)

purs_any_t purs_any_record_empty = PURS_ANY_RECORD(NULL);

const purs_record_t* purs_record_merge(const purs_record_t* r1,
				       const purs_record_t* r2) {
	if (r1 == NULL && r2 == NULL) return NULL;
	if (r1 == NULL) return purs_record_copy_shallow(r2);
	if (r2 == NULL) return purs_record_copy_shallow(r1);
	purs_record_t *copy = (purs_record_t*) purs_record_copy_shallow(r2);
	const purs_record_node_t *e, *tmp;
	HASH_ITER(hh, r1->root, e, tmp) {
		purs_record_add_mut(copy, e->key, e->value);
	}
	return copy;
}

static void purs_record_free(const struct purs_rc *ref) {
	purs_record_t *x = container_of(ref, purs_record_t, rc);
	const purs_record_node_t *e, *tmp;
	HASH_ITER(hh, x->root, e, tmp) {
		HASH_DEL(x->root, (purs_record_node_t *) e);
		PURS_ANY_RELEASE(e->value);
		PURS_RC_RELEASE(e->key);
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
	purs_record_add_multi_mut_va(x, count, ap);
	va_end(ap);
	x->rc = ((struct purs_rc) { purs_record_free, 1 });
	return (const purs_record_t *) x;
}

/* create a shallow copy of the record */
const purs_record_t * purs_record_copy_shallow(const purs_record_t * source) {
	if (source == NULL) return NULL;
	const purs_record_node_t *src, *tmp;
	purs_record_t *x = purs_new(purs_record_t);
	x->root = NULL;
	HASH_ITER(hh, source->root, src, tmp) {
		// prepare copy
		purs_record_node_t *dst = purs_new(purs_record_node_t);
		dst->key = src->key;
		dst->value = src->value;

		// retain key and value
		PURS_RC_RETAIN(dst->key);
		PURS_ANY_RETAIN(dst->value);

		// fill hash and length cache
		purs_str_force_hash((purs_str_t*)dst->key);

		// insert into hash map
		HASH_ADD_KEYPTR_BYHASHVALUE(
			hh,
			x->root,
			dst->key->data,
			dst->key->data_len,
			dst->key->hash,
			dst);
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

	purs_record_t * copy;
	if (source == NULL) {
	    copy = purs_new(purs_record_t);
	    copy->root = NULL;
	    copy->rc = ((struct purs_rc) { purs_record_free, 1 });
	} else {
	    copy = (purs_record_t *) purs_record_copy_shallow(source);
	}
	va_list args;
	va_start(args, count);
	purs_record_add_multi_mut_va(copy, count, args);
	va_end(args);
	return (const purs_record_t *) copy;
}

const purs_record_t* purs_record_remove(const purs_record_t *record,
					const purs_str_t *key) {
	if (record == NULL) return NULL;
	purs_record_t *copy = (purs_record_t *) purs_record_copy_shallow(record);
	purs_record_remove_mut(copy, key);
	return copy;
}

void purs_record_remove_mut(purs_record_t *record,
			    const purs_str_t* key) {
	if (record == NULL) return;
	purs_record_node_t *result;

	// fill hash and length cache
	purs_str_force_hash((purs_str_t*)key);

	HASH_FIND_BYHASHVALUE(hh,
			      record->root,
			      key->data,
			      key->data_len,
			      key->hash,
			      result);
	if (result != NULL) {
		PURS_ANY_RELEASE(result->value);
		HASH_DEL(record->root, result);
		PURS_RC_RELEASE(result->key);
		purs_free((purs_record_node_t *) result);
	}
	return;
}

void purs_record_add_multi_mut(purs_record_t *x,
			       size_t count,
			       ...) {
	va_list ap;
	va_start(ap, count);
	purs_record_add_multi_mut_va(x, count, ap);
	va_end(ap);
}

void purs_record_add_multi_mut_va(purs_record_t *x,
				  size_t count,
				  va_list ap) {
	for (int i = 0; i < count; i++) {
		const purs_str_t *key = va_arg(ap, const purs_str_t *);
		purs_any_t value = va_arg(ap, purs_any_t);

		// fill hash and length cache
		purs_str_force_hash((purs_str_t*)key);

		purs_record_node_t *existing = NULL;
		HASH_FIND_BYHASHVALUE(
			hh,
			x->root,
			key->data,
			key->data_len,
			key->hash,
			existing);

		if (existing != NULL /* swap */) {
			PURS_ANY_RELEASE(existing->value);
			PURS_ANY_RETAIN(value);
			existing->value = value;
		} else {
			// prepare entry
			purs_record_node_t *entry = purs_new(purs_record_node_t);
			entry->key = key;
			entry->value = value;

			// retain value and key
			PURS_RC_RETAIN(key);
			PURS_ANY_RETAIN(value);

			HASH_ADD_KEYPTR_BYHASHVALUE(
				hh,
				x->root,
				key->data,
				key->data_len,
				key->hash,
				entry);
		}
	}
}

purs_any_t* purs_record_find_by_key(const purs_record_t *record,
				    const purs_str_t *key) {
	if (record == NULL) return NULL;
	purs_record_node_t *result;

	// fill hash and length cache
	purs_str_force_hash((purs_str_t*)key);

	HASH_FIND_BYHASHVALUE(hh,
			      record->root,
			      key->data,
			      key->data_len,
			      key->hash,
			      result);
	if (result == NULL) return NULL;
	return &result->value;
}
