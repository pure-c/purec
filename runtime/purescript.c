#include <gc/gc.h>
#include "Block.h"
#include "runtime/purescript.h"

// -----------------------------------------------------------------------------
// managed data: garbage collected data
// -----------------------------------------------------------------------------

const managed_t * managed_new (void * data, const managed_release_func release) {
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

const managed_block_t * managed_block_new (void * block) {
	return managed_new(block, managed_block_release);
}

// -----------------------------------------------------------------------------
// managed utf8 strings
// -----------------------------------------------------------------------------
void managed_utf8str_release (managed_t * data) {
	free(data);
}

const managed_utf8str_t * managed_utf8str_new (void * data) {
	return managed_new(data, managed_utf8str_release);
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

const purs_cons_t * purs_any_get_cons (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == CONS) {
		return & x->value.cons;
	} else {
		return NULL;
	}
}

const abs_t purs_any_get_abs (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS) {
		return (abs_t)(x->value.fn);
	} else {
		return NULL;
	}
}

const int * purs_any_get_int (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == INT) {
		return &x->value.num_int;
	} else {
		return NULL;
	}
}

const float * purs_any_get_float (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == FLOAT) {
		return &x->value.num_float;
	} else {
		return NULL;
	}
}

const managed_block_t * purs_any_get_abs_block (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == ABS_BLOCK) {
		return (managed_block_t *) x->value.block;
	} else {
		return NULL;
	}
}

const managed_utf8str_t * purs_any_get_string (const purs_any_t * x) {
	x = purs_any_unthunk(x);
	if (x->tag == STRING) {
		return (const managed_block_t *) x->value.string;
	} else {
		return NULL;
	}
}

#define PURS_ANY_SET_IMPL(_name, _type, _tag, _key) \
	purs_any_t * _name (purs_any_t * any, _type val) { \
		any->tag = _tag; \
		any->value._key = val; \
		return any; \
	}

PURS_ANY_SET_IMPL(purs_any_set_abs, const abs_t, ABS, fn)
PURS_ANY_SET_IMPL(purs_any_set_abs_block, const managed_block_t *, ABS_BLOCK, block)
PURS_ANY_SET_IMPL(purs_any_set_float, float, FLOAT, num_float)
PURS_ANY_SET_IMPL(purs_any_set_int, int, INT, num_int)
PURS_ANY_SET_IMPL(purs_any_set_cons, purs_cons_t, CONS, cons)
PURS_ANY_SET_IMPL(purs_any_set_string, const managed_utf8str_t *, STRING, string)

const int invalid_tag = 0;

const purs_any_t * purs_any_app (const purs_any_t * x, const purs_any_t * arg) {
	const void * f;
	const managed_block_t * b;

	x = purs_any_unthunk(x);

	b = purs_any_get_abs_block(x);
	if (b != NULL) {
		return ((abs_block_t) b->data)(arg);
	}

	f = purs_any_get_abs(x);
	if (f != NULL) {
		return ((abs_t) f)(arg);
	}

	assert(invalid_tag);
}

int purs_any_eq_string (const purs_any_t * x, void * str) {
	const managed_utf8str_t * a = purs_any_get_string(x);
	if (a != NULL) {
		return utf8cmp(a->data, str) == 0;
	}
	assert(invalid_tag);
}

int purs_any_eq_int (const purs_any_t * x, int y) {
	const int * a = purs_any_get_int(x);
	if (a != NULL) {
		return *a == y;
	}
	assert(invalid_tag);
}

int purs_any_eq_float (const purs_any_t * x, float y) {
	const float * a = purs_any_get_float(x);
	if (a != NULL) {
		return *a == y;
	}
	assert(invalid_tag);
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
