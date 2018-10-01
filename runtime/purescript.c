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

inline const ANY * purs_any_thunk_new(purs_any_thunk_t * thunk) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_THUNK;
	v->value.thunk = thunk;
	return v;
}

const ANY * purs_any_cons_new(int tag, const ANY ** values) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_CONS;
	v->value.cons.tag = tag;
	v->value.cons.values = values;
	return v;
}

const ANY * purs_any_record_new(const purs_record_t * record) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_RECORD;
	v->value.record = record;
	return v;
}

inline const ANY * purs_any_string_new(const char * s) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_STRING;
	v->value.str = managed_new(afmt("%s", s), NULL);
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
		purs_assert(TAG == v->tag, "expected tag: %s, but got: %s",\
			    purs_any_tag_str(TAG),\
			    purs_any_tag_str(v->tag));\
	} while (0)


const purs_any_int_t purs_any_get_int (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_INT);
	return v->value.i;
}

const purs_any_num_t purs_any_get_num (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_NUM);
	return v->value.n;
}

const purs_cont_t * purs_any_get_cont (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONT);
	return (const purs_cont_t *) &v->value.cont;
}

const purs_cons_t * purs_any_get_cons (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CONS);
	return (const purs_cons_t *) &v->value.cons;
}

const purs_record_t * purs_any_get_record (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_RECORD);
	return v->value.record;
}

const void * purs_any_get_string (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_STRING);
	return v->value.str->data;
}

const utf8_int32_t purs_any_get_char (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_CHAR);
	return v->value.chr;
}

const purs_vec_t * purs_any_get_array (const ANY * v) {
	_PURS_ASSERT_TAG(PURS_ANY_TAG_ARRAY);
	return v->value.array;
}

const purs_foreign_t * purs_any_get_foreign (const ANY * v) {
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
		tmp = out->value.thunk(NULL, NULL);
		purs_assert(tmp != out, "infinite unthunk loop");
		out = tmp;
	}
	return (const ANY *) out;
}

inline const ANY * purs_any_app(const ANY * f, const ANY * v) {
	f = purs_any_unthunk(f);
	if (f->tag == PURS_ANY_TAG_CONT) {
		return f->value.cont.fn(f->value.cont.ctx, v);
	}
	return NULL;
}

// -----------------------------------------------------------------------------
// Any: built-in functions
// -----------------------------------------------------------------------------

inline int purs_any_eq_char (const ANY * x, utf8_int32_t y) {
	return purs_any_get_char(x) == y;
}

inline int purs_any_eq_string (const ANY * x, const void * str) {
	return utf8cmp(purs_any_get_string(x), str) == 0;
}

inline int purs_any_eq_int (const ANY * x, purs_any_int_t y) {
	return purs_any_get_int(x) == y;
}

inline int purs_any_eq_number (const ANY * x, double y) {
	return purs_any_get_number(x) == y;
}

/**
 Concatenate two dyanmic values into a new dynamic value
*/
const ANY * purs_any_concat(const ANY * x, const ANY * y) {
	x = purs_any_unthunk(x);
	y = purs_any_unthunk(y);

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
				afmt("%s%s",
				     purs_any_get_string(x),
				     purs_any_get_string(y)));
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

// -----------------------------------------------------------------------------
// Code-gen helpers
// -----------------------------------------------------------------------------

inline int purs_cons_get_tag (const purs_cons_t * cons) {
	return cons->tag;
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
