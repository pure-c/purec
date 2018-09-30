#include <purescript.h>

inline const ANY * purs_any_cont_new(const void * ctx, purs_any_fun_t * fn) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_CONT;
	v->value.cont.fn = fn;
	v->value.cont.ctx = ctx;
	return v;
}

inline const ANY * purs_any_num_new(const purs_any_num_t n) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_NUM;
	v->value.n = n;
	return v;
}

inline const ANY * purs_any_int_new(const purs_any_int_t i) {
	ANY * v = purs_new(ANY);
	v->tag = PURS_ANY_TAG_INT;
	v->value.i = i;
	return v;
}
