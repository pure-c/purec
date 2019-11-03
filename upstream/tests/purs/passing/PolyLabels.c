#include <purescript.h>

PURS_FFI_FUNC_2(Main_unsafeGet, _s, _o) {
	const purs_record_t * o = purs_any_get_record(_o);
	const void * s = purs_any_get_string(_s);
	const purs_record_t * v = purs_record_find_by_key(o, s);
	assert(v != NULL);
	return v->value;
}

PURS_FFI_FUNC_3(Main_unsafeSet, _s, _a, _o) {
	const purs_record_t * o = purs_any_get_record(_o);
	const void * s = purs_any_get_string(_s);
	return purs_any_record_new(purs_record_add_multi(o, 1, s, _a));
}
