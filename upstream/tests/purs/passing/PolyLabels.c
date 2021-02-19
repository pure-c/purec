#include <purescript.h>

PURS_FFI_FUNC_2(Main_unsafeGet, _s, _o) {
	const purs_record_t *o = purs_any_force_record(_o);
	const purs_str_t *s = purs_any_force_string(_s);
	purs_any_t *v = purs_record_find_by_key(o, s->data);
	assert(v != NULL);
	PURS_ANY_RETAIN(*v);
	PURS_RC_RELEASE(o);
	PURS_RC_RELEASE(s);
	return *v;
}

PURS_FFI_FUNC_3(Main_unsafeSet, _s, _a, _o) {
	const purs_record_t *o = purs_any_force_record(_o);
	const purs_str_t *s = purs_any_force_string(_s);
	const purs_record_t *result = purs_record_add_multi(o, 1, s->data, _a);
	PURS_RC_RELEASE(o);
	PURS_RC_RELEASE(s);
	return purs_any_record(result);
}
