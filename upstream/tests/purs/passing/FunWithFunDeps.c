#include <purescript.h>

PURS_FFI_VALUE(Main_fnil, PURS_ANY_ARRAY(NULL));

PURS_FFI_FUNC_2(Main_fcons, _head, _tail) {
	const purs_vec_t *vec = purs_vec_new_va(1, _head);
	purs_any_t ret = purs_any_concat(purs_any_array(vec), _tail);
	PURS_RC_RELEASE(vec);
	return ret;
}

PURS_FFI_FUNC_3(Main_fappend, _, _left, _right) {
	return purs_any_concat(_left, _right);
}

PURS_FFI_FUNC_2(Main_fflatten, _, _v) {
	const purs_vec_t *v = purs_any_force_array(_v);
	int i;
	purs_any_t tmp;
	purs_any_t o = purs_any_array(NULL);
	purs_vec_foreach(v, tmp, i) {
		o = purs_any_concat(o, tmp);
	}
	PURS_RC_RELEASE(v);
	return o;
}

PURS_FFI_FUNC_1(Main_ftoArray, x) {
	PURS_ANY_RETAIN(x);
	return x;
}
