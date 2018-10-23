#include <purescript.h>

PURS_FFI_VALUE(Main_fnil, PURS_ANY_ARRAY(NULL));

PURS_FFI_FUNC_2(Main_fcons, _hd, _tl, {
	return purs_any_concat(purs_any_array_new(purs_vec_new_va(1, _hd)), _tl);
});

PURS_FFI_FUNC_3(Main_fappend, _, _left, _right, {
	return purs_any_concat(_left, _right);
});

PURS_FFI_FUNC_2(Main_fflatten, _, _v, {
	const purs_vec_t * v = purs_any_get_array(_v);
	int i;
	const purs_any_t * tmp;
	const purs_any_t * o = purs_any_array_new(purs_vec_new());
	purs_vec_foreach(v, tmp, i) {
		o = purs_any_concat(o, tmp);
	}
	return o;
});

PURS_FFI_FUNC_1(Main_ftoArray, x, {
	return x;
});
