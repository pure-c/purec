#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"

#define purs_any_string_concat(u, v)\
	PURS_ANY_STRING(\
		afmt("%s%s",\
		     purs_any_get_string(u)->data,\
		     purs_any_get_string(v)->data))

PURS_FFI_FUNC_DEF_2(Example1_concatStringImpl, x, y, {
	return purs_any_string_concat(x, y);
})

PURS_FFI_FUNC_DEF_2(Example1_mapArrayImpl, f, xs, {
	const purs_vec_t * zs = purs_any_get_array(xs);
	const purs_vec_t * out = purs_vec_copy(zs);
	const purs_any_t * tmp;
	int i;
	purs_vec_foreach(out, tmp, i) {
		out->data[i] = purs_any_app(f, tmp);
	}
	return PURS_ANY_ARRAY(out);
})

#endif // Example1_FFI_H
