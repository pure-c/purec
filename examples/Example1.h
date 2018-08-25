#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"

// TODO: Implement https://github.com/purescript/purescript-prelude/blob/7a691ce2658bd8eaf28439391e29506dd154fb3d/src/Data/Show.js#L29-L51
PURS_FFI_FUNC_DEF(Example1_showStringImpl, x, {
	return PURS_ANY_STRING(
		afmt("\"%s\"", purs_any_get_string(x)->data));
})

PURS_FFI_FUNC_DEF(Example1_showIntImpl, x, {
	return PURS_ANY_STRING(
		afmt("%d", * purs_any_get_int(x)));
})

#define purs_any_string_concat(u, v)\
	PURS_ANY_STRING(\
		afmt("%s%s",\
		     purs_any_get_string(u)->data,\
		     purs_any_get_string(v)->data))

PURS_FFI_FUNC_DEF(Example1_concatStringImpl, x, {
	return PURS_FFI_LAMBDA(y, {
		return purs_any_string_concat(x, y);
	});
})

PURS_FFI_FUNC_DEF(Example1_mapArrayImpl, f, {
	return PURS_FFI_LAMBDA(xs, {
		const purs_vec_t * zs = purs_any_get_array(xs);
		const purs_vec_t * out = purs_vec_copy(zs);
		const purs_any_t * tmp;
		int i;
		purs_vec_foreach(out, tmp, i) {
			out->data[i] = purs_any_app(f, tmp);
		}
		return PURS_ANY_ARRAY(out);
	});
})

PURS_FFI_FUNC_DEF(Example1_showArrayImpl, f, {
	return PURS_FFI_LAMBDA(xs, {
		const purs_vec_t * zs = purs_any_get_array(xs);
		const purs_any_t * tmp;
		const managed_utf8str_t * tmp_s;
		int i;
		char * out;
		char * tmp_out;

		purs_vec_foreach(zs, tmp, i) {
			tmp_s = purs_any_get_string(purs_any_app(f, tmp));
			tmp_out = out;

			if (i == 0) {
				if (i == zs->length - 1) {
					out = afmt("[]");
				} else {
					out = afmt("[%s, ", tmp_s->data);
				}
			} else {
				if (i == zs->length - 1) {
					out = afmt("%s%s]", out, tmp_s->data);
				} else {
					out = afmt("%s%s, ", out, tmp_s->data);
				}
			}

			if (tmp_out != NULL) {
				free(tmp_out);
			}
		}

		return PURS_ANY_STRING(out);
	});
})

#endif // Example1_FFI_H
