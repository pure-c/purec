#include <purescript.h>

PURS_FFI_FUNC_UNCURRIED_3(Main_add3, _a, _b, _c) {
	return purs_any_string(
		purs_str_new(
			"%s%s%s",
			purs_any_unsafe_get_string(_a)->data,
			purs_any_unsafe_get_string(_b)->data,
			purs_any_unsafe_get_string(_c)->data));
}
