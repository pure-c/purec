#include <purescript.h>

PURS_FFI_FUNC_UNCURRIED_3(Main_add3, _a, _b, _c, {
	return purs_any_string_new("%s%s%s",
				   purs_any_get_string(_a),
				   purs_any_get_string(_b),
				   purs_any_get_string(_c));
});
