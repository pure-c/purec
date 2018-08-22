#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"

// TODO: Implement https://github.com/purescript/purescript-prelude/blob/7a691ce2658bd8eaf28439391e29506dd154fb3d/src/Data/Show.js#L29-L51
PURS_FFI_FUNC(Example1_showStringImpl, x, {
	return PURS_ANY_STRING(
		afmt("\"%s\"", purs_any_get_string(x)->data));
})

PURS_FFI_FUNC(Example1_showIntImpl, x, {
	return PURS_ANY_STRING(
		afmt("%d", * purs_any_get_int(x)));
})

PURS_FFI_FUNC(Example1_concatStringImpl, x, {
	return PURS_FFI_LAMBDA(y, {
		return PURS_ANY_STRING(
			afmt("%s%s",
				 purs_any_get_string(x)->data,
				 purs_any_get_string(y)->data));
	});
})


#endif // Example1_FFI_H
