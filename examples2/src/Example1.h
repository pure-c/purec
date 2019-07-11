#ifndef Example1_H
#define Example1_H

#include <purescript.h>

PURS_FFI_FUNC_2(Example1_putStr, s, _, {
	printf("%s", purs_any_get_string(s));
	return purs_any_null;
});

#endif // Example1_H
