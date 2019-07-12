#ifndef Main_H
#define Main_H

#include <purescript.h>

PURS_FFI_FUNC_2(Main_putStrLn, s, _, {
	printf("%s\n", purs_any_get_string(s));
	return purs_any_int_zero;
});

#endif // Main_H
