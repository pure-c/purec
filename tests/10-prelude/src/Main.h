#ifndef Main_H
#define Main_H

#include <purescript.h>

PURS_FFI_FUNC_2(Main_putStrLn, s_, _, {
	const purs_str_t * s = purs_any_force_string(s_);
	PURS_RC_RELEASE(s);
	return purs_any_int_zero;
});

#endif // Main_H
