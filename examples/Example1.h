#ifndef Example1_FFI_H
#define Example1_FFI_H

#include "runtime/purescript.h"

PURS_FFI_FUNC_1(Example1_runGC, _, {
	GC_gcollect();
})

PURS_FFI_FUNC_2(Example1_usleep, x, _, {
	usleep(*purs_any_get_int(x));
})

PURS_FFI_FUNC_1(Example1_unsafeCoerce, x, {
	return x;
})

PURS_FFI_FUNC_2(Example1_consoleLog, _s, _, {
	const void * s = purs_any_get_string(_s)->data;
	printf("%s\n", s);
	return NULL;
})

#endif // Example1_FFI_H
