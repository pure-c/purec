#ifndef Example2_H
#define Example2_H

#include <purescript.h>

PURS_FFI_FUNC_1(Example2_runGC, _, {
	GC_gcollect();
	return NULL;
})

PURS_FFI_FUNC_2(Example2_usleep, x, _, {
	usleep(*purs_any_get_int(x));
	return NULL;
})

PURS_FFI_FUNC_1(Example2_unsafeCoerce, x, {
	return x;
})

PURS_FFI_FUNC_2(Example2_consoleLog, _s, _, {
	const void * s = purs_any_get_string(_s)->data;
	printf("%s\n", s);
	return NULL;
})

#endif // Example2_H
